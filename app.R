# Oceanus Folk Influence Explorer Shiny App

library(shiny)
library(tidyverse)
library(jsonlite)
library(janitor)
library(lubridate)
library(tidygraph)
library(igraph)
library(ggraph)
library(DT)
library(plotly)
library(visNetwork)
library(factoextra)
library(FactoMineR)
library(fmsb)
library(forcats)
library(shinydashboard)
library(shinythemes)
library(treemapify)
library(cluster)
library(NbClust)
library(dbscan)
library(RColorBrewer)


# ===== Data Preprocessing =====
data_path <- "data/MC1_graph.json"
kg <- fromJSON(data_path)

nodes_tbl <- as_tibble(kg$nodes) %>% clean_names()
edges_tbl <- as_tibble(kg$links) %>% clean_names()

nodes_tbl <- nodes_tbl %>%
  mutate(release_date = as.integer(release_date)) %>%
  mutate(id = as.integer(id)) %>%
  distinct()

# --- ID Mapping ---
id_map <- tibble(id = nodes_tbl$id, index = seq_len(nrow(nodes_tbl)))

edges_tbl_mapped <- edges_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>% rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>% rename(to = index) %>%
  filter(!is.na(from), !is.na(to)) %>%
  mutate(from = as.integer(from), to = as.integer(to))

# --- Build Graph Object ---
graph <- tbl_graph(nodes = nodes_tbl, edges = edges_tbl_mapped, directed = TRUE)

# --- Define Node Supertypes ---
nodes_tbl <- nodes_tbl %>%
  mutate(
    supertype = case_when(
      node_type %in% c("Song", "Album") ~ "Work",
      node_type %in% c("Person") ~ "Individual",
      node_type %in% c("MusicalGroup") ~ "Group",
      node_type %in% c("RecordLabel") ~ "Organization",
      TRUE ~ "Other"
    )
  )

# Add edge_type into mapped table first BEFORE any joins
edges_tbl_mapped <- edges_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>% rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>% rename(to = index) %>%
  filter(!is.na(from), !is.na(to)) %>%
  mutate(from = as.integer(from), to = as.integer(to))

# Now define superedge using edge_type which is already in the table
edges_tbl_mapped <- edges_tbl_mapped %>%
  mutate(
    superedge = case_when(
      edge_type %in% c("ComposerOf", "LyricistOf", "ProducerOf", "RecordedBy", "PerformerOf") ~ "Contributes",
      edge_type %in% c("CoverOf", "DirectlySamples", "InterpolatesFrom", "LyricalReferenceTo") ~ "Collaborations",
      edge_type %in% c("DistributedBy") ~ "Business",
      edge_type %in% c("MemberOf") ~ "Membership",
      edge_type %in% c("InStyleOf") ~ "StyleInfluence",
      TRUE ~ "Other"
    )
  )

# --- Extract People and Works using supertype ---
people_tbl <- nodes_tbl %>%
  filter(supertype == "Individual") %>%
  select(person_id = id, name)

works_tbl <- nodes_tbl %>%
  filter(supertype == "Work") %>%
  select(work_id = id, release_date, genre, notable)

# --- Contribution Mapping based on superedge ---
created_links <- edges_tbl_mapped %>%
  filter(superedge == "Contributes") %>%
  left_join(id_map, by = c("from" = "index")) %>% rename(person_id = id) %>%
  left_join(id_map, by = c("to" = "index")) %>% rename(work_id = id)

# --- Artist-Work Mapping with Metadata ---
artist_works <- created_links %>%
  left_join(works_tbl, by = "work_id") %>%
  filter(!is.na(release_date))

max_release <- max(artist_works$release_date, na.rm = TRUE)

# --- Artists Profile Summary ---
artists_profile <- artist_works %>%
  group_by(person_id) %>%
  summarise(
    total_works = n(),
    notable_works = sum(notable, na.rm = TRUE),
    oceanus_folk_works = sum(genre == "Oceanus Folk", na.rm = TRUE),
    first_release = min(release_date, na.rm = TRUE),
    first_notable = suppressWarnings(min(release_date[notable == TRUE], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    first_notable = ifelse(is.infinite(first_notable), NA_integer_, first_notable),
    time_to_notability = ifelse(!is.na(first_notable), 
                                first_notable - first_release, 
                                max_release + 5 - first_release)
  )

# --- Collaboration Count using superedge ---
collaborations <- edges_tbl_mapped %>%
  filter(superedge == "Collaborations") %>%
  left_join(id_map, by = c("from" = "index")) %>%
  count(id, name = "collaborations") %>%
  rename(person_id = id)

# --- Compute Genre Diversity ---
genre_diversity_tbl <- artist_works %>%
  filter(!is.na(genre)) %>%
  group_by(person_id) %>%
  summarise(genre_diversity = n_distinct(genre), .groups = "drop")


# --- Final Profile Merge ---
artists_profile <- artists_profile %>%
  left_join(collaborations, by = "person_id") %>%
  left_join(genre_diversity_tbl, by = "person_id") %>%
  mutate(
    collaborations = replace_na(collaborations, 0),
    genre_diversity = replace_na(genre_diversity, 0)
  ) %>%
  inner_join(people_tbl, by = "person_id") %>%
  relocate(person_id, name)

# Precompute the necessary variables for clustering outside of the cluster analysis function.

cluster_data <- artists_profile %>%
  select(total_works, notable_works, oceanus_folk_works, collaborations, time_to_notability, genre_diversity) %>%
  na.omit() %>%
  scale()  # Standardize the variables



### ---- Get Sailor Shift ID ----
sailor_id <- nodes_tbl %>% 
  filter(str_detect(name, fixed("Sailor Shift", ignore_case = TRUE))) %>%
  pull(id)

# ---- Influence Edge Types ----
influence_types <- c("LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "DirectlySamples", "InStyleOf", 
                     "PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf", "RecordedBy")

# ==== Precompute Oceanus Folk Songs and Influence Edges ====
of_songs <- nodes_tbl %>%
  filter(node_type == "Song", genre == "Oceanus Folk")

influence_edges <- edges_tbl %>%
  filter(edge_type %in% influence_types)

# ---- Extract 1-hop Influence Edges ----
sailor_edges <- edges_tbl %>%
  filter(edge_type %in% influence_types, source == sailor_id | target == sailor_id)

# ---- Get unique node IDs involved ----
influence_ids <- unique(c(sailor_edges$source, sailor_edges$target))

vis_nodes <- nodes_tbl %>%
  filter(id %in% influence_ids) %>%
  mutate(
    label = name,
    group = ifelse(id == sailor_id, "Sailor Shift", node_type),
    color = ifelse(id == sailor_id, "darkblue", "skyblue")
  ) %>%
  select(id, label, group, color)

vis_edges <- sailor_edges %>%
  select(from = source, to = target, label = edge_type) 


# ===== UI =====
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = tags$strong(icon("music"), "FOLK FLOW")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Artists Profiles", tabName = "artists", icon = icon("users")),
      menuItem("Influence Network", icon = icon("project-diagram"), startExpanded = FALSE,
               menuSubItem("Who has Sailor influenced?", tabName = "network"),
               menuSubItem("Who has influenced Sailor?", tabName = "sailor_influencers"),
               menuSubItem("Oceanus Folk Inlfuence", tabName = "oceanus")),
      menuItem("Cluster Analysis", tabName = "cluster", icon = icon("layer-group")),
      menuItem("Future Predictions", tabName = "future", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("kpi_total_artists"),
                valueBoxOutput("kpi_total_notable"),
                valueBoxOutput("kpi_avg_time_to_notability"),
                valueBoxOutput("kpi_total_groups"),
                valueBoxOutput("kpi_avg_members"),
                valueBoxOutput("kpi_top_genre")
              ),
              fluidRow(
                box(title = "Year Range and Genre Selector", width = 2, status = "warning", solidHeader = TRUE,
                    sliderInput("donut_year_range", "Select Year Range:",
                                min = min(artist_works$release_date, na.rm = TRUE),
                                max = max(artist_works$release_date, na.rm = TRUE),
                                value = c(1990, 2000),
                                step = 5,
                                sep = ""),
                    selectizeInput("selected_genres", "Select up to 3 Genres:",
                                   choices = unique(artist_works$genre),
                                   selected = c("Oceanus Folk"),
                                   multiple = TRUE,
                                   options = list(maxItems = 3)),
                    div(style = "margin-top: 10px;",
                        actionButton("toggle_total_line", "Total Songs Overtime",
                                     class = "btn-info btn-block"))
                ),
                box(title = "Genre Percentage (Donut Chart)", width = 5, status = "warning", solidHeader = TRUE,
                    plotlyOutput("genreDonutPlot", height = "300px")
                ),
                box(title = "Genre Comparison Over Time", width = 5, status = "success", solidHeader = TRUE,
                    plotlyOutput("genreTimelinePlot", height = "300px"))
              ),
              fluidRow(
                box(title = "Artists Profile Table", width = 12, status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("overview_artists_table"))
              )
      ),
      
      tabItem(tabName = "eda",
              tabsetPanel(
                tabPanel("Edge Types", plotOutput("edgeTypePlot")),
                tabPanel("Node Types", plotOutput("nodeTypePlot")),
                
                tabPanel("Genre Trends",
                         fluidRow(
                           column(
                             width = 3,
                             tags$div(
                               class = "floating-panel",
                               box(
                                 title = "Select Year Range", width = NULL, solidHeader = TRUE, status = "primary",
                                 sliderInput("year_range", "Release Year Range:",
                                             min = min(nodes_tbl$release_date, na.rm = TRUE),
                                             max = max(nodes_tbl$release_date, na.rm = TRUE),
                                             value = c(min(nodes_tbl$release_date, na.rm = TRUE),
                                                       max(nodes_tbl$release_date, na.rm = TRUE)),
                                             sep = ""
                                 )
                               )
                             )
                           ),
                           column(
                             width = 9,
                             plotlyOutput("genreHeatmap")
                           )
                         )
                ),
                
                tabPanel("Notable Songs", plotOutput("notableSongsPlot"))
              )
      ),
      
      tabItem(tabName = "artists",
              fluidRow(
                box(
                  title = "Artist Filters", width = 3, status = "primary", solidHeader = TRUE,
                  sliderInput("artist_year_range", "Select Year Range:",
                              min = min(artist_works$release_date, na.rm = TRUE),
                              max = max(artist_works$release_date, na.rm = TRUE),
                              value = c(1975, 2040),
                              step = 5, sep = ""),
                  selectizeInput("selected_artists", "Select Artist(s) up to 3:",
                                 choices = sort(unique(artists_profile$name)),
                                 selected = c("Sailor Shift"), 
                                 multiple = TRUE,
                                 options = list(maxItems = 3)),
                  radioButtons("ego_hop", "Ego Network Hop Level:",
                               choices = c("1-hop", "2-hop"),
                               selected = "1-hop"),
                  selectInput("ranking_metric", "Rank Artists By:",
                              choices = c("Total Works", "Notable Works", "Oceanus Folk Involvement")),
                  numericInput("top_n_artists", "Top N Artists to Display:", value = 10, min = 1),
                  uiOutput("toggle_benchmark_ui"),
                  br(),
                  uiOutput("toggle_artist_table_ui"),
                ),
                box(
                  title = "Artist Development & Relationships", width = 9, status = "success", solidHeader = TRUE,
                  tabsetPanel(
                    tabPanel("Career Timeline", plotlyOutput("careerTimelinePlot", height = "500px")),
                    tabPanel("Artist Work Count", plotlyOutput("artistsWorkcountTimeline", height = "500px")),
                    tabPanel("Ego Network", visNetworkOutput("egoArtistNetwork", height = "500px")),
                    tabPanel("Radar Comparison", plotOutput("radarComparisonPlot", height = "500px"))
                  )
                )
              ),
              fluidRow(
                uiOutput("benchmark_table_box")
              ),
              fluidRow(
                uiOutput("artist_table_box")
              )
      ),
      
      tabItem(tabName = "network",
              fluidRow(
                div(class = "custom-box-green",
                    box(
                      width = 3,
                      title = "Filter Options",
                      solidHeader = TRUE,
                      selectInput("hop_level", "Select hop level:",
                                  choices = c("1-hop", "2-hop"), selected = "2-hop"),
                      selectizeInput("influence_types_selected", "Select influence types:",
                                     choices = influence_types,
                                     selected = influence_types,
                                     multiple = TRUE)
                    )
                ),
                div(class = "custom-box-green",
                    box(
                      width = 9,
                      title = "Sailor Shift Influence Network",
                      solidHeader = TRUE,
                      visNetworkOutput("dynamicSailorNetwork", height = "800px")
                    )
                )
              ))
      ,
      
      tabItem(tabName = "sailor_influencers",
              fluidRow(
                div(class = "custom-box-green",
                    box(
                      title = "Filter Influence Types",
                      width = 3,
                      status = "success",
                      solidHeader = TRUE,
                      checkboxGroupInput("edge_type_input", "Influence Type:",
                                         choices = influence_types,
                                         selected = influence_types)
                    )
                ),
                
                div(class = "custom-box-green",
                    box(
                      width = 9,
                      title = "Sailor Shift Influence Network",
                      solidHeader = TRUE,
                      plotOutput("ggraphSailorNetwork", height = "800px")
                    )
                    
                ))
      )
      ,
      tabItem(tabName = "oceanus",
              tabsetPanel(
                tabPanel("Genres Influenced by Oceanus Folk",
                         sidebarLayout(
                           sidebarPanel( width = 3 ,
                                         selectizeInput(
                                           inputId = "of_song_genre",
                                           label = "Select Genre:",
                                           choices = sort(unique(nodes_tbl$genre[nodes_tbl$node_type == "Song"])),
                                           selected = "Oceanus Folk",
                                           multiple = FALSE
                                         )
                           ),
                           mainPanel(
                             plotOutput("of_treemap", height = "700px")
                           )
                         )
                ),
                tabPanel("Top Artists Inlfuenced by Oceanus Folk",
                         sidebarLayout(
                           sidebarPanel(width = 3,
                                        selectInput("source_genre", "Select Source Genre:",
                                                    choices = sort(unique(nodes_tbl$genre[nodes_tbl$node_type == "Song"])),
                                                    selected = "Oceanus Folk"
                                        ),
                                        sliderInput("top_n", "Top N Influenced Artists:", min = 10, max = 35, value = 12),
                                        selectizeInput("influence_types", "Influence Edge Types:",
                                                       choices = influence_types,
                                                       selected = "LyricalReferenceTo",
                                                       multiple = TRUE,
                                                       options = list(placeholder = 'Select one or more edge types')
                                                       )
                           ),
                           mainPanel(
                             plotlyOutput("lollipopPlot", height = "800px")
                           )
                         )
                ),
                tabPanel("Genres Influenced Oceanus Folk",
                         sidebarLayout(
                           sidebarPanel(width = 3,
                                        sliderInput("min_influence", "Minimum Influence Count:",
                                                    min = 1, max = 10, value = 1),
                                        numericInput("top_n_genres", "Top N Genres to Display:", value = 10, min = 1, max = 20),
                                        selectInput("target_genre_network", "Target Genre (Node):",
                                                    choices = sort(unique(nodes_tbl$genre[nodes_tbl$node_type == "Song"])),
                                                    selected = "Oceanus Folk")
                           ),
                           mainPanel(
                             visNetworkOutput("genre_influence_net", height = "650px")
                           )
                         )
                )
                
              )),
      
      tabItem(tabName = "cluster",
              fluidRow(
                # Sidebar Box
                box(title = "Clustering Panel", width = 3, status = "info",
                    selectInput("cluster_vars", "Select Variables for Clustering:",
                                choices = c("total_works", "notable_works", "oceanus_folk_works", 
                                            "collaborations", "time_to_notability", "genre_diversity"),
                                selected = c("total_works", "notable_works", "oceanus_folk_works"),
                                multiple = TRUE),
                    radioButtons("cluster_method", "Clustering Method:",
                                 choices = c("K-means", "DBSCAN", "PAM"),
                                 selected = "K-means"),
                    conditionalPanel(
                      condition = "input.cluster_method == 'K-means' || input.cluster_method == 'PAM'",
                      sliderInput("n_clusters", "Number of Clusters:", 2, 8, value = 3)
                    ),
                    conditionalPanel(
                      condition = "input.cluster_method == 'DBSCAN'",
                      sliderInput("eps", "Epsilon (Neighborhood Radius):", 
                                  min = 0.1, max = 2, value = 0.5, step = 0.1),
                      numericInput("minPts", "Minimum Points:", value = 5, min = 2)
                    ),
                    actionButton("run_cluster", "Run Cluster Analysis")
                ),
                
                # Main Content Box
                box(title = "Cluster Analysis Results", width = 9, status = "success",
                    tabsetPanel(
                      tabPanel("Optimal Clusters",
                               plotOutput("elbowPlot" , height = "550px")
                      ),
                      tabPanel("Cluster Propotion",
                               plotlyOutput("clusterProportionPlot", height = "550px")
                      ),
                      tabPanel("Cluster Plot",
                               plotlyOutput("clusterPlot", height = "550px")
                      ),
                      tabPanel("Cluster Characteristics", 
                               plotlyOutput("clusterChars", height = "550px") 
                      )
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("aic_box", width = 3),
                valueBoxOutput("bic_box", width = 3),
                valueBoxOutput("lr_box", width = 3),
                valueBoxOutput("entropy_box", width = 3)
              )
      ),
      
      tabItem(tabName = "future",
              DT::dataTableOutput("futureStarsTable"),
              plotOutput("radar1"),
              plotOutput("radar2"),
              plotOutput("radar3")
      )
    )
  )
)

# ===== Server =====
server <- function(input, output, session) {
  
  # ================= Overview Page =======================
  
  # === Vale Boxes Calculations for Overview Page ===
  
  output$kpi_total_artists <- renderValueBox({
    valueBox(
      value = nrow(artists_profile),
      subtitle = "Total Artists",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$kpi_total_notable <- renderValueBox({
    valueBox(
      value = sum(artists_profile$notable_works, na.rm = TRUE),
      subtitle = "Total Notable Works",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$kpi_avg_time_to_notability <- renderValueBox({
    avg_time <- mean(artists_profile$time_to_notability, na.rm = TRUE)
    valueBox(
      value = round(avg_time, 1),
      subtitle = "Avg. Time to Notability (Years)",
      icon = icon("clock"),
      color = "teal"
    )
  })
  
  output$kpi_top_genre <- renderValueBox({
    top_genre <- artist_works %>%
      filter(!is.na(genre)) %>%
      count(genre, sort = TRUE) %>%
      slice_head(n = 1) %>%
      pull(genre)
    
    valueBox(
      value = top_genre,
      subtitle = "Most Frequent Genre",
      icon = icon("music"),
      color = "olive"
    )
  })
  
  output$kpi_total_groups <- renderValueBox({
    group_count <- nodes_tbl %>% filter(node_type == "MusicalGroup") %>% nrow()
    valueBox(
      value = group_count,
      subtitle = "Total Musical Groups",
      icon = icon("people-group"),
      color = "maroon"
    )
  })
  
  output$kpi_avg_members <- renderValueBox({
    member_links <- edges_tbl_mapped %>% filter(edge_type == "MemberOf")
    avg_members <- member_links %>%
      count(to) %>%
      summarise(avg = mean(n, na.rm = TRUE)) %>%
      pull(avg)
    valueBox(
      value = round(avg_members, 1),
      subtitle = "Avg. Members per Group",
      icon = icon("user-friends"),
      color = "fuchsia"
    )
  })
  
  # === End of Value Box ===
  
  # === Genre Donut, Genre Time Line Boxes ===
  
  # === Genre Donut ===
  output$genreDonutPlot <- renderPlotly({
    # Filter data by selected year range
    filtered <- artist_works %>%
      filter(release_date >= input$donut_year_range[1],
             release_date <= input$donut_year_range[2]) %>%
      filter(!is.na(genre))
    
    # Aggregate and simplify labels
    genre_counts <- filtered %>%
      count(genre, sort = TRUE) %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        label = genre,
        hover_label = paste0(genre, ": ", pct, "%")
      )
    
    # Optional: Group small genres into "Others"
    genre_counts <- genre_counts %>%
      mutate(group = ifelse(pct < 3, "Other", genre)) %>%
      group_by(group) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        hover_label = paste0(group, ": ", pct, "%")
      )
    
    # Plot
    plot_ly(genre_counts,
            labels = ~group,
            values = ~n,
            type = 'pie',
            hole = 0.5,
            textinfo = "none",  # hides cluttered labels
            hoverinfo = "text",
            text = ~hover_label,
            marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = list(text = "Genre Share by Selected Years", x = 0.5),
             showlegend = TRUE)
  })
  
  # === Genre Timeline Plot ===
  
  show_total <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_total_line, {
    show_total(!show_total())
  })
  
  output$genreTimelinePlot <- renderPlotly({
    req(input$donut_year_range, input$selected_genres)
    
    timeline_data <- artist_works %>%
      filter(release_date >= input$donut_year_range[1],
             release_date <= input$donut_year_range[2])
    
    genre_lines <- timeline_data %>%
      filter(genre %in% input$selected_genres) %>%
      count(genre, release_date) %>%
      mutate(type = "Genre")
    
    total_line <- timeline_data %>%
      count(release_date) %>%
      mutate(genre = "Total", type = "Total")
    
    combined_data <- genre_lines
    if (show_total()) {
      combined_data <- bind_rows(genre_lines, total_line)
    }
    
    p <- ggplot(combined_data, aes(x = release_date, y = n, color = genre, linetype = type)) +
      geom_line(size = 0.9) +
      geom_point(size = 1.5) +
      labs(title = "Song Releases Over Time",
           x = "Year", y = "Number of Songs", color = "Genre") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  output$overview_artists_table <- DT::renderDataTable({
    datatable(
      artists_profile,
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # === End of Plots Boxes ===
  
  
  # ================== End of Overview Page =====================
  
  
  # ==================== EDA Page ======================
  
  output$edgeTypePlot <- renderPlot({
    ggplot(edges_tbl_mapped, aes(y = edge_type)) +
      geom_bar() +
      labs(title = "Distribution of Edge Types", y = "Edge Type", x = "Count") +
      theme_minimal()
  })
  
  output$nodeTypePlot <- renderPlot({
    ggplot(nodes_tbl, aes(y = node_type)) +
      geom_bar() +
      labs(title = "Distribution of Node Types", y = "Node Type", x = "Count") +
      theme_minimal()
  })
  
  
  output$genreHeatmap <- renderPlotly({
    heatmap_data <- nodes_tbl %>%
      filter(node_type == "Song", !is.na(release_date),
             release_date >= input$year_range[1],
             release_date <= input$year_range[2]) %>%
      count(genre, release_date) %>%
      ungroup() %>%
      na.omit()
    
    p <- ggplot(heatmap_data,
                aes(x = release_date,
                    y = fct_rev(fct_infreq(genre)),
                    fill = n,
                    text = paste0("Genre: ", genre,
                                  "<br>Release Date: ", release_date,
                                  "<br>Number of Songs: ", n))) +
      geom_tile(color = "white", size = 1) +
      theme_minimal() +
      coord_equal() +
      scale_fill_gradient(name = "# of Songs", low = "skyblue", high = "darkblue") +
      labs(x = "Release Year", y = NULL, title = "Song Releases Over Time by Genre") +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 6),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6),
            legend.position = "top") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$notableSongsPlot <- renderPlot({
    nodes_tbl %>%
      filter(notable == TRUE & node_type == "Song") %>%
      count(genre, sort = TRUE) %>%
      ggplot(aes(x = reorder(genre, n), y = n)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_text(aes(label = n), hjust = -0.1, size = 2.5) +
      coord_flip() +
      labs(
        title = "Count of Notable Songs by Genre",
        x = "Genre",
        y = "Number of Notable Songs"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  # ================== End of EDA Page =====================
  
  
  # ================= Artists Profile Page =======================
  
  # === Artists Career Timeline Plot ===
  output$careerTimelinePlot <- renderPlotly({
    req(input$selected_artists)
    
    # Step 1: Prepare top 6 genres in selected subset
    top_genres <- artist_works %>%
      left_join(people_tbl, by = "person_id") %>%
      filter(name %in% input$selected_artists) %>%
      count(genre, sort = TRUE) %>%
      slice_head(n = 6) %>%
      pull(genre)
    
    levels_genre_rev <- sort(top_genres, decreasing = TRUE)
    
    # Step 2: Prepare filtered data
    artist_subset <- artist_works %>%
      left_join(people_tbl, by = "person_id") %>%
      filter(name %in% input$selected_artists,
             release_date >= input$artist_year_range[1],
             release_date <= input$artist_year_range[2],
             genre %in% top_genres) %>%
      mutate(
        genre = factor(genre, levels = levels_genre_rev),
        notable = as.logical(notable),
        label = paste0("Artist: ", name,
                       "<br>Year: ", release_date,
                       "<br>Genre: ", genre,
                       ifelse(notable, "<br><b>Notable</b>", ""))
      )
    
    # Step 3: Plot
    p <- ggplot(
      artist_subset,
      aes(x = release_date, y = name, shape = genre, color = notable, text = label)
    ) +
      geom_jitter(width = 0.3, height = 0.3, size = 3, alpha = 0.7) +
      scale_color_manual(values = c(`TRUE` = "#a1d99b", `FALSE` = "#fcbba1")) +
      labs(
        title  = "Career Timelines (Top Genres Sorted Z–A)",
        x = "Release Year",
        y = "Artist",
        shape = "Genre",
        color = "Notable"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  # === Artists Work Count Timeline Table ===
  output$artistsWorkcountTimeline <- renderPlotly({
    req(input$selected_artists)
    
    # Prepare data: count works by artist and release year
    count_data <- artist_works %>%
      left_join(people_tbl, by = "person_id") %>%
      filter(name %in% input$selected_artists,
             release_date >= input$artist_year_range[1],
             release_date <= input$artist_year_range[2]) %>%
      group_by(name, release_date) %>%
      summarise(work_count = n(), .groups = "drop")
    
    # Plot line chart of works per year
    p <- ggplot(count_data, aes(x = release_date, y = work_count, color = name)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Number of Works per Year",
        x = "Release Year",
        y = "Count",
        color = "Artist"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # === Ego Network for Artists === 
  output$egoArtistNetwork <- renderVisNetwork({
    req(input$selected_artists)
    
    # Step 1: Get artist nodes
    ego_artist_nodes <- nodes_tbl %>%
      filter(node_type == "Person", name %in% input$selected_artists) %>%
      left_join(id_map, by = c("id" = "id")) %>%
      select(artist_name = name, artist_id = id, index)
    
    ego_ids <- ego_artist_nodes$artist_id
    req(length(ego_ids) > 0)
    
    # Step 2: Subset the graph manually using edges
    all_edges <- as_tibble(edges_tbl_mapped)
    if (input$ego_hop == "2-hop") {
      # First hop
      first_hop <- all_edges %>% filter(source %in% ego_ids | target %in% ego_ids)
      first_hop_ids <- unique(c(first_hop$source, first_hop$target))
      
      # Second hop
      second_hop <- all_edges %>% filter(source %in% first_hop_ids | target %in% first_hop_ids)
      second_hop_ids <- unique(c(second_hop$source, second_hop$target))
      
      final_ids <- unique(c(ego_ids, first_hop_ids, second_hop_ids))
      final_edges <- all_edges %>% filter(source %in% final_ids & target %in% final_ids)
    } else {
      final_edges <- all_edges %>% filter(source %in% ego_ids | target %in% ego_ids)
      final_ids <- unique(c(final_edges$source, final_edges$target, ego_ids))
    }
    
    # Step 3: Filter nodes
    nodes_data <- nodes_tbl %>%
      filter(id %in% final_ids) %>%
      mutate(
        label = ifelse(!is.na(name), name, node_type),
        group = node_type,
        title = paste0("<b>", label, "</b><br>Type: ", node_type)
      ) %>%
      select(id, label, group, title)
    
    # Step 4: Prepare edges data
    edges_data <- final_edges %>%
      mutate(
        from = source,
        to = target,
        title = paste("Edge Type:", edge_type),
        label = edge_type
      ) %>%
      select(from, to, label, title)
    
    # Step 5: Render visNetwork
    visNetwork(nodes_data, edges_data, height = "500px", width = "100%") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = TRUE
      ) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLayout(randomSeed = 42) %>%
      visLegend(
        useGroups = TRUE,
        position = "right",
        main = "Node Type"
      )
  })
  
  # === Radar Plot ===
  show_benchmark_table <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_benchmark_btn, {
    show_benchmark_table(!show_benchmark_table())
  })
  
  output$radarComparisonPlot <- renderPlot({
    req(input$selected_artists)
    
    selected <- input$selected_artists
    radar_data <- artists_profile %>%
      filter(name %in% selected) %>%
      select(name, total_works, notable_works, oceanus_folk_works, collaborations, time_to_notability, genre_diversity) %>%
      column_to_rownames("name") %>%
      as.data.frame()
    
    radar_data$time_to_notability <- max(radar_data$time_to_notability, na.rm = TRUE) - radar_data$time_to_notability
    
    radar_data_scaled <- as.data.frame(lapply(radar_data, function(x) scales::rescale(x, to = c(0, 100))))
    rownames(radar_data_scaled) <- rownames(radar_data)
    
    if (all(rowSums(radar_data_scaled, na.rm = TRUE) == 0)) {
      plot.new()
      title("Radar data unavailable for selected artist(s).")
      return()
    }
    
    par(mfrow = c(1, min(nrow(radar_data_scaled), 3)), mar = c(2, 2, 4, 2))
    
    for (i in 1:nrow(radar_data_scaled)) {
      artist_name <- rownames(radar_data_scaled)[i]
      artist_values <- radar_data_scaled[i, , drop = FALSE]
      
      if (all(is.na(artist_values)) || all(artist_values == 0)) next
      
      radar_individual <- rbind(rep(100, ncol(radar_data_scaled)), rep(0, ncol(radar_data_scaled)), artist_values)
      
      radarchart(radar_individual,
                 axistype = 1,
                 pcol = "darkorange",
                 pfcol = rgb(1, 0.5, 0, 0.3),
                 plwd = 2,
                 cglcol = "gray",
                 cglty = 1,
                 cglwd = 0.8,
                 axislabcol = "darkblue",
                 caxislabels = paste0(seq(0, 100, 25), "%"),
                 vlcex = 0.8,
                 title = artist_name,
                 calcex = 0.8,
                 cex.main = 1.2)
    }
  })
  
  output$benchmark_table <- DT::renderDataTable({
    req(input$selected_artists)
    benchmark_data <- artists_profile %>%
      filter(name %in% input$selected_artists) %>%
      select(name, total_works, notable_works, oceanus_folk_works, collaborations, time_to_notability, genre_diversity)
    
    datatable(
      benchmark_data,
      options = list(scrollX = TRUE, pageLength = 5)
    )
  })
  
  output$benchmark_table_box <- renderUI({
    if (show_benchmark_table()) {
      box(
        title = "Radar Benchmark Table",
        width = 12,
        status = "success",
        solidHeader = TRUE,
        DT::dataTableOutput("benchmark_table")
      )
    } else {
      NULL
    }
  })
  
  output$toggle_benchmark_ui <- renderUI({
    actionButton(
      inputId = "toggle_benchmark_btn",
      label = if (show_benchmark_table()) "Hide Benchmark Table" else "Show Benchmark Table",
      class = if (show_benchmark_table()) "btn-danger btn-block" else "btn-success btn-block"
    )
  })
  
  
  # === Artists Profile Data Table ===
  # Reactive flag to show/hide the profile box
  show_artist_table <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_artist_table_btn, {
    show_artist_table(!show_artist_table())
  })
  
  # Reactive filter for top N based on metric
  filtered_artists <- reactive({
    metric <- input$ranking_metric
    top_n <- input$top_n_artists
    
    if (metric == "Total Works") {
      artists_profile %>% arrange(desc(total_works)) %>% head(top_n)
    } else if (metric == "Notable Works") {
      artists_profile %>% arrange(desc(notable_works)) %>% head(top_n)
    } else if (metric == "Oceanus Folk Involvement") {
      artists_profile %>% arrange(desc(oceanus_folk_works)) %>% head(top_n)
    } else {
      artists_profile %>% head(top_n)
    }
  })
  
  output$artist_table <- DT::renderDataTable({
    datatable(
      filtered_artists(),
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # Render toggle button with dynamic label and style
  output$toggle_artist_table_ui <- renderUI({
    actionButton(
      inputId = "toggle_artist_table_btn",
      label = if (show_artist_table()) "Hide Artist Table" else "Show Artist Table",
      class = if (show_artist_table()) "btn-danger btn-block" else "btn-info btn-block"
    )
  })
  
  # Conditionally show or hide the artist table box
  output$artist_table_box <- renderUI({
    if (show_artist_table()) {
      box(
        title = "Artist Work & Collaboration Table",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        DT::dataTableOutput("artist_table")
      )
    } else {
      NULL
    }
  })
  
  
  
  # ================= End of Artists Profile Page =======================
  
  
  # ================= Influence Network Page =======================
 
   ## ================= Who did Sailor influence  =======================
  
    output$ggraphSailorNetwork <- renderPlot({
    req(input$edge_type_input)
    
    sailor_id <- nodes_tbl %>%
      filter(str_detect(name, fixed("Sailor Shift", ignore_case = TRUE))) %>%
      pull(id)
    
    sailor_in_edges <- edges_tbl %>%
      filter(edge_type %in% input$influence_types_selected,
             target == sailor_id)
    
    all_ids <- unique(c(sailor_in_edges$source, sailor_in_edges$target))
    
    vis_nodes <- nodes_tbl %>%
      filter(id %in% all_ids,
             node_type %in% c("Person", "MusicalGroup", "RecordLabel")) %>%
      mutate(
        label = name,
        group = ifelse(id == sailor_id, "Sailor Shift", node_type)
      ) %>%
      select(id, label, group)
    
    vis_nodes <- vis_nodes %>%
      mutate(row_id = row_number())
    id_map <- vis_nodes %>% select(id, row_id)
    
    vis_edges <- sailor_in_edges %>%
      left_join(id_map, by = c("source" = "id")) %>%
      rename(from = row_id) %>%
      left_join(id_map, by = c("target" = "id")) %>%
      rename(to = row_id) %>%
      select(from, to, label = edge_type)
    
    if (nrow(vis_nodes) == 0 || nrow(vis_edges) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selected filters.", col = "red", cex = 1.5)
      return()
    }
    
    graph_tidy <- tbl_graph(
      nodes = vis_nodes %>% arrange(row_id) %>% select(label, group),
      edges = vis_edges,
      directed = TRUE
    )
    
    ggraph(graph_tidy, layout = "fr") +
      geom_edge_link(aes(label = label),
                     arrow = arrow(length = unit(3, 'mm')),
                     end_cap = circle(3, 'mm'),
                     label_size = 4,
                     label_colour = "gray40",
                     angle_calc = "along",
                     label_dodge = unit(2.5, 'mm'),
                     show.legend = FALSE) +
      geom_node_point(aes(color = group), size = 14) +
      geom_node_text(aes(label = label), repel = TRUE, size = 6) +
      theme_void() +
      theme(legend.position = "right")
  })
  
  ## ==============End of Who did Sailor influence  ======================
  
  ## ================= Who influenced Sailor ======================
  
  output$dynamicSailorNetwork <- renderVisNetwork({
    req(input$influence_types_selected)
    
    sailor_id <- nodes_tbl %>% 
      filter(str_detect(name, fixed("Sailor Shift", ignore_case = TRUE))) %>%
      pull(id)
    
    selected_types <- input$influence_types_selected  # ✅ FIXED ID
    
    sailor_edges <- edges_tbl %>%
      filter(edge_type %in% selected_types,
             source == sailor_id | target == sailor_id)
    
    first_hop_ids <- unique(c(sailor_edges$source, sailor_edges$target))
    
    if (input$hop_level == "2-hop") {  # ✅ Compare to string value
      second_hop_edges <- edges_tbl %>%
        filter(edge_type %in% selected_types,
               source %in% first_hop_ids | target %in% first_hop_ids)
      
      all_edges <- bind_rows(sailor_edges, second_hop_edges) %>% distinct()
    } else {
      all_edges <- sailor_edges
    }
    
    all_ids <- unique(c(all_edges$source, all_edges$target))
    
    vis_nodes <- nodes_tbl %>%
      filter(id %in% all_ids, node_type %in% c("Person", "MusicalGroup")) %>%
      mutate(
        label = name,
        group = ifelse(id == sailor_id, "Sailor Shift", node_type),
        color = ifelse(id == sailor_id, "darkblue", "lightblue")
      ) %>%
      select(id, label, group, color)
    
    valid_ids <- vis_nodes$id
    vis_edges <- all_edges %>%
      filter(source %in% valid_ids, target %in% valid_ids) %>%
      select(from = source, to = target, label = edge_type)
    
    connected_ids <- unique(c(vis_edges$from, vis_edges$to))
    vis_nodes <- vis_nodes %>% filter(id %in% connected_ids)
    
    visNetwork(vis_nodes, vis_edges, height = "800px", width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(arrows = "to", font = list(size = 10)) %>%
      visNodes(font = list(size = 10), size = 25) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(stabilization = TRUE)
  })
  
  ## ================= End of Who influenced Sailor ======================
  
  ## ================= Oceanus Folk Influence ======================
  
  ### ====== Genre influenced bu oceanus folk ==============
  
  output$of_treemap <- renderPlot({
    req(input$of_song_genre)
    
    influence_edges <- edges_tbl %>%
      filter(edge_type %in% influence_types)
    
    of_songs <- nodes_tbl %>%
      filter(node_type == "Song", genre == input$of_song_genre)
    
    of_genres <- influence_edges %>%
      filter(source %in% of_songs$id) %>%
      left_join(nodes_tbl, by = c("target" = "id")) %>%
      filter(node_type == "Song", !is.na(genre)) %>%
      count(genre, sort = TRUE) %>%
      mutate(label_text = paste0(genre, "\n(", n, ")"))
    
    if (nrow(of_genres) == 0) {
      plot.new()
      title("No influenced genres found for selected genre.")
    } else {
      ggplot(of_genres, aes(area = n, fill = genre, label = label_text)) +
        geom_treemap() +
        geom_treemap_text(color = "white", place = "center", size = 10, reflow = TRUE) +
        labs(title = paste("Genres Influenced by", input$of_song_genre)) +
        theme(legend.position = "none")
    }
  })
  
  ### ====== End of Genre influenced by oceanus folk ==============
  
  ### ====== Artisits influenced by Oceanus folk =========
  
  output$lollipopPlot <- renderPlotly({
    req(input$source_genre, input$influence_types)
    
    # Step 1: Get songs of the selected genre
    source_songs <- nodes_tbl %>%
      filter(node_type == "Song", genre == input$source_genre)
    
    # Step 2: Get influence edges from selected edge types
    edges_of_interest <- edges_tbl %>%
      filter(edge_type %in% input$influence_types,
             source %in% source_songs$id)
    
    # Step 3: Get influenced song IDs
    influenced_song_ids <- edges_of_interest$target
    
    # Step 4: Count top artists who performed those influenced songs
    top_artist_ids <- edges_tbl %>%
      filter(edge_type == "PerformerOf", target %in% influenced_song_ids) %>%
      count(source, sort = TRUE) %>%
      arrange(desc(n), source) %>%       # enforce a stable tie-break
      slice_head(n = input$top_n)        # strict top-N only
    
    
    # Step 5: Join to get artist names
    top_artists <- top_artist_ids %>%
      left_join(nodes_tbl, by = c("source" = "id")) %>%
      filter(!is.na(name)) %>%
      rename(artist = name, count = n) %>%
      mutate(artist = fct_reorder(artist, count))
    
    # Step 6: Plot lollipop
    plot_ly(top_artists) %>%
      add_segments(x = 0, xend = ~count,
                   y = ~artist, yend = ~artist,
                   line = list(color = 'gray', width = 1.5),
                   showlegend = FALSE) %>%
      add_markers(x = ~count, y = ~artist,
                  marker = list(color = 'firebrick', size = 10),
                  text = ~paste0("<b>", artist, "</b><br>Songs Influenced: ", count),
                  hoverinfo = "text") %>%
      layout(
        title = list(
          text = paste("Top", input$top_n, "Artists Influenced by", input$source_genre),
          x = 0.5,
          y = 0.95
          ),
        xaxis = list(title = "Number of Influenced Songs",
                     showgrid = FALSE),
        yaxis = list(title = "", tickfont = list(size = 11), automargin = TRUE,
                     showgrid = FALSE),
                margin = list(l = 200, t = 40),  # <-- Increase top margin here
        height = 20 * nrow(top_artists),
        plot_bgcolor = "#f4edf4",   # inside the chart area
        paper_bgcolor = "#f4edf4"   # outside the chart area
      )
  })
  
  
  ### ========= End of Artists influenced by Oceanus folk =========
  
  ### ======== Genres Influenced Oceanus Folk ===============

  output$genre_influence_net <- renderVisNetwork({
    req(input$min_influence, input$top_n_genres, input$target_genre_network)
    
    # Step 1: Get song IDs of selected target genre
    target_songs <- nodes_tbl %>%
      filter(node_type == "Song", genre == input$target_genre_network)
    target_song_ids <- target_songs$id
    
    # Step 2: Edges going INTO that genre
    influences_into_target <- influence_edges %>%
      filter(target %in% target_song_ids)
    
    # Step 3: Get source genres
    genre_edges <- influences_into_target %>%
      left_join(nodes_tbl, by = c("source" = "id")) %>%
      filter(node_type == "Song", !is.na(genre)) %>%
      transmute(from = genre, to = input$target_genre_network)
    
    # Step 4: Count strength
    genre_strength <- genre_edges %>%
      count(from, name = "influence_count") %>%
      filter(influence_count >= input$min_influence) %>%
      arrange(desc(influence_count)) %>%
      slice_head(n = input$top_n_genres)
    
    # Step 5: Create nodes
    top_genres <- genre_strength$from
    top_colors <- brewer.pal(min(length(top_genres), 9), "Blues")
    
    genre_nodes <- tibble(id = unique(c(top_genres, input$target_genre_network))) %>%
      left_join(genre_strength, by = c("id" = "from")) %>%
      mutate(
        influence_count = replace_na(influence_count, 1),
        label = id,
        value = influence_count * 2,
        color = case_when(
          id == input$target_genre_network ~ "#FF6347",
          TRUE ~ top_colors[match(id, top_genres)]
        ),
        title = paste0("<b>Genre:</b> ", id,
                       "<br><b>Influence Count:</b> ", influence_count)
      )
    
    genre_edges_filtered <- genre_edges %>%
      filter(from %in% top_genres)
    
    # Step 6: visNetwork
    visNetwork(genre_nodes, genre_edges_filtered) %>%
      visNodes(font = list(size = 15), shadow = TRUE) %>%
      visEdges(arrows = "to") %>%
      visOptions(
        highlightNearest = TRUE) %>%
      visLegend(addNodes = list(
        list(label = "Target Genre", shape = "dot", color = "#FF6347"),
        list(label = "Influencing Genres", shape = "dot", color = "#3182bd")
      ), useGroups = FALSE,
          width = 0.4,
          stepY = 100) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLayout(randomSeed = 42) %>%
      visPhysics(stabilization = FALSE, enabled = FALSE)
  })
  
  
  
  ### ======== End of Genres Influenced Oceanus Folk ===============
  
  ## ================= End of Oceanus Folk Influence ======================
  
  # ================= End of Influence Network Page =======================
  
  # ================= Cluster Analysis Page =======================
  
  cluster_results <- eventReactive(input$run_cluster, {
    req(cluster_data)
    
    # Subset data based on selected variables
    selected_vars <- input$cluster_vars
    if(is.null(selected_vars)) {
      selected_vars <- c("total_works", "notable_works", "oceanus_folk_works")
    }
    
    current_data <- cluster_data[, selected_vars, drop = FALSE]
    
    if(input$cluster_method == "K-means") {
      set.seed(123)
      kmeans(current_data, centers = input$n_clusters, nstart = 25)
    } else if(input$cluster_method == "DBSCAN") {
      dbscan::dbscan(current_data, eps = input$eps, minPts = input$minPts)
    } else {
      pam(current_data, k = input$n_clusters)
    }
  })
  
  # Reactive cluster data frame
  cluster_df <- reactive({
    req(input$cluster_vars, cluster_results())
    
    cluster_data[, input$cluster_vars, drop = FALSE] %>%
      as.data.frame() %>%
      mutate(Cluster = as.factor(cluster_results()$cluster),
             person_id = as.integer(rownames(.))) %>%
      left_join(artists_profile %>% select(person_id, name), by = "person_id")
  })
  
  # Reactive PCA results
  pca_result <- reactive({
    req(cluster_df(), input$cluster_vars)
    prcomp(cluster_df()[, input$cluster_vars], scale. = FALSE)
  })
  
  # Reactive PCA data frame
  pca_df <- reactive({
    req(pca_result(), cluster_df())
    df <- as.data.frame(pca_result()$x[, 1:2])
    df$Cluster <- cluster_df()$Cluster
    df$name <- cluster_df()$name
    df
  })
  
  # Silhouette plot 
  output$silhouettePlot <- renderPlot({
    req(cluster_results())
    
    selected_vars <- input$cluster_vars
    if(is.null(selected_vars)) {
      selected_vars <- c("total_works", "notable_works", "oceanus_folk_works")
    }
    current_data <- cluster_data[, selected_vars, drop = FALSE]
    
    if(input$cluster_method != "DBSCAN") {
      silhouette_avg <- silhouette(cluster_results()$cluster, dist(current_data))
      fviz_silhouette(silhouette_avg) +
        theme_minimal()
    }
  })
  
  # Elbow plot for optimal clusters
  output$elbowPlot <- renderPlot({
    req(cluster_data)
    
    if(input$cluster_method != "DBSCAN") {
      fviz_nbclust(cluster_data[, input$cluster_vars, drop = FALSE], 
                   kmeans, method = "wss") +
        labs(title = "Elbow Method for Optimal Number of Clusters") +
        theme_minimal()
    } else {
      # For DBSCAN, show kNN distance plot to help determine eps
      kNNdistplot(cluster_data[, input$cluster_vars, drop = FALSE], 
                  k = input$minPts)
      abline(h = input$eps, col = "red", lty = 2)
      title("kNN Distance Plot (Help determine eps)")
    }
  })
  
  # Cluster proportion plot
  output$clusterProportionPlot <- renderPlotly({
    req(cluster_results())
    
    # Get cluster assignments
    clusters <- if (input$cluster_method == "DBSCAN") {
      cluster_results()$cluster
    } else if (!is.null(cluster_results()$cluster)) {
      cluster_results()$cluster
    } else {
      return(NULL)
    }
    
    # Create proportion data with cluster labels
    prop_data <- data.frame(Cluster = factor(clusters)) %>%
      count(Cluster) %>%
      mutate(
        Percentage = round(n / sum(n) * 100, 1),
        ClusterLabel = ifelse(Cluster == 0, "Noise", paste("Cluster", Cluster)),
        hover_text = paste0(
          "<b>", ClusterLabel, "</b><br>",
          "Count: ", n, "<br>",
          "Percentage: ", Percentage, "%"
        )
      )
    
    # Create interactive plot
    plot_ly(
      prop_data,
      labels = ~ClusterLabel,
      values = ~n,
      type = 'pie',
      textposition = 'inside',
      textinfo = 'percent',
      hoverinfo = 'text',
      text = ~hover_text,
      marker = list(
        colors = RColorBrewer::brewer.pal(nrow(prop_data), "Set3"),
        line = list(color = '#FFFFFF', width = 1)
      ),
      showlegend = TRUE
    ) %>%
      layout(
        title = list(
          text = "<b>Cluster Distribution</b>",
          x = 0.5,
          font = list(size = 14)
        ),
        margin = list(t = 50, b = 20, l = 20, r = 20),
        legend = list(
          orientation = "h",
          y = -0.1
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Cluster plot
  output$clusterPlot <- renderPlotly({
    req(cluster_results())
    
    selected_vars <- input$cluster_vars
    if (is.null(selected_vars)) {
      selected_vars <- c("total_works", "notable_works", "oceanus_folk_works")
    }
    
    # Prepare merged data
    cluster_df <- cluster_data[, selected_vars, drop = FALSE] %>%
      as.data.frame() %>%
      mutate(Cluster = as.factor(cluster_results()$cluster),
             person_id = as.integer(rownames(.))) %>%
      left_join(artists_profile %>% select(person_id, name), by = "person_id")
    
    # PCA transformation
    pca_result <- prcomp(cluster_df[, selected_vars], scale. = TRUE)
    pca_df <- pca_df()
    colnames(pca_df) <- c("PC1", "PC2")
    pca_df$Cluster <- cluster_df$Cluster
    pca_df$Artist <- cluster_df$name
    
    # Plotly scatter plot
    plot_ly(
      data = pca_df,
      x = ~PC1, y = ~PC2,
      type = 'scatter',
      mode = 'markers',
      color = ~Cluster,
      text = ~paste("Artist:", Artist, "<br>Cluster:", Cluster),
      hoverinfo = "text",
      marker = list(size = 8, line = list(width = 1, color = 'black'))
    ) %>%
      layout(
        title = "Cluster Plot (PCA Projection)",
        xaxis = list(title = "PC1"),
        yaxis = list(title = "PC2")
      )
  })
  
  # Cluster Characteristics
  output$clusterChars <- renderPlotly({
    req(cluster_results())
    
    selected_vars <- input$cluster_vars
    if(is.null(selected_vars)) {
      selected_vars <- c("total_works", "notable_works", "oceanus_folk_works")
    }
    current_data <- cluster_data[, selected_vars, drop = FALSE]
    
    clusters <- if (input$cluster_method == "DBSCAN") {
      cluster_results()$cluster
    } else if (!is.null(cluster_results()$cluster)) {
      cluster_results()$cluster
    } else {
      return(NULL)
    }
    
    cluster_data_with_cluster <- current_data %>%
      as.data.frame() %>%
      mutate(Cluster = as.factor(clusters))
    
    plot_data <- cluster_data_with_cluster %>%
      pivot_longer(-Cluster, names_to = "Variable", values_to = "Value") %>%
      filter(!is.na(Value))
    
    p <- ggplot(plot_data, aes(x = Variable, y = Value, fill = Cluster)) +
      geom_boxplot() +
      facet_wrap(~ Cluster, ncol = 2) +
      labs(title = "Cluster Characteristics by Variable",
           x = "", y = "Standardized Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Value Box calculations
  model_stats <- reactive({
    req(cluster_results(), input$cluster_method)
    
    if (input$cluster_method %in% c("K-means", "PAM")) {
      k <- if (input$cluster_method == "K-means") input$n_clusters else cluster_results()$nc
      data_used <- cluster_data[, input$cluster_vars, drop = FALSE]
      n <- nrow(data_used)
      p <- ncol(data_used)
      
      # Total within-cluster sum of squares
      wss <- if (input$cluster_method == "K-means") {
        sum(cluster_results()$withinss)
      } else {
        sum(cluster_results()$withinss)
      }
      
      # Log-likelihood approximation from WSS
      log_likelihood <- -n * p / 2 * log(wss / n)
      
      # AIC and BIC
      aic <- -2 * log_likelihood + 2 * k * p
      bic <- -2 * log_likelihood + log(n) * k * p
      
      # Pseudo likelihood ratio: compare to total variance
      total_ss <- sum(scale(data_used, scale = FALSE)^2)
      likelihood_ratio <- total_ss - wss
      
      # Entropy (optional and rough): based on cluster proportions
      cluster_sizes <- table(cluster_results()$cluster)
      proportions <- cluster_sizes / sum(cluster_sizes)
      entropy <- -sum(proportions * log(proportions)) / log(length(proportions))
      
      list(
        AIC = round(aic),
        BIC = round(bic),
        LikelihoodRatio = round(likelihood_ratio),
        Entropy = round(entropy, 3)
      )
    } else {
      NULL  # For DBSCAN, skip this
    }
  })
  
  # Value boxes
  
  output$aic_box <- renderValueBox({
    req(model_stats())
    valueBox(
      value = formatC(model_stats()$AIC, format = "d", big.mark = ","),
      subtitle = "AIC",
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$bic_box <- renderValueBox({
    req(model_stats())
    valueBox(
      value = formatC(model_stats()$BIC, format = "d", big.mark = ","),
      subtitle = "BIC",
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$lr_box <- renderValueBox({
    req(model_stats())
    valueBox(
      value = formatC(model_stats()$LikelihoodRatio, format = "d", big.mark = ","),
      subtitle = "Likelihood Ratio",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$entropy_box <- renderValueBox({
    req(model_stats())
    valueBox(
      value = model_stats()$Entropy,
      subtitle = "Entropy",
      icon = icon("braille"),
      color = "yellow"
    )
  })
  
  
  # ================= End of Cluster Analysis Page =======================
  
}

# ===== Run App =====
shinyApp(ui = ui, server = server)
