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
    time_to_notability = ifelse(!is.na(first_notable), first_notable - first_release, NA)
  )

# --- Collaboration Count using superedge ---
collaborations <- edges_tbl_mapped %>%
  filter(superedge == "Collaborations") %>%
  left_join(id_map, by = c("from" = "index")) %>%
  count(id, name = "collaborations") %>%
  rename(person_id = id)

# --- Final Profile Merge ---
artists_profile <- artists_profile %>%
  left_join(collaborations, by = "person_id") %>%
  mutate(collaborations = replace_na(collaborations, 0)) %>%
  inner_join(people_tbl, by = "person_id") %>%
  relocate(person_id, name)

### ---- Get Sailor Shift ID ----
sailor_id <- nodes_tbl %>% 
  filter(str_detect(name, fixed("Sailor Shift", ignore_case = TRUE))) %>%
  pull(id)

# ---- Influence Edge Types ----
influence_types <- c("LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "DirectlySamples", "InStyleOf", 
                     "PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf", "RecordedBy")

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
        menuSubItem("Who has influenced Sailor?", tabName = "sailor_influencers")),
      menuItem("Cluster Analysis", tabName = "cluster", icon = icon("layer-group")),
      menuItem("Artist Comparison", tabName = "compare", icon = icon("user-friends")),
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
              DT::dataTableOutput("artists_table")
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
                div(class = "custom-box-green" ,
                box(
                  title = "Filter Influences",
                  width = 3,
                  status = "success",
                  solidHeader = TRUE,
                  checkboxGroupInput("influencer_type_input", "Influencer Type:",
                                     choices = c("Person", "MusicalGroup", "RecordLabel"),
                                     selected = c("Person", "MusicalGroup", "RecordLabel")),
                  
                  checkboxGroupInput("edge_type_input", "Influence Type:",
                                     choices = influence_types,
                                     selected = influence_types)
                )),
                div(class = "custom-box-green",
                    box(
                      width = 9,
                      title = "Sailor Shift Influence Network (GGRAPH)",
                      solidHeader = TRUE,
                      plotOutput("ggraphSailorNetwork", height = "800px")
                    )
                    
              ))
      )
      ,
      
      tabItem(tabName = "cluster",
              plotOutput("elbowPlot"),
              plotlyOutput("pcaPlot"),
              DT::dataTableOutput("clusterSummary")
      ),
      
      tabItem(tabName = "compare",
              DT::dataTableOutput("artistComparisonTable"),
              plotOutput("timelinePlot"),
              visNetworkOutput("egoNetwork", height = "600px")
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
    artists_profile
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
  
  output$artists_table <- DT::renderDataTable({
    artists_profile
  })
  
  # ================= End of Artists Profile Page =======================
  
  
  # ================= Influence Network Page =======================
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
  
  # ==== Preprocessing: Who Influenced Sailor ====
  output$ggraphSailorNetwork <- renderPlot({
    req(input$influence_types_selected)
    
    selected_types <- input$influence_types_selected
    
    sailor_id <- nodes_tbl %>%
      filter(str_detect(name, fixed("Sailor Shift", ignore_case = TRUE))) %>%
      pull(id)
    
    sailor_edges <- edges_tbl %>%
      filter(edge_type %in% selected_types,
             source == sailor_id | target == sailor_id)
    
    first_hop_ids <- unique(c(sailor_edges$source, sailor_edges$target))
    
    if (input$hop_level == "2-hop") {
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
        group = ifelse(id == sailor_id, "Sailor Shift", node_type)
      ) %>%
      select(id, label, group)
    
    vis_edges <- all_edges %>%
      filter(source %in% vis_nodes$id, target %in% vis_nodes$id) %>%
      select(from = source, to = target, label = edge_type)
    
    # Validation check to avoid graph errors
    if (nrow(vis_nodes) == 0 || nrow(vis_edges) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selected filters.", col = "red", cex = 1.5)
      return()
    }
    
    graph_tidy <- tbl_graph(nodes = vis_nodes, edges = vis_edges, directed = TRUE)
    
    ggraph(graph_tidy, layout = "fr") +
      geom_edge_link(aes(label = label),
                     arrow = arrow(length = unit(3, 'mm')),
                     end_cap = circle(3, 'mm'),
                     label_size = 3,
                     label_colour = "gray40",
                     angle_calc = "along",
                     label_dodge = unit(2.5, 'mm'),
                     show.legend = FALSE) +
      geom_node_point(aes(color = group), size = 8) +
      geom_node_text(aes(label = label), repel = TRUE, size = 3) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  
  
  # ================= End of Influence Network Page =======================
  
  
  
  
}

# ===== Run App =====
shinyApp(ui = ui, server = server)
