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
library(dbscan)
library(NbClust)
library(RColorBrewer)
library(fmsb)

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
      menuItem("Prediction Analysis", icon = icon("chart-line"), startExpanded = FALSE,
               menuSubItem("Future Predictions", tabName = "future"),
               menuSubItem("Historical Analysis", tabName = "historical"))
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
                                 choices = NULL,
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
                      visNetworkOutput("ggraphSailorNetwork", height = "800px")
                    )
                    
                ))
      )
      ,
      tabItem(tabName = "oceanus",
              tabsetPanel(
                tabPanel("Influence Over Time",
                         fluidRow(
                           box(
                             width = 3,
                             class = "custom-box-green",
                             title = "Select Genre",
                             solidHeader = TRUE,
                             selectInput("selected_timeline_genre", "Select Genre:",
                                         choices = sort(unique(nodes_tbl$genre[nodes_tbl$node_type == "Song"])),
                                         selected = "Oceanus Folk")
                           ),
                           box(
                             width = 9,
                             class = "custom-box-green",
                             title = "Temporal Spread of Influence",
                             solidHeader = TRUE,
                             plotlyOutput("oceanus_timeline", height = "500px")
                           )
                         )
                ),
                
                tabPanel("Genres Influenced by Oceanus Folk",
                         fluidRow(
                           box(
                             width = 3,
                             class = "custom-box-green",
                             title = "Filter Options",
                             solidHeader = TRUE,
                             selectizeInput(
                               inputId = "of_song_genre",
                               label = "Select Genre:",
                               choices = sort(unique(nodes_tbl$genre[nodes_tbl$node_type == "Song"])),
                               selected = "Oceanus Folk",
                               multiple = FALSE
                             )
                           ),
                           box(
                             width = 9,
                             class = "custom-box-green",
                             solidHeader = TRUE,
                             plotOutput("of_treemap", height = "700px")
                           )
                         )
                ),
                
                tabPanel("Top Artists Inlfuenced by Oceanus Folk",
                         fluidRow(
                           box(
                             width = 3,
                             class = "custom-box-green",
                             title = "Filter Options",
                             solidHeader = TRUE,
                             selectInput("source_genre", "Select Source Genre:",
                                         choices = sort(unique(nodes_tbl$genre[nodes_tbl$node_type == "Song"])),
                                         selected = "Oceanus Folk"),
                             sliderInput("top_n", "Top N Influenced Artists:", min = 10, max = 35, value = 12),
                             selectizeInput("influence_types", "Influence Edge Types:",
                                            choices = influence_types,
                                            selected = "LyricalReferenceTo",
                                            multiple = TRUE,
                                            options = list(placeholder = 'Select one or more edge types'))
                           ),
                           box(
                             width = 9,
                             class = "custom-box-green",
                             solidHeader = TRUE,
                             plotlyOutput("lollipopPlot", height = "800px")
                           )
                         )
                ),
                
                tabPanel("Genres Influenced Oceanus Folk",
                         fluidRow(
                           box(
                             width = 3,
                             class = "custom-box-green",
                             title = "Filter Options",
                             solidHeader = TRUE,
                             sliderInput("min_influence", "Minimum Influence Count:",
                                         min = 1, max = 10, value = 1),
                             numericInput("top_n_genres", "Top N Genres to Display:",
                                          value = 20, min = 1, max = 20),
                             selectInput("target_genre_network", "Target Genre (Node):",
                                         choices = sort(unique(nodes_tbl$genre[nodes_tbl$node_type == "Song"])),
                                         selected = "Oceanus Folk")
                           ),
                           box(
                             width = 9,
                             class = "custom-box-green",
                             title = "Genre-to-Genre Influence Network",
                             solidHeader = TRUE,
                             visNetworkOutput("genre_influence_net", height = "650px")
                           )
                         )
                )
                
              )      )
      
      ,
      
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
      
      # Future Predictions Tab
      tabItem(tabName = "future",
              fluidRow(
                box(title = "Prediction Controls", width = 3, status = "info",
                    selectInput("prediction_method", "Select Prediction Method:",
                                choices = c("Composite Score", 
                                            "Growth Trajectory", "Network Centrality"),
                                selected = "Composite Score"),
                    conditionalPanel(
                      condition = "input.prediction_method == 'Growth Trajectory'",
                      sliderInput("min_data_points", "Minimum Data Points for Analysis:",
                                  min = 3, max = 10, value = 5),
                      checkboxInput("show_models", "Show Individual Models", value = FALSE)
                    ),
                    sliderInput("future_timeframe", "Future Time Frame:",
                                min = max(artist_works$release_date, na.rm = TRUE) + 1,  # strictly future
                                max = max(artist_works$release_date, na.rm = TRUE) + 20,
                                value = c(max(artist_works$release_date, na.rm = TRUE) + 1, 
                                          max(artist_works$release_date, na.rm = TRUE) + 5),
                                step = 1, sep = ""),
                    selectizeInput("future_genres", "Filter by Genre (Optional):",
                                   choices = unique(artist_works$genre),
                                   selected = NULL,
                                   multiple = TRUE,
                                   options = list(placeholder = 'All genres')),
                    checkboxInput("show_all_predictions", "Show All Predictions", value = TRUE),
                    conditionalPanel(
                      condition = "input.prediction_method == 'Composite Score'",
                      numericInput("top_n_predictions", "Top N Artists to Show (Radar & Table):", 
                                 value = 3, min = 1, max = 9)
                      ),
                    conditionalPanel(
                      condition = "input.prediction_method == 'Network Centrality'",
                      sliderInput("top_n_network", "Number of Top Artists to Show:", min = 1, max = 6, value = 3)
                    ),
                    actionButton("run_future_prediction", "Run Future Prediction", 
                                 icon = icon("chart-line"),
                                 class = "btn-primary")
                ),
                box(title = "Future Predictions Results", width = 9, status = "success",
                    tabsetPanel(
                      tabPanel("Prediction Table", 
                               conditionalPanel(
                                 condition = "input.prediction_method == 'Growth Trajectory'",
                                 DTOutput("growthDiagnosticsTable")
                               ),
                               conditionalPanel(
                                 condition = "input.prediction_method != 'Growth Trajectory'",
                                 DTOutput("futureStarsTable")
                               )
                               ),
                      tabPanel("Metrics", 
                               plotlyOutput("futureMetricsPlot", height = "600px"),
                               ),
                      tabPanel("Visualization",
                               conditionalPanel(
                                 condition = "input.prediction_method == 'Growth Trajectory'",
                                 plotlyOutput("growthTrajectoryPlot", height = "600px")
                               ),
                               conditionalPanel(
                                 condition = "input.prediction_method == 'Composite Score'",
                                 plotOutput("futureRadarPlot", height = "600px")
                               ),
                               conditionalPanel(
                                 condition = "input.prediction_method == 'Network Centrality'",
                                 uiOutput("futureNetworkPlot", height = "600px")
                               )
                               )
                    )
                )
              )
      ),
      
      # Historical Analysis Tab
      tabItem(tabName = "historical",
              fluidRow(
                box(title = "Historical Controls", width = 3, status = "info",
                    sliderInput("historical_timeframe", "Historical Time Frame:",
                                min = min(artist_works$release_date, na.rm = TRUE),
                                max = max(artist_works$release_date, na.rm = TRUE),
                                value = c(min(artist_works$release_date, na.rm = TRUE), 
                                          min(artist_works$release_date, na.rm = TRUE) + 20),
                                step = 5, sep = ""),
                    selectizeInput("historical_genres", "Filter by Genre (Optional):",
                                   choices = unique(artist_works$genre),
                                   selected = NULL,
                                   multiple = TRUE,
                                   options = list(placeholder = 'All genres')),
                    actionButton("run_historical_analysis", "Run Historical Analysis", 
                                 icon = icon("chart-line"),
                                 class = "btn-primary")
                ),
                box(title = "Historical Analysis Results", width = 9, status = "success",
                    tabsetPanel(
                      tabPanel("Trend Analysis", 
                               plotlyOutput("historicalTrendPlot", height = "550px")),
                      tabPanel("Top Artists", 
                               DT::dataTableOutput("historicalTopArtistsTable", height = "550px")),
                      tabPanel("Comparison", 
                               plotlyOutput("historicalComparisonPlot", height = "600px"))
                    )
                )
              )
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
  
  updateSelectizeInput(session, "selected_artists", 
                       choices = sort(unique(artists_profile$name)),
                       server = TRUE)
  
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
  
  ## ================= Who influenced Sailor =======================
  
  output$ggraphSailorNetwork <- renderVisNetwork({
    req(input$edge_type_input)
    
    sailor_id <- nodes_tbl %>%
      filter(str_detect(name, fixed("Sailor Shift", ignore_case = TRUE))) %>%
      pull(id)
    
    sailor_in_edges <- edges_tbl %>%
      filter(edge_type %in% input$edge_type_input, target == sailor_id)
    
    all_ids <- unique(c(sailor_in_edges$source, sailor_in_edges$target))
    
    # Build vis nodes
    vis_nodes <- nodes_tbl %>%
      filter(id %in% all_ids) %>%
      mutate(
        label = name,
        group = node_type,
        color = case_when(
          id == sailor_id ~ "darkblue",
          node_type == "Person" ~ "#91c788",
          node_type == "MusicalGroup" ~ "#f48c8c",
          node_type == "RecordLabel" ~ "#70d6ff",
          TRUE ~ "#cccccc"
        ),
        title = paste0("<b>", name, "</b><br>Type: ", node_type)
      ) %>%
      select(id, label, group, color, title)
    
    # Build vis edges
    vis_edges <- sailor_in_edges %>%
      select(from = source, to = target, edge_type) %>%
      mutate(
        arrows = "to",
        label = edge_type,
        color = "#848484",       # ✅ just use a flat column
        font.size = 12,          # ✅ use column naming convention for visNetwork
        font.align = "middle",   # ✅ flat attributes
        smooth = TRUE
      )
    
    
    
    visNetwork(vis_nodes, vis_edges, height = "800px", width = "100%") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1),
        nodesIdSelection = TRUE
      ) %>%
      visInteraction(
        dragView = TRUE,
        zoomView = TRUE,
        navigationButtons = TRUE
      ) %>%
      visGroups(groupname = "MusicalGroup", color = "#f48c8c") %>%
      visGroups(groupname = "Person", color = "#91c788") %>%
      visGroups(groupname = "RecordLabel", color = "#70d6ff") %>%
      visGroups(groupname = "Sailor Shift", color = "darkblue") %>%
      visLegend(useGroups = TRUE, position = "right", main = "Node Type") %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -60),  # more spacing
        stabilization = TRUE
      ) %>%
      visLayout(randomSeed = 42, improvedLayout = TRUE)
  })
  
  
  ## ==============End of Who influenced Sailor  ======================
  
  ## ================= Who Sailor influenced======================
  
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
  
  ## ================= End of Who Sailor influenced ======================
  
  ## ================= Oceanus Folk Influence ======================
  
  ### ============ Oceanus Folk timeline =============
  
  output$oceanus_timeline <- renderPlotly({
    req(input$selected_timeline_genre)
    
    of_songs <- nodes_tbl %>%
      filter(node_type == "Song", genre == input$selected_timeline_genre)
    
    influence_over_time <- influence_edges %>%
      filter(source %in% of_songs$id) %>%
      left_join(nodes_tbl, by = c("target" = "id")) %>%
      filter(!is.na(release_date)) %>%
      count(release_date) %>%
      mutate(release_year = as.integer(release_date)) %>%
      arrange(release_year)
    
    plot_ly(influence_over_time,
            x = ~release_year,
            y = ~n,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'steelblue'),
            marker = list(size = 6, color = 'black'),
            text = ~paste("Year:", release_year, "<br>Influenced Songs:", n),
            hoverinfo = 'text') %>%
      layout(title = paste("Influence of", input$selected_timeline_genre, "Over Time"),
             xaxis = list(title = "Release Year"),
             yaxis = list(title = "Number of Influenced Songs"),
             hoverlabel = list(bgcolor = "white"),
             dragmode = "zoom")
  })
  
  
  ### =========== End of Oceanus Folk timeline=============
  
  ### ====== Genre influenced by oceanus folk ==============
  
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
  
  # ===== Optimized Cluster Analysis Section =====
  
  # Reactive values to store all cluster results
  cluster_store <- reactiveValues(
    results = NULL,
    df = NULL,
    pca_result = NULL,
    pca_df = NULL,
    stats = NULL,
    last_run = NULL
  )
  
  # Progress indicators
  cluster_progress <- reactiveValues(
    running = FALSE,
    message = ""
  )
  
  # Observe the run button and perform all calculations
  observeEvent(input$run_cluster, {
    req(input$cluster_vars)
    
    # Set progress indicators
    cluster_progress$running <- TRUE
    cluster_progress$message <- "Running cluster analysis..."
    
    tryCatch({
      # Subset data based on selected variables
      selected_vars <- input$cluster_vars
      current_data <- cluster_data[, selected_vars, drop = FALSE]
      
      # Validate data
      if (nrow(current_data) == 0) {
        stop("No data available for selected variables")
      }
      
      # Perform clustering with progress
      withProgress(message = 'Performing clustering...', value = 0.3, {
        if (input$cluster_method == "K-means") {
          set.seed(123)
          cluster_store$results <- kmeans(current_data, centers = input$n_clusters, nstart = 25)
        } else if (input$cluster_method == "DBSCAN") {
          cluster_store$results <- dbscan(current_data, eps = input$eps, minPts = input$minPts)
        } else if (input$cluster_method == "PAM") {
          cluster_store$results <- pam(current_data, k = input$n_clusters)
        }
      })
      
      # Create cluster data frame
      withProgress(message = 'Preparing results...', value = 0.6, {
        cluster_store$df <- current_data %>%
          as.data.frame() %>%
          mutate(Cluster = as.factor(cluster_store$results$cluster),
                 person_id = as.integer(rownames(.))) %>%
          left_join(artists_profile %>% select(person_id, name), by = "person_id")
        
        # Perform PCA if we have more than 1 cluster
        if (length(unique(cluster_store$results$cluster)) > 1) {
          cluster_store$pca_result <- prcomp(cluster_store$df[, selected_vars], scale. = FALSE)
          
          # Create PCA data frame
          cluster_store$pca_df <- as.data.frame(cluster_store$pca_result$x[, 1:2])
          cluster_store$pca_df$Cluster <- cluster_store$df$Cluster
          cluster_store$pca_df$name <- cluster_store$df$name
        } else {
          cluster_store$pca_result <- NULL
          cluster_store$pca_df <- NULL
        }
        
        # Calculate model statistics (only for K-means and PAM)
        if (input$cluster_method %in% c("K-means", "PAM")) {
          k <- if (input$cluster_method == "K-means") input$n_clusters else cluster_store$results$nc
          data_used <- current_data
          n <- nrow(data_used)
          p <- ncol(data_used)
          
          wss <- if (input$cluster_method == "K-means") {
            sum(cluster_store$results$withinss)
          } else {
            sum(cluster_store$results$clusinfo[, "av_diss"])
          }
          
          log_likelihood <- -n * p / 2 * log(wss / n)
          aic <- -2 * log_likelihood + 2 * k * p
          bic <- -2 * log_likelihood + log(n) * k * p
          total_ss <- sum(scale(data_used, scale = FALSE)^2)
          likelihood_ratio <- total_ss - wss
          
          cluster_sizes <- table(cluster_store$results$cluster)
          proportions <- cluster_sizes / sum(cluster_sizes)
          entropy <- -sum(proportions * log(proportions)) / log(length(proportions))
          
          cluster_store$stats <- list(
            AIC = round(aic),
            BIC = round(bic),
            LikelihoodRatio = round(likelihood_ratio),
            Entropy = round(entropy, 3)
          )
        } else {
          cluster_store$stats <- NULL
        }
      })
      
      cluster_store$last_run <- Sys.time()
      cluster_progress$message <- "Cluster analysis completed successfully"
    }, error = function(e) {
      cluster_progress$message <- paste("Error:", e$message)
      showNotification(cluster_progress$message, type = "error")
    }, finally = {
      cluster_progress$running <- FALSE
      removeModal()
    })
  })
  
  # Show progress UI
  output$cluster_progress_ui <- renderUI({
    if (cluster_progress$running) {
      tagList(
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped active", 
                role = "progressbar",
                style = "width: 100%")),
        p(style = "text-align: center;", cluster_progress$message)
      )
    } else {
      NULL
    }
  })
  
  # ===== Optimized Output Renderers =====
  
  # Silhouette plot - only shown for K-means and PAM
  output$silhouettePlot <- renderPlot({
    req(cluster_store$results)
    
    if (input$cluster_method %in% c("K-means", "PAM")) {
      selected_vars <- input$cluster_vars
      current_data <- cluster_data[, selected_vars, drop = FALSE]
      
      n_clusters <- length(unique(cluster_store$results$cluster))
      if (n_clusters < 2 || n_clusters > 10) {
        plot.new()
        text(0.5, 0.5, "Silhouette plot requires 2-10 clusters", col = "red")
        return()
      }
      
      silhouette_avg <- silhouette(as.numeric(cluster_store$results$cluster), dist(current_data))
      fviz_silhouette(silhouette_avg) +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "Silhouette not available for DBSCAN", col = "grey")
    }
  })
  
  # Elbow plot - shown immediately (doesn't require button press)
  output$elbowPlot <- renderPlot({
    req(input$cluster_vars)
    
    current_data <- cluster_data[, input$cluster_vars, drop = FALSE]
    
    if (input$cluster_method != "DBSCAN") {
      fviz_nbclust(current_data, kmeans, method = "wss", k.max = min(8, nrow(current_data)-1)) +
        labs(title = "Elbow Method for Optimal Number of Clusters") +
        theme_minimal()
    } else {
      kNNdistplot(current_data, k = input$minPts)
      abline(h = input$eps, col = "red", lty = 2)
      title("kNN Distance Plot (Help determine eps)")
    }
  })
  
  # Cluster proportion plot
  output$clusterProportionPlot <- renderPlotly({
    req(cluster_store$results)
    
    clusters <- cluster_store$results$cluster
    if (is.null(clusters)) {
      return(NULL)
    }
    
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
    
    # Limit to reasonable number of clusters for visualization
    if (nrow(prop_data) > 10) {
      prop_data <- prop_data %>% slice_max(n, n = 10)
    }
    
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
        colors = RColorBrewer::brewer.pal(min(nrow(prop_data), 9), "Set3"),
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
    req(cluster_store$results, input$cluster_vars)
    
    # Get data and perform PCA
    selected_data <- cluster_data[, input$cluster_vars, drop = FALSE]
    pca_result <- prcomp(selected_data, scale. = FALSE)
    pca_df <- as.data.frame(pca_result$x[, 1:2])
    
    # Add cluster information
    pca_df$Cluster <- as.factor(cluster_store$results$cluster)
    pca_df$name <- artists_profile$name[as.integer(rownames(pca_df))]
    
    # Create convex hulls for each cluster
    hulls <- pca_df %>%
      group_by(Cluster) %>%
      slice(chull(PC1, PC2))
    
    # Plot
    p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(aes(text = paste("Artist:", name)), alpha = 0.7) +
      geom_polygon(data = hulls, aes(fill = Cluster), alpha = 0.2) +
      labs(title = "Cluster Visualization (PCA Projection)",
           x = "Principal Component 1",
           y = "Principal Component 2") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Cluster Characteristics
  output$clusterChars <- renderPlotly({
    req(cluster_store$df)
    
    # Limit data size for better performance
    plot_data <- cluster_store$df
    if (nrow(plot_data) > 1000) {
      plot_data <- plot_data %>% sample_n(1000)
    }
    
    plot_data <- plot_data %>%
      pivot_longer(-c(Cluster, person_id, name), names_to = "Variable", values_to = "Value") %>%
      filter(!is.na(Value))
    
    # Limit number of clusters shown
    unique_clusters <- unique(plot_data$Cluster)
    if (length(unique_clusters) > 6) {
      plot_data <- plot_data %>% 
        filter(Cluster %in% unique_clusters[1:6])
    }
    
    p <- ggplot(plot_data, aes(x = Variable, y = Value, fill = Cluster)) +
      geom_boxplot() +
      facet_wrap(~ Cluster, ncol = 2) +
      labs(title = "Cluster Characteristics by Variable",
           x = "", y = "Standardized Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # ===== Optimized Value Boxes =====
  
  output$aic_box <- renderValueBox({
    req(cluster_store$stats)
    if (input$cluster_method == "PAM") {
      return(NULL)  # Hide AIC for PAM
    }
    valueBox(
      value = formatC(cluster_store$stats$AIC, format = "d", big.mark = ","),
      subtitle = "AIC",
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$bic_box <- renderValueBox({
    req(cluster_store$stats)
    if (input$cluster_method == "PAM") {
      return(NULL)  # Hide BIC for PAM
    }
    valueBox(
      value = formatC(cluster_store$stats$BIC, format = "d", big.mark = ","),
      subtitle = "BIC",
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$lr_box <- renderValueBox({
    req(cluster_store$stats)
    valueBox(
      value = formatC(cluster_store$stats$LikelihoodRatio, format = "d", big.mark = ","),
      subtitle = "Likelihood Ratio",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$entropy_box <- renderValueBox({
    req(cluster_store$stats)
    valueBox(
      value = cluster_store$stats$Entropy,
      subtitle = "Entropy",
      icon = icon("braille"),
      color = "yellow"
    )
  })
  
  # ================= End of Cluster Analysis Page =======================
    
  # ================= Predicition Analysis Page =======================  
    
  # === Future Prediction Subpage ===
  
  observe({
    if(input$future_timeframe[1] > max_release) {
      showNotification("Note: Predicting for future years based on recent artist activity", 
                       type = "warning")
    }
  })
  
  # Replace the future_filtered_artists reactive with this:
  future_filtered_artists <- reactive({
    req(input$future_timeframe)
    
    # Get artists active in recent years (last 5 years of historical data)
    recent_artists <- artist_works %>%
      filter(release_date >= (max_release - 5)) %>%
      distinct(person_id) %>%
      pull(person_id)
    
    # Filter profile to only include recently active artists
    artists_profile %>%
      filter(person_id %in% recent_artists)
  })
  
  # Reactive function to filter future artists by genre if selected
  future_genre_filtered_artists <- reactive({
    req(future_filtered_artists())
    
    if (!is.null(input$future_genres)) {
      # Get artists who have works in the selected genres
      genre_artists <- artist_works %>%
        filter(genre %in% input$future_genres) %>%
        distinct(person_id) %>%
        pull(person_id)
      
      future_filtered_artists() %>%
        filter(person_id %in% genre_artists)
    } else {
      future_filtered_artists()
    }
  })
  
  # Composite Score Prediction for future
  composite_score_prediction <- reactive({
    req(input$run_future_prediction)
    
    # Get recently active artists (from future_filtered_artists)
    active_artists <- future_filtered_artists()$person_id
    
    # Filter by genre if selected
    if (!is.null(input$future_genres)) {
      genre_artists <- artist_works %>%
        filter(genre %in% input$future_genres) %>%
    distinct(person_id) %>%
    pull(person_id)
  active_artists <- intersect(active_artists, genre_artists)
    }
    
    artists_profile %>%
      filter(person_id %in% active_artists) %>%
      mutate(
        productivity_score = scales::rescale(total_works, to = c(0, 10), na.rm = TRUE),
        notability_score = scales::rescale(notable_works, to = c(0, 15), na.rm = TRUE),
        genre_score = ifelse(oceanus_folk_works > 0, 5, 0),
        collab_score = scales::rescale(collaborations, to = c(0, 10), na.rm = TRUE),
        diversity_score = scales::rescale(genre_diversity, to = c(0, 5), na.rm = TRUE),
        time_score = scales::rescale(-time_to_notability, to = c(0, 10), na.rm = TRUE),
        recency_bonus = ifelse(first_release >= (max_release - 5), 5, 0),
        future_star_score = round(
          productivity_score + notability_score + genre_score +
            collab_score + diversity_score + time_score + recency_bonus,
          1
        ),
        prediction_tier = case_when(
          future_star_score >= 40 ~ "High Potential",
          future_star_score >= 30 ~ "Moderate Potential",
          TRUE ~ "Emerging"
        )
      ) %>%
      arrange(desc(future_star_score)) %>%
      select(name, future_star_score, prediction_tier,
             total_works, notable_works, oceanus_folk_works,
             collaborations, genre_diversity, time_to_notability,
             productivity_score, notability_score, collab_score,
             diversity_score, time_score, genre_score)
  })
  
  # Growth trajectory analysis function
  growth_prediction <- reactive({
    req(input$run_future_prediction, input$min_data_points)
    
    withProgress(message = 'Analyzing growth trajectories...', value = 0.3, {
      artist_history <- artist_works %>%
        group_by(person_id, release_date) %>%
        summarise(works_count = n(), .groups = "drop") %>%
        left_join(people_tbl, by = "person_id") %>%
        filter(!is.na(release_date))
      
      valid_artists <- artist_history %>%
        group_by(person_id) %>%
        filter(n() >= input$min_data_points) %>%
        pull(person_id) %>%
        unique()
      
      growth_models <- artist_history %>%
        filter(person_id %in% valid_artists) %>%
        group_by(person_id, name) %>%
        do({
          model <- lm(works_count ~ release_date, data = .)
          data.frame(
            slope = coef(model)[2],
            intercept = coef(model)[1],
            r_squared = summary(model)$r.squared,
            p_value = summary(model)$coefficients[2,4],
            last_year = max(.$release_date),
            recent_works = .$works_count[which.max(.$release_date)]
          )
        }) %>%
        ungroup() %>%
        left_join(artists_profile, by = c("person_id", "name")) %>%
        mutate(
          growth_tier = case_when(
            slope > quantile(slope, 0.75, na.rm = TRUE) ~ "High Growth",
            slope > quantile(slope, 0.25, na.rm = TRUE) ~ "Moderate Growth",
            TRUE ~ "Low Growth"
          ),
          significance = ifelse(p_value < 0.05, "Significant", "Not Significant")
        )
      
      incProgress(0.7)
      return(growth_models)
    })
  })
  
  # Growth trajectory plot
  output$growthTrajectoryPlot <- renderPlotly({
    req(input$run_future_prediction)
    
    plot_data <- artist_works %>%
      left_join(people_tbl, by = "person_id") %>%
      inner_join(
        growth_prediction() %>% 
          arrange(desc(slope)) %>% 
          slice_head(n = 12) %>%  # Show top 12 for clarity
          select(person_id, slope, growth_tier),
        by = "person_id"
      ) %>%
      group_by(person_id, release_date, name, slope, growth_tier) %>%
      summarise(works_count = n(), .groups = "drop") %>%
      mutate(
        tooltip = paste0(
          "<b>", name, "</b><br>",
          "Year: ", release_date, "<br>",
          "Works: ", works_count, "<br>",
          "Growth Rate: ", round(slope, 2)
        )
      )
    
    # Create small multiples
    p <- ggplot(plot_data, aes(x = release_date, y = works_count)) +
      geom_line(aes(color = growth_tier), alpha = 0.7) +
      geom_point(aes(color = growth_tier, text = tooltip), size = 1, alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      facet_wrap(~ reorder(name, -slope), scales = "free_y", ncol = 3) +
      labs(title = "Top 12 Artists by Growth Rate",
           x = "Release Year", y = "Works Produced") +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(t = 80)
      )
  })
  
  # Growth diagnostics table
  output$growthDiagnosticsTable <- renderDT({
    req(input$run_future_prediction)
    
    growth_prediction() %>%
      select(name, slope, r_squared, p_value, significance, 
             total_works, notable_works, growth_tier) %>%
      mutate(across(c(slope, r_squared, p_value), ~round(., 3))) %>%
      arrange(desc(slope)) %>%
      datatable(
        caption = "Growth Model Diagnostics (Sorted by Growth Rate)",
        extensions = c("Buttons", "Scroller"),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('csv', 'excel'),
          scrollY = "400px",
          scroller = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        "significance",
        backgroundColor = styleEqual(c("Significant", "Not Significant"), 
                                     c("#E1F5FE", "#FFEBEE"))
      )
  })
  
  # Network Centrality Prediction Method
  network_prediction <- reactive({
    req(input$run_future_prediction)
    
    tryCatch({
      # Calculate centrality measures on the full graph
      centrality_measures <- graph %>%
        activate(nodes) %>%
        mutate(
          degree = centrality_degree(mode = "all"),
          betweenness = centrality_betweenness(directed = TRUE),
          closeness = centrality_closeness(mode = "all"),
          eigen = centrality_eigen(directed = FALSE)
        ) %>%
        as_tibble() %>%
        # Filter for people/groups and handle NA names
        filter(node_type %in% c("Person", "MusicalGroup")) %>%
        mutate(
          name = ifelse(is.na(name), paste0("Unknown_", id), name),
          name = as.character(name)
        ) %>%
        select(id, name, degree, betweenness, closeness, eigen)
      
      # Standardize centrality measures (0-100 scale)
      centrality_scaled <- centrality_measures %>%
        mutate(across(c(degree, betweenness, closeness, eigen), 
                      ~ scales::rescale(., to = c(0, 100))))
      
      # Calculate composite centrality score (weighted average)
      centrality_scores <- centrality_scaled %>%
        mutate(
          central_score = round(
            0.3 * degree + 
              0.4 * betweenness + 
              0.2 * closeness + 
              0.1 * eigen,
            1
          ),
          prediction_tier = case_when(
            central_score >= quantile(central_score, 0.9, na.rm = TRUE) ~ "High Influence",
            central_score >= quantile(central_score, 0.7, na.rm = TRUE) ~ "Moderate Influence",
            TRUE ~ "Low Influence"
          )
        ) %>%
        arrange(desc(central_score))
      
      # Join with artist profile data using person_id = id
      network_pred <- centrality_scores %>%
        left_join(
          artists_profile %>% 
            select(person_id, total_works, notable_works, collaborations),
          by = c("id" = "person_id")
        ) %>%
        select(id, name, central_score, prediction_tier,
               degree, betweenness, closeness, eigen,
               total_works, notable_works, collaborations)
      
      # Filter by genre if selected
      if (!is.null(input$future_genres)) {
        genre_artists <- artist_works %>%
          filter(genre %in% input$future_genres) %>%
          distinct(person_id) %>%
          pull(person_id)
        
        network_pred <- network_pred %>%
          filter(id %in% genre_artists)
      }
      
      return(network_pred)
      
    }, error = function(e) {
      showNotification(paste("Network Centrality Error:", e$message), type = "error")
      return(data.frame()) # Return empty dataframe on error
    })
  })
    
  
  # Future prediction table output
  output$futureStarsTable <- DT::renderDataTable({
    if (input$prediction_method == "Composite Score") {
      prediction_data <- composite_score_prediction()
      
      if (!input$show_all_predictions) {
        prediction_data <- prediction_data %>% 
          slice_head(n = input$top_n_predictions)
      }
      
      datatable(
        prediction_data,
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE,
        caption = "Future Stars Prediction - Composite Score Method"
      ) %>%
        formatStyle(
          'prediction_tier',
          backgroundColor = styleEqual(
            c("High Potential", "Moderate Potential", "Emerging"),
            c("#4daf4a", "#377eb8", "#e41a1c")  # Green/Blue/Red
          )
        )
    } 
    # Network Centrality Prediction
    else if (input$prediction_method == "Network Centrality") {
      prediction_data <- network_prediction()
      
      if (nrow(prediction_data) == 0) {
        return(datatable(data.frame(Message = "No network data available"), 
                         options = list(dom = 't')))
      }
      
      if (!input$show_all_predictions) {
        prediction_data <- prediction_data %>% slice_head(n = input$top_n_predictions)
      }
      
      datatable(
        prediction_data,
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE,
        caption = "Future Stars Prediction - Network Centrality Method"
      ) %>%
        formatStyle(
          'prediction_tier',
          backgroundColor = styleEqual(
            c("High Influence", "Moderate Influence", "Low Influence"),
            c("#4daf4a", "#377eb8", "#e41a1c")
          )
        )
    }
  })
  
  # Future radar plot
  output$futureRadarPlot <- renderPlot({
    req(input$run_future_prediction)
    
    prediction_data <- composite_score_prediction()
    # Historical comparison plot
    output$historicalComparisonPlot <- renderPlotly({
      req(input$run_prediction)
      
      # Get current predictions
      current_pred <- switch(input$prediction_method,
                             "Composite Score" = composite_score_prediction(),
                             "Cluster Analysis" = cluster_prediction(),
                             "Growth Trajectory" = growth_prediction(),
                             "Network Centrality" = network_prediction())
      
      # Get top artists from current prediction
      top_current <- current_pred %>% slice_head(n = 5) %>% pull(name)
      
      # Get historical data for these artists
      historical_data <- artist_works %>%
        left_join(people_tbl, by = "person_id") %>%
        filter(name %in% top_current) %>%
        count(name, release_date) %>%
        complete(name, release_date = full_seq(release_date, 1), fill = list(n = 0))
      
      # Plot
      ggplotly(
        ggplot(historical_data, aes(x = release_date, y = n, color = name)) +
          geom_line() +
          geom_point() +
          labs(
            title = "Historical Work Output of Predicted Stars",
            x = "Year", y = "Number of Works", color = "Artist"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
      )
      })
    
    if (nrow(prediction_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No predictions available for the selected filters", col = "red")
      return()
    }
    
    required_cols <- c("productivity_score", "notability_score", "collab_score", "diversity_score", "time_score")
    if (!all(required_cols %in% colnames(prediction_data))) {
      plot.new()
      text(0.5, 0.5, "Required score columns are missing.", col = "red")
      return()
    }
    
    # Invert time_score
    prediction_data$time_score <- max(prediction_data$time_score, na.rm = TRUE) - prediction_data$time_score
    
    # Apply Top N logic
    if (input$show_all_predictions) {
      top_artists <- prediction_data %>% slice_head(n = 3)
    } else {
      top_n <- min(input$top_n_predictions, nrow(prediction_data))
      top_artists <- prediction_data %>% slice_head(n = top_n)
    }
    
    # Scale radar values
    radar_scaled <- top_artists %>%
      select(name, all_of(required_cols)) %>%
      column_to_rownames("name") %>%
      mutate(across(everything(), ~scales::rescale(., to = c(0, 100))))
    
    # Set color palette
    colors <- RColorBrewer::brewer.pal(max(3, nrow(radar_scaled)), "Set2")[1:nrow(radar_scaled)]
    
    # Layout: Max 3 per row
    n_plots <- nrow(radar_scaled)
    ncol <- min(3, n_plots)
    nrow <- ceiling(n_plots / ncol)
    par(mfrow = c(nrow, ncol), mar = c(2, 2, 4, 2))
    
    # Loop through each artist
    for (i in 1:n_plots) {
      artist_name <- rownames(radar_scaled)[i]
      artist_values <- radar_scaled[i, , drop = FALSE]
      
      radar_frame <- rbind(rep(100, ncol(artist_values)), rep(0, ncol(artist_values)), artist_values)
      
      radarchart(radar_frame,
                 axistype = 1,
                 pcol = colors[i],
                 pfcol = adjustcolor(colors[i], alpha.f = 0.3),
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
  
  
  # Future metrics plot
  output$futureMetricsPlot <- renderPlotly({
    req(input$run_future_prediction)
    
    pred_data <- switch(input$prediction_method,
                        "Composite Score" = composite_score_prediction(),
                        "Growth Trajectory" = growth_prediction(),
                        "Network Centrality" = network_prediction(),
                        "Cluster Analysis" = cluster_prediction()
    )
    
    # Return NULL if no data
    if (is.null(pred_data) || nrow(pred_data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No data available for selected filters."))
    }
    
    # ===== Composite Score Metrics =====
    if (input$prediction_method == "Composite Score") {
      
      # Box plot of scores by prediction tier
      p <- plot_ly(pred_data, 
                   y = ~future_star_score, 
                   color = ~prediction_tier, 
                   type = "box",
                   boxpoints = "all",
                   jitter = 0.3,
                   pointpos = 0,
                   hoverinfo = "y+name") %>%
        layout(
          title = "Distribution of Composite Scores by Prediction Tier",
          yaxis = list(title = "Composite Score"),
          xaxis = list(title = "Prediction Tier"),
          boxmode = "group"
        )
      
      # Add density plot
      density_plot <- plot_ly(pred_data, 
                              x = ~future_star_score, 
                              type = "histogram",
                              histnorm = "probability density",
                              opacity = 0.7,
                              name = "Score Distribution") %>%
        layout(
          xaxis = list(title = "Composite Score"),
          yaxis = list(title = "Density")
        )
      
      # Combine plots
      subplot(p, density_plot, nrows = 2, margin = 0.05) %>%
        layout(title = "Composite Score Analysis")
    }
    
    # ===== Growth Trajectory Metrics =====
    else if (input$prediction_method == "Growth Trajectory") {
      growth_data <- growth_prediction()
      
      # Create ggplot2-based stats plot
      stats_plot <- ggstatsplot::ggbetweenstats(
        data = growth_data,
        x = growth_tier,
        y = slope,
        type = "parametric",
        var.equal = FALSE,
        plot.type = "boxviolin",
        pairwise.comparisons = TRUE,
        pairwise.display = "significant",
        p.adjust.method = "holm",
        centrality.plotting = FALSE,
        ggplot.component = list(
          ggplot2::labs(
            x = "Growth Tier",
            y = "Growth Rate (Slope)",
            title = "Growth Rate Distribution with Statistical Comparisons"
          ),
          ggplot2::theme_minimal()
        )
      )
      
      # Convert to plotly
      fig2 <- ggplotly(stats_plot) %>% 
        layout(showlegend = FALSE)
      
      # Create scatter plot (fig1)
      fig1 <- plot_ly(growth_data,
                      x = ~slope,
                      y = ~r_squared,
                      color = ~growth_tier,
                      colors = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
                      type = 'scatter',
                      mode = 'markers',
                      size = ~total_works,
                      sizes = c(5, 30),
                      text = ~paste(
                        "<b>", name, "</b><br>",
                        "Growth Rate: ", round(slope, 3), "<br>",
                        "R²: ", round(r_squared, 3), "<br>",
                        "Works: ", total_works
                      ),
                      hoverinfo = 'text',
                      marker = list(opacity = 0.8, line = list(width = 1))) %>%
        layout(
          xaxis = list(title = "Growth Rate (Slope)"),
          yaxis = list(title = "Model Fit (R²)"),
          showlegend = TRUE
        )
      
      # Combine plots
      subplot(
        fig1, 
        fig2,
        nrows = 2,
        margin = 0.05,
        heights = c(0.35, 0.65)
      ) %>%
        layout(
          title = list(
            text = "<b>Comprehensive Growth Trajectory Analysis</b>",
            x = 0.5,
            y = 0.95,
            font = list(size = 16)
          ),
          margin = list(t = 80)
        )
    } 
    
    # ===== Network Centrality Metrics =====
    else if (input$prediction_method == "Network Centrality") {
      data <- network_prediction()
      
      # Box plot of centrality by tier
      p1 <- plot_ly(data, 
                    y = ~central_score, 
                    color = ~prediction_tier, 
                    type = "box") %>%
        layout(title = "Centrality Distribution by Tier")
      
      # ANOVA-style plot: Collaborations by Tier (mean ± SD)
      p2 <- data %>%
        group_by(prediction_tier) %>%
        summarise(
          mean_collab = mean(collaborations, na.rm = TRUE),
          sd_collab = sd(collaborations, na.rm = TRUE),
          n = n()
        ) %>%
        mutate(
          se = sd_collab / sqrt(n)
        ) %>%
        plot_ly(
          x = ~prediction_tier,
          y = ~mean_collab,
          type = "bar",
          error_y = list(type = "data", array = ~sd_collab, visible = TRUE),
          name = "Mean Collaborations"
        ) %>%
        layout(
          title = "Mean Collaborations by Prediction Tier (± SD)",
          yaxis = list(title = "Mean Collaborations"),
          xaxis = list(title = "Prediction Tier")
        )
      
      subplot(p1, p2, nrows = 2)
    }
    
  })
  
  
  output$futureNetworkPlot <- renderUI({
    req(input$prediction_method == "Network Centrality")
    req(input$top_n_network)
    
    n_display <- min(input$top_n_network, 6)
    
    top_artists <- network_prediction() %>%
      inner_join(nodes_tbl %>% filter(supertype == "Individual") %>% select(id, name), by = "id") %>%
      arrange(desc(central_score)) %>%
      slice_head(n = n_display)
    
    if (nrow(top_artists) == 0) {
      return(h4("No individual artists available for ego networks."))
    }
    
    tagList(
      lapply(seq_len(nrow(top_artists)), function(i) {
        artist_id <- top_artists$id[i]
        artist_name <- top_artists$name[i]
        output_id <- paste0("egoGraph_", i)
        
        output[[output_id]] <- renderVisNetwork({
          # --- Use tidygraph operations for ego filtering ---
          ego_subgraph <- graph %>%
            activate(nodes) %>%
            filter(node_is_adjacent(which(nodes_tbl$id == artist_id)) | id == artist_id)
          
          # --- Prepare nodes data ---
          nodes_data <- ego_subgraph %>%
            activate(nodes) %>%
            as_tibble() %>%
            mutate(
              id = id,  # already valid
              label = ifelse(!is.na(name), name, node_type),
              group = node_type,
              color = ifelse(id == artist_id, "gold", "#8FBC8F"),
              title = paste0("<b>", label, "</b><br>Type: ", node_type)
            ) %>%
            select(id, label, group, title, color)
          
          # --- Prepare edges data ---
          edges_data <- ego_subgraph %>%
            activate(edges) %>%
            as_tibble() %>%
            mutate(
              from = as.integer(from),
              to = as.integer(to),
              label = edge_type,
              title = paste("Edge Type:", edge_type)
            ) %>%
            select(from, to, label, title)
          
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
        
        tagList(
          h4(paste0("Ego Network: ", artist_name)),
          visNetworkOutput(output_id, height = "500px"),
          tags$hr()
        )
      })
    )
  })
  
  
  
  
  
  
  # === Historical Analysis Subpage ===
  
  # Reactive function to filter artists by historical time frame
  historical_filtered_artists <- reactive({
    req(input$historical_timeframe)
    
    # Get artists active in the selected historical time frame
    active_artists <- artist_works %>%
      filter(release_date >= input$historical_timeframe[1],
             release_date <= input$historical_timeframe[2]) %>%
      distinct(person_id) %>%
      pull(person_id) 
    
    # Filter profile to only include active artists
    artists_profile %>%
      filter(person_id %in% active_artists)
  })
  
  # Reactive function to filter historical artists by genre if selected
  historical_genre_filtered_artists <- reactive({
    req(historical_filtered_artists())
    
    if (!is.null(input$historical_genres)) {
      # Get artists who have works in the selected genres
      genre_artists <- artist_works %>%
        filter(genre %in% input$historical_genres) %>%
        distinct(person_id) %>%
        pull(person_id)
      
      historical_filtered_artists() %>%
        filter(person_id %in% genre_artists)
    } else {
      historical_filtered_artists()
    }
  })
  
  # Historical trend plot
  output$historicalTrendPlot <- renderPlotly({
    req(input$run_historical_analysis)
    
    # Get data for selected time frame and genres
    trend_data <- artist_works %>%
      left_join(people_tbl, by = "person_id") %>%
      filter(release_date >= input$historical_timeframe[1],
             release_date <= input$historical_timeframe[2])
    
    if (!is.null(input$historical_genres)) {
      trend_data <- trend_data %>%
        filter(genre %in% input$historical_genres)
    }
    
    # Aggregate by year
    yearly_data <- trend_data %>%
      count(release_date, name = "count")
    
    # Plot
    p <- ggplot(yearly_data, aes(x = release_date, y = count)) +
      geom_line(color = "#377eb8") +
      geom_point(color = "#e41a1c") +
      labs(title = "Historical Trend of Artist Activity",
           x = "Year", y = "Number of Works") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Historical top artists table
  output$historicalTopArtistsTable <- DT::renderDataTable({
    req(input$run_historical_analysis)
    
    # Get top artists by notable works in the historical period
    top_artists <- historical_genre_filtered_artists() %>%
      arrange(desc(notable_works)) %>%
      select(name, notable_works, total_works, oceanus_folk_works, collaborations)
    
    datatable(
      top_artists,
      options = list(scrollX = TRUE, pageLength = 10),
      rownames = FALSE,
      colnames = c("Artist", "Notable Works", "Total Works", "Oceanus Folk Works", "Collaborations")
    )
  })
  
  # Historical comparison plot
  output$historicalComparisonPlot <- renderPlotly({
    req(input$run_historical_analysis)
    
    # Get top artists from historical period
    top_artists <- historical_genre_filtered_artists() %>%
      arrange(desc(notable_works)) %>%
      slice_head(n = 5) %>%
      pull(name)
    
    # Get their complete career data with notable works marked
    career_data <- artist_works %>%
      left_join(people_tbl, by = "person_id") %>%
      filter(name %in% top_artists) %>%
      group_by(name, release_date) %>%
      summarise(
        total_works = n(),
        notable_count = sum(notable, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        name = factor(name, levels = top_artists),
        tooltip = paste0(
          "<b>", name, "</b><br>",
          "Year: ", release_date, "<br>",
          "Total works: ", total_works, "<br>",
          "Notable works: ", notable_count
        )
      )
    
    # Calculate padding for y-axis (based on max point size)
    y_padding <- 0.2 * max(career_data$total_works, na.rm = TRUE)
    
    # Create facet plot with expanded y-axis
    p <- ggplot(career_data, aes(x = release_date, y = total_works)) +
      geom_line(aes(color = name), alpha = 0.7) +
      geom_point(aes(color = name, size = notable_count, text = tooltip), alpha = 0.8) +
      facet_wrap(~name, ncol = 2, scales = "free_y") +
      scale_y_continuous(limits = function(x) c(0, max(x) + y_padding)) + # Add padding
      labs(
        title = "Faceted Career Trajectories of Historical Top Artists",
        x = "Year",
        y = "Number of Works",
        color = "Artist",
        size = "Notable Works"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 10, face = "bold")
      ) +
      scale_size_continuous(range = c(3, 8)) + # Adjusted point size range
      scale_color_brewer(palette = "Set1")
    
    # Convert to interactive plotly with facet wrapping
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(t = 60, b = 60)  # Add top and bottom margin
      ) %>%
      config(displayModeBar = TRUE)
  })
  
    
    
    
  # ================= End of Predicition Analysis Page =======================
  
}

# ===== Run App =====
shinyApp(ui = ui, server = server)
