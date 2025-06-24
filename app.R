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

# ===== Data Preprocessing =====
data_path <- "data/MC1_graph.json"
kg <- fromJSON(data_path)

nodes_tbl <- as_tibble(kg$nodes) %>% clean_names()
edges_tbl <- as_tibble(kg$links) %>% clean_names()
nodes_tbl <- nodes_tbl %>% mutate(release_date = as.integer(release_date))
id_map <- tibble(id = nodes_tbl$id, index = seq_len(nrow(nodes_tbl)))
edges_tbl_mapped <- edges_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>% rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>% rename(to = index) %>%
  filter(!is.na(from), !is.na(to)) %>%
  mutate(from = as.integer(from), to = as.integer(to))
nodes_tbl_clean <- nodes_tbl %>% mutate(id = as.integer(id)) %>% distinct()
graph <- tbl_graph(nodes = nodes_tbl_clean, edges = edges_tbl_mapped, directed = TRUE)

people <- nodes_tbl_clean %>% filter(node_type == "Person") %>% select(person_id = id, name)
songs_albums <- nodes_tbl_clean %>% filter(node_type %in% c("Song", "Album")) %>% select(work_id = id, release_date, genre, notable)

contribution_types <- c("ComposerOf", "PerformerOf", "ProducerOf", "LyricistOf", "RecordedBy")
collab_types <- c("LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "DirectlySamples")

created_links <- edges_tbl_mapped %>%
  filter(edge_type %in% contribution_types) %>%
  left_join(id_map, by = c("from" = "index")) %>% rename(person_id = id) %>%
  left_join(id_map, by = c("to" = "index")) %>% rename(work_id = id)

artist_works <- created_links %>% left_join(songs_albums, by = "work_id") %>% filter(!is.na(release_date))

rising_star_profile <- artist_works %>%
  group_by(person_id) %>%
  summarise(
    total_works = n(),
    notable_works = sum(notable, na.rm = TRUE),
    oceanus_folk_works = sum(genre == "Oceanus Folk", na.rm = TRUE),
    first_release = min(as.integer(release_date), na.rm = TRUE),
    first_notable = min(as.integer(release_date[notable == TRUE]), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    first_notable = ifelse(is.infinite(first_notable), NA_integer_, first_notable),
    time_to_notability = ifelse(!is.na(first_notable), first_notable - first_release, NA)
  )

collaborations <- edges_tbl_mapped %>%
  filter(edge_type %in% collab_types) %>%
  count(from, name = "collaborations") %>%
  left_join(id_map, by = c("from" = "index")) %>% rename(person_id = id)

rising_star_profile <- rising_star_profile %>%
  left_join(collaborations, by = "person_id") %>%
  mutate(collaborations = replace_na(collaborations, 0)) %>%
  inner_join(people, by = "person_id") %>%
  relocate(person_id, name)

# ===== UI =====
ui <- fluidPage(
  titlePanel("Oceanus Folk Influence Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      navlistPanel(
        id = "main_tabs",
        widths = c(12, 12),
        tabPanel("Overview"),
        tabPanel("EDA"),
        tabPanel("Rising Stars"),
        tabPanel("Influence Network"),
        tabPanel("Cluster Analysis"),
        tabPanel("Artist Comparison"),
        tabPanel("Future Predictions")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'EDA'",
        sliderInput("year_range", "Select Year Range:",
                    min = min(nodes_tbl$release_date, na.rm = TRUE),
                    max = max(nodes_tbl$release_date, na.rm = TRUE),
                    value = c(2000, 2040), step = 1, sep = ""),
        selectInput("genre_select", "Select Genres:",
                    choices = sort(unique(na.omit(nodes_tbl$genre))),
                    selected = NULL, multiple = TRUE)
      )
    ),
    mainPanel(
      uiOutput("subtabs_ui")
    )
  )
)

# ===== Server =====
server <- function(input, output, session) {
  
  output$subtabs_ui <- renderUI({
    switch(input$main_tabs,
           "Overview" = h3("Explore Sailor Shift's career, Oceanus Folk genre evolution, and rising music stars."),
           
           "EDA" = tabsetPanel(
             tabPanel("Edge Types", plotOutput("edgeTypePlot")),
             tabPanel("Node Types", plotOutput("nodeTypePlot")),
             tabPanel("Genre Trends", plotlyOutput("genreHeatmap")),
             tabPanel("Notable Songs", plotOutput("notableSongsPlot"))
           ),
           
           "Rising Stars" = DT::dataTableOutput("risingStarTable"),
           
           "Influence Network" = visNetworkOutput("influenceNetwork", height = "700px"),
           
           "Cluster Analysis" = tagList(
             plotOutput("elbowPlot"),
             plotlyOutput("pcaPlot"),
             DT::dataTableOutput("clusterSummary")
           ),
           
           "Artist Comparison" = tagList(
             DT::dataTableOutput("artistComparisonTable"),
             plotOutput("timelinePlot"),
             visNetworkOutput("egoNetwork", height = "600px")
           ),
           
           "Future Predictions" = tagList(
             DT::dataTableOutput("futureStarsTable"),
             plotOutput("radar1"),
             plotOutput("radar2"),
             plotOutput("radar3")
           )
    )
  })
  
  output$edgeTypePlot <- renderPlot({
    ggplot(edges_tbl_mapped, aes(y = edge_type)) +
      geom_bar() +
      labs(title = "Distribution of Edge Types", y = "Edge Type", x = "Count") +
      theme_minimal()
  })
  
  output$nodeTypePlot <- renderPlot({
    ggplot(nodes_tbl_clean, aes(y = node_type)) +
      geom_bar() +
      labs(title = "Distribution of Node Types", y = "Node Type", x = "Count") +
      theme_minimal()
  })
  
  output$risingStarTable <- DT::renderDataTable({
    rising_star_profile
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
  
  output$risingStarTable <- DT::renderDataTable({
    rising_star_profile
  })
}

# ===== Run App =====
shinyApp(ui = ui, server = server)
