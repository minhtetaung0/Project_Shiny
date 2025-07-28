# 🎶 Oceanus Folk Influence Explorer

**Oceanus Folk Influence Explorer** is an interactive R Shiny dashboard developed for the VAST Challenge 2025. The app visualizes the evolution and influence of the *Oceanus Folk* music genre, especially in relation to the emerging *Sailor Shift* movement. It enables deep dives into artist profiles, genre timelines, influence networks, and cluster-based insights.

## 🔍 Key Features

- **Overview Dashboard**: Summary KPIs, donut plots, genre timelines, artist table
- **Influence Network**: Interactive graph of artist influences with filtering options
- **Artist Profiles**: Ranked listing with customizable metrics and top-N filtering
- **Clustering Analysis**: Segment artists based on musical traits using DBSCAN, PAM, etc.
- **Growth Trajectory**: Identify rising stars using predictive profiling
- **Future Star Prediction**: Rule-based or metric-driven future star identification
- **Genre Timeline**: Track changes in Oceanus Folk over time

## 📁 Project Structure

Project_Shiny/
├── app.R # Main Shiny app
├── data/
│ ├── MC1_graph.json # Original dataset
│ ├── processed/ # Preprocessed .rds files
├── www/
│ ├── custom.css # CSS for dashboard styling
├── README.md # Project documentation


## ⚙️ Getting Started

To run the app locally:

1. Clone the repository:
   ```bash
   git clone https://github.com/minhtetaung0/Project_Shiny.git
   cd Project_Shiny

2. Open R or RStudio and install dependencies:
   ```{r}
   install.packages(c("shiny", "tidyverse", "igraph", "ggraph", "DT", 
                   "plotly", "shinydashboard", "cluster", "dbscan", 
                   "FactoMineR", "factoextra", "visNetwork", "fmsb", 
                   "RColorBrewer", "treemapify", "lubridate", "jsonlite", 
                   "janitor", "forcats", "shinythemes", "NbClust"))
   ```
3. Run the app:
   ```{r}
   shiny::runApp("app.R")
   ```

## 👨‍💻 Developed By
This project was created as part of the Visual Analytics Application module at SMU MITB (2025).
Developed by Group 13:

- Huang Zihan
- Min Htet Aung
- Ta Nguyen Thao Nguyen

## 📜 License
All rights reserved to the team.
Developed for academic purposes only.
