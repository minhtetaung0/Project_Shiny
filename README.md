# 🎶 Oceanus Folk Influence Explorer

**Oceanus Folk Influence Explorer** is an interactive R Shiny dashboard developed for the VAST Challenge 2025. The app visualizes the evolution and influence of the *Oceanus Folk* music genre, especially in relation to the emerging *Sailor Shift* movement. It enables deep dives into artist profiles, genre timelines, influence networks, and cluster-based insights.
🔗 **Live Shiny APP:** [https://vaa-project-group13.shinyapps.io/Project_Shiny/](https://vaa-project-group13.shinyapps.io/Project_Shiny/)

## 🔍 Key Features

- **Overview Dashboard**: Summary KPIs, donut plots, genre timelines, artist table
- **Influence Network**: Interactive graph of artist influences with filtering options
- **Artist Profiles**: Ranked listing with customizable metrics and top-N filtering
- **Clustering Analysis**: Segment artists based on musical traits using DBSCAN, PAM, etc.
- **Growth Trajectory**: Identify rising stars using predictive profiling
- **Future Star Prediction**: Rule-based or metric-driven future star identification
- **Genre Timeline**: Track changes in Oceanus Folk over time

## 📁 Project Structure

- `app.R`: The main Shiny application file.
- `data/`: Folder containing input and processed data.
  - `MC1_graph.json`: The original knowledge graph dataset.
  - `processed/`: Folder with preprocessed `.rds` files used by the app.
- `www/`: Static files such as custom CSS and images.
  - `custom.css`: Styling for the dashboard.
- `README.md`: This documentation file.


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

- [Huang Zihan](https://www.linkedin.com/in/zihan-huang-00775822a/)
- [Min Htet Aung](https://www.linkedin.com/in/min-htet-aung-david-li/)
- [Ta Nguyen Thao Nguyen](https://www.linkedin.com/in/cindy-ta-84771a254/)

## 📜 License
All rights reserved to the team.
Developed for academic purposes only.
