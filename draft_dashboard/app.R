library(tidytext)
library(DT)
library(babynames)
library(shiny)
library(plotly)
library(tidyverse)

source("../R/functions/similar_players.R")
source("../R/functions/highlight.R")
scouting_reports_df <- read_csv("../data/scouting_reports.csv")
total_clusters_df <- read_csv("../data/clusters.csv")
total_wv <- read_csv("../data/word_vectors.csv") %>%
  column_to_rownames(var = "player") %>%
  as.matrix()


ui <- fluidPage(
  
  titlePanel("Scouting Report Similarities"),
  
  fluidRow(
    column(5, plotlyOutput("cluster_plot")),
    column(7, DT::dataTableOutput("similarity_table"))
  ),
  
  fluidRow(
    column(4, selectInput(inputId = "draft_class", label = "Draft Class",
                          choices = sort(unique(
                            scouting_reports_df$draft_class)),
                          selected = 2020),
           br(),
           uiOutput("available_players"),
           style = "padding:20px;"),
    column(4, HTML("<b>Strengths Report</b>"), htmlOutput("strength_output"),
           style = "padding:20px;"),
    column(4, HTML("<b>Weaknesses Report</b>"), htmlOutput("weakness_output"),
           style = "padding:20px;")
  )
    
  
)

server <- function(input, output) {
  get_available_players <- reactive({
    available_players <- scouting_reports_df %>%
      filter(draft_class == input$draft_class)
    
    output <- available_players
  })
  
  output$available_players <- renderUI({
    available_teams <- get_available_players()
    
    selectInput("players", "Players", sort(available_teams$player))
  })
  
  similar_player_output <- reactive({
    similar_players(input$players, scouting_reports_df, total_wv)
  })
  
  output$similarity_table <- DT::renderDataTable({
    datatable(similar_player_output()[[1]] %>%
                left_join(scouting_reports_df) %>%
                mutate(similarities = round(similarities, 2)) %>%
                select(-c(strengths, weaknesses)),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't'))
  })
  
  output$strength_output <- renderText({
    strength <- similar_player_output()[[2]]
    
    player_filter <- (similar_player_output()[[1]] %>%
                        left_join(scouting_reports_df))[input$similarity_table_rows_selected, ]
    
    highlight(sub(":", "", player_filter$strengths[1]), strength$word)
  })
  
  output$weakness_output <- renderText({
    weakness <- similar_player_output()[[3]]
    
    player_filter <- (similar_player_output()[[1]] %>%
                        left_join(scouting_reports_df))[input$similarity_table_rows_selected, ]
    
    highlight(sub(":", "", player_filter$weaknesses[1]), weakness$word)
  })
  
  
  output$cluster_plot <- renderPlotly({
    similar_player_df <- similar_player_output()[[1]]
    
    cluster_plot_output <- scouting_reports_df %>%
      left_join(total_clusters_df %>%
                  select(player, tsne_dim1, tsne_dim2)) %>%
      left_join(similar_player_df) %>%
      mutate(player_filter = ifelse(player %in%
                                      c(input$players,
                                        similar_player_df$player),
                                    "Y", "N")) %>%
      ggplot(aes(x = tsne_dim1, y = tsne_dim2, fill = player_filter,
                 alpha = player_filter,
                 text = paste0("Player: ", player, "\n",
                               "Position: ", p, "\n",
                               "Draft Class: ", draft_class, "\n",
                               "Similarity: ", round(similarities, 2)))) +
      geom_point(pch = 21, size = 4) +
      scale_fill_manual(values = c("lightgrey", "skyblue")) +
      scale_alpha_manual(values = c(.2, 1)) +
      labs(x = "", y = "") +
      theme(legend.position = "none")
    
    ggplotly(cluster_plot_output, tooltip = "text") %>%
      style(hoverinfo = "none", traces = 1)
  })
}

shinyApp(ui = ui, server = server)