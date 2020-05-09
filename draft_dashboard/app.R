library(shiny)
library(tidyverse)

source("../R/functions/similar_players.R")
scouting_reports_df <- read_csv("../data/scouting_reports.csv")
total_clusters_df <- read_csv("../data/clusters.csv")
total_wv <- read_csv("../data/word_vectors.csv") %>%
  column_to_rownames(var = "player") %>%
  as.matrix()

strength_tokenized <- scouting_reports_df %>%
  unnest_tokens(input = strengths, output = "word", token = "ngrams",
                n = 1) %>%
  select(player, word) %>%
  # Removing stop words
  anti_join(stop_words, by = "word") %>%
  anti_join(babynames, by = c("word" = "name")) %>%
  # Removing some common words that don't hold a lot of meaning
  filter(!(word %in% c("game", "ability", "ball", "_blank"))) %>%
  # Removing any token with a number
  filter(!(grepl("[0-9]", word)))
# Same for weakness
weakness_tokenized <- scouting_reports_df %>%
  unnest_tokens(input = weaknesses, output = "word", token = "ngrams",
                n = 1) %>%
  select(player, word) %>%
  # Removing stop words
  anti_join(stop_words, by = "word") %>%
  anti_join(babynames, by = c("word" = "name")) %>%
  # Removing some common words that don't hold a lot of meaning
  filter(!(word %in% c("game", "ability", "ball", "_blank"))) %>%
  # Removing any token with a number
  filter(!(grepl("[0-9]", word)))


ui <- fluidPage(
  
  titlePanel("Scouting Report Similarities"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "draft_class", label = "Draft Class",
                  choices = sort(unique(scouting_reports_df$draft_class))),
      
      uiOutput("available_players")
      ),
    mainPanel(plotOutput("cluster_plot"))
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
    similar_players(input$players, total_wv,
                    strength_tokenized, weakness_tokenized)
  })
  
  output$cluster_plot <- renderPlot({
    similar_player_df <- similar_player_output()[[1]]
    
    total_clusters_df %>%
      mutate(player_filter = ifelse(player %in%
                                      c(input$players,
                                        similar_player_df$player),
                                    "Y", "N"),
             cluster = factor(cluster)) %>%
      ggplot(aes(x = tsne_dim1, y = tsne_dim2, col = cluster,
                 alpha = player_filter)) +
      geom_point(size = 4)
  })
}

shinyApp(ui = ui, server = server)