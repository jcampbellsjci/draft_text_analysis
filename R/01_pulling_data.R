library(rvest)
library(tidyverse)


#### Pulling Data ####

# Looping through urls of mock drafts
# This will allow us to pull names of eligible players
urls <- list(`2011` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2011",
             `2012` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2012",
             `2013` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2013",
             `2014` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2014",
             `2015` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2015",
             `2016` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2016",
             `2017` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2017",
             `2018` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2018",
             `2019` = "https://www.nbadraft.net/nba-mock-drafts/?year-mock=2019",
             `2020` = "https://www.nbadraft.net/nba-mock-drafts/")
# 1st round
prospects <- map_dfr(urls,
                     ~ read_html(.) %>%
                       html_node(css = '#nba_mock_consensus_table') %>%
                       html_table())
# 2nd round
prospects_second <- map_dfr(urls,
                            ~ read_html(.) %>%
                              html_node(css = '#nba_mock_consensus_table2') %>%
                              html_table())
# Combine first and second rounds
prospects <- prospects %>%
  bind_rows(prospects_second)


# Cleaning up player column
clean_players <- prospects %>%
  mutate(player_url = tolower(Player)) %>%
  mutate(player_url = gsub(" ", "-", player_url)) %>%
  mutate(player_url = gsub("[.']", "", player_url)) %>%
  mutate(player_url = ifelse(player_url == "shai-gilgeous-al",
                             "shai-gilgeous-alexander", player_url)) %>%
  select(player_url)


# Creating a set of urls for player scouting reports
player_urls <- map(clean_players$player_url,
                   ~ paste0("https://www.nbadraft.net/players/", .))
# Pulling scouting reports
scouting_reports <- map(player_urls,
                        ~ read_html(.) %>%
                          html_node(css = '#analysis') %>%
                          html_text())
names(scouting_reports) <- clean_players$player_url

# Cleaning up scouting reports
cleaned_sr <- map(scouting_reports,
                  ~ gsub("\n", " ", .) %>%
                    # Remove everything before strengths
                    sub(".*?Strengths", "", .) %>%
                    # After weaknesses, we can run into a couple of things
                    # Whether it's notes, outlook, etc.
                    # We can get rid of whatever is after weaknesses
                    sub("Notes:.*", "", .) %>%
                    sub("Outlook:.*", "", .) %>%
                    sub("Overall:.*", "", .) %>%
                    # Split strengths and weaknesses
                    strsplit(., "Weaknesses"))

# Remove anyone who only has one element
# Should have 2: Strengths and Weaknesses
# If they have less, they probably don't have a scouting report
cleaned_sr <- cleaned_sr[lapply(sapply(cleaned_sr, "[[", 1), length) > 1]

# Create a tibble to store player, strengths, and weaknesses
scouting_reports_df <- tibble(player = names(cleaned_sr),
                              strengths = sapply(lapply(cleaned_sr,
                                                        "[[", 1), "[[", 1),
                              weaknesses = sapply(lapply(cleaned_sr,
                                                         "[[", 1), "[[", 2))
