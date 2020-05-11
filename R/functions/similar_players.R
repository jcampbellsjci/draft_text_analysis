# This script houses the similar players function
# A player is input and the 10 most similar players are returned
# Also outputs 10 most frequent strength and weakness words of these players

similar_players <- function(input_player, scouting_reports_df, total_wv){
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
  
  
  wv_vector <- total_wv %>%
    as.data.frame() %>%
    rownames_to_column("player") %>%
    filter(player == input_player) %>%
    column_to_rownames(var = "player") %>%
    as.matrix() %>%
    .[1, ]
  
  similarities <- total_wv %*% wv_vector /
    sqrt(sum(wv_vector * wv_vector) * rowSums(total_wv * total_wv))
  
  similar_players_df <- tibble(player = rownames(similarities),
                               similarities = similarities[, 1]) %>%
    top_n(n = 10, similarities) %>%
    arrange(desc(similarities))
  
  top_strength <- similar_players_df %>%
    left_join(strength_tokenized, by = "player") %>%
    group_by(word) %>%
    count() %>%
    ungroup() %>%
    top_n(50, n) %>%
    arrange(desc(n))
  
  top_weakness <- similar_players_df %>%
    left_join(weakness_tokenized, by = "player") %>%
    group_by(word) %>%
    count() %>%
    ungroup() %>%
    top_n(50, n) %>%
    arrange(desc(n))
  
  list(similar_players_df, top_strength, top_weakness)
  
}
