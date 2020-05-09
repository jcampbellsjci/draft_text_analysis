# This script houses the similar players function
# A player is input and the 10 most similar players are returned
# Also outputs 10 most frequent strength and weakness words of these players

similar_players <- function(input_player, total_wv, strength_tokenized,
                            weakness_tokenized){
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
    filter(similarities < 1) %>%
    top_n(n = 10, similarities) %>%
    arrange(desc(similarities))
  
  top_strength <- similar_players_df %>%
    left_join(strength_tokenized, by = "player") %>%
    group_by(word) %>%
    count() %>%
    ungroup() %>%
    top_n(10, n) %>%
    arrange(desc(n))
  
  top_weakness <- similar_players_df %>%
    left_join(weakness_tokenized, by = "player") %>%
    group_by(word) %>%
    count() %>%
    ungroup() %>%
    top_n(10, n) %>%
    arrange(desc(n))
  
  list(similar_players_df, top_strength, top_weakness)
  
}
