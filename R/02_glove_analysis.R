library(babynames)
library(Rtsne)
library(tidytext)
library(text2vec)
library(tidyverse)


#### Data Prep ####

# Tokenizing both words in the strength and weakness categories
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


#### Building GloVe Model ####

# Data prep for model
t_ls_strength <- list(strength_tokenized$word)
it_strength <- itoken(t_ls_strength)
vocab_strength <- create_vocabulary(it_strength)
vocab_strength <- prune_vocabulary(vocab_strength, term_count_min = 1)
vectorizer_strength <- vocab_vectorizer(vocab_strength)
tcm_strength <- create_tcm(it_strength, vectorizer_strength,
                           skip_grams_window = 3)
# Same for weaknesses
t_ls_weakness <- list(weakness_tokenized$word)
it_weakness <- itoken(t_ls_weakness)
vocab_weakness <- create_vocabulary(it_weakness)
vocab_weakness <- prune_vocabulary(vocab_weakness, term_count_min = 1)
vectorizer_weakness <- vocab_vectorizer(vocab_weakness)
tcm_weakness <- create_tcm(it_weakness, vectorizer_weakness,
                           skip_grams_window = 3)

# Initializing and building model
glove_strength <- GlobalVectors$new(rank = 50, x_max = 10)
wv_matrix_strength <- glove_strength$fit_transform(tcm_strength)
wv_context_strength <- glove_strength$components
word_vectors_strength <- wv_matrix_strength + t(wv_context_strength)
# Same for weakness
glove_weakness <- GlobalVectors$new(rank = 50, x_max = 10)
wv_matrix_weakness <- glove_weakness$fit_transform(tcm_weakness)
wv_context_weakness <- glove_weakness$components
word_vectors_weakness <- wv_matrix_weakness + t(wv_context_weakness)


#### Averaging Results ####

# Squishing values by player
# Each player will have one row representing their strengths and weaknesses
avg_wv_strength <- strength_tokenized %>%
  inner_join(word_vectors_strength %>%
               as.data.frame() %>%
               rownames_to_column("word")) %>%
  group_by(player) %>%
  distinct(word, .keep_all = T) %>%
  summarize_at(.vars = vars(V1:V50), .funs = ~ mean(.)) %>%
  ungroup() %>%
  column_to_rownames(var = "player") %>%
  as.matrix()
# Same for weakness
avg_wv_weakness <- weakness_tokenized %>%
  inner_join(word_vectors_weakness %>%
               as.data.frame() %>%
               rownames_to_column("word")) %>%
  group_by(player) %>%
  distinct(word, .keep_all = T) %>%
  summarize_at(.vars = vars(V1:V50), .funs = ~ mean(.)) %>%
  ungroup() %>%
  column_to_rownames(var = "player") %>%
  rename_all(.funs = ~ paste0("V", 51:100)) %>%
  as.matrix()

# Combining strength and weakness ogether
total_wv <- cbind(avg_wv_strength, avg_wv_weakness)


#### Clustering Results ####

# Scaling tibble
words_scaled <- scale(total_wv)

# Performing dimensionality reduction for plotting
total_tsne <- Rtsne(words_scaled, dims = 2, verbose = T,
                    perplexity = 10)

# Clustering results
total_clusters <- kmeans(words_scaled, centers = 4)
total_clusters$size

# Plotting results by reduced dimensions
total_clusters_df <- tibble(player = rownames(total_wv),
                            cluster = factor(total_clusters$cluster),
                            tsne_dim1 = total_tsne$Y[, 1],
                            tsne_dim2 = total_tsne$Y[, 2])
total_clusters_df %>%
  ggplot(aes(x = tsne_dim1, y = tsne_dim2, col = cluster)) +
  geom_point(size = 2)

# Looking at most frequent words in each cluster
# Most frequent strength words
map(sort(unique(total_clusters$cluster)),
    ~ total_clusters_df %>%
      left_join(strength_tokenized, by = "player") %>%
      filter(cluster == .x) %>%
      count(word) %>%
      top_n(n = 10) %>%
      arrange(desc(n)) %>%
      column_to_rownames(var = "word") %>%
      t())
# Most frequent weakness words
map(sort(unique(total_clusters$cluster)),
    ~ total_clusters_df %>%
      left_join(weakness_tokenized, by = "player") %>%
      filter(cluster == .x) %>%
      count(word) %>%
      top_n(n = 10) %>%
      arrange(desc(n)) %>%
      column_to_rownames(var = "word") %>%
      t())


#### Similarities ####

# Creating a function to find most similar players to a certain player
# The output will be the 10 most common players and the most frequent shared words
similar_players <- function(input_player){
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

similar_players("myles-turner")
similar_players("luka-doncic")
similar_players("kawhi-leonard")

