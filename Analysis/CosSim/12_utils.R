# Created by: CR
# Date: 7/25/26
# Functionalizing Pipeline to save code for testing

# Safe dot product function that handles null embeddings created by lag

safe_dot <- function(x, y) {
  if (is.null(x) || is.null(y) || length(x) == 0 || length(y) == 0) {
    return(NA_real_)
  }
  
  sum(x * y)
}

clean_data <- function(embeddings, releases) {
  
  # getting number of embeddings
  num_emb <- length(colnames(embeddings)) - 2
  
  embeddings <- read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings.parquet")
  # turns the embedding columns into one list column
  embedding_columns <- names(embeddings)[
    startsWith(names(embeddings), "embedding_")
  ]
  
  embedding_matrix <- as.matrix(
    embeddings[, embedding_columns]
  )
  
  embeddings_list <- tibble(
    source_row = embeddings$source_row,
    Title = embeddings$Title,
    embedding = unname(asplit(embedding_matrix, MARGIN = 1))
  )
  
  releases2 <- releases %>%
    mutate(source_row = row_number() - 1)
  
  # join the vector embeddings to the press release data
  combo <- releases2 %>% left_join(embeddings_list, 
                              join_by(Title == Title, source_row == source_row)) 
  
  all_states_complete <- combo %>%
    group_by(State, Agency, Date) %>% # if there are multiple releases on one day, take the vector mean
    summarise(
      embedding = list(matrix(unlist(embedding), ncol = num_emb, byrow = TRUE) |>
                         colMeans()),
      .groups = "drop"
    ) %>%
    group_by(State, Agency) %>% # creating rows for days with no observations
    complete(Date = seq.Date(
      from = as.Date("2020-03-01"),
      to   = as.Date("2022-12-31"),
      by   = "day"
    )) %>%
    arrange(Date) %>%
    fill(everything(), .direction = "down") %>%
    drop_na() %>% 
    mutate(agency_min_date = min(Date)) %>% # state-agency min
    ungroup() %>%
    group_by(State) %>%
    mutate(state_max_min = max(agency_min_date)) %>% # state max-min
    ungroup() %>%
    filter(Date >= state_max_min) %>% # filter to state max-min
    pivot_wider(
      id_cols = c(State, Date),
      names_from = Agency, 
      values_from = embedding) %>%
    drop_na() %>% # Florida has no University
    # grouping by state so that lag gets the previous days embeddings, if they exist, within the state
    group_by(State) %>% 
    arrange(Date, .by_group = TRUE) %>%
    # adding 1 day lag for comparison between agencies across days
    mutate(lag1_Governor = lag(Governor),
           lag1_Health = lag(Health),
           lag1_University = lag(University)) %>%
    ungroup() %>%
    # calculating cosine similarity scores
    mutate(GU = map2_dbl(Governor, University, safe_dot),
           GH = map2_dbl(Governor, Health, safe_dot),
           HU = map2_dbl(Health, University, safe_dot)) %>%
    # cosine similarity scores for lagged agencies
    mutate(G_Ulag1 = map2_dbl(Governor, lag1_University, safe_dot),
           G_Hlag1 = map2_dbl(Governor, lag1_Health, safe_dot),
           H_Ulag1 = map2_dbl(Health, lag1_University, safe_dot),
           Glag1_U = map2_dbl(lag1_Governor, University, safe_dot),
           Glag1_H = map2_dbl(lag1_Governor, Health, safe_dot),
           Hlag1_U = map2_dbl(lag1_Health, University, safe_dot))
  
  return(select(all_states_complete, -c(Governor, Health, University, starts_with("lag1")))
)
}