KfoldIndex <- function(df, k = 10, seed = 2018){
  #Stratified kfold sampler
  #takes a 1 column dataframe which should be split into k-folds, returns a 1 column data frame where each row contains
  #an integer that represents the fold. Is just a custom version of other off the shelf versions
  AddKFold<- function(df, k, seed){
    set.seed(seed)
    df %>% 
      mutate(fold_ID = rep(1:k, length.out = nrow(df)) %>%
               sample(., size = nrow(df)))}
  
  ColumnName <- names(df)[1]
  
  df <- df %>% select(Y = 1) %>%
    mutate(ID_for_sampling = 1:n())
  
  
  Pos <- df %>% 
    filter(Y == TRUE) %>% AddKFold(., k, seed)
  
  Neg <- df %>% 
    filter(Y == FALSE) %>% AddKFold(.,k, seed)
  
  df <- bind_rows(Pos, Neg) %>%
    arrange(ID_for_sampling) %>%
    select(fold_ID) %>% setNames(ColumnName)
  
  return(df)
}