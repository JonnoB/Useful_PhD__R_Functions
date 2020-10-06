#' get_embeddings_knn_performance 
#' 
#' @param embeddings_df a dataframe. 
#' @param active_period A numeric/integer vector. The year or years that are being evaluated
#' @param network_vote_performance a dataframe. The baseline of neighbour voting classifiation
#' 
#' 
#' @export
#' 

get_embeddings_knn_performance <- function(embeddings_df, active_period, network_vote_performance){
  fb_metrics <- metric_set(accuracy, kap, bal_accuracy, f_meas)
  
  target_vect <- embeddings_df$target
  
  knn_perf <- seq(from = 1, to = 21, by = 2) %>% 
    map_df(~{
      mod <- class::knn.cv(train = embeddings_df %>% select(X1, X2), 
                           cl = fct_drop(target_vect), 
                           k = .x)
      
      tibble(truth = fct_drop(target_vect) %>% factor(., levels = 2:1),
             estimate = mod %>% factor(., levels = 2:1)) %>%
        fb_metrics(truth = truth, estimate = estimate) %>%
        mutate(k = .x)
      
    }) %>%
    rename(metric = .metric,
           estimate = .estimate) %>%
    select(-.estimator) %>%
    mutate(file_name = file_name,
           active_period = paste(active_period, collapse = ", "),
           student_1 = sum(target_vect==1),
           student_2 = sum(target_vect==2), 
           model = unique(embeddings_df$model)[1]) %>%
    left_join(network_vote_performance, by = "metric" ) 
  
}
