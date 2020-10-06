#' Calculate the graph neighbour voting classifier performance
#' 
#' This function is a helper function used in calculating the performance of the different embeddings methods
#' 
#' @param folder_path A string. The path the folder containing the facebook graphs
#' @param file_name A string. The name of the uni file to be loaded. this is passed into the function as part of the loop it sits in
#' @param data_node_details A dataframe. It contains the node id the class (called taget) and the year of graduation
#' @param active_period A numeric/integer vector. The year or years that are being evaluated
#' 
#' @details 
#' This function predicts node class based on neighbour majority vote. It then calculates the performance the predictions
#' using 4 evaluation metrics.
#' 
#' @export

create_network_vote_performance <- function(folder_path, file_name, data_node_details, active_period){
  fb_metrics <- metric_set(accuracy, kap, bal_accuracy, f_meas)
  
  g_list <- readRDS(list.files(folder_path, full.names = T, pattern = file_name)) %>% as_data_frame()
  
  #This chunk of code finds the the majority neighbour class for each node in the network
  #It outputs a dataframe with both the predicted and true value of the network
  voted_preds <- readRDS(list.files(folder_path, full.names = T, pattern = file_name)) %>% 
    as_data_frame() %>%
    bind_rows(g_list %>% rename(to2 = from, from2 = to) %>% rename(from = from2, to = to2)) %>%
    #insert the cleaned target values to ensure the graph and the embeddings have the same values
    #the values have to be converted to integers otherwise there is some wierd side effects. They are converted back to factors in the next code block
    left_join(data_node_details %>% mutate(target = as.integer(as.character(target))) %>% 
                select(node, from_target = target, year) , by =c("from" ="node")) %>%
    left_join(data_node_details %>% mutate(target = as.integer(as.character(target)))  %>% 
                select(node, to_target = target), by =c("to" ="node")) %>%
    mutate(same_class = (to_target ==from_target)*1) %>%
    #as only data for the active years is included there is a lot of NA values.
    #these can be removed by keeping only complete cases
    filter(complete.cases(.)) %>%
    #get the counts for each year for every node
    group_by(from, from_target, to_target, year) %>%
    summarise(counts = n()) %>%
    #for each node find the year with most votes, also find if there is a tie
    #if a node is max and there is no tie then that year goes into the preds column otherwise it is 0
    group_by(from, from_target, year) %>%
    mutate(is_max = max(counts)== counts,
           is_tie = sum(is_max)>1,
           preds = ifelse(is_max & !is_tie, to_target, 0)) %>%
    group_by(from, year) %>%
    #summarise again get the truth and the prediction for each year
    summarise(truth = first(from_target),
              preds = max(preds)) %>%
    ungroup %>%
    #remove the other years and the other student types
    filter(
      year %in% active_period,
      truth !=0) 
  
  #This is the performance of predicting class using neighbour majority vote
  network_vote_performance <- fb_metrics(voted_preds, truth = factor(truth, levels = 2:1), 
                                         estimate = factor(preds, levels = 2:1)) %>%
    rename(metric = .metric,
           network_knn = .estimate) %>%
    select(-.estimator) %>%
    bind_cols(tibble(network_type1 = sum(voted_preds$truth==1), network_type2 = sum(voted_preds$truth==2)) %>%
                #makes the data frame the same rows for all the metrics
                slice(rep(1, length(attr(fb_metrics, "metrics")))))
  
  return(network_vote_performance)
}
