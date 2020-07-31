#' dominance cross validation
#' 
#' This function trains the network on all but some fraction of the network. 
#' 
#'  @param file_path A character string. The path to the DL format graph file of some directed dominance network
#'  @param folds An integer. The number of folds in each model. set to the number of folds for a test of each node 
#'  @param repeats An integer. The number of repeat random samples
#'  
#'  @export



dominance_xvalidation <- function(file_path, folds = 10, seed = 10){
  
  g_dl <- load_dl_graph(file_path, directed = FALSE, return_graph = TRUE) %>%
    simplify
  
  g_mat <- load_dl_graph(file_path, directed = TRUE, return_graph = FALSE)
  
    
  #create the positions of all the non-zero values in the matrix
  {
    non_zero_g_mat <- 1:nrow(g_mat) %>% map_df(~{
      
      g_mat %>% select(-from) %>% as.matrix(.) %>% {. +t(.)} %>% as.tibble() %>%
        pull(.x) %>% {. !=0} %>% which() %>%
        tibble(row_a = . +0 , column_a = .x+1) %>% select(-1)
      
      
    }) %>%
      mutate(row_b = column_a-1,
             column_b = row_a +1,
             winner = g_mat$from[row_a],
             loser = g_mat$from[row_b])
    
    conflict_results <-g_mat %>% select(-from) %>% as.matrix() %>%
      {./(.+t(.))} %>% #score is A/(A+B) thus varies between 0 and 1 has NaN when A+B = 0
      as_tibble(.) %>%
      mutate(from =g_mat$from) %>%
      select(from, everything()) %>%
      rename(winner = from) %>%
      pivot_longer(cols = -winner, names_to = "loser", values_to = "outcome") %>%
      mutate(outcome = outcome >=0.5)
    
    #only have each edge once  
    non_zero_g_mat <- as_data_frame(g_dl) %>%
      rename(winner = from, loser = to) %>%
      left_join(non_zero_g_mat, by = c("winner","loser")) %>%
      left_join(., conflict_results , by = c("winner","loser")) %>%
      as_tibble
    
    # non_zero_g_mat <- non_zero_g_mat %>% 
    #   left_join(., conflict_results , by = c("winner","loser")) %>%
    #   as_tibble
  }
  #removing a random 10% of the network. how well is the that 10% predicted?
  
  #create a k-fold vector that breaks the model into 10 percent blocks
  start <-Sys.time()
  set.seed(seed)
  
  fold_vector <- rep(1:folds, length.out =nrow(non_zero_g_mat)) %>%
    sample(., size = length(.), replace = FALSE)#sample(1:folds, size = nrow(non_zero_g_mat), replace = TRUE)
  
  Out  <- 1:folds %>% map(~{
    
    print(paste("fold", .x, "of", folds))
    
    #These are the nodes that will be removed
    non_zero_g_mat <- non_zero_g_mat %>%
      filter(fold_vector==.x)
    
    g_mat2 <- g_mat
    #delete all the conflicts between nodes that were randomly selected
    for( i in nrow(non_zero_g_mat)){
      
      g_mat2[non_zero_g_mat$row_a[i], non_zero_g_mat$column_a[i]] <-0  
      g_mat2[non_zero_g_mat$row_b[i], non_zero_g_mat$column_b[i]] <-0  
      
    }
    
    #total wins between pairs
    hold_out_df <- g_mat2 %>% select(-from) %>% as.matrix() %>% {. +t(.)} %>% as_tibble() %>%
      mutate(from =g_mat2$from) %>%
      select(winner = from, everything()) %>%
      pivot_longer(cols = -winner, names_to = "loser", values_to = "total")
    
    
    #calculate total win/loss ratio for each node
    node_win_loss_df <- tibble(node = g_mat2 %>% pull(from), node_wins = g_mat2 %>% select(-from) %>% rowSums()) %>%
      left_join(tibble(node = g_mat2 %>% select(-from) %>% names, node_losses = g_mat2 %>% select(-from) %>% colSums()),
                by = "node") %>%
      mutate(node_ratio = node_wins/(node_wins + node_losses)) #higher the ratio the more wins
    
    #do the domination projection
    dominance_df <- domination_function(g_dl, g_mat2, verbose = FALSE)
    
    #create the performance dataframe
    #add in the holdout data markers
    performance_df <- dominance_performance(g_dl, g_mat, dominance_df)%>% 
      left_join(non_zero_g_mat %>% select(winner, loser) %>% mutate(holdout = TRUE), by = c("winner", "loser")) %>%
      mutate(holdout = ifelse(is.na(holdout), FALSE, TRUE)) %>%
      left_join( hold_out_df, by = c("winner", "loser")) %>% #add in total conlicts per edge/node pair
      left_join(node_win_loss_df %>% select(winner = node, winner_node_ratio = node_ratio), by = "winner" ) %>%
      left_join(node_win_loss_df %>% select(loser = node, loser_node_ratio = node_ratio), by = "loser" ) %>%
      mutate(relative_win_ratio = winner_node_ratio/loser_node_ratio,
             naive_res = case_when(
               relative_win_ratio>=0.5 & win_ratio>=0.5 ~"correct",
               relative_win_ratio<=0.5 & win_ratio<=0.5 ~"correc",
               TRUE ~"error"
             ))
    
    acc_metrics <- metric_set(accuracy, bal_accuracy, kap, f_meas, sens, spec, mcc )
    #####
    ##
    ##The Cutoff doesn't make sense as the cutoff has to be 1 for symmetry so the belo code is commented out
    ##
    #####  
    #check performance by cutoff for the held out values
    # test_res <- seq(0,2, by = 0.05) %>%
    #   map_df(~{
    #     
    #     #Dominance is declared if the win_ratio is 0.5 or higher
    #     temp_res <- performance_df %>% filter(holdout) %>%
    #       mutate( 
    #         truth = factor(win_ratio>=0.5, levels = c(TRUE, FALSE)),
    #         estimate= factor(ratio_euc >= .x, levels = c(TRUE, FALSE))) 
    #     
    #     temp_res <- temp_res %>%
    #       acc_metrics(., truth = truth, estimate = estimate) %>%
    #       mutate(cutoff = .x)
    #     
    #     return(temp_res)
    #   }) %>%
    #   mutate(fold = .x,
    #          seed = seed)
    
    #Dominance is declared if the win_ratio is 0.5 or higher
    test_res <- performance_df %>% filter(holdout) %>%
      mutate( 
        truth = factor(win_ratio>=0.5, levels = c(TRUE, FALSE)),
        estimate= factor(ratio_euc >= 1, levels = c(TRUE, FALSE)),
        estimate2 = factor(relative_win_ratio >= 1, levels = c(TRUE, FALSE)))  #node a has a better win ratio than node b
    
    test_res <- test_res %>%
      acc_metrics(., truth = truth, estimate = estimate) %>%
      mutate(correct = sum(test_res$truth == test_res$estimate),
             total_obs = nrow(test_res),
             type = "SETSe") %>%
      bind_rows(., test_res %>%
                  acc_metrics(., truth = truth, estimate = estimate2) %>%
                  mutate(correct = sum(test_res$truth == test_res$estimate2),
                         total_obs = nrow(test_res),
                         type = "naive")
      ) %>%
      mutate(fold = .x,
             seed = seed)
    
    
    Out <- list( performance_df = performance_df %>%
                   mutate(fold = .x,
                          seed = seed),
                 model_analysis = test_res)
    
    return(Out)
    
  }) %>% transpose()
  
  Out <- Out %>%
    map(~{
      
      .x %>% bind_rows()
      
    })
  
  stop <-Sys.time()
  print(stop-start)
  
  return(Out)
}
