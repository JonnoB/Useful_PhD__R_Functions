#' Peel conflict subclass performance
#' 
#' 
#' This function analyses node performance for a conflict on a Peel networks
#' @param peels_results A dataframe. the results of peels conflict function
#' @param peels_results_uniform A dataframe. the results of peels conflict function using uniform k
#' 
#' 
#' @export
#' 
peel_conflict_subclass_performance <- function(peels_results, peels_results_uniform){
   
  
  #split the data frame so that each node has it's full results this means A-B is split so
  #A is a row and B is a row
  full_mat_peel_1 <-   peels_results %>%
    bind_rows(peels_results %>%
                rename(sub_class3 = sub_class, 
                       sub_class = sub_class2,
                       node3 = node1,
                       node1 = node2) %>%
                rename(sub_class2 = sub_class3,
                       node2 = node3) %>%
                mutate(across(contains("fract"), ~1-.x),
                       betweenness_ratio = 1/betweenness_ratio)) 
  
  
  wins_1 <- full_mat_peel_1 %>%
    group_by(sub_class, sub_class2, graph_type, sample) %>%
    summarise(wins = sum(clustering_elev_fract>0.5),
              draws = sum(clustering_elev_fract==0.5),
              loss = sum(clustering_elev_fract<0.5))%>%
    ungroup
  
  full_mat_peel_uniform <-   peels_results_uniform %>%
    bind_rows(peels_results_uniform %>%
                rename(sub_class3 = sub_class, 
                       sub_class = sub_class2,
                       node3 = node1,
                       node1 = node2) %>%
                rename(sub_class2 = sub_class3,
                       node2 = node3)%>%
                mutate(across(contains("fract"), ~1-.x),
                       betweenness_ratio = 1/betweenness_ratio)) 
  
  
  wins_uniform <- full_mat_peel_uniform %>%
    group_by(sub_class, sub_class2, graph_type, sample) %>%
    summarise(wins_uniform = sum(clustering_elev_fract>0.5),
              draws_uniform = sum(clustering_elev_fract==0.5),
              loss_uniform = sum(clustering_elev_fract<0.5)) %>%
    ungroup
  
  victory_point <-mean(wins_uniform$sample)
  
  performance_change <-left_join(wins_1, wins_uniform, by = c("sub_class", "sub_class2", "sample")) %>%
    #rename(node = node1) %>%
    group_by(sample) %>%
    mutate(win_diff = wins- wins_uniform,
           draw_diff = draws-draws_uniform,
           loss_diff = loss-loss_uniform,
           status_change = case_when( #did the node go from generally winning, to generally losing?
             wins> victory_point& wins_uniform<victory_point ~TRUE,
             wins<victory_point & wins_uniform>victory_point ~TRUE,
             TRUE~FALSE
             
           ),
           rank = rank(wins),
           rank_uniform = rank(wins_uniform),
           rank_change = rank -rank_uniform) %>%
    ungroup
  
  return(performance_change)
}