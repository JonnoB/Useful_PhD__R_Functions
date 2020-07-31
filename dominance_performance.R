#' Dominance performance
#' 
#' Each row represents competition to dominate an edge in the network.
#' The winner can in fact lose control of the edge by having a win_ratio smaller than 0.5.
#' The dataframe produced is twice as long as it needs to be as it has all edges competing from both sides.
#' This can be minimised afterwords
#' 
#' @param g_dl an igraph object. The graph all of the network in question
#' @param g_mat a dataframe the graph is not used as this dataframe may have been changed to model connections
#' @param dominance_df a dataframe. the output of dominance_function
#' 
#' @export

dominance_performance <- function(g_dl, g_mat, dominance_df){
  
  temp <- g_mat %>% select(-from) %>% as.matrix() %>%
    {./(.+t(.))} %>% #score is A/(A+B) thus varies between 0 and 1 has NaN when A+B = 0
    as_tibble(.) %>%
    mutate(from =g_mat$from) %>%
    select(from, everything()) %>%
    rename(winner = from) %>%
    pivot_longer(cols = -winner, names_to = "loser", values_to = "win_ratio") %>%
    filter(winner !=loser, is.finite(win_ratio)) %>%
    left_join(dominance_df$node %>% rename_all(~paste0("winner_",.)) %>%
                mutate(winner_node = as.character(winner_node)), by = c("winner" ="winner_node"))%>%
    left_join(dominance_df$node %>% rename_all(~paste0("loser_",.)) %>%
                mutate(loser_node = as.character(loser_node)), by = c("loser" ="loser_node")) %>%
    mutate(ratio_sum = winner_sum/loser_sum,
           ratio_euc = winner_euc/loser_euc,
           class = case_when(
             win_ratio>=0.5 & ratio_euc>1 ~"domination",
             win_ratio<0.5 & ratio_euc>1 ~"elevation",
             win_ratio>=0.5 & ratio_euc<1 ~"threat",
             TRUE  ~"subordinate"
           ),
           class2 = ifelse(class == "domination"| class =="subordinate", "correct", "error"))
  
  Out <- as_data_frame(g_dl) %>%
    rename(winner = from, loser = to) %>%
    left_join(temp, by = c("winner","loser")) %>% as_tibble
  
  return(Out)
  
}