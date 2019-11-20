Permute_excess_capacity <- function(g, random_seeds, fract = 1, edge_limit = edge_limit, power_flow = power_flow){
  #This function randonmly permutes the edge excess capacity and returns the associated alpha value of that permutation
  #g a graph
  #random_seeds the a vector of intergers to use as the seed in the permutation
  
  seed_alpha <- random_seeds %>% map_df(~{

    #uses this sub-function to ensure that random ordering of the edges is identical
    temp <- create_scrambled_edges(g, .x, fract = fract, edge_limit = edge_limit, power_flow = power_flow)
    
    tibble(seed = .x, 
           mean_alpha = mean(temp$alpha),
           median_alpha = median(temp$alpha),
           mean_loading =mean(1/temp$alpha),
           median_loading =median(1/temp$alpha)
           )
    }) %>% arrange(mean_alpha)
  
  return(seed_alpha)
  
}
