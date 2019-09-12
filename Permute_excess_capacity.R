Permute_excess_capacity <- function(g, random_seeds, fract = 1){
  #This function randonmly permutes the edge excess capacity and returns the associated alpha value of that permutation
  #g a graph
  #random_seeds the a vector of intergers to use as the seed in the permutation
  
  seed_alpha <- random_seeds %>% map_df(~{

    #uses this sub-function to ensure that random ordering of the edges is identical
    temp <- Create_scrambled_edges(g, .x, fract = fract)
    
    tibble(seed = .x, alpha = mean(temp$alpha))
    }) %>% arrange(alpha)
  
  return(seed_alpha)
  
}
