sub_selection_of_seed_alpha <- function(seed_alpha, total_samples = 10, seed = 123){
  #Takes a subselection of the possible alpha values produced by the create_seed_alpha function
  #seed_alpha: the df output by the create_seed_alpha function
  #total_samples: the total number of samples to have, the minimum must be 2
  #seed the random seed to use when sampling
  
  set.seed(seed) #ensure the same random selection is taken everytime, horror and punishment if not set.
  target_orders <- bind_rows(seed_alpha %>%
                               slice(1,n()), 
                             seed_alpha %>%
                               sample_n(total_samples-2)
  ) %>%
    arrange(mean_alpha)
  
  return(target_orders)
  
}