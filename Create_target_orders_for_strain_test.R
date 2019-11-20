#' create_target_orders_for_strain_tes

#' This function creates a consistant set of scrambled edges for a given graph.
#' This makes the attack set reproducible.
#' @param g the network that is going to be scrambled
#' @param scramble_ec_values a numeric vector that gives the original alpha value of the network before the edges are scrambled
#' @param total_sample_space A numeric value.The number of samples to generate, each sampled produces a randomly scrambled network. large values can be slow
#' @param required_samples_out A numeric value. The total number of samples to return. A subsample ot total_sampe_space
#' @param seed an integer value. used to set the random seed generator
#' @param subselect Will a subselection of the generated data be taken or all data
#' @export
create_target_orders_for_strain_test <- function(g, fract_vect, Scramble_ec_values,
                                                 total_sample_space = 10000,
                                                 required_samples_out = 10,
                                                 seed = 123,
                                                 subselect = TRUE
){
  #The propertional_load function has hard coded variables
  #Create the dataframe that contains all fraction, ec pairs
  Combos <- expand.grid(fract_vect, Scramble_ec_values) %>% as_tibble() %>%
    rename(fract = Var1, ec = Var2)
  
  set.seed(seed)
  random_permutation_seeds <- sample(1:(total_sample_space*10), total_sample_space)
  random_sample_seed <- sample(1:(nrow(Combos)*10), nrow(Combos)) #preiovusly this was not here and the value used was 'seed'
  
  target_orders <- 1:nrow(Combos) %>% map_df(~{
    
    Iter = Combos %>% slice(.x)
    
    print(paste("Creating the random permutations:", .x, "of", nrow(Combos)))
    
    #create network
    Scrambled_edge_cap <-  Proportional_Load(g, alpha = Iter$ec, 
                                             PowerFlow = "power_flow",
                                             Link.Limit = "edge_limit")
    #permute edges
    
    seed_alpha <- Permute_excess_capacity(Scrambled_edge_cap, random_permutation_seeds, fract = Iter$fract, power_flow = power_flow,
                                          edge_limit = edge_limit)
    
    if(subselect){
      
      #take subselection
      target_orders <- sub_selection_of_seed_alpha(seed_alpha, total_samples = required_samples_out, seed = random_sample_seed[.x]) %>%
        mutate(ec = Iter$ec,
               v = 1:n(),
               fract = Iter$fract)
      
    } else{
      target_orders <- seed_alpha %>%
        mutate(ec = Iter$ec,
               v = 1:n(),
               fract = Iter$fract)
    }

    
    return(target_orders)
    
  })
  
}