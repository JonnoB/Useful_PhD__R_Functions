Create_target_orders_for_strain_test <- function(g, fract_vect, Scramble_ec_values,
                                                 total_sample_space = 10000,
                                                 required_samples_out = 10,
                                                 seed = 123
){
  
  Combos <- expand.grid(fract_vect, Scramble_ec_values) %>% as_tibble() %>%
    rename(fract = Var1, ec = Var2)
  
  set.seed(seed)
  random_seeds <- sample(1:(total_sample_space*10), total_sample_space)
  
  target_orders <- 1:nrow(Combos) %>% map_df(~{
    
    Iter = Combos %>% slice(.x)
    
    print(paste("Creating the random permutations:", .x, "of", nrow(Combos)))
    
    #create network
    Scrambled_edge_cap <-  Proportional_Load(IEEE_118, alpha = Iter$ec)
    #permute edges
    
    seed_alpha <- Permute_excess_capacity(Scrambled_edge_cap, random_seeds, fract = Combos$fract)
    #take subselection
    target_orders <- sub_selection_of_seed_alpha(seed_alpha, total_samples = required_samples_out, seed = seed) %>%
      mutate(ec = Iter$ec,
             v = 1:n(),
             fract = Iter$fract)
    
    return(target_orders)
    
  })
  
}