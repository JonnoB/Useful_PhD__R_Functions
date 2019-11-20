#' THis function is out of date it may not be useful any more and should be considered for removal

Create_strain_alpha_results_df <- function(g, target_orders, Summary_folder){
  #g the graph that has been analysed across various edge alpha values and different edge loadings
  #target_orders a data frame that contains the simulation parameters, random seed and alpha of the network, created by...
  #Summary_folder the parent folder that contains all the summarised attacks reffered to in the target orders dataframe
  
  current_permutation <- g %>%
    set.edge.attribute(., "distance", value = 1)%>%
    set.edge.attribute(., "Link.Limit", value = Inf)
  
  #load strain
  print("loading strain files")
  Permuted_strain_set_df <- 1:nrow(target_orders) %>%
    map_df(~{
      
      load_file <- target_orders %>% slice(.x)
      #print(.x)
      Out <- read_rds(load_file$file_path) %>%
        Calc_line_strain(current_permutation, ., distance = "distance", capacity = "Link.Limit", flow = "PowerFlow") %>%
        summarise(strain = mean(strain)) %>%
        bind_cols(load_file, .)
      
      return(Out)
    }) %>%
    select(-file_path)
  
  print("merging dataframes and aggregating number of nodes attacker per simulation")
  Permuted_IEEE_118_results <- list.files(path = Summary_folder, 
                                          pattern = ".rds", 
                                          full.names = TRUE, 
                                          recursive = TRUE) %>%
    map_df(~read_rds(.x)%>%
             mutate(file_path = .x))   %>%
    arrange(-TotalNodes) %>%
    mutate(has_gc = mean_degree_sqrd > 2*mean_degree) %>%
    filter(!has_gc) %>%
    group_by(simulationID, file_path) %>%
    summarise_all(first)  %>%
    select(-file_path) %>%
    rename(file_path = alpha) %>%
    mutate(file_path = basename(file_path)) %>%
    left_join(target_orders %>% #add in target orders to get the simulation params
                mutate(file_path = basename(file_path) %>% gsub(".rds", "", .)), by = "file_path" ) %>% 
    ungroup %>%
    group_by(ec, v, fract, permute) %>%
    summarise(NodesAttacked = mean(NodesAttacked)) %>%
    left_join(target_orders , by = c("ec", "v", "fract", "permute")) %>% ##join the target orders to get the alpha values
    #The second join may be able to be joined in the first step, but it isn't super important.
    select(ec:NodesAttacked) %>%
    left_join(Permuted_strain_set_df,by = c("ec", "v", "fract", "permute")) %>%
    select(-calc_on_this_machine, -ID) %>%
    ungroup %>%
    mutate(alpha = 1/alpha)
  
}