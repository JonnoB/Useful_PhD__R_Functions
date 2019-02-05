NodeCharacteristiceScrambler <- function(g, Sims, seed = 1983){
  #This function generates a matrix that gives the new node function order.
  #for example it says that node 1 now has the function of node 5.
  #It simply generates an integer dataframe giving the new node function for each simulation
  #g: an Igraph object
  #Sims: the number of simulations
  
  TotNodes <- vcount(g)
  NodeNames <- tibble(name = get.vertex.attribute(g, name = "name"))
  
  set.seed(seed)
  Out <- 1:Sims %>%
    map(~{
      
      df <- sample(1:TotNodes, TotNodes, replace = FALSE) %>%
        as.matrix(., ncol = 1) %>%
        as_tibble() %>%
        setNames(paste0("Sim_", .x))
      return(df)
    }) %>%
    bind_cols(NodeNames, .)
  
}

