#' Redistibute excess capacity
#'
#' This function redsitribute excess capacity by taking x fraction of the edges
#' with the most excess capacity and distributing that over y fraction of the edges with
#' the least excess capcity or visa versa.
#'
#' Depending on how the 'robin_hood_mode' variable is set. The function either takes from the 
#' rich to give to the poor, or takes from the poor to give to the rich.
#'
#' @param g An igraph object of a flow network which has edge capacity.
#' @param largest A numeric fraction. The largest x of edges varies between 0 and 0.5.
#' @param smallest A numeric fraction. The smallest x of edges varies between 0 and 0.5.
#' @param fraction A numeeric fraction. The fraction of total excess capacity that will be moved from the target edges to the recipient edges.
#' @param flow An unqouted character string. The name of the edge attribute that contains the flow data of the network.
#' @param capacity An unquoted character string. The name of the edge attribute that contains the flow data of the network.
#' @param robin_hood_mode A logical value. Take from the rich give to the poor... or the otherway round.
#' @param output_graph A logical value. Outputs a graph with updated edge capacity. Otherwise outputs a data frame

redistribute_excess <- function(g, 
                                largest, 
                                smallest, 
                                fraction, 
                                flow = power_flow, 
                                edge_capacity = edge_capacity,
                                robin_hood_mode = TRUE,
                                output_graph = TRUE){
  #The function has a second distribution method that is not used. 
  #The distribution method used applies a flat rate excess capacity across all nodes
  #The other method provides excess capacity proportionally relative to the size of the current excess capacity
  
  
  #An upgrade to this function is to distribute the excess relative to the centrality or electrical centrality of the edge
  
  if(largest >0.5 | smallest > 0.5) stop(" 'largest' and 'smallest' must be smaller than or equal to 0.5")
  if((fraction<0)|(fraction>1)) stop("Fraction must be between 0 and 1")
  
  
  edge_df <- as_data_frame(g) %>%
    mutate(excess_capacity = {{edge_capacity}} - abs({{flow}}),
           rank_perc = percent_rank(excess_capacity)) #ranks the smallest value as 0 largest value as 1
  
  if(robin_hood_mode){
    
    take_from <- edge_df %>%
      filter(rank_perc >= ( 1 - largest ))
    
    give_to <- edge_df %>%
      filter(rank_perc <= smallest) %>%
      mutate(give_fract = excess_capacity/sum(excess_capacity),
             excess_capacity_2 = sum(take_from$excess_capacity)*fraction/n() + excess_capacity,
             excess_capacity_3 = sum(take_from$excess_capacity)*fraction*give_fract + excess_capacity)
    
  }else { #Robin hood mode is false aka full fuedalism
    
    take_from <- edge_df %>%
      filter(rank_perc <= smallest )
    
    give_to <- edge_df %>%
      filter(rank_perc >= ( 1 - largest )) %>%
      mutate(give_fract = excess_capacity/sum(excess_capacity),
             excess_capacity_2 = sum(take_from$excess_capacity)*fraction/n() + excess_capacity,
             excess_capacity_3 = sum(take_from$excess_capacity)*fraction*give_fract + excess_capacity)
    
    
  }
  
  
  new_vals <- edge_df  %>%
    filter(!(edge_name %in% give_to$edge_name)) %>%
    bind_rows(give_to) %>%
    mutate(
      excess_capacity_2 = case_when(
        !is.na(excess_capacity_2) ~ excess_capacity_2,
        (edge_name %in% take_from$edge_name) ~ excess_capacity*(1-fraction),
        TRUE ~excess_capacity
      ),
      excess_capacity_3 = case_when(
        !is.na(excess_capacity_3) ~ excess_capacity_3,
        (edge_name %in% take_from$edge_name) ~ excess_capacity*(1-fraction),
        TRUE ~excess_capacity
      ),
      alpha_1 =(abs({{flow}})+ excess_capacity)/abs({{flow}}),
      alpha_2 =(abs({{flow}})+ excess_capacity_2)/abs({{flow}}),
      alpha_3 =(abs({{flow}})+ excess_capacity_3)/abs({{flow}}),
      load_1 =1/alpha_1,
      load_2 =1/alpha_2,
      load_3 =1/alpha_3
    )
  

  if(output_graph){
    
    Out <- edge_df %>%
      left_join(., 
                new_vals %>%
                  select(edge_name, excess_capacity_2),
                by = "edge_name") %>% #merge the new capacity onto the edge dataframe
      mutate({{edge_capacity}} := abs({{flow}}) + excess_capacity_2) %>% #re-calculate the edge capacities
      select(-excess_capacity_2, -rank_perc) %>% #remove the columns that have been added on
      graph_from_data_frame(.,
                            directed = FALSE,
                            vertices = as_data_frame(g, 
                                                     what = "vertices")
                            ) #convert back into a graph the torturous but safe way
  } else {
    #If a graph is not going to be outputted, return a dataframe that provides a summary of the load and alpha for these settings
    Out <- new_vals %>%
      select(alpha_1:load_3) %>%
      summarise_all(mean) %>%
      mutate(take_from = sum(take_from$excess_capacity),
             give_to = sum(give_to$excess_capacity),
             excess_2 = sum(new_vals$excess_capacity_2),
             fract = fraction,
             largest = largest, 
             smallest = smallest,
             robin_hood_mode  = robin_hood_mode)
    
  }
  
  
  
  return(Out)
  
}