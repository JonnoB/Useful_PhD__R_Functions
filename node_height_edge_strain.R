node_height_edge_strain <- function(g, height_df, coords, distance, capacity, flow = "PowerFlow"){
  #This function creates the dataframes of node ehight and edge strain. 
  #'It is used to prep the data for kriging, outputting a list
  #of two dataframes that contain all information needed for strain and height kriging.
  #g: the graph that is to be kriged: it only needs to be topologically correct for the purposes of kriging
  #height_df: the output datframe from the Create_stabilised_blocks function or the Nodestatus of the Find_network_balance function
  #coords: the coordinate data from the the MakeMapDF function
  #distance: the quoted name of the distance variable used in the graph
  line_strain <-Calc_line_strain(g, height_df, distance, capacity, flow)
  
  #create a dataframe where the height of each node and the strain of the edges is linked to the coordinates of the nodes
  z_network_map <- coords  %>%
    left_join(line_strain, by = c("Link"))%>%
    left_join(., height_df %>% select(node, z), by = c("Node"= "node")) %>% 
    as_tibble
  
 #node height seperated out
  node_z <-z_network_map  %>%
    select(Node, Longitude, Latitude, value = z) %>%
    distinct(.keep_all = T) 
  
  #This is the strain of the network on each edge. It allows us to see the slope of the network
  edge_strain <- z_network_map %>% 
    select(Longitude, Latitude, value = strain, Link) %>%
    group_by(Link) %>%
    summarise_all(mean)
  
  edge_line_load <- z_network_map %>% 
    select(Longitude, Latitude, value = line_load, Link) %>%
    group_by(Link) %>%
    summarise_all(mean)
  
  Out<- list(node_z, edge_strain, edge_line_load)
  names(Out) <- c("node_z", "edge_strain", "edge_line_load")
  
  return(Out)
  
}
