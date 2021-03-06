#'this is a fiendish internal function that I wish I had described in greater detail
#'
#' This function is only used by Create_stabilised_blocks_expanded. A much more efficient version is used by 
#' 'Create_stabilised_blocks'. If possible this function should be removed and replaced
#'
#' @param g An igraph object of the network
#' @param max_iter The maximum number of iterations used in the SETSe process
#' @param force A character string  of the variable containing the network force
#' 
#' @export

adjust_components <- function(g, max_iter, force){
  
  
  #The origin block is the largest connected component
  
  List_of_BiConComps <- create_balanced_blocks(g, 
                                               force = force)
  
  #find the largest component and use that as the origin block
  OriginBlock_number <-List_of_BiConComps %>% purrr::map_dbl(~igraph::vcount(.x)) %>% which.max()
  
  biconnected_g_info <- igraph::biconnected.components(g)
  
  #the articulations nodes and their graph vertex ids
  articulation_nodes_df <-tibble::tibble(name = names(biconnected_g_info$articulation_points), id = biconnected_g_info$articulation_points)
  
  ##get the first node that is NOT an articulation point
  #This is done my numbering all the nodes from 1 to the total nodes in the component. then finding the names of the node in the origin block
  #that are also biconnected components. These nodes are removed and the the node with the smallest ID is the origin point from which
  #distances and shortes paths will be measured.
  origin_block_base_node_name <-igraph::get.vertex.attribute(g,"name")[-which(igraph::get.vertex.attribute(List_of_BiConComps[[OriginBlock_number]],"name") %in% articulation_nodes_df$name)] %>% 
    min
  
  #The distance from the base node is calculated here in a named vector,
  distance_from_origin <- igraph::distances(g)[articulation_nodes_df$id, origin_block_base_node_name]
  
  #articulation nodes in each component are found here by looping through each 
  #component and seeing which of the articulation nodes are present
  #the warnings are suppress as each iteration causes a "Vectorizing 'igraph.vs' elements may not preserve their attributes" message
suppressWarnings(  node_bicomp_relation <- 1:length(biconnected_g_info$components) %>% 
    purrr::map_df(~{
      
      articulation_nodes_df %>%
        dplyr::filter(id %in% biconnected_g_info$components[[.x]]) %>%
        dplyr::mutate(component = .x)
      
      
    }) %>%
    dplyr::left_join(., tibble::tibble(name = names(distance_from_origin), distance = distance_from_origin), by = "name") %>%#add in distance from origin data
    dplyr::group_by(component) %>%
    dplyr::mutate(is_floor = (min(distance)== distance) & OriginBlock_number != component ) %>% #mark the node that is closest to the origin in each component
    dplyr::ungroup)
  
  #Using the shortest path from the origin node to the articulation point find all the floor and ceiling nodes
  #for a given destination point each component has a pair of articulations points
  #one is the floor the other is the ceiling.
  #the floor node is the node that is closest to the origin the ceiling is the node that is furthest away.
  
  #calculates the shortest path for every floor node in each component
  #As the components are bi-components are bi-connected and the distance is from a single point
  #components can only have a single and unique floor node. However, that floor node
  #can be the floor node of multiple components
  bicomp_shortest_paths <- shortest_paths(g, 
                                          from = origin_block_base_node_name, 
                                          to = unique(node_bicomp_relation$name[node_bicomp_relation$is_floor]),
                                          output = "both")
  
  #Which compenents are on the shortest path of the component being checked component being checked?
  #This is important as a node can be a floor for multiple components. We don't want to re-level for all the components
  #the node is part of. We only want components for which there are two nodes on the shortest path as well as the terminating 
  #node becuase that is in the final component
  
  #This requires another loop. which checks to see if the component is part of the shortest path route for the nodes
  
  floor_df_2 <- ceiling_df_2 <- node_bicomp_relation  %>%
    dplyr::mutate(path_component = NA,
           path_component2 = NA,
           target_node_name = NA) %>% dplyr::slice(0)
  
  for(i in 1:length(bicomp_shortest_paths$vpath) ){
    target_shortest_path <-i
    
    #component_specific_floor_ceiling_nodes <- bicomp_shortest_paths$vpath[[target_shortest_path]]
    node_path <- bicomp_shortest_paths$vpath[[target_shortest_path]]
    edge_path <- bicomp_shortest_paths$epath[[target_shortest_path]] #this may not be necessary and then output can be "vpath" only
    
    component_specific_floor_ceiling_nodes_2 <- names(bicomp_shortest_paths$vpath[[target_shortest_path]])[-1]
    
    template_nodes_df <-node_bicomp_relation %>%
      dplyr::ungroup() %>%
      dplyr::group_by(component) %>%
      dplyr::mutate(path_component = (name %in% component_specific_floor_ceiling_nodes_2)*1,
             path_component2 = sum(path_component)) %>%
      dplyr::ungroup %>%
      dplyr::mutate(target_node_name = names(node_path[length(node_path)]))
    
    floor_df <- template_nodes_df  %>%
      dplyr::filter(path_component==1, #the node has to be on the shortest path
             (path_component2 > 1 | #And the component the node is in needs to have a floor and ceiling
                name == target_node_name) &  # or be the target node
               is_floor)#and all the nodes need to be floors
    
    #This finds the ceilings using the floor dataframe to subset the data
    ceiling_df <- template_nodes_df %>%
      dplyr::filter((component %in% floor_df$component) | #The component has to be one of the components that contains a floor
               component ==OriginBlock_number, #or it has to be the origin block component
             id %in% floor_df$id, #And the node id has to be a node that is also a floor
             !is_floor) #But the node cannot be in a component where it is actually a floor
    
    floor_df_2 <- bind_rows(floor_df_2, floor_df)
    ceiling_df_2 <- bind_rows(ceiling_df_2, ceiling_df)
  }
  
  #this dataframe is the relationship between all the nodes and the components
  node_component_df <- 1:length(List_of_BiConComps) %>%
    purrr::map_df(~{
      
      as_data_frame(List_of_BiConComps[[.x]], what = "vertices") %>%
        dplyr::mutate(component = .x)
      
    }) %>%
    rename(node = name) %>%
    dplyr::mutate(node_component  = paste(node, component, sep ="-"))
  
  #these two peieces of code create the data frames that provide the references for all
  
  #The filtering logic is a bit tricky here might require various networks to test on
  ceiling_df <- node_bicomp_relation %>%
    dplyr::left_join(ceiling_df_2 %>% dplyr::select(ref_node = name, ref_component = component, name = target_node_name) , by = "name") %>%
    dplyr::filter(component !=  OriginBlock_number,
           component != ref_component) %>% #no ceiling can be in the same component as the target component
    dplyr::distinct(component, ref_node, ref_component) %>%
    make_interaction_matrix(., node_component_df) #calls the interaction function to create the final sparse matrix
  
  floor_df <- node_bicomp_relation %>%
    dplyr::left_join(floor_df_2 %>% dplyr::select(ref_node = name, ref_component = component, name = target_node_name) , by = "name") %>%
    dplyr::filter(component !=  OriginBlock_number,
           is_floor) %>% #A floor has to logically be a floor
    dplyr::distinct(component, ref_node, ref_component) %>%
    make_interaction_matrix(., node_component_df) #calls the interaction function to create the final sparse matrix
  
  #This creates the aggregation matrix indicating the node-component to node relationship. It is a bi-partite graph
  #matrix rows are the number of edge-component pairs. The columns are the number of nodes
  #Mulitplying this matrix by a diagonal square matrix of the adjusted height allows much faster aggregation
  aggregation_matrix <- node_component_df %>%
    dplyr::arrange(node) %>% #ensures that the nodes are in alpha numeric order which is the smae as the floor and ceiling dfs
    dplyr::mutate(node_component  = paste(node, component, sep ="-")) %>%
    dplyr::select(node_component, node) %>%
    igraph::graph_from_data_frame(., #The edges in the meta-graph
                                              directed = T) %>%
    igraph::as_adjacency_matrix() %>% .[1:length(unique(node_component_df$node_component)), 
                                -c(1:length(unique(node_component_df$node_component)))] 
  
  
  
  #expand the sparse matrix to block diagnonal form. This means each time iteration is only multiplied by itself. 
  block_diag_floor <- bdiag(lapply(1:(max_iter+1), function(n){floor_df}))
  block_diag_ceiling <- bdiag(lapply(1:(max_iter+1), function(n){ceiling_df}))
  block_diag_aggregation <- bdiag(lapply(1:(max_iter+1), function(n){aggregation_matrix}))
  
  return(list(floor = block_diag_floor, 
              ceiling = block_diag_ceiling, 
              aggregation_matrix = block_diag_aggregation,
              node_order = colnames(as.matrix(aggregation_matrix)) #The node order may not be neccessary butleave it for now
              ))
  
}