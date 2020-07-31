#'Create a kirged SETSe map
#'
#'This function produces a map using the 3 SETS embeddings and line load.
#'
#' @param SETSe_List A list . The output of one of the SETSe producing functions.
#' @param coords A dataframe. the coordinate data from the the MakeMapDF function.
#' @param spatial_df a spatial polygons dataframe giving the extent of the map over which kriging will interpolate.
#' @param model A character string. Parameter is passed to the autofitVariogram function.

krige_height_strain_maps <-function(SETSe_list, coords, spatial_df, model = "Sph"){
  #This function performs all the steps necessary to krige the height and strain data for plotting next to each other in ggplot
  #This function is to simplify the code and make the process easily repeatable
  #SETSe_List the output of one of the SETSe producing functions
  #coords: the coordinate data from the the MakeMapDF function
  #spatial_df a spatial polygons dataframe giving the extent of the map over which kriging will interpolate
  
  #Nodes
  node_coords <- coords %>%
    select(Latitude, Longitude, node =  Node) %>%
    distinct() %>%
    left_join(., SETSe_list$node_embeddings, by = "node" )
  
  #Edges
  edge_coords <- coords %>%
    select(Latitude, Longitude, edge_name) %>%
    distinct() %>%
    group_by(edge_name) %>%
    summarise_all(mean) %>%
    left_join(., SETSe_list$edge_embeddings, by = "edge_name" ) %>%
    mutate(alpha = ifelse(is.na(alpha), 1, alpha),
           line_load = ifelse(is.na(line_load), 1, line_load))
  
  print("Calculating Elevation")
  kriged_elevation <- Create_kriged_df(node_coords, "elevation", spatial_df, 8e4, model = model) %>% mutate(type = "Elevation")
#  print("Calculating Strain") # strain and tension are basically the same
#  kriged_strain    <- Create_kriged_df(edge_coords, "strain", spatial_df, 8e4, model = model) %>% mutate(type = "Strain")
  print("Calculating Tension")
  kriged_tension   <- Create_kriged_df(edge_coords, "tension", spatial_df, 8e4, model = model) %>% mutate(type = "Tension")
  print("Calculating Load")
  kriged_load      <- Create_kriged_df(edge_coords, "line_load", spatial_df, 8e4, model = model) %>% mutate(type = "Load")
  
 Out <- bind_rows(kriged_elevation %>% rename(value = elevation), 
              #    kriged_strain %>% rename(value = strain), 
                  kriged_tension %>% rename(value = tension), 
                  kriged_load %>% rename(value = line_load)
                  ) %>%
    group_by(type) %>%
    mutate(value_perc = percent_rank(value)) %>%
   ungroup
 
 return(Out)
  
}