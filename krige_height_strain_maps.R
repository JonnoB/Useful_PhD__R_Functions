krige_height_strain_maps <-function(g, height_df, coords, distance, capacity, flow, spatial_df, model = "Sph"){
  #This function performs all the steps necessary to krige the height and strain data for plotting next to each other in ggplot
  #This function is to simplify the code and make the process easily repeatable
  #g: the graph that is to be kriged: it only needs to be topologically correct for the purposes of kriging
  #height_df: the output datframe from the Create_stabilised_blocks function or the Nodestatus of the Find_network_balance function
  #coords: the coordinate data from the the MakeMapDF function
  #distance: the quoted name of the distance variable used in the graph
  #spatial_df a spatial polygons dataframe giving the extent of the map over which kriging will interpolate
  
  Strain_Height_List <-node_height_edge_strain(g, height_df, coords, distance, capacity, flow)
  
  kriged_df <- Create_kriged_df(Strain_Height_List$node_z, "value", spatial_df, 8e4, model = model) %>% mutate(type = "Height")
  kriged_df_strain <- Create_kriged_df(Strain_Height_List$edge_strain, "value", spatial_df, 8e4, model = model) %>% mutate(type = "Strain")
  kriged_df_alpha <- Create_kriged_df(Strain_Height_List$edge_line_load, "value", spatial_df, 8e4, model = model) %>% mutate(type = "Line Load")
  
 Out <- bind_rows(kriged_df, kriged_df_strain, kriged_df_alpha) %>%
    group_by(type) %>%
    mutate(value_perc = percent_rank(value)) %>%
   ungroup
 
 return(Out)
  
}