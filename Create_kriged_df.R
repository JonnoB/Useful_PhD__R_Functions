Create_kriged_df <- function(data_df, variable = "z", spatial_poly, grid_points = 80000, model = "Sph"){
  #This function kriges a dataframe within a specific geography
  
  #data_df a dataframe with columns Latitude, Longitude, and a column with variables that will be kriged
  #variable a character string of the name of the variable to be kriged
  #spatial_poly an object of class SpatialPolygonsDataFrame that defines the geography to be kriged
  #grid_points the number of grid points to impute withing the geography
  #model a character string describing the type of model used in the function autofitVariogram
  
  
  print("Setting up data")
  data_df <- data_df %>% rename(input_var = variable)
  
  grd <- makegrid(spatial_poly, n = grid_points)
  colnames(grd) <- c('x','y')
  
  grd_pts <- SpatialPoints(coords = grd, 
                           proj4string=CRS(proj4string(spatial_poly)))
  
  print("subsetting geography")
  # find all points in `grd_pts` that fall within `spatial_poly`
  grd_pts_in <- grd_pts[spatial_poly, ]
  
  # transform grd_pts_in back into a data frame
  gdf <- as.data.frame(coordinates(grd_pts_in))
  
  
  #create spatial points of height data
  DT_sf  <-  st_as_sf(data_df, coords = c("Longitude", "Latitude"),
                      crs = "+proj=longlat +datum=WGS84", agr = "constant")%>% 
    as_Spatial %>%
    remove.duplicates() #remove duplicate points aka points super close
  
  #create spatial points of data that will have input_var interpolated
  grd_sf  <-  gdf %>% #expand.grid(x =unique(gdf$x), y = unique(gdf$y)) %>% as.tibble %>% #if expand grid isn't used problems
    rename(Longitude = x, Latitude = y) %>%
    st_as_sf(., coords = c("Longitude","Latitude"),
             crs = "+proj=longlat +datum=WGS84", agr = "constant") %>%
    as_Spatial

  print("Fitting variogram")
  dt.fit <- autofitVariogram(input_var ~ 1, 
                             DT_sf,
                             model = model, #commenting out this autfits the variogram. that produces the lowest error but doesn't look that great. I would think the sperical one was more accurate I don't understand much about it though
                             kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                             fix.values = c(NA, NA, NA),
                             start_vals = c(NA,NA,NA),
                             verbose = T)
  
  # vgm() list of models
  
  print("Kriging data")
  lzn.kriged <- krige((input_var) ~ 1, remove.duplicates(DT_sf), grd_sf, model=dt.fit$var_mode) 
  
  kriged_df <- lzn.kriged %>% as.tibble %>%
    rename(!!variable := var1.pred) %>%
    select(-var1.var) %>%
    rename(Longitude = coords.x1, Latitude = coords.x2) 
  
  
  return(kriged_df)
  
}