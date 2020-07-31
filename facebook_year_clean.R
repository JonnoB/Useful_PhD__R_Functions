#' Clean facebook years
#' 
#' This is to easily clean the facebook years so that they can be projected in to SETSe space
#' 
#'  @param g an igraph object
#'  
#'  @export

facebook_year_clean <- function(g){
  #this function can be easily generalised by switching out year
  #this gives the force per non-small variable
  temp <- facebook_fraction(g, "year") %>%
    filter(percentage > 0.01) %>%
    arrange(year) %>%
    mutate(force =year- weighted.mean(year, w = known))
  
  year_vect <- vertex_attr(g, "year") 
  year_vect[!(year_vect %in% pull(temp, year))] <- NA 
  mean_year <- mean(year_vect, na.rm = TRUE)
  year_vect <- ifelse(is.na(year_vect), mean_year, year_vect)
  g <- set_vertex_attr(g, "year", value = year_vect)
  
  return(g)
  
}