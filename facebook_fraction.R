#' calculate known fractions of facebook 100
#' 
#' Of all students what percentage of the variable is known? It ignores values coded as 0 as these are unknown
#' 
#'  @param g The graph of the univeristy
#'  @param var a character string nameing the vertex attribute to be checked.
#' @export
#' 

facebook_fraction <-  function(g, var){
  
  fraction <- tibble(target  = vertex_attr(g, var)) %>%
    mutate(target = as.integer(target)) %>%
    group_by(target) %>%
    summarise(counts = n()) %>%
    mutate(known = ifelse(target == 0, 0, counts),
           percentage = known/sum(known)) %>%
    arrange(known) %>%
    mutate(perc_cumsum = cumsum(percentage), 
           classes = fct_lump_prop(as.character(target), prop = 0.01, w = perc_cumsum, other_level = "Other"),
           classes2 = fct_lump_prop(as.character(target), prop = 0.005, w = perc_cumsum, other_level = "Other")) %>%
    rename({{var}} := target)
  
  
  return(fraction)
  
}

