#' Reorder Attributes
#'
#'This only works on undirected dataframes
#'
#' @param g An igraph object of a graph
#' @param root_attribute A logical graph attribute. This determines whether the vertices will be reordered. The value is unquoted
#' @param change_attributes A character vector of attribute names. These are the attributes that will be re-ordered for the vertices described previously.
#'  All the attributes are re-ordered identically
#' @param seed An integer. the random seed for the sample
#'
#'@export

reorder_vertex_attributes <- function(g, root_attribute, change_attributes, seed){
  #This function simply re orders the load and demand of a netowork accross nodes.
  #An upgrade to this function would allow for choice over Nodetype to be selected
  
  g_df <- as_data_frame(g, what = "vertices") %>%
    filter({{root_attribute}})%>% 
      mutate(re_order_id = 1:n()) 
  
  set.seed(seed)
  new_order <- sample(pull(g_df, re_order_id), size = nrow(g_df))
  
  
  for(n in change_attributes){

    temp <- pull(g_df, !!enquo(n)) %>% {.[new_order]}
    
    g_df <-  g_df %>%
      mutate(!!enquo(n) := temp)
    
  }
  
  new_attribute <- bind_rows(as_data_frame(g, what = "vertices") %>%
              filter(!{{root_attribute}}), g_df %>% select(-re_order_id)) %>%
    arrange(name)
  
  Out <- as_data_frame(g) %>%
    graph_from_data_frame(., directed = FALSE, vertices = new_attribute)
  
  return(Out)
}
