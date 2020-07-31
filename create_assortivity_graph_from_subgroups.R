#' create_assortivity_graph_from_subgroups
#'
#' This functions creates a network that is defined by the relationship edge between groups and sub groups of nodes.
#'  It is based on the paper by Peel et al 2019
#' 
#' currently only works for sub-classes of constant size. upgrade shouldn't be very hard though. 
#' To do the replace ment need to double check the rows and columns section
#' @param class_data a dataframe.
#' @param sub_class_edge_data a dataframe.
#' @param sub_class_size an integer
#' @export

create_assortivity_graph_from_subgroups <- function(class_data, sub_class_edge_data , sub_class_size){
  #This functions creates a network that is defined by the relationship edge between groups and sub groups of nodes
  #It is based on the paper by Peel et al 2019
  
  #currently only works for sub-classes of constant size. upgrade shouldn't be very hard though
  #to do the replace ment need to double check the rows and columns section
  
  class_data <- class_data %>%
    mutate(position = 1+lag(cumsum(size), default = 0)) #create the position that each subclass and class will start at
  
  #add position onto the sub class data frame
  sub_class_edge_data <- sub_class_edge_data%>%
    left_join(class_data %>% select(sub_class_1 = sub_class, position.x = position), by =  "sub_class_1") %>%
    left_join(class_data %>% select(sub_class_2 = sub_class, position.y = position), by ="sub_class_2")
  
  
  full_matrix <- matrix(NA, ncol = sum(class_data$size), nrow = sum(class_data$size) )
  submatrix_size <- sub_class_size^2 
  numeric_submatrix  <- matrix(1:submatrix_size, nrow = sub_class_size)  
  
  for(n in 1:nrow(sub_class_edge_data)){
    #in the loop
   # print(n)
    df <- sub_class_edge_data %>% slice(n)
    
    logic_matrix  <- matrix(FALSE, nrow = sub_class_size, ncol = sub_class_size)
    
    #if a subclass is being sampled only the upper triangle can be used to prevent double links and self links
    if(df$sub_class_1 == df$sub_class_2){
      sample_vect <-  numeric_submatrix[upper.tri(numeric_submatrix)]
    } else{
      
      sample_vect <- 1:submatrix_size
      
    }
    
    #Convert the sampled matrix elements to TRUE, this will be used for the edges
    logic_matrix[sample(sample_vect, df$edges , replace = FALSE)] <- TRUE
    
    x.start <- pull(df, position.x)
    y.start <- pull(df, position.y)
    
    full_matrix[x.start:(x.start+ncol(logic_matrix)-1), y.start:(y.start+nrow(logic_matrix)-1)] <- logic_matrix
    
    #END LOOP
  }
  
  full_matrix[lower.tri(full_matrix, diag = TRUE)] <- NA
  
  #classs
  colnames(full_matrix) <- rep(class_data$class, class_data$size)
  #subclass
  rownames(full_matrix) <- rep(class_data$sub_class, class_data$size)
  
  spec_g <-graph_from_adjacency_matrix(full_matrix, mode = "undirected", diag = FALSE, add.colnames ="class", add.rownames = "sub_class")
  
  return(spec_g)
}