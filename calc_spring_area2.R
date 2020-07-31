#' Calculate the youngs modulus of the spring
#' 
#' This function adds the graph characteristic A which is the cross sectional area of the springy. 
#' The  cross section is dependent on the edge and system capacity as well as the total flow
#'
#' @param g an igraph object. The graph representing the network
#' @param value a character string. The name of the flow edge atribute
#' @param alpha a character string. The name of the edge value giving edge_capacity/flow
#' @param minimum_value a numeric value. Indicating the most stretchy value of youngs modulus
#' @param range a numeric value. This gives the range of A values above the minimum, the maximum value value.
#' @param edge_capacity_thresh a numeric. The threshold required before area increases
#' @param system_capacity_thresh a numeric. the system threshold before area increases
#' @export
#' 
calc_spring_area2 <- function(g, value = "power_flow", edge_capacity = "edge_capacity", minimum_value, range, edge_capacity_thresh =2, system_capacity_thresh = 2){

  temp <- as_data_frame(g) %>% as_tibble %>%
    rename(value_2 = !!value,
           edge_capacity2 = !!edge_capacity) %>%
    mutate(
      value_2 = abs(value_2), #power flow is now guarenteed positive
      alpha2 = edge_capacity2/value_2,
      A = case_when(
        is.finite(value_2) ~  range*(value_2 - min(value_2))/(max(value_2)-min(value_2)) + minimum_value,
        TRUE ~range + minimum_value
      ),
      
      A = ifelse(is.finite(A), A, minimum_value), #prevents NaNs from 0 values or other such annoying stuff
      alpha_contraint  =  alpha2<=edge_capacity_thresh      
    ) 

  if((sum(temp$alpha2*temp$value_2)/sum(temp$value_2))>= system_capacity_thresh){
    
    temp <- temp %>%
      mutate(
       A = ifelse(alpha_contraint, A, minimum_value) #This edit is specifically designed to target largest edges in the
       #systems that have error spikes only
        )
  }
  
  g2 <- set.edge.attribute(g, "Area", value = temp$A)
  return(g2)
}
