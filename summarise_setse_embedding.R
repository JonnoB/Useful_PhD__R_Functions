#' SUmmarise embedding daata
#' 
#' Provides a single line output for a setse object
#' 
#' The function reduces a setse object to a single line that is used in the analysis of the robustness paper.
#' 
#' @param temp the output of any of the setse embeddings functions aka a setse object
#' @export


summarise_setse_embedding <- function(temp){

temp_edge <- temp$edge_embeddings

temp_node <- temp$node_embeddings %>%
  summarise(
    mean_elev = mean(abs(elevation)),
    median_elev = median(abs(elevation))
  )

 out <- temp_edge %>%
  summarise(
    mean_ratiostrain = mean(strain, na.rm = T)/median(strain, na.rm = T),
    mean_ratiotension =  mean(tension, na.rm = T)/median(tension, na.rm = T),
    mean_loading =mean(line_load, na.rm = T),
    median_loading = median(line_load, na.rm = T),
    mean_alpha = mean(1/line_load, na.rm = T),
    median_alpha = median(1/line_load, na.rm = T),
    #mean_logstrain = mean(log10(strain), na.rm = T),
    # mean_logtension = mean(log10(tension), na.rm = T),
    mean_strain = mean(strain, na.rm = T),
    median_strain = median(strain, na.rm = T),
    mean_tension = mean(tension, na.rm = T),
    median_tension = median(tension, na.rm = T)
  ) %>%
  bind_cols(temp_node) %>%
  mutate(
    static_force = sum(abs(temp$node_embeddings$static_force)))

 return(out)
}