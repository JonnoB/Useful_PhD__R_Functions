#' Process RC test data
#' 
#' 
#' 
#' @param save_file_path A character string. The path where the processed data is held or if not yet processed saved to.
#' @param load_file_path A character string. The path where the raw data files are stored as .rds files
#' @param aggregated_attack_df A data frame. The aggregated attack data that will be joined onto the strain data
#' @export
process_rc_comparisons <- function(save_file_path, load_file_path, aggregated_attack_df ){
  
  if(file.exists(save_file_path)){
    
    test_rc_metrics <-read_rds(save_file_path)
    
  } else {
    
    test_rc_metrics <- list.files(load_file_path, 
                                  full.names = T, 
                                  pattern = ".rds") %>% map_df(~{
                                    print(.x)
                                    temp <- readRDS(.x) #aggreagte_strain_files(file_name)
                                    
                                    parts <- str_split(basename(.x), pattern = "_", simplify = T)
                                    
                                    #convert to range values
                                    strain_norm_df <-temp$edge_embeddings %>% 
                                      summarise(mean_strain = mean(strain),
                                                median_strain = median(strain),
                                                mean_tension = mean(tension),
                                                median_tension = median(tension),
                                                energy = sum(0.5*k*strain^2)) %>%
                                      mutate(
                                        static_force = sum(abs(temp$node_embeddings$static_force)),
                                        fract = parts[2],
                                        carrying_capacity = parts[4],
                                        largest = parts[6],
                                        smallest = parts[8],
                                        robin_hood_mode = parts[11],
                                        r = parts[13],
                                        c = str_remove(parts[15], pattern = ".rds"))
                                  })
    saveRDS(test_rc_metrics, save_file_path)
  }
  
  test_rc_metrics <- aggregated_attack_df %>%
    #mutate(mean_alpha = 1/mean_alpha) %>%
    select(-gc_present, -simulation_id) %>%
    left_join(test_rc_metrics %>%  
                mutate_at(., .vars = vars(fract:smallest), .funs = as.numeric) %>%
                mutate(robin_hood_mode = robin_hood_mode =="TRUE"), 
              by = c("carrying_capacity", "smallest", "largest", "fract", "robin_hood_mode")) %>%
    rename(mean_energy = energy) %>%
    pivot_longer(cols = mean_loading:mean_energy, names_to = "metric") %>%
    separate(., col = metric, into =c("average_type", "metric"),  sep="_" )
  
  return(test_rc_metrics)
  
}