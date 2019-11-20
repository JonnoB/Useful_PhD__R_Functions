#' Save parameter files for HPC simulation
#'
#' This function saves the attack, strain and HPC start up files necessary for running an HPC simulation
#'
#'This function is used in the Spring_Embeddings_Data_Generation.Rmd file to save the generated parameter files
#'
#' @param parameter_df A data frame. The parameter dataframe that will be used by the HPC script. 
#' This can be the parameters of several simulations
#' @param HPC_startup_parameter_file_path A character string. The file pather of the HPC start up parameters
#' @param analysis_parameter_file_path A character string. The file path for the analysis parameters
#' @seealso \code{\link{generate_simulation_parameter_df}}
#' @export
#' 
save_params_for_HPC <- function(parameter_df, 
                                HPC_startup_parameter_file_path, 
                                analysis_parameter_file_path){
  
  unique(parameter_df$graph_path) %>% walk(~{
    
    #the file name the two parameter files will be saved under
    file_name <- paste0("base_", basename(.x))
    
    #The temporary file used by the R HPC script
    temp <- parameter_df %>%
      filter(parameter_df$graph_path == .x)
    
    #The temp file used by the attack HPC itself when starting up R
    attack_startup_temp <- temp %>%
      distinct(compute_group) %>%  #The compute group to be solved in that iteration of the script, this will be set depending on what 
      #I decide to use
      mutate(load_file = file_name) #the parameter_df file that will be loaded by the HPC R script
    
    
    #The temp file used by the strain HPC itself when starting up R.
    #The difference with previous is that the strain only needs to use a single simulation where as attack obviously needs all of them
    strain_startup_temp <- temp %>%
      ungroup %>% #just incase
      filter(simulation_id==1) %>%
      mutate(load_file = file_name) %>% #the parameter_df file that will be loaded by the HPC R script
      select(compute_group_strain, v, fract, ec, load_file)
    
    message(paste("Saving HPC attack and strain startup files to", HPC_startup_parameter_file_path ))
    #save the parameter files for the attack simulations
    write_delim(attack_startup_temp, 
                path = file.path(HPC_startup_parameter_file_path, 
                                 paste0("attack_", 
                                        gsub(".rds", ".txt",file_name))), 
                delim = " ") #save the HPC start up parameter file. This defines the entire array
    
    #save the parameter files for the strain  simulations
    write_delim(strain_startup_temp, 
                path = file.path(HPC_startup_parameter_file_path, 
                                 paste0("strain_", 
                                        gsub(".rds", ".txt",file_name))), 
                delim = " ") #save the HPC start up parameter file. This defines the entire array
    
    message(paste("Saving simulation parameter files to", analysis_parameter_file_path))
    #save the parameter files used by both HPC scripts
    saveRDS(temp, 
            file.path(analysis_parameter_file_path, 
                      file_name))  #save the parameter df file
    
  }
  )
}