#' Untar files collapse summary created by an array job on Myriad
#' 
#'  This is a helper function as files produced on myriad are generated in a large number of nested tar files.
#'  This function extracts them and places them in the desired folder for ease of use.
#'  
#'  This function should only be used to extract tar files that contain the tar output of the HPC_attack script
#'  The point of this script is to not extract the actual collapse data which can be large. As an example IEEE_300 is 1GB per block.
#'  
#'  @param tar_path A character string. The folder where the tar file is found
#'  @param tar_file A character string. The name of the tar file.
#'  @param extraction_directory A character string. The path name of the directory the data will be extracted to. This folder must exist.
#'  @seealso \code{\link{untar_myriad_strain_files}}
#'  @export
#'  
untar_myriad_collapse_summaries <- function(tar_path, tar_file, extraction_directory){
  
  all_files_in_tar <- untar(tarfile = file.path(tar_path, tar_file), list = TRUE)
  
  just_rds <- file.path(all_files_in_tar[grep(".rds", all_files_in_tar)])
  
  just_collapse_summary_data <- just_rds[grep("xxsummaries", just_rds)]
  
  
  #print(basename(just_collapse_summary_data))
  untar(tarfile = file.path(tar_path, tar_file), 
        files = just_collapse_summary_data,
        exdir = extraction_directory#, 
      #  extras = "--strip-components 4"  #take only the useful bits of the zip file
      )
}