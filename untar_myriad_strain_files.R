#' Untar files strain created by an array job on Myriad
#' 
#'  This is a helper function as files produced on myriad generated in a large number of nested tar files.
#'  This function extracts them and places them in the desired folder for ease of use.
#'  
#'  This function should only be used to extract tar files that contain the tar output of the HPC_strain script
#'  
#'  @param tar_path A character string. The folder where the tar file is found
#'  @param tar_file A character string. The name of the tar file.
#'  @param extraction_directory A character string. The path name of the directory the data will be extracted to. This folder must exist.
#'  @seealso \code{\link{untar_myriad_collapse_summaries}}
#'  @export

untar_myriad_strain_files <- function(tar_path, tar_file, extraction_directory){
  
  all_files_in_tar <- untar(tarfile = file.path(tar_path, tar_file), list = TRUE)
  
  data_file_path <- file.path(all_files_in_tar[grep(".rds", all_files_in_tar)])
  
  print(basename(data_file_path))
  untar(tarfile = file.path(tar_path, tar_file), 
        files = data_file_path,
        exdir = extraction_directory, 
        extras = "--strip-components 4") #take only the useful bits of the 
  
}