################################################################################
#                                                                              #
#               FUNCTION FOR MERGING DIFFERENT MDB FILES                       #
#                                                                              #
################################################################################

#' Function to extract tables from multiple Microsoft Access databases.
#'
#' @param mdb_folder Path to the folder containing the .mdb files
#' @param target_table The table to be extracted from each mdb file. This should
#'                     unique name accross all the .mdb files.
#'
#' @return Returns a list all all the extracted tables indexed by their names.
#' @export
#'
#' @examples
#' 
#' # How to run the function
#' prep_mdb_table_extract(mdb_folder, target_table)
#' 
#' 

prep_mdb_table_extract <- function(mdb_folder, target_table){
  # Get the start time of the function
  start_time <- Sys.time()
  tryCatch({
    # a) check if the foldername exist
    if(!dir.exists(mdb_folder)){
      cli::cli_alert_warning(paste("Folder path", mdb_folder, "does not exist."))
    }
    
    # b) get the list of all the files with mdb extension from the folder
    mdb_files <-
      dir(path = mdb_folder,
          pattern = "\\.mdb",
          full.names = T)
    
    if(length(mdb_files > 0)){
      # c) loop through each .mdb file and look for the target table
      afp_list <- list()
      
      cli::cli_progress_bar("Extracting data", total = 100)
      for(mdb_file in mdb_files) {
        
        #print(path)
        ch1 <-
          RODBC::odbcDriverConnect(paste0(
            "DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
            mdb_file
          ))
        # Get list of all tables in the mdb
        tables <- RODBC::sqlTables(ch1)
        # Get the only tables with type TABLE
        tables <- tables$TABLE_NAME[tables$TABLE_TYPE == "TABLE"]
        # Check if the target table exist in list of tables in an .mdb
        if(target_table %in% tables){
          query <- RODBC::sqlQuery(ch1, query = paste("select * from ", target_table))
          afp_list[[paste0(basename(mdb_file), "_", target_table)]] <- query
        }else{
          cli::cli_text(paste("No table named : ", target_table, " in ", basename(mdb_file)))
        }
        # close db connection
        RODBC::odbcCloseAll()
        cli::cli_progress_update(total = length(mdb_files))
      }
      
      # print the time taken to complete the process
      elapsed_time <- Sys.time() - start_time
      cli::cli_alert_success(
        paste("Process completed sucessfully in ", 
              elapsed_time, "Sec"
        ))
      # return the list of tables
      return(afp_list)
    } else{
      # print the time taken to complete the process
      cli::cli_alert_warning(
        paste("No mdb file to extract table from"))
    }
    
  }, error = function(e){
    # print out the error message
    cli::cli_alert_warning(e$message)
  }, warning = function(w){
    # print out the warning message
    cli::cli_alert_warning(w$message)
  })
}


#prep_mdb_table_extract(mdb_folder, target_table)


