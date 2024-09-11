#' Export and Read Microsoft Access Database Table
#'
#' This function exports a specified table from a Microsoft Access database
#' (.mdb file) to a CSV file, then reads the CSV into an R dataframe.
#'
#' @param mdb_path Character string. Full path to the .mdb file.
#' @param table_name Character string. Name of the table to export from the
#'    .mdb file.
#' @param output_csv Character string. Path for the temporary CSV file.
#'    Default is "/tmp/temp_export.csv".
#'
#' @return A dataframe containing the data from the specified Access table.
#'
#' @details This function requires 'mdb-tools' to be installed on the system.
#' If not installed, it will attempt to install it automatically on macOS
#' (using Homebrew) and Linux (using apt-get). For Windows, manual installation
#' is required. The function uses the system command 'mdb-export' to export
#' the table to CSV, mdb-schema' to get column information, and '
#' poliprep::read' for importing.
#'
#' @examples
#' \dontrun{
#' df <- export_and_read_mdb(
#'     "/path/to/database.mdb",
#'     "TableName"
#' )
#' }
#'
#' @export
export_and_read_mdb <- function(mdb_path,
                                table_name,
                                output_csv = "/tmp/temp_export.csv") {
    # Check if mdb_path exists
    if (!file.exists(mdb_path)) {
        stop("The specified .mdb file does not exist.")
    }

    # Check if mdb-tools is installed, if not, attempt to install
    if (system("which mdb-export",
        ignore.stdout = TRUE, ignore.stderr = TRUE
    ) != 0) {
        os <- Sys.info()["sysname"]
        if (os == "Darwin") {
            system("brew reinstall mdbtools")
        } else if (os == "Linux") {
            system("sudo apt-get install -y mdbtools")
        } else if (os == "Windows") {
            stop("mdb-tools is not available for Windows. Please install manually.")
        } else {
            stop("Unsupported operating system for automatic mdb-tools installation.")
        }
    }

    # Construct and execute the mdb-export command
    cmd <- paste(
        "mdb-export",
        shQuote(mdb_path),
        shQuote(table_name), ">",
        shQuote(output_csv)
    )
    system_result <- system(cmd)

    # Check if the system command was successful
    if (system_result != 0) {
        stop("Failed to export the table from the .mdb file.")
    }

    # Read the CSV file
    tryCatch(
        {
            df <- poliprep::read(output_csv)

            # Remove the temporary CSV file
            file.remove(output_csv)

            return(df)
        },
        error = function(e) {
            # Remove the temporary CSV file if an error occurs
            file.remove(output_csv)
            stop("Error reading the exported CSV file: ", e$message)
        }
    )
}
