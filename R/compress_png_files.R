#' Compress PNG Files in a Directory with pngquant
#'
#' This function iterates over all PNG files in a specified directory
#' and applies pngquant optimization to each file using the optout package.
#'
#' @param directory A string specifying the path to the directory containing
#' PNG files.
#' @examples
#' # compress_png_files("path/to/your/folder")
#'
#' @export
compress_png_files <- function(directory) {
  
  # Check for pngquant installation on macOS or Linux
  if (Sys.info()["sysname"] == "Darwin" || 
      Sys.info()["sysname"] == "Linux") {
    
    if (system("which pngquant", ignore.stdout = TRUE, 
               ignore.stderr = TRUE) != 0) {
      stop(
        paste0("pngquant is not installed or not in the ",
               "system PATH. Please install pngquant."))
    }
    pngquant_path <- "pngquant"
  } else if (Sys.info()["sysname"] == "Windows") {
    # Check for pngquant installation on Windows
    pngquant_path <- file.path(
      "C:", "Program Files", "pngquant", "pngquant.exe")
    
    if (!file.exists(pngquant_path)) {
      stop(paste0("pngquant.exe not found. Please install ",
                  "pngquant and ensure it's in the specified path."))
    }
  } else {
    stop("Unsupported operating system.")
  }
  
  # List all PNG files in the directory
  png_files <- list.files(
    path = directory, pattern = "\\.png$", full.names = TRUE)
  
  # Initialize progress bar
  pb <- progress::progress_bar$new(
    format = "Compressing files [:bar] :percent  ETA: :eta",
    total = length(png_files),
    width = 60
  )
  
  # Counter for successfully compressed files
  compressed_count <- 0
  
  # Apply pngquant to each file and update progress bar
  for (file in png_files) {
    optout::pngquant(file)
    
    if (file.exists(file)) {
      compressed_count <- compressed_count + 1
    }
    pb$tick()
  }
  
  cli::cli_alert_success(
    paste(compressed_count, 
          "out of", length(png_files),
          "files compressed")
  )
}
