#' Compress PNG Files in a Directory or a Single PNG File with pngquant
#'
#' This function compresses either a single PNG file or all PNG files in a
#' specified directory using pngquant optimization to reduce file size while
#' maintaining visual quality. pngquant is a lossy compression tool that can
#' reduce file sizes by up to 70% while preserving full alpha transparency.
#'
#' @param path A string specifying either the path to a single PNG file or
#' a directory containing PNG files.
#' @param auto_install Logical. If TRUE, will attempt to automatically install
#'    pngquant if not found on the system. Default is FALSE.
#' @param force Logical. If TRUE, will overwrite existing files. Default is
#'    FALSE.
#' @param speed Integer. Speed/quality trade-off from 1 (brute-force) to 10
#' (fastest). Default is 3. Speed 10 has 5% lower quality but is 8 times
#'    faster.
#' @return Returns invisibly NULL. The function works by side effect,
#' compressing PNG files in place.
#' @examples
#' # Compress all PNG files in a directory
#' # optimize_png("path/to/your/folder")
#'
#' # Compress a single PNG file
#' # optimize_png("path/to/your/image.png")
#'
#' # Compress with automatic installation if pngquant is not found
#' # optimize_png("path/to/your/folder", auto_install = TRUE)
#'
#' # Compress and overwrite existing files
#' # optimize_png("path/to/your/folder", force = TRUE)
#'
#' @export
optimize_png <- function(path, auto_install = FALSE, force = FALSE,
                         speed = 1) {
  os <- Sys.info()[["sysname"]]
  pngquant_path <- Sys.which("pngquant")
  if (pngquant_path == "" && os == "Windows") {
    potential_paths <- c(
      "C:/Program Files/pngquant/pngquant.exe",
      "pngquant_bin/pngquant/pngquant.exe",
      "C:/Program Files (x86)/pngquant/pngquant.exe"
    )
    for (p in potential_paths) {
      if (file.exists(p)) {
        pngquant_path <- p
        break
      }
    }
  }

  if (pngquant_path == "") {
    if (!auto_install) {
      cli::cli_alert_warning(
        paste0(
          "pngquant is not installed. Install it manually ",
          "or set auto_install=TRUE."
        )
      )
      return(invisible(NULL))
    }
    ans <- readline(
      "pngquant is not installed. Install automatically? (y/n): "
    )
    if (tolower(ans) != "y") {
      cli::cli_alert_warning(
        "pngquant installation aborted. Exiting function."
      )
      return(invisible(NULL))
    }

    if (os == "Windows") {
      if (Sys.which("git") != "") {
        clone_cmd <- paste0(
          "git clone -b msvc --recursive ",
          "https://github.com/kornelski/pngquant.git"
        )
        if (system(
          clone_cmd
        ) != 0) {
          stop("Failed to clone pngquant repository.")
        }
        src_dir <- file.path(getwd(), "pngquant")
        build_cmd <- sprintf(
          "cmd /c cd %s && cargo build --release", shQuote(src_dir)
        )
        if (system(build_cmd) != 0) {
          stop(
            "Failed to build pngquant."
          )
        }
        pngquant_path <- file.path(
          src_dir, "target", "release", "pngquant.exe"
        )
        if (!file.exists(
          pngquant_path
        )) {
          stop("pngquant binary not found after build.")
        }
        cli::cli_alert_info(paste("pngquant installed at", pngquant_path))
      } else {
        message("Git not available. Downloading pre-built pngquant binary...")
        zip_url <- "https://pngquant.org/pngquant-windows.zip"
        zip_file <- "pngquant-windows.zip"
        utils::download.file(zip_url, destfile = zip_file, mode = "wb")
        utils::unzip(zip_file, exdir = "pngquant_bin")
        files_extracted <- list.files(
          "pngquant_bin",
          pattern = "pngquant\\.exe$", full.names = TRUE, recursive = TRUE
        )
        if (length(files_extracted) == 0) {
          stop("Pre-built pngquant binary not found after extraction.")
        }
        pngquant_path <- files_extracted[1]
        cli::cli_alert_info(paste("pngquant installed at", pngquant_path))
      }
    } else {
      if (Sys.which("git") != "") {
        clone_cmd <- paste0(
          "git clone --recursive https://github.com/kornelski/pngquant.git"
        )
        if (
          system(clone_cmd) != 0) {
          stop("Failed to clone pngquant repository.")
        }
        src_dir <- file.path(getwd(), "pngquant")
      } else {
        zip_url <-
          "https://github.com/kornelski/pngquant/archive/refs/heads/master.zip"
        zip_file <- "pngquant-master.zip"
        utils::download.file(zip_url, destfile = zip_file, mode = "wb")
        utils::unzip(zip_file)
        src_dir <- file.path(getwd(), "pngquant-master")
      }
      build_cmd <- sprintf(
        "cd %s && cargo build --release", shQuote(src_dir)
      )
      if (system(build_cmd) != 0) stop("Failed to build pngquant.")
      pngquant_path <- file.path(
        src_dir, "target", "release", "pngquant"
      )
      if (!file.exists(
        pngquant_path
      )) {
        stop("pngquant binary not found after build.")
      }
      cli::cli_alert_info(
        paste("pngquant installed at", pngquant_path)
      )
    }
  }

  # Normalize path to handle relative paths correctly
  path <- normalizePath(path, mustWork = FALSE)

  # Check if path is a file or directory
  is_file <- FALSE
  if (file.exists(path)) {
    if (!dir.exists(path) && grepl("\\.png$", path, ignore.case = TRUE)) {
      is_file <- TRUE
      png_files <- path
    } else if (dir.exists(path)) {
      png_files <- list.files(path, pattern = "\\.png$", full.names = TRUE)
      if (length(png_files) == 0) {
        cli::cli_alert_warning("No PNG files found in the directory.")
        return(invisible(NULL))
      }
    } else {
      cli::cli_alert_warning("Path is neither a PNG file nor a directory.")
      return(invisible(NULL))
    }
  } else {
    cli::cli_alert_warning(paste0("Path does not exist: ", path))
    cli::cli_alert_info("Please check that the path is correct and accessible.")
    return(invisible(NULL))
  }

  # Function to compress a single PNG file
  compress_single_file <- function(file) {
    cmd_parts <- c(
      shQuote(pngquant_path),
      "--speed", as.character(speed),
      "--force",
      "--ext", ".png",
      shQuote(file)
    )
    result <- system(
      paste(cmd_parts, collapse = " "),
      intern = TRUE,
      ignore.stderr = FALSE
    )
    stat <- attr(result, "status")
    if (is.null(stat) || stat == 0 || stat == 99) {
      if (is.null(stat) || stat == 0) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  # Process files
  if (is_file) {
    # Single file processing
    success <- compress_single_file(png_files)
    if (success) {
      cli::cli_alert_success("File compressed successfully.")
    } else {
      cli::cli_alert_warning("Error compressing file.")
    }
  } else {
    # Directory processing
    pb <- progress::progress_bar$new(
      format = "Compressing files [:bar] :percent  ETA: :eta",
      total = length(png_files), width = 60
    )

    compressed_count <- 0
    errors <- character()
    for (file in png_files) {
      success <- compress_single_file(file)
      if (success) {
        compressed_count <- compressed_count + 1
      } else {
        errors <- c(errors, paste("Error with file:", file))
      }
      pb$tick()
    }

    cli::cli_alert_success(
      paste(
        compressed_count, "out of", length(png_files), "files compressed"
      )
    )

    if (length(errors) > 0) {
      cli::cli_alert_warning(
        paste(length(errors), "files could not be compressed:")
      )
      for (err in errors) {
        cli::cli_alert_info(err)
      }
    }
  }

  invisible(NULL)
}
