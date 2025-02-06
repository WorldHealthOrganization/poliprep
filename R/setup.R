#' Package Initialization and Dependency Check
#'
#' @description
#' This function checks for suggested packages and prompts the user to install
#' any missing ones that are needed for full functionality. It handles both
#' CRAN and GitHub packages.
#'
#' @param libname The library name where the package is installed (not used)
#' @param pkgname The name of the package being loaded (not used)
#'
#' @details
#' The function maintains a predefined list of suggested packages and checks if
#' they are installed. For missing packages, it prompts the user for
#' installation in interactive sessions. Special handling is included for
#' GitHub packages like esri2sf'.
#'
#' The function uses 'cli' for user communication and handles errors gracefully
#' during installation attempts. In non-interactive sessions, it skips
#' installation and returns with a warning.
#'
#' @return
#' Returns NULL invisibly. The function's main effects are:
#' \itemize{
#'   \item Checking for missing suggested packages
#'   \item Displaying missing packages to user
#'   \item Installing packages if user agrees
#'   \item Providing feedback on installation success/failure
#' }
#'
#' @note
#' - Function requires an interactive session for package installation
#' - Some functionality may be limited if suggested packages are not installed
#' - Installation errors are caught and reported but don't stop execution
#'
#' @keywords internal
#' @export
install_suggested_packages <- function(libname = NULL, pkgname = NULL) {
  suggested_pkgs <- c(
    "testthat", "progressr", "janitor",
    "withr", "stringdist", "crayon", "glue",
    "stringi", "httpcode", "yaml", "scales",
    "webshot", "gt", "parzer", "readr",
    "zoo", "epoxy", "officer", "ggplot2",
    "progress", "filelock", "lifecycle", "here"
  )
  
  missing_pkgs <- suggested_pkgs[!(
    suggested_pkgs %in% utils::installed.packages()[, "Package"])]
  
  if (length(missing_pkgs) > 0) {
    cli::cli_h2("Package Installation Required")
    cli::cli_text("The following packages are missing:")
    cli::cli_ol(paste0("{.pkg ", missing_pkgs, "}"))
    
    # Handle non-interactive sessions
    if (!interactive()) {
      cli::cli_alert_warning(
        "Non-interactive session detected. Skipping package installation."
      )
      return(invisible(NULL))
    }
    
    # Prompt user for installation
    user_choice <- readline(
      prompt = cli::col_blue(
        "Do you want to install all missing packages? (y/n): "
      )
    )
    
    if (tolower(user_choice) == "y") {
      cli::cli_alert_success("Installing missing packages...")
      
      # Ensure remotes is installed
      if (!requireNamespace("remotes", quietly = TRUE)) {
        tryCatch({
          utils::install.packages("remotes", quiet = TRUE)
        }, error = function(e) {
          cli::cli_alert_danger(paste0(
            "Failed to install 'remotes'. Error: ", e$message
          ))
          invisible(NULL)
        })
      }
      
      # Install CRAN packages
      cran_pkgs <- missing_pkgs[missing_pkgs != "optout"]
      if (length(cran_pkgs) > 0) {
        for (pkg in cran_pkgs) {
          tryCatch({
            utils::install.packages(pkg, quiet = TRUE)
          }, error = function(e) {
            cli::cli_alert_danger(paste0(
              "Failed to install package: ", pkg,
              ". Error: ", e$message
            ))
          })
        }
      }
      
      # Install GitHub packages
      if ("esri2sf" %in% missing_pkgs) {
        tryCatch({
          remotes::install_github("coolbutuseless/optout", quiet = TRUE)
        }, error = function(e) {
          cli::cli_alert_danger(paste0(
            "Failed to install GitHub package 'optout'. Error: ", e$message
          ))
        })
      }
      
      cli::cli_alert_success("Installation of all packages complete.")
    } else {
      cli::cli_alert_warning(paste0(
        "Skipping installation of packages. ",
        "Some functionality might be limited."
      ))
    }
  } else {
    cli::cli_alert_success("All suggested packages are already installed.")
  }
  
  invisible(NULL)
}

#' Create a Standardized Project Folder Structure
#'
#' This function creates a standardized folder structure for organizing
#' data, scripts, and outputs within a project directory. It ensures
#' consistency and reproducibility for data-related workflows.
#'
#' @param base_path A character string specifying the root directory where
#'   the folder structure will be created. Defaults to `here::here()`
#'   to use the current project directory.
#'
#' @return Creates directories under the specified `base_path`. Returns
#'   invisible `NULL` and prints messages about folder creation status.
#'
#' @details The function generates the following folder structure:
#' \preformatted{
#' # 01_data/
#' # ├── processed/
#' # ├── raw/
#' # 02_scripts/
#' # 03_outputs/
#' # ├── 3a_visualizations/
#' # ├── 3b_tables/
#' # ├── 3c_powerpoint_slides/
#' # └── 3d_model_outputs/
#' }
#'
#' @examples
#' # Create the project folder structure in the current directory
#' # create_project_structure()
#'
#' # Create the folder structure in a specific directory
#' # create_project_structure(base_path = "~/my_project")
#'
#' @export
init_folders <- function(base_path = here::here()) {
  
  # Conditional loading for packages
  required_packages <- c(
    "here"
  )
  
  missing_packages <- required_packages[!sapply(
    required_packages, requireNamespace,
    quietly = TRUE
  )]
  
  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "Package(s) ", paste(missing_packages, collapse = ", "),
        " required but not installed. Please install them."
      ),
      call. = FALSE
    )
  }
  
  # Define relative directories
  relative_dirs <- c(
    "01_data/processed",
    "01_data/raw",
    "02_scripts",
    "03_outputs/3a_visualizations",
    "03_outputs/3b_tables",
    "03_outputs/3c_powerpoint_slides",
    "03_outputs/3d_model_outputs"
  )
  
  # Construct full paths and create directories
  for (relative_dir in relative_dirs) {
    dir_path <- normalizePath(file.path(base_path, relative_dir),
                              winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cli::cli_alert_info("Created: {dir_path}")
    } else {
      cli::cli_alert_warning("Exists: {dir_path}")
    }
  }
  
  cli::cli_alert_success("Folder structure created successfully.")
}
