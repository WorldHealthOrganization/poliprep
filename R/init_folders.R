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
