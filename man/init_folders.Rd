% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{init_folders}
\alias{init_folders}
\title{Create a Standardized Project Folder Structure}
\usage{
init_folders(base_path = here::here())
}
\arguments{
\item{base_path}{A character string specifying the root directory where
the folder structure will be created. Defaults to \code{here::here()}
to use the current project directory.}
}
\value{
Creates directories under the specified \code{base_path}. Returns
invisible \code{NULL} and prints messages about folder creation status.
}
\description{
This function creates a standardized folder structure for organizing
data, scripts, and outputs within a project directory. It ensures
consistency and reproducibility for data-related workflows.
}
\details{
The function generates the following folder structure:
\preformatted{
# 01_data/
# ├── processed/
# ├── raw/
# 02_scripts/
# 03_outputs/
# ├── 3a_visualizations/
# ├── 3b_tables/
# ├── 3c_powerpoint_slides/
# └── 3d_model_outputs/
}
}
\examples{
# Create the project folder structure in the current directory
# create_project_structure()

# Create the folder structure in a specific directory
# create_project_structure(base_path = "~/my_project")

}
