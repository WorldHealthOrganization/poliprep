% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_mdb_merge.R
\name{prep_mdb_table_extract}
\alias{prep_mdb_table_extract}
\title{Function to extract tables from multiple Microsoft Access databases.}
\usage{
prep_mdb_table_extract(mdb_folder, target_table)
}
\arguments{
\item{mdb_folder}{Path to the folder containing the .mdb files}

\item{target_table}{The table to be extracted from each mdb file. This should
unique name accross all the .mdb files.}
}
\value{
Returns a list all all the extracted tables indexed by their names.
}
\description{
Function to extract tables from multiple Microsoft Access databases.
}
\examples{
# How to run the function
prep_mdb_table_extract(mdb_folder, target_table)
}
