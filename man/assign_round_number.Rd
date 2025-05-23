% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_round_number.R
\name{assign_round_number}
\alias{assign_round_number}
\title{Assign Round Numbers to Dates Based on Proximity}
\usage{
assign_round_number(
  df,
  date_col = "today",
  additional_grouping = NULL,
  threshold = 7
)
}
\arguments{
\item{df}{A data frame containing at least one column of dates.}

\item{date_col}{A string specifying the name of the date column in the data
frame. Default is "today".}

\item{additional_grouping}{A character vector specifying additional columns
to group by (e.g., c("states", "lgas")). Default is NULL, meaning only
clustering by dates will be performed.}

\item{threshold}{A numeric value representing the maximum number of days
between dates to be considered part of the same round. Default is 7.}
}
\value{
A data frame with a new column \code{round_group} containing the assigned
round names (month and year) based on the proximity of the dates, and
ordered as factors.
}
\description{
This function assigns a round name to a data frame containing dates based on
their proximity within a specified threshold. Dates that are within the
threshold (in days) of each other will be assigned the same round name.
Additionally, the function allows for grouping by additional columns, such
as states or LGAs. This is particularly useful in scenarios where campaigns
may overlap, such as when campaigns occurring in early October 2024 are
closely related to those in September 2024. By clustering campaigns based on
proximity, the function effectively handles edge cases and provides coherent
round groupings.
}
\examples{
#  df <- data.frame(
#    today = as.Date(
#     c("2024-04-28", "2024-05-03", "2024-05-04", "2024-05-03",
#       "2024-06-01", "2024-06-02", "2024-07-29", "2024-07-30")),
#   states = c("State1", "State1", "State1", "State1", "State2", "State2", 
#  "State2", "State2")
# )
# result <- assign_round_number(df, date_col = "today", 
#                             additional_grouping = "states")
# print(result)

}
