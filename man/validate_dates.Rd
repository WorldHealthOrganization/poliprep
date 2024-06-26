% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_dates.R
\name{validate_dates}
\alias{validate_dates}
\title{Validate Date Columns}
\usage{
validate_dates(data, date_col1, date_col2)
}
\arguments{
\item{data}{A data frame containing the date columns.}

\item{date_col1}{The name of the first date column.}

\item{date_col2}{The name of the second date column.}
}
\value{
The modified data frame with additional columns indicating issues
found during the checks.
Columns added:
- \verb{\{date_col\}_missing}: Indicates if the date is missing.
- \verb{\{date_col\}_non_date}: Indicates if the value is not a valid date.
- \verb{\{date_col\}_invalid}: Indicates if the date is not sensible (e.g.,
not starting with '20').
- \verb{\{date_col\}_future}: Indicates if the date is in the future.
- \verb{\{date_col\}_leap_issue}: Indicates if there are leap year issues.
- \verb{\{date_col\}_format_issue}: Indicates if there are improper
formatting issues.
- \verb{\{date_col\}_invalid_order}: Indicates if the first date is not
before the second date.
}
\description{
This function checks and validate two date columns in a data frame. It
performs various checks for missing dates, non-date values, sensible dates,
leap year validity, date formatting, similarity in date formatting, and
if the first date is before the second date.
}
\examples{
data <- data.frame(
  country = c("Rwanda", "Burundi", "Ethiopia", "Zambia", "Zambia",
              "Chad", "Niger", "Angola"),
  date1 = c(
    "2024-06-15", "2024-07-20", NA, "1999-12-31", "2025-08-22",
    "2020/23/10", "2020-02-29", "2024-02-29"
  ),
  date2 = c(
    "2023-06-15", "2024-07-20", "2022-05-10", "2019-02-29",
    "2026-09-23", "2020/23/10", "2020-02-29", "2022-02-29"
  )
)

res <-  poliprep::validate_dates(data, "date1",  "date1") 

# Check for countries where there is a 
# non-date issue in the date1 column
res |> 
 dplyr::filter(date1_non_date == TRUE) |> 
 dplyr::count(country)

# Check for countries where there is a 
# invalid leap year issue in date1
res |> 
dplyr::filter(date1_leap_issue == TRUE) |> 
dplyr::count(country)

# Check for countries where there is a 
# improper formatting of date1 col  
res |> 
 dplyr::filter(date1_format_issue == TRUE) |> 
 dplyr::count(country)

# Check for countries where date1 is after date2
res |> 
 dplyr::filter(date1_invalid_order == TRUE) |> 
 dplyr::count(country)
 
}
