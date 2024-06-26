% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ona.R
\name{get_ona_data}
\alias{get_ona_data}
\title{Get Data from ONA API}
\usage{
get_ona_data(
  base_url = "https://api.whonghub.org",
  form_id,
  api_token,
  selected_columns = NULL
)
}
\arguments{
\item{base_url}{A string specifying the URL for the ONA API,
could be https://esurv.afro.who.int/api/v1/data eSurv.
Default is https://api.whonghub.org/api/v1/data}

\item{form_id}{A string or numeric value as the dataset id.
Could be found by looking at the URL of the form on ONA.}

\item{api_token}{A string specifying the API token for ONA}

\item{selected_columns}{Selected columns to download. Default is NULL.
It's in stringed list like c("year", "form_id")}
}
\value{
A list containing the data geted from the ONA API.
}
\description{
This function getes data for a specified form from the ONA API using a
provided API token and form ID. It returns the data in a structured format
if the request is successful. If the request fails, the function stops and
returns an error message indicating the failure reason.
}
\examples{
# base_url <- https://api.ona.io/api/v1/data
# api_token <- "your_api_token_here"
# form_id <- 123456
# data <- get_ona_data(base_url, form_id, api_token)
}
\seealso{
\url{https://api.ona.io/api/v1/data/} for more info on ONA API
}
