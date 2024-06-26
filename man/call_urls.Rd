% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ona.R
\name{call_urls}
\alias{call_urls}
\title{Call multiple URLs}
\usage{
call_urls(urls, api_token)
}
\arguments{
\item{urls}{array of url strings}

\item{api_token}{A string specifying the API token for ONA}
}
\value{
tibble with all data
}
\description{
Call multiple URLs
}
\details{
Function to pull multiple URLS in parallel and with progress bar displayed.
Taken from Nishant Kishores \code{tidypolis}.
}
