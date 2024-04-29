
#' Detect the Date Format of a Vector of Dates
#'
#' This function automatically detects and returns the most likely date format
#' from a given vector of date strings. It attempts to parse the dates using
#' a set of common date formats and selects the format that successfully parses
#' the majority of the entries.
#'
#' @param date_vector A character vector of dates.
#' @param common_formats An optional character vector specifying the date 
#'        formats to try. If NULL, a default set of formats is used.
#' @param ... Additional arguments passed to \code{lubridate::parse_date_time}.
#'
#' @return A character string representing the date format that had the highest
#'         success rate of parsing the dates in \code{date_vector}. Returns
#'         \code{NA} if no suitable format is found.
#'
#' @examples
#' # date_vector <- c("01-02-2021", "03-15-1988", "2023-05-12",
#' #                  "19/02/2021", "1988-15-03", "05-12-2023", "05.27.2022")
#' # detect_date_format(date_vector)
#'
#' @export
detect_date_format <- function(date_vector, common_formats = NULL, ...) {
  
  # Provide default common formats if not specified ----------------------------
  if (is.null(common_formats)) {
    common_formats <- c("%d-%m-%Y", "%m-%d-%Y", "%Y-%m-%d",
                        "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d",
                        "%d.%m.%Y", "%m.%d.%Y", "%Y.%m.%d", 
                        "%Y %m %d",  "%Y%m%d",
                        "%Y-%m-%d %H:%M:%S",
                        "%d/%m/%Y %I:%M %p",
                        "%m.%d.%Y %H-%M-%S",
                        "%Y %b %d %H%M",
                        "%d-%m-%Y %H:%M",
                        "%m/%d/%Y %I:%M %p"
    )
  }
  
  # Calculate the number of successful parses for each format ------------------
  format_counts <- vapply(common_formats, function(fmt) {
    sum(!is.na(
      lubridate::parse_date_time(
        date_vector, orders = fmt, quiet = TRUE, exact = TRUE, ...)))
  }, numeric(1)) 
  
  # Select the format with the maximum count of successful parses --------------
  if (max(format_counts) > 0) {
    return(names(which.max(format_counts)))
  } else {
    return(NA)  
  }
}