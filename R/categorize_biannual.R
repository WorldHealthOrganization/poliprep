#' Categorize Dates into Biannual Intervals
#'
#' This function categorizes dates in a given dataset into non-overlapping
#' six-month intervals, working backward from the maximum date in the dataset.
#' Each date is assigned to a category based on the interval it falls into.
#'
#' @param data A data frame containing the date column.
#' @param date_column A string specifying the name of the date column in the
#'        data frame.
#'
#' @return A data frame with an additional factor column `date_categ`
#'   that contains the interval labels.
#'
#' @examples
#' data <- data.frame(
#'   virus_date = as.Date(c("2021-05-01", "2021-11-30", "2022-01-15"))
#' )
#' categorized_data <- categorize_biannual(data, "virus_date")
#'
#' @export
#'
categorize_biannual <- function(data, date_column) {
  
  data[[date_column]] <- as.Date(data[[date_column]], format = "%Y-%m-%d")
  max_date <- max(data[[date_column]], na.rm = TRUE)
  min_date <- min(data[[date_column]], na.rm = TRUE)
  
  # Simplify adjusted start date calculation
  adjusted_start_date <- lubridate::ceiling_date(
    min_date, "6 months") - months(6)
  adjusted_end_date <- lubridate::ceiling_date(
    max_date, "6 months")
  
  # Sequence generation
  start_dates <- seq(from = adjusted_start_date,
                     to = adjusted_end_date, by = "6 months")
  end_dates <- start_dates + months(6) - lubridate::days(1)
  
  intervals <- data.frame(
    start = start_dates,
    end = end_dates,
    start_month = stringr::str_sub(
      lubridate::month(start_dates, label = TRUE), start = 1, end = 3),
    end_month = stringr::str_sub(
      lubridate::month(end_dates, label = TRUE), start = 1, end = 3),
    start_year = lubridate::year(start_dates) %% 100,
    end_year = lubridate::year(end_dates) %% 100
  )
  
  # Expanding intervals for join
  expanded_intervals <- tidyr::expand_grid(
    !!date_column := seq(min_date, max_date, by = "day"),
    intervals
  ) |>
    dplyr::filter(!!rlang::sym(date_column) >= start &
                    !!rlang::sym(date_column) <= end) |>
    dplyr::mutate(
      label = glue::glue("{start_month} {start_year} / {end_month} {end_year}")
    ) |>
    dplyr::select(!!rlang::sym(date_column) , label)
  
  # Joining and coalescing the labels
  data <- data |>
    dplyr::left_join(expanded_intervals,
                     by = date_column,
                     relationship = "many-to-many"
    ) |>
    dplyr::mutate(date_categ = dplyr::coalesce(label, "Other")) |>
    dplyr::select(-label)
  
  # Order the labels chronologically and set the levels
  labels <- data |>
    dplyr::select(date_categ, !!rlang::sym(date_column)) |>
    dplyr::arrange(!!rlang::sym(date_column)) |>
    dplyr::pull(date_categ)
  
  data <- data |>
    dplyr::mutate(date_categ = factor(date_categ, levels = unique(labels)))
  
  return(data)
}