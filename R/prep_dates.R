#' Check Leap Year Issues in Date Column
#'
#' This function checks if the dates in the given column have leap year issues.
#' It identifies dates that are February 29 in a non-leap year.
#'
#' @param date_col A character vector representing date values.
#' @return A logical vector indicating which dates have leap year issues.
#' @examples
#' dates <- c("2020-02-29", "2019-02-29", "2021-03-01")
#' check_leap_issue(dates)
#' @export
check_leap_issue <- function(date_col) {
  date_col <- gsub("/", "-", date_col)
  date_col <- gsub("T.*$", "", date_col) # remove time from date
  date_parts <- strsplit(date_col, "-")
  leap_issue <- sapply(date_parts, function(parts) {
    if (length(parts) == 3) {
      year <- as.integer(parts[[1]])
      month <- as.integer(parts[[2]])
      day <- as.integer(parts[[3]])
      if (!is.na(year) && !is.na(month) && !is.na(day)) {
        return(month == 2 && day == 29 && !lubridate::leap_year(year))
      }
    }
    return(FALSE)
  })
  return(leap_issue)
}

#' Validate Date Column
#'
#' This function checks and validates a date column in a data frame. It
#' performs various checks for missing dates, non-date values, sensible dates,
#' leap year validity, and date formatting.
#'
#' @param data A data frame containing the date column.
#' @param date_col The name of the date column.
#' @return The modified data frame with additional columns indicating issues
#'         found during the checks.
#'         Columns added:
#'         - `{date_col}_missing`: Indicates if the date is missing.
#'         - `{date_col}_non_date`: Indicates if the value is not a valid date.
#'         - `{date_col}_invalid`: Indicates if the date is not sensible (e.g., 
#'           not starting with '20').
#'         - `{date_col}_future`: Indicates if the date is in the future.
#'         - `{date_col}_leap_issue`: Indicates if there are leap year issues.
#'         - `{date_col}_format_issue`: Indicates if there are improper 
#'            formatting issues.
#' @examples
#' data <- data.frame(
#'   country = c("Rwanda", "Burundi", "Ethiopia", "Zambia", "Zambia",
#'               "Chad", "Niger", "Angola"),
#'   date = c(
#'     "2023-06-15", "2024-07-20", NA, "1999-12-31", "2025-08-22",
#'     "2020/23/10", "2020-02-29", "2019-02-29"
#'   )
#' )
#'
#' # check whether dates column is valid 
#' res <-  poliprep::validate_date(data, "date") 
#'
#'
#' # Check for countries where there is a 
#' # non-date issue in the date column
#' res |> 
#'  dplyr::filter(date_non_date == TRUE) |> 
#'  dplyr::count(country)
#'
#' # Check for countries where there is a 
#' # invalid leap year issue
#' res |> 
#' dplyr::filter(date_leap_issue == TRUE) |> 
#' dplyr::count(country)
#'
#' # Check for countries where there is a 
#' # improper formatting of the date  
#' res |> 
#'  dplyr::filter(date_format_issue == TRUE) |> 
#'  dplyr::count(country)
#'  
#' @export
validate_date <- function(data, date_col) {
  # 1: Check for missing dates -------------------------------------------------
  cli::cli_h1("Check for missing dates")
  data[[paste0(date_col, "_missing")]] <- is.na(data[[date_col]])
  
  if (any(data[[paste0(date_col, "_missing")]])) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col), " has ",
      crayon::red(
        format(
          sum(data[[paste0(date_col, "_missing")]], 
              na.rm = T), big.mark = ",")),
      " missing date(s)! Check column ",
      crayon::green(paste0(date_col, "_missing")), "."
    ))
  } else {
    cli::cli_alert_success("Date column is non-missing!")
  }
  
  # 2: Check for non-date values -----------------------------------------------
  cli::cli_h1("Check for non-date values")
  non_date <- is.na(as.Date(data[[date_col]], format = "%Y-%m-%d")) &
    !is.na(data[[date_col]])
  
  data[[paste0(date_col, "_non_date")]] <- non_date
  
  if (any(non_date)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col), " has ",
      crayon::red(
        format(sum(non_date, na.rm = T), big.mark = ",")),
      " non-date value(s)! Check column ",
      crayon::green(paste0(date_col, "_non_date")), "."
    ))
  } else {
    cli::cli_alert_success("Date column has fully valid dates!")
  }
  
  # 3: Check for sensible dates ------------------------------------------------
  cli::cli_h1("Check for sensible dates")
  invalid_date <- !stringr::str_detect(data[[date_col]], "^20")
  
  current_year <- lubridate::year(Sys.Date())
  
  future_date <- lubridate::year(
    as.Date(data[[date_col]], format = "%Y-%m-%d")
  ) > current_year
  
  data[[paste0(date_col, "_invalid")]] <- invalid_date
  data[[paste0(date_col, "_future")]] <- future_date
  
  if (any(invalid_date)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col), " has ",
      crayon::red(
        format(sum(invalid_date, na.rm = T), big.mark = ",")),
      " non-sensible date(s) (not starting with '20')! Check column ",
      crayon::green(paste0(date_col, "_invalid")), "."
    ))
  }
  if (any(future_date)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col), " has ",
      crayon::red(
        format(sum(future_date, na.rm = T), big.mark = ",")),
      " date(s) with a year greater than the current year! Check column ",
      crayon::green(paste0(date_col, "_future")), "."
    ))
  }
  if (!any(invalid_date) && !any(future_date)) {
    cli::cli_alert_success("Date column has sensible dates!")
  }
  
  # 4: Check for leap year validity --------------------------------------------
  cli::cli_h1("Check for leap year validity")
  na_issue <- is.na(data[[date_col]])
  
  leap_issue <- check_leap_issue(data[[date_col]])
  
  data[[paste0(date_col, "_leap_issue")]] <- leap_issue
  
  if (any(leap_issue)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col), " has ",
      crayon::red(
        format(sum(leap_issue, na.rm = T), big.mark = ",")),
      " invalid leap year date(s)! Check column ",
      crayon::green(paste0(date_col, "_leap_issue")), "."
    ))
  } else {
    cli::cli_alert_success("Date column has no leap year issues!")
  }
  
  # 5: Check for improper date formatting --------------------------------------
  cli::cli_h1("Check for improper date formatting")
  valid_indices <- !leap_issue & !na_issue
  
  date_col_no_leap <- data[[date_col]][valid_indices]
  
  date_col_converted <- as.Date(date_col_no_leap, format = "%Y-%m-%d")
  
  format_issue <- is.na(date_col_converted)
  
  full_format_issue <- rep(FALSE, nrow(data))
  full_format_issue[valid_indices] <- format_issue
  
  data[[paste0(date_col, "_format_issue")]] <- full_format_issue
  
  if (any(full_format_issue)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col), " has ",
      crayon::red(
        format(sum(full_format_issue, na.rm = T), big.mark = ",")),
      " improperly formatted date(s)! Check column ",
      crayon::green(paste0(date_col, "_format_issue")), "."
    ))
  } else {
    cli::cli_alert_success("Date column has no formatting issues!")
  }
  
  return(data)
}

#' Validate Date Columns
#'
#' This function checks and validate two date columns in a data frame. It
#' performs various checks for missing dates, non-date values, sensible dates,
#' leap year validity, date formatting, similarity in date formatting, and
#' if the first date is before the second date.
#'
#' @param data A data frame containing the date columns.
#' @param date_col1 The name of the first date column.
#' @param date_col2 The name of the second date column.
#' @return The modified data frame with additional columns indicating issues
#'         found during the checks.
#'         Columns added:
#'         - `{date_col}_missing`: Indicates if the date is missing.
#'         - `{date_col}_non_date`: Indicates if the value is not a valid date.
#'         - `{date_col}_invalid`: Indicates if the date is not sensible (e.g., 
#'           not starting with '20').
#'         - `{date_col}_future`: Indicates if the date is in the future.
#'         - `{date_col}_leap_issue`: Indicates if there are leap year issues.
#'         - `{date_col}_format_issue`: Indicates if there are improper 
#'            formatting issues.
#'         - `{date_col}_invalid_order`: Indicates if the first date is not 
#'           before the second date.
#' @examples
#' data <- data.frame(
#'   country = c("Rwanda", "Burundi", "Ethiopia", "Zambia", "Zambia",
#'               "Chad", "Niger", "Angola"),
#'   date1 = c(
#'     "2024-06-15", "2024-07-20", NA, "1999-12-31", "2025-08-22",
#'     "2020/23/10", "2020-02-29", "2024-02-29"
#'   ),
#'   date2 = c(
#'     "2023-06-15", "2024-07-20", "2022-05-10", "2019-02-29",
#'     "2026-09-23", "2020/23/10", "2020-02-29", "2022-02-29"
#'   )
#' )
#'
#' res <-  poliprep::validate_dates(data, "date1",  "date1") 
#'
#' # Check for countries where there is a 
#' # non-date issue in the date1 column
#' res |> 
#'  dplyr::filter(date1_non_date == TRUE) |> 
#'  dplyr::count(country)
#'
#' # Check for countries where there is a 
#' # invalid leap year issue in date1
#' res |> 
#' dplyr::filter(date1_leap_issue == TRUE) |> 
#' dplyr::count(country)
#'
#' # Check for countries where there is a 
#' # improper formatting of date1 col  
#' res |> 
#'  dplyr::filter(date1_format_issue == TRUE) |> 
#'  dplyr::count(country)
#'
#' # Check for countries where date1 is after date2
#' res |> 
#'  dplyr::filter(date1_invalid_order == TRUE) |> 
#'  dplyr::count(country)
#'  
#' @export
validate_dates <- function(data, date_col1, date_col2) {
  # 1: Check for missing dates -------------------------------------------------
  cli::cli_h1("Check for missing dates")
  data[[paste0(date_col1, "_missing")]] <- is.na(data[[date_col1]])
  data[[paste0(date_col2, "_missing")]] <- is.na(data[[date_col2]])
  
  if (any(data[[paste0(date_col1, "_missing")]])) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col1), " has ",
      crayon::red(
        format(sum(data[[paste0(date_col1, "_missing")]]), big.mark = ",")
      ),
      " missing date(s)! Check column ",
      crayon::green(paste0(date_col1, "_missing")), "."
    ))
  }
  if (any(data[[paste0(date_col2, "_missing")]])) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col2), " has ",
      crayon::red(
        format(sum(data[[paste0(date_col2, "_missing")]]), big.mark = ",")),
      " missing date(s)! Check column ",
      crayon::green(paste0(date_col2, "_missing")), "."
    ))
  }
  if (!any(data[[paste0(date_col1, "_missing")]]) &&
      !any(data[[paste0(date_col2, "_missing")]])) {
    cli::cli_alert_success("Both date columns are non-missing!")
  }
  
  # 2: Check for non-date values -----------------------------------------------
  cli::cli_h1("Check for non-date values")
  non_date1 <- is.na(as.Date(data[[date_col1]], format = "%Y-%m-%d")) &
    !is.na(data[[date_col1]])
  non_date2 <- is.na(as.Date(data[[date_col2]], format = "%Y-%m-%d")) &
    !is.na(data[[date_col2]])
  
  data[[paste0(date_col1, "_non_date")]] <- non_date1
  data[[paste0(date_col2, "_non_date")]] <- non_date2
  
  if (any(non_date1)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col1), " has ",
      crayon::red(
        format(sum(non_date1, na.rm = T), big.mark = ",")
      ),
      " non-date value(s)! Check column ",
      crayon::green(paste0(date_col1, "_non_date")), "."
    ))
  }
  if (any(non_date2)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col2), " has ",
      crayon::red(
        format(sum(non_date2, na.rm = T), big.mark = ",")
      ),
      " non-date value(s)! Check column ",
      crayon::green(paste0(date_col2, "_non_date")), "."
    ))
  }
  if (!any(non_date1) && !any(non_date2)) {
    cli::cli_alert_success("Both date columns have fully valid dates!")
  }
  
  # 3: Check for sensible dates ------------------------------------------------
  
  cli::cli_h1("Check for sensible dates")
  invalid_date1 <- !stringr::str_detect(data[[date_col1]], "^20")
  invalid_date2 <- !stringr::str_detect(data[[date_col2]], "^20")
  
  current_year <- lubridate::year(Sys.Date())
  
  future_date1 <- lubridate::year(
    as.Date(data[[date_col1]], format = "%Y-%m-%d")
  ) > current_year
  future_date2 <- lubridate::year(
    as.Date(data[[date_col2]], format = "%Y-%m-%d")
  ) > current_year
  
  data[[paste0(date_col1, "_invalid")]] <- invalid_date1
  data[[paste0(date_col2, "_invalid")]] <- invalid_date2
  data[[paste0(date_col1, "_future")]] <- future_date1
  data[[paste0(date_col2, "_future")]] <- future_date2
  
  if (any(invalid_date1)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col1), " has ",
      crayon::red(
        format(sum(invalid_date1, na.rm = T), big.mark = ",")),
      " non-sensible date(s) (not starting with '20')! Check column ",
      crayon::green(paste0(date_col1, "_invalid")), "."
    ))
  }
  if (any(invalid_date2)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col2), " has ",
      crayon::red(
        format(sum(invalid_date2, na.rm = T), big.mark = ",")),
      " non-sensible date(s) (not starting with '20')! Check column ",
      crayon::green(paste0(date_col2, "_invalid")), "."
    ))
  }
  if (any(future_date1)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col1), " has ",
      crayon::red(
        format(sum(future_date1, na.rm = T), big.mark = ",")),
      " date(s) with a year greater than the current year! Check column ",
      crayon::green(paste0(date_col1, "_future")), "."
    ))
  }
  if (any(future_date2)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col2), " has ",
      crayon::red(
        format(sum(future_date2, na.rm = T), big.mark = ",")),
      " date(s) with a year greater than the current year! Check column ",
      crayon::green(paste0(date_col2, "_future")), "."
    ))
  }
  if (!any(invalid_date1) && !any(invalid_date2) &&
      !any(future_date1) && !any(future_date2)) {
    cli::cli_alert_success("Both date columns have sensible dates!")
  }
  
  # 4: Check for leap year validity --------------------------------------------
  cli::cli_h1("Check for leap year validity")
  na_issue1 <- is.na(data[[date_col1]])
  na_issue2 <- is.na(data[[date_col2]])
  
  leap_issue1 <- check_leap_issue(data[[date_col1]])
  leap_issue2 <- check_leap_issue(data[[date_col2]])
  
  data[[paste0(date_col1, "_leap_issue")]] <- leap_issue1
  data[[paste0(date_col2, "_leap_issue")]] <- leap_issue2
  
  if (any(leap_issue1)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col1), " has ",
      crayon::red(
        format(sum(leap_issue1, na.rm = T), big.mark = ",")),
      " invalid leap year date(s)! Check column ",
      crayon::green(paste0(date_col1, "_leap_issue")), "."
    ))
  }
  if (any(leap_issue2)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col2), " has ",
      crayon::red(
        format(sum(leap_issue2, na.rm = T), big.mark = ",")),
      " invalid leap year date(s)! Check column ",
      crayon::green(paste0(date_col2, "_leap_issue")), "."
    ))
  }
  if (!any(leap_issue1) && !any(leap_issue2)) {
    cli::cli_alert_success("Both date columns have no leap year issues!")
  }
  
  # 5: Check for improper date formatting --------------------------------------
  cli::cli_h1("Check for improper date formatting")
  valid_indices1 <- !leap_issue1 & !na_issue1
  valid_indices2 <- !leap_issue2 & !na_issue2
  
  date_col1_no_leap <- data[[date_col1]][valid_indices1]
  date_col2_no_leap <- data[[date_col2]][valid_indices2]
  
  date_col1_converted <- as.Date(date_col1_no_leap, format = "%Y-%m-%d")
  date_col2_converted <- as.Date(date_col2_no_leap, format = "%Y-%m-%d")
  
  format_issue1 <- is.na(date_col1_converted)
  format_issue2 <- is.na(date_col2_converted)
  
  full_format_issue1 <- rep(FALSE, nrow(data))
  full_format_issue1[valid_indices1] <- format_issue1
  
  full_format_issue2 <- rep(FALSE, nrow(data))
  full_format_issue2[valid_indices2] <- format_issue2
  
  data[[paste0(date_col1, "_format_issue")]] <- full_format_issue1
  data[[paste0(date_col2, "_format_issue")]] <- full_format_issue2
  
  if (any(full_format_issue1)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col1), " has ",
      crayon::red(
        format(sum(full_format_issue1, na.rm = T), big.mark = ",")),
      " improperly formatted date(s)! Check column ",
      crayon::green(paste0(date_col1, "_format_issue")), "."
    ))
  }
  if (any(full_format_issue2)) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col2), " has ",
      crayon::red(
        format(sum(full_format_issue2, na.rm = T), big.mark = ",")),
      " improperly formatted date(s)! Check column ",
      crayon::green(paste0(date_col2, "_format_issue")), "."
    ))
  }
  if (!any(full_format_issue1) && !any(full_format_issue2)) {
    cli::cli_alert_success("Both date columns have no formatting issues!")
  }
  
  # 6: Check similarity in date formatting -------------------------------------
  
  cli::cli_h1("Check similarity in date formatting")
  date1_fmt <- poliprep::detect_date_format(
    as.Date(data[[date_col1]], format = "%Y-%m-%d")
  )
  date2_fmt <- poliprep::detect_date_format(
    as.Date(data[[date_col2]], format = "%Y-%m-%d")
  )
  
  if (date1_fmt != date2_fmt) {
    cli::cli_alert_danger(paste0(
      "Date column ", crayon::blue(date_col1), " has format ", date1_fmt,
      " while date column ", crayon::blue(date_col2), " has format ", date2_fmt,
      ". Check columns ", crayon::green(paste0(date_col1, "_fmt")), " and ",
      crayon::green(paste0(date_col2, "_fmt")), "."
    ))
  } else {
    cli::cli_alert_success("Both date columns have the same format!")
  }
  
  # 7: Check if the first date is before the second date -----------------------
  
  cli::cli_h1("Check if the first date is before the second date")
  invalid_order <- as.Date(
    data[[date_col1]],
    format = "%Y-%m-%d"
  ) > as.Date(data[[date_col2]], format = "%Y-%m-%d")
  
  data[[paste0(date_col1, "_invalid_order")]] <- invalid_order
  
  if (any(invalid_order, na.rm = TRUE)) {
    cli::cli_alert_danger(paste0(
      "There are ", crayon::red(
        format(sum(invalid_order, na.rm = T), big.mark = ",")),
      " instances where the first date is not before the second date! ",
      "Check column ", crayon::green(paste0(date_col1, "_invalid_order")), "."
    ))
  } else {
    cli::cli_alert_success("All date pairs are in the correct order!")
  }
  
  return(data)
}
