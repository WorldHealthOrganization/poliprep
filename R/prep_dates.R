#' Try parsing dates with multiple formats
#' 
#' This helper function attempts to parse dates using specified formats and 
#' handles ISO 8601 formatted dates (containing 'T') separately using ymd_hms.
#'
#' @param x A character vector of dates to parse
#' @param formats A character vector of date formats to try (see ?strptime for 
#'   format specifications)
#' @return A POSIXct vector of parsed dates
try_parsing <- function(x, formats) {
  
  parsed <- lubridate::parse_date_time(x,
                                       orders = formats, quiet = TRUE
  )
  
  # Handle special cases
  iso_indices <- grepl("T", x)
  parsed[iso_indices] <- lubridate::ymd_hms(x[iso_indices], quiet = TRUE)
  
  return(parsed)
}

#' Available date formats for `autoparse_dates` function
#' 
#' A character vector containing all supported date formats that can be parsed 
#' by `autoparse_dates()`. These formats follow standard date-time components:
#' \itemize{
#'   \item d: day
#'   \item m: month
#'   \item Y/y: year (Y=4 digits, y=2 digits)
#'   \item B: full month name
#'   \item b: abbreviated month name
#'   \item H: hour
#'   \item M: minute
#'   \item S: second
#' }
#' @format A character vector of date format strings
#' @examples
#' head(available_date_formats)
#' @export
available_date_formats <- c(
  "dmY", "mdY", "Ymd", "Ydm",
  "dmy", "mdy", "ymd", "ydm",
  "dmY HMS", "mdY HMS", "Ymd HMS", "Ydm HMS",
  "dmy HMS", "mdy HMS", "ymd HMS", "ydm HMS",
  "Y-m-d", "Y-m-d H:M:S", "Y-m-d H:M",
  "y-m-d", "y-m-d H:M:S", "y-m-d H:M",
  "d-m-Y H:M:S", "d.m.Y H:M:S", "d.m.Y H:M",
  "d-m-y H:M:S", "d.m.y H:M:S", "d.m.y H:M",
  "d B Y", "B d, Y", "Y B d",
  "d B y", "B d, y", "y B d",
  "d b Y", "b d, Y", "Y b d",
  "d b y", "b d, y", "y b d",
  "dmY", "mdY", "d-m-Y H:M", "d.m.Y", "d.m.Y H:M",
  "dmy", "mdy", "d-m-y H:M", "d.m.y", "d.m.y H:M",
  "d/m/Y", "d/m/Y H:M:S", "d/m/Y H:M", "d/m/Y HMS", "d/m/Y HM",
  "d/m/y", "d/m/y H:M:S", "d/m/y H:M", "d/m/y HMS", "d/m/y HM",
  "Y/m/d", "Y/m/d H:M:S", "Y/m/d H:M", "Y/m/d HMS", "Y/m/d HM",
  "y/m/d", "y/m/d H:M:S", "y/m/d H:M", "y/m/d HMS", "y/m/d HM"
)

#' Parse Dates in a Data Frame
#'
#' This function parses date columns in a data frame using various date
#' formats. It standardizes the output to a specified format and provides
#' verbose feedback on parsing results.
#'
#' @param data A data frame containing columns with date values to parse.
#' @param date_cols A character vector specifying the names of columns with
#'    date values.
#' @param output_format A character string specifying the desired output format
#'    for the parsed dates. Default is \code{"\%Y-\%m-\%d"}.
#' @param additional_format A character string specifying any additional formats 
#'    that are not included in the default formats 
#'    (see `available_date_formats`). Default is NULL.
#' @param verbose Logical. If \code{TRUE}, prints messages about parsing
#'   success or failure for each column. Default is \code{TRUE}.
#'
#' @return Returns the original data frame with the specified date columns
#'  parsed and formatted.
#'
#' @details
#' This function supports a wide range of date formats, including:
#' \itemize{
#'   \item Basic formats: \code{"dmY"}, \code{"mdY"}, \code{"Ymd"},
#'   \code{"Ydm"}, \code{"dmy"}, \code{"mdy"}, \code{"ymd"}, \code{"ydm"}
#'   \item Formats with time: \code{"dmY HMS"}, \code{"mdY HMS"},
#'   \code{"Ymd HMS"}, \code{"Ydm HMS"}, \code{"dmy HMS"}, \code{"mdy HMS"},
#'   \code{"ymd HMS"}, \code{"ydm HMS"}
#'   \item Additional formats: \code{"Y-m-d"}, \code{"Y-m-d H:M:S"},
#'   \code{"Y-m-d H:M"}, \code{"d-m-Y H:M:S"}, \code{"d.m.Y H:M:S"},
#'   \code{"d.m.Y H:M"}, \code{"d-m-Y H:M"}, \code{"d.m.Y"}
#'   \item ISO 8601 format: e.g., \code{"2021-03-20T00:01:00.513+01:00"}
#' }
#'
#' @examples
#' # Define a sample data frame
#' df <- data.frame(
#'   `dmY` = c("03-10-2023", "11-09-2022", "25-12-2021", "15-08-2020"),
#'   `mdY` = c("10-03-2023", "09-11-2022", "12-25-2021", "08-15-2020"),
#'   `Ymd` = c("2023-10-03", "2022-09-11", "2021-12-25", "2020-08-15"),
#'   `Ydm` = c("2023-03-10", "2022-11-09", "2021-25-12", "2020-15-08"),
#'   `dmy` = c("03-10-23", "11-09-22", "25-12-21", "15-08-20"),
#'   `mdy` = c("10-03-23", "09-11-22", "12-25-21", "08-15-20"),
#'   `ymd` = c("23-10-03", "22-09-11", "21-12-25", "20-08-15"),
#'   `ydm` = c("23-03-10", "22-11-09", "21-25-12", "20-15-08"),
#'   `dmY_HMS` = c(
#'     "03-10-2023 14:30:00", "11-09-2022 05:45:12",
#'     "25-12-2021 23:59:59", "15-08-2020 00:00:00"
#'   ),
#'   `mdY_HMS` = c(
#'     "10-03-2023 14:30:00", "09-11-2022 05:45:12",
#'     "12-25-2021 23:59:59", "08-15-2020 00:00:00"
#'   ),
#'   `Ymd_HMS` = c(
#'     "2023-10-03 14:30:00", "2022-09-11 05:45:12",
#'     "2021-12-25 23:59:59", "2020-08-15 00:00:00"
#'   ),
#'   `Ydm_HMS` = c(
#'     "2023-03-10 14:30:00", "2022-11-09 05:45:12",
#'     "2021-25-12 23:59:59", "2020-15-08 00:00:00"
#'   ),
#'   `dmy_HMS` = c(
#'     "03-10-23 14:30:00", "11-09-22 05:45:12",
#'     "25-12-21 23:59:59", "15-08-20 00:00:00"
#'   ),
#'   `mdy_HMS` = c(
#'     "10-03-23 14:30:00", "09-11-22 05:45:12",
#'     "12-25-21 23:59:59", "08-15-20 00:00:00"
#'   ),
#'   `ymd_HMS` = c(
#'     "23-10-03 14:30:00", "22-09-11 05:45:12",
#'     "21-12-25 23:59:59", "20-08-15 00:00:00"
#'   ),
#'   `ydm_HMS` = c(
#'     "23-03-10 14:30:00", "22-11-09 05:45:12",
#'     "21-25-12 23:59:59", "20-15-08 00:00:00"
#'   ),
#'   `Y-m-d` = c("2023-10-03", "2022-09-11", "2021-12-25", "2020-08-15"),
#'   `Y-m-d_HMS` = c(
#'     "2023-10-03 14:30:00", "2022-09-11 05:45:12",
#'     "2021-12-25 23:59:59", "2020-08-15 00:00:00"
#'   ),
#'   `Y-m-d_HM` = c(
#'     "2023-10-03 14:30", "2022-09-11 05:45",
#'     "2021-12-25 23:59", "2020-08-15 00:00"
#'   ),
#'   `d-m-Y_HMS` = c(
#'     "03-10-2023 14:30:00", "11-09-2022 05:45:12",
#'     "25-12-2021 23:59:59", "15-08-2020 00:00:00"
#'   ),
#'   `d.m.Y_HMS` = c(
#'     "03.10.2023 14:30:00", "11.09.2022 05:45:12",
#'     "25.12.2021 23:59:59", "15.08.2020 00:00:00"
#'   ),
#'   `d.m.Y_HM` = c(
#'     "03.10.2023 14:30", "11.09.2022 05:45",
#'     "25.12.2021 23:59", "15.08.2020 00:00"
#'   ),
#'   `d-m-Y_HM` = c(
#'     "03-10-2023 14:30", "11-09-2022 05:45",
#'     "25-12-2021 23:59", "15-08-2020 00:00"
#'   ),
#'   `d.m.Y` = c("03.10.2023", "11.09.2022", "25.12.2021", "15.08.2020"),
#'   `iso8601` = c("2021-03-20T00:01:00.513+01:00", 
#'                 "2022-11-05T23:15:59.123+01:00",
#'                 "2023-06-15T12:30:45.789Z", 
#'                 "2020-01-01T00:00:00.000-05:00"),
#'   `mixed_formats` = c("2023-10-03", "11.09.2022",
#'                      "25-12-21 23:59", "2020-08-15T00:00:00Z"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Parse the date columns
#' parsed_df <- autoparse_dates(df,
#'   date_cols = colnames(df),
#'   output_format = "%Y-%m-%d"
#' )
#'
#' @export
autoparse_dates <- function(data, date_cols,
                            output_format = "%Y-%m-%d", 
                            additional_format = NULL,
                            verbose = TRUE) {
  if (!is.character(date_cols)) date_cols <- as.character(date_cols)
  
  parse_results <- vector("list", length(date_cols))
  names(parse_results) <- date_cols
  
  # set up formats by also including additional formats
  formats <- c(available_date_formats, additional_format)
  
  data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(date_cols), \(x) {
        parsed <- try_parsing(x, formats)
        parse_results[[dplyr::cur_column()]] <<- sum(is.na(parsed))
        
        if (output_format %in% "%Y-%m-%d") {
          as.Date(parsed)  } else { as.Date(parsed) |> format(output_format) }
      })) -> data
  
  if (verbose) {
    purrr::iwalk(
      parse_results, \(result, col) {
        if (result > 0) {
          cli::cli_alert_warning(
            "Warning: {result} dates could not be parsed in column '{col}'"
          )
        }
      })
    
    if (all(
      purrr::map_lgl(parse_results, \(x) x == 0))) {
      cli::cli_alert_success(
        "All columns have been successfully parsed to the given format"
      )
    }
  }
  
  return(data)
}

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
#' @param tests A character vector specifying which tests to run. Default is
#'        c("missing", "non_date", "sensible", "leap_year", "format").
#' @param min_year The minimum year to consider as valid. Default is 2000.
#' @return The modified data frame with additional columns indicating issues
#'         found during the checks.
#' @examples
#' data <- data.frame(
#'   country = c(
#'     "Rwanda", "Burundi", "Ethiopia", "Zambia", "Zambia",
#'     "Chad", "Niger", "Angola"
#'   ),
#'   date = c(
#'     "2023-06-15", "2024-07-20", NA, "1999-12-31", "2025-08-22",
#'     "2020/23/10", "2020-02-29", "2019-02-29"
#'   )
#' )
#'
#' # check whether dates column is valid
#' res <- validate_date(data, "date")
#'
#' # Check for countries where there is a non-date issue in the date column
#' res |>
#'   dplyr::filter(date_non_date == TRUE) |>
#'   dplyr::count(country)
#'
#' # Check for countries where there is an invalid leap year issue
#' res |>
#'   dplyr::filter(date_leap_issue == TRUE) |>
#'   dplyr::count(country)
#'
#' # Check for countries where there is improper formatting of the date
#' res |>
#'   dplyr::filter(date_format_issue == TRUE) |>
#'   dplyr::count(country)
#'
#' @export
validate_date <- function(data, date_col,
                          tests = c(
                            "missing", "non_date", "sensible",
                            "leap_year", "format"
                          ),
                          min_year = 2000) {
  results <- list()
  
  suppressWarnings(
    # parse datest correctly at the start
    data <- data |>
      dplyr::mutate(
        parsed_date = lubridate::parse_date_time(
          !!rlang::sym(date_col),
          c(
            "dmY", "mdY", "Ymd", "Ydm",
            "dmy", "mdy", "ymd", "ydm",
            "dmY HMS", "mdY HMS", "Ymd HMS", "Ydm HMS",
            "dmy HMS", "mdy HMS", "ymd HMS", "ydm HMS"
          ),
          train = TRUE
        ),
        parsed_date = as.character(parsed_date),
        !!rlang::sym(date_col) := as.character(!!rlang::sym(date_col))
      ) |>
      dplyr::mutate(
        !!rlang::sym(date_col) := dplyr::if_else(
          is.na(parsed_date), !!rlang::sym(date_col), parsed_date
        )
      ) |>
      dplyr::select(-parsed_date)
  )
  
  # 1: Check for missing dates -------------------------------------------------
  if ("missing" %in% tests) {
    cli::cli_h1("Check for missing dates")
    data[[paste0(date_col, "_missing")]] <- is.na(data[[date_col]])
    
    if (any(data[[paste0(date_col, "_missing")]], na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col), " has ",
        crayon::red(
          format(
            sum(data[[paste0(date_col, "_missing")]],
                na.rm = T
            ),
            big.mark = ","
          )
        ),
        " missing date(s)! Check column ",
        crayon::green(paste0(date_col, "_missing")), "."
      ))
    } else {
      cli::cli_alert_success("Date column is non-missing!")
    }
    
    results$missing <- data[[paste0(date_col, "_missing")]]
  }
  
  # 2: Check for non-date values -----------------------------------------------
  if ("non_date" %in% tests) {
    cli::cli_h1("Check for non-date values")
    non_date <- is.na(as.Date(data[[date_col]], format = "%Y-%m-%d")) &
      !is.na(data[[date_col]])
    
    data[[paste0(date_col, "_non_date")]] <- non_date
    
    if (any(non_date, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col), " has ",
        crayon::red(
          format(sum(non_date, na.rm = T), big.mark = ",")
        ),
        " non-date value(s)! Check column ",
        crayon::green(paste0(date_col, "_non_date")), "."
      ))
    } else {
      cli::cli_alert_success("Date column has fully valid dates!")
    }
    
    results$non_date <- non_date
  }
  
  # 3: Check for sensible dates ------------------------------------------------
  if ("sensible" %in% tests) {
    cli::cli_h1("Check for sensible dates")
    
    invalid_date <- !stringr::str_detect(
      data[[date_col]],
      paste0("^", substr(min_year, 1, 2) |> as.numeric())
    )
    
    current_year <- lubridate::year(Sys.Date())
    
    future_date <- lubridate::year(
      as.Date(data[[date_col]], format = "%Y-%m-%d")
    ) > current_year
    
    data[[paste0(date_col, "_invalid")]] <- invalid_date
    data[[paste0(date_col, "_future")]] <- future_date
    
    if (any(invalid_date, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col), " has ",
        crayon::red(
          format(sum(invalid_date, na.rm = T), big.mark = ",")
        ),
        " non-sensible date(s) (not starting with '", min_year,
        "')! Check column ",
        crayon::green(paste0(date_col, "_invalid")), "."
      ))
    }
    
    if (any(future_date, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col), " has ",
        crayon::red(
          format(sum(future_date, na.rm = T), big.mark = ",")
        ),
        " date(s) with a year greater than the current year! Check column ",
        crayon::green(paste0(date_col, "_future")), "."
      ))
    }
    if (!any(invalid_date, na.rm = TRUE) && !any(future_date, na.rm = TRUE)) {
      cli::cli_alert_success("Date column has sensible dates!")
    }
    
    results$invalid <- invalid_date
    results$future <- future_date
  }
  
  # 4: Check for leap year validity --------------------------------------------
  if ("leap_year" %in% tests) {
    cli::cli_h1("Check for leap year validity")
    na_issue <- is.na(data[[date_col]])
    
    leap_issue <- check_leap_issue(data[[date_col]])
    
    data[[paste0(date_col, "_leap_issue")]] <- leap_issue
    
    if (any(leap_issue, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col), " has ",
        crayon::red(
          format(sum(leap_issue, na.rm = T), big.mark = ",")
        ),
        " invalid leap year date(s)! Check column ",
        crayon::green(paste0(date_col, "_leap_issue")), "."
      ))
    } else {
      cli::cli_alert_success("Date column has no leap year issues!")
    }
    
    results$leap_issue <- leap_issue
  }
  
  # 5: Check for improper date formatting --------------------------------------
  if ("format" %in% tests) {
    cli::cli_h1("Check for improper date formatting")
    valid_indices <- !leap_issue & !na_issue
    
    date_col_no_leap <- data[[date_col]][valid_indices]
    
    date_col_converted <- as.Date(date_col_no_leap, format = "%Y-%m-%d")
    
    format_issue <- is.na(date_col_converted)
    
    full_format_issue <- rep(FALSE, nrow(data))
    full_format_issue[valid_indices] <- format_issue
    
    data[[paste0(date_col, "_format_issue")]] <- full_format_issue
    
    if (any(full_format_issue, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col), " has ",
        crayon::red(
          format(sum(full_format_issue, na.rm = T), big.mark = ",")
        ),
        " improperly formatted date(s)! Check column ",
        crayon::green(paste0(date_col, "_format_issue")), "."
      ))
    } else {
      cli::cli_alert_success("Date column has no formatting issues!")
    }
    
    results$format_issue <- full_format_issue
  }
  
  return(data)
}

#' Validate Date Columns
#'
#' This function checks and validates two date columns in a data frame. It
#' performs various checks for missing dates, non-date values, sensible dates,
#' leap year validity, date formatting, similarity in date formatting, and
#' if the first date is before the second date.
#'
#' @param data A data frame containing the date columns.
#' @param date_col1 The name of the first date column.
#' @param date_col2 The name of the second date column.
#' @param tests A character vector specifying which tests to run. Default is
#'        c("missing", "non_date", "sensible", "leap_year", "format",
#'        "similarity", "order").
#' @param min_year The minimum year to consider as valid. Default is 2000.
#' @return The modified data frame with additional columns indicating issues
#'         found during the checks.
#' @examples
#' data <- data.frame(
#'   country = c(
#'     "Rwanda", "Burundi", "Ethiopia", "Zambia", "Zambia",
#'     "Chad", "Niger", "Angola"
#'   ),
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
#' res <- validate_dates(data, "date1", "date2")
#'
#' # Check for countries where there is a non-date issue in the date1 column
#' res |>
#'   dplyr::filter(date1_non_date == TRUE) |>
#'   dplyr::count(country)
#'
#' # Check for countries where there is an invalid leap year issue in date1
#' res |>
#'   dplyr::filter(date1_leap_issue == TRUE) |>
#'   dplyr::count(country)
#'
#' # Check for countries where there is improper formatting of date1 col
#' res |>
#'   dplyr::filter(date1_format_issue == TRUE) |>
#'   dplyr::count(country)
#'
#' # Check for countries where date1 is after date2
#' res |>
#'   dplyr::filter(date1_invalid_order == TRUE) |>
#'   dplyr::count(country)
#'
#' @export
validate_dates <- function(data, date_col1, date_col2,
                           tests = c(
                             "missing", "non_date", "sensible",
                             "leap_year", "format", "similarity",
                             "order"
                           ),
                           min_year = 2000) {
  results <- data
  
  # parse datest correctly at the start
  suppressWarnings(
    data <- data |>
      dplyr::mutate(
        parsed_date1 := lubridate::parse_date_time(
          !!rlang::sym(date_col1),
          c(
            "dmY HMS", "mdY HMS", "Ymd HMS", "Ydm HMS",
            "dmy HMS", "mdy HMS", "ymd HMS", "ydm HMS"
          ),
          train = TRUE
        ),
        parsed_date2 := lubridate::parse_date_time(
          !!rlang::sym(date_col2),
          c(
            "dmY HMS", "mdY HMS", "Ymd HMS", "Ydm HMS",
            "dmy HMS", "mdy HMS", "ymd HMS", "ydm HMS"
          ),
          train = TRUE
        ),
        parsed_date1 = as.character(parsed_date1),
        parsed_date2 = as.character(parsed_date2),
        !!rlang::sym(date_col1) := as.character(!!rlang::sym(date_col1)),
        !!rlang::sym(date_col2) := as.character(!!rlang::sym(date_col2))
      ) |>
      dplyr::mutate(
        !!rlang::sym(date_col1) := dplyr::if_else(
          is.na(parsed_date1), !!rlang::sym(date_col1), parsed_date1
        ),
        !!rlang::sym(date_col2) := dplyr::if_else(
          is.na(parsed_date2), !!rlang::sym(date_col2), parsed_date2
        )
      ) |>
      dplyr::select(-parsed_date1, -parsed_date2)
  )
  
  # 1: Check for missing dates -------------------------------------------------
  if ("missing" %in% tests) {
    cli::cli_h1("Check for missing dates")
    results[[paste0(date_col1, "_missing")]] <- is.na(data[[date_col1]])
    results[[paste0(date_col2, "_missing")]] <- is.na(data[[date_col2]])
    
    if (any(results[[paste0(date_col1, "_missing")]], na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col1), " has ",
        crayon::red(
          format(sum(results[[paste0(date_col1, "_missing")]], na.rm = TRUE),
                 big.mark = ","
          )
        ),
        " missing date(s)! Check column ",
        crayon::green(paste0(date_col1, "_missing")), "."
      ))
    }
    if (any(results[[paste0(date_col2, "_missing")]], na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col2), " has ",
        crayon::red(
          format(sum(results[[paste0(date_col2, "_missing")]], na.rm = TRUE),
                 big.mark = ","
          )
        ),
        " missing date(s)! Check column ",
        crayon::green(paste0(date_col2, "_missing")), "."
      ))
    }
    if (!any(results[[paste0(date_col1, "_missing")]], na.rm = TRUE) &&
        !any(results[[paste0(date_col2, "_missing")]], na.rm = TRUE)) {
      cli::cli_alert_success("Both date columns are non-missing!")
    }
  }
  
  # 2: Check for non-date values -----------------------------------------------
  if ("non_date" %in% tests) {
    cli::cli_h1("Check for non-date values")
    non_date1 <- is.na(as.Date(data[[date_col1]], format = "%Y-%m-%d")) &
      !is.na(data[[date_col1]])
    non_date2 <- is.na(as.Date(data[[date_col2]], format = "%Y-%m-%d")) &
      !is.na(data[[date_col2]])
    
    results[[paste0(date_col1, "_non_date")]] <- non_date1
    results[[paste0(date_col2, "_non_date")]] <- non_date2
    
    if (any(non_date1, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col1), " has ",
        crayon::red(
          format(sum(non_date1, na.rm = TRUE), big.mark = ",")
        ),
        " non-date value(s)! Check column ",
        crayon::green(paste0(date_col1, "_non_date")), "."
      ))
    }
    if (any(non_date2, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col2), " has ",
        crayon::red(
          format(sum(non_date2, na.rm = TRUE), big.mark = ",")
        ),
        " non-date value(s)! Check column ",
        crayon::green(paste0(date_col2, "_non_date")), "."
      ))
    }
    if (!any(non_date1, na.rm = TRUE) && !any(non_date2, na.rm = TRUE)) {
      cli::cli_alert_success("Both date columns have fully valid dates!")
    }
  }
  
  # 3: Check for sensible dates ------------------------------------------------
  if ("sensible" %in% tests) {
    cli::cli_h1("Check for sensible dates")
    
    current_year <- lubridate::year(Sys.Date())
    
    invalid_date1 <- !between(lubridate::year(
      as.Date(data[[date_col1]], format = "%Y-%m-%d")
    ), min_year, current_year)
    invalid_date2 <- !between(lubridate::year(
      as.Date(data[[date_col2]], format = "%Y-%m-%d")
    ), min_year, current_year)
    
    future_date1 <- lubridate::year(
      as.Date(data[[date_col1]], format = "%Y-%m-%d")
    ) > current_year
    future_date2 <- lubridate::year(
      as.Date(data[[date_col2]], format = "%Y-%m-%d")
    ) > current_year
    
    results[[paste0(date_col1, "_invalid")]] <- invalid_date1
    results[[paste0(date_col2, "_invalid")]] <- invalid_date2
    results[[paste0(date_col1, "_future")]] <- future_date1
    results[[paste0(date_col2, "_future")]] <- future_date2
    
    if (any(invalid_date1, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col1), " has ",
        crayon::red(
          format(sum(invalid_date1, na.rm = TRUE), big.mark = ",")
        ),
        " non-sensible date(s) (not between ",
        min_year, " and ", current_year, ")! Check column ",
        crayon::green(paste0(date_col1, "_invalid")), "."
      ))
    }
    if (any(invalid_date2, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col2), " has ",
        crayon::red(
          format(sum(invalid_date2, na.rm = TRUE), big.mark = ",")
        ),
        " non-sensible date(s) (not between ",
        min_year, " and ", current_year, ")! Check column ",
        crayon::green(paste0(date_col2, "_invalid")), "."
      ))
    }
    if (any(future_date1, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col1), " has ",
        crayon::red(
          format(sum(future_date1, na.rm = TRUE), big.mark = ",")
        ),
        " date(s) with a year greater than the current year! Check column ",
        crayon::green(paste0(date_col1, "_future")), "."
      ))
    }
    if (any(future_date2, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col2), " has ",
        crayon::red(
          format(sum(future_date2, na.rm = TRUE), big.mark = ",")
        ),
        " date(s) with a year greater than the current year! Check column ",
        crayon::green(paste0(date_col2, "_future")), "."
      ))
    }
    if (!any(invalid_date1, na.rm = TRUE) && !any(invalid_date2, na.rm = TRUE) &&
        !any(future_date1, na.rm = TRUE) && !any(future_date2, na.rm = TRUE)) {
      cli::cli_alert_success("Both date columns have sensible dates!")
    }
  }
  
  # 4: Check for leap year validity --------------------------------------------
  if ("leap_year" %in% tests) {
    cli::cli_h1("Check for leap year validity")
    leap_issue1 <- check_leap_issue(data[[date_col1]])
    leap_issue2 <- check_leap_issue(data[[date_col2]])
    
    results[[paste0(date_col1, "_leap_issue")]] <- leap_issue1
    results[[paste0(date_col2, "_leap_issue")]] <- leap_issue2
    
    if (any(leap_issue1, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col1), " has ",
        crayon::red(
          format(sum(leap_issue1, na.rm = TRUE), big.mark = ",")
        ),
        " invalid leap year date(s)! Check column ",
        crayon::green(paste0(date_col1, "_leap_issue")), "."
      ))
    }
    if (any(leap_issue2, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col2), " has ",
        crayon::red(
          format(sum(leap_issue2, na.rm = TRUE), big.mark = ",")
        ),
        " invalid leap year date(s)! Check column ",
        crayon::green(paste0(date_col2, "_leap_issue")), "."
      ))
    }
    if (!any(leap_issue1, na.rm = TRUE) && !any(leap_issue2, na.rm = TRUE)) {
      cli::cli_alert_success("Both date columns have no leap year issues!")
    }
  }
  
  # 5: Check for improper date formatting --------------------------------------
  if ("format" %in% tests) {
    cli::cli_h1("Check for improper date formatting")
    valid_indices1 <- !is.na(data[[date_col1]])
    valid_indices2 <- !is.na(data[[date_col2]])
    
    date_col1_no_na <- data[[date_col1]][valid_indices1]
    date_col2_no_na <- data[[date_col2]][valid_indices2]
    
    date_col1_converted <- as.Date(date_col1_no_na, format = "%Y-%m-%d")
    date_col2_converted <- as.Date(date_col2_no_na, format = "%Y-%m-%d")
    
    format_issue1 <- is.na(date_col1_converted)
    format_issue2 <- is.na(date_col2_converted)
    
    full_format_issue1 <- rep(FALSE, nrow(data))
    full_format_issue1[valid_indices1] <- format_issue1
    
    full_format_issue2 <- rep(FALSE, nrow(data))
    full_format_issue2[valid_indices2] <- format_issue2
    
    results[[paste0(date_col1, "_format_issue")]] <- full_format_issue1
    results[[paste0(date_col2, "_format_issue")]] <- full_format_issue2
    
    if (any(full_format_issue1, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col1), " has ",
        crayon::red(
          format(sum(full_format_issue1, na.rm = TRUE), big.mark = ",")
        ),
        " improperly formatted date(s)! Check column ",
        crayon::green(paste0(date_col1, "_format_issue")), "."
      ))
    }
    if (any(full_format_issue2, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col2), " has ",
        crayon::red(
          format(sum(full_format_issue2, na.rm = TRUE), big.mark = ",")
        ),
        " improperly formatted date(s)! Check column ",
        crayon::green(paste0(date_col2, "_format_issue")), "."
      ))
    }
    if (!any(full_format_issue1, na.rm = TRUE) && !any(
      full_format_issue2,
      na.rm = TRUE
    )) {
      cli::cli_alert_success("Both date columns have no formatting issues!")
    }
  }
  
  # 6: Check similarity in date formatting -------------------------------------
  if ("similarity" %in% tests) {
    cli::cli_h1("Check similarity in date formatting")
    date1_fmt <- poliprep::detect_date_format(
      as.Date(data[[date_col1]], format = "%Y-%m-%d")
    )
    date2_fmt <- poliprep::detect_date_format(
      as.Date(data[[date_col2]][!is.na(data[[date_col2]])], format = "%Y-%m-%d")
    )
    
    if (date1_fmt != date2_fmt) {
      cli::cli_alert_info(paste0(
        "Date column ", crayon::blue(date_col1), " has format ", date1_fmt,
        " while date column ", crayon::blue(date_col2), " has format ", date2_fmt,
        ". Check columns ", crayon::green(paste0(date_col1, "_fmt")), " and ",
        crayon::green(paste0(date_col2, "_fmt")), "."
      ))
    } else {
      cli::cli_alert_success("Both date columns have the same format!")
    }
  }
  
  # 7: Check if the first date is before the second date -----------------------
  if ("order" %in% tests) {
    cli::cli_h1("Check if the first date is before the second date")
    invalid_order <- as.Date(
      data[[date_col1]],
      format = "%Y-%m-%d"
    ) > as.Date(data[[date_col2]], format = "%Y-%m-%d")
    
    results[[paste0(date_col1, "_invalid_order")]] <- invalid_order
    
    if (any(invalid_order, na.rm = TRUE)) {
      cli::cli_alert_info(paste0(
        "There are ", crayon::red(
          format(sum(invalid_order, na.rm = TRUE), big.mark = ",")
        ),
        " instances where the first date is not before the second date! ",
        "Check column ", crayon::green(paste0(date_col1, "_invalid_order")), "."
      ))
    } else {
      cli::cli_alert_success("All date pairs are in the correct order!")
    }
  }
  
  return(results)
}
