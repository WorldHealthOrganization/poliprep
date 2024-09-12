#' Check for Missing Values in a Data Frame
#'
#' This function analyzes a data frame for missing values, providing both
#' counts and percentages of missing data for specified columns or all columns
#' if not specified. It also allows grouping by one or more variables.
#'
#' @param data A data frame to check for missing values.
#' @param cols_to_check An optional character vector of column names to check.
#'        If NULL (default), all columns are checked.
#' @param key_columns An optional character vector of column names that
#'        are considered key for the analysis. Any missing values in these
#'        columns will be flagged as "High Priority" in the output.
#' @param group_by An optional character vector of column names to group by.
#'        If provided, missing values will be calculated within each group.
#'
#' @return A tibble with columns:
#'   \item{Column}{The name of the checked column}
#'   \item{Missing Count}{The number of missing values in the column}
#'   \item{Missing Percent}{The percentage of missing values in the column}
#'   \item{Priority}{Indicates if the column is "High Priority" or "Standard"}
#'   \item{Is Completely Null}{Indicates if the column is completely null}
#'   If grouping is used, additional columns for each grouping variable will be
#'   included.
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, NA), b = c("x", NA, "z"), c = c(TRUE, FALSE, TRUE),
#'   Year = c(2020, 2021, 2020), Country = c("USA", "Canada", "USA")
#' )
#' check_missing(df)
#' check_missing(df, c("a", "b"))
#' check_missing(df, group_by = c("Year", "Country"))
#'
#' @export
check_missing <- function(data, cols_to_check = NULL,
                          key_columns = NULL, group_by = NULL) {
  # If no columns specified, use all columns except grouping variables
  if (base::is.null(cols_to_check)) {
    cols_to_check <- base::setdiff(base::names(data), group_by)
  } else {
    cols_to_check <- base::setdiff(cols_to_check, group_by)
  }
  
  # Prepare the grouping
  if (!base::is.null(group_by)) {
    data <- data |> dplyr::group_by(dplyr::across(dplyr::all_of(group_by)))
  }
  
  result <- data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(cols_to_check),
        .fns = base::list(
          missing_count = ~ base::sum(base::is.na(.)),
          missing_percent = ~ base::mean(base::is.na(.)) * 100
        )
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(group_by),
      names_to = c("column", ".value"),
      names_pattern = "(.*)_(missing_count|missing_percent)"
    ) |>
    dplyr::arrange(dplyr::desc(missing_percent)) |>
    dplyr::mutate(
      missing_count = big_mark(missing_count),
      missing_percent = round(missing_percent, 2),
      priority = dplyr::case_when(
        column %in% key_columns ~ "High Priority",
        TRUE ~ "Standard"
      )
    ) |>
    dplyr::rename(
      Column = column,
      `Missing Count` = missing_count,
      `Missing Percent` = missing_percent,
      Priority = priority
    ) |>
    dplyr::mutate(
      `Is Completely Null` = ifelse(`Missing Percent` == 100, TRUE, FALSE),
      Column = factor(Column, levels = cols_to_check),
      key_order = match(Column, key_columns)
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_by)),
      key_order, Column, dplyr::desc(`Missing Percent`)
    ) |>
    dplyr::select(
      dplyr::all_of(group_by),
      Column, Priority,
      `Missing Count`, `Missing Percent`,
      `Is Completely Null`
    ) |>
    as.data.frame()
  
  # drop Priority col if key column
  # is not specified
  if (is.null(key_columns)) {
    result <- result |>
      dplyr::select(-Priority)
  }
  
  # Initialize a list to store results
  missing_results <- list()
  
  # Loop through each row in the result
  for (i in seq_len(nrow(result))) {
    col <- result$Column[i]
    missing_percent <- result$`Missing Percent`[i]
    
    # Check if there are missing values
    if (missing_percent > 0) {
      missing_results[[length(missing_results) + 1]] <- list(
        column = col,
        missing_percent = missing_percent,
        missing_count = result$`Missing Count`[i]
      )
    }
  }
  
  # Summarize results row by row
  cli::cli_h1("Completely Null Columns")
  
  for (i in seq_len(nrow(result))) {
    if (result$`Is Completely Null`[i]) {
      cli::cli_alert_warning(paste0(
        "Column '", result$Column[i], "' is completely null."
      ))
    }
  }
  
  # Summarize results row by row
  cli::cli_h1("Columns with Missing Values")
  
  missing_cols <- result |>
    dplyr::filter(`Missing Count` > 0 & `Is Completely Null` == FALSE) |>
    dplyr::arrange(dplyr::desc(`Missing Percent`))
  
  for (i in seq_len(nrow(missing_cols))) {
    row <- missing_cols[i, ]
    
    cli::cli_h2(paste("Column:", row$Column))
    
    # Report on columns with missing values
    cli::cli_alert_warning(paste0(
      "Column '", crayon::blue(row$Column), "' has ",
      crayon::red(row$`Missing Percent`), "% (",
      crayon::red(row$`Missing Count`), ") missing values."
    ))
    
    cli::cli_text("")
  }
  
  return(result)
}

#' Get IDs with Missing Values for a Specific Column
#'
#' This function returns the IDs of rows where a specified column has
#' missing values.
#'
#' @param data A data frame to check for missing values.
#' @param id_column A string specifying the name of the ID column.
#' @param variable A string specifying the name of the column to
#'                  check for missing values.
#'
#' @return A vector of IDs corresponding to rows with missing values in
#'        the specified column.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:5,
#'   a = c(1, NA, 3, NA, 5),
#'   b = c("x", "y", NA, "w", "z")
#' )
#' get_missing_ids(df, "id", "a")
#' get_missing_ids(df, "id", "b")
#'
#' @export
get_missing_ids <- function(data, id_column, variable) {
  data |>
    dplyr::filter(base::is.na(!!dplyr::sym(variable))) |>
    dplyr::pull(!!dplyr::sym(id_column))
}

#' Validate Administrative Hierarchy
#'
#' This function performs validation checks on administrative hierarchy data,
#' focusing on hierarchical integrity and uniqueness of combinations.
#'
#' @param data A data frame containing the administrative hierarchy data.
#' @param column_combos A list of character vectors, each representing a set of
#'        hierarchical columns to check.
#' @param id_columns A character vector specifying the column(s) containing
#'        unique identifiers. Default is NULL.
#' @param return_non_unique A logical value indicating whether to return
#'        non-unique IDs. Default is FALSE.
#' @return If return_non_unique is FALSE (default), returns a data frame
#'         containing the results of hierarchical integrity checks. If
#'         return_non_unique is TRUE and id_columns is provided, returns a
#'         vector of non-unique IDs.
#'
#' @examples
#' # Sample data
#' sample_data <- data.frame(
#'   Id = c(1, 2, 3, 4, 5, 6, 7),
#'   Admin0GUID = c("A1", "A1", "A2", "A2", "A1", "A2", "A1"),
#'   Admin0Name = c(
#'     "Country1", "Country1", "Country2", "Country2",
#'     "Country1", "Country2", "Country1"
#'   ),
#'   Admin1GUID = c("B1", "B2", "B3", "B3", "B1", "B3", "B2"),
#'   Admin1Name = c(
#'     "Province1", "Province2", "Province3", "Province3",
#'     "Province1", "Province3", "Province2"
#'   ),
#'   Admin2GUID = c("C1", "C2", "C3", "C4", "C1", "C3", "C2"),
#'   Admin2Name = c(
#'     "District1", "District2", "District3", "District4",
#'     "District1", "District3", "District2"
#'   )
#' )
#'
#' # Check hierarchical integrity
#' result1 <- validate_admin_hierarchy(
#'   data = sample_data,
#'   column_combos = list(
#'     c("Admin0GUID", "Admin1GUID", "Admin2GUID"),
#'     c("Admin0Name", "Admin1Name", "Admin2Name")
#'   )
#' )
#' print(result1)
#'
#' # Get non-unique IDs
#' result2 <- validate_admin_hierarchy(
#'   data = sample_data,
#'   column_combos = list(
#'     c("Admin0GUID", "Admin1GUID", "Admin2GUID"),
#'     c("Admin0Name", "Admin1Name", "Admin2Name")
#'   ),
#'   id_columns = "Id",
#'   return_non_unique = TRUE
#' )
#' print(result2)
#'
#' @export
validate_admin_hierarchy <- function(data, column_combos,
                                     id_columns = NULL,
                                     return_non_unique = FALSE) {
  # Check if id_columns is provided when return_non_unique is TRUE
  if (return_non_unique && is.null(id_columns)) {
    stop("id_columns must be provided when return_non_unique is TRUE")
  }
  
  # Validate that id_columns exist in the data
  if (!is.null(id_columns) && !all(id_columns %in% names(data))) {
    missing_cols <- setdiff(id_columns, names(data))
    stop(paste(
      "The following id_columns are not present in the data:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Validate that column_combos are present in the data
  all_columns <- unique(unlist(column_combos))
  if (!all(all_columns %in% names(data))) {
    missing_cols <- setdiff(all_columns, names(data))
    stop(paste(
      "The following columns are not present in the data:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Convert columns to character
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(all_columns), as.character))
  
  result_list <- purrr::map(column_combos, function(combo) {
    data |>
      dplyr::distinct(dplyr::across(dplyr::all_of(combo))) |>
      dplyr::summarise(
        total_count = dplyr::n(),
        unique_count = dplyr::n_distinct(dplyr::pick(dplyr::everything())),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        non_unique_count = total_count - unique_count,
        proportion_unique = (unique_count / total_count) * 100
      ) |>
      dplyr::select(total_count, non_unique_count, proportion_unique)
  })
  
  names(result_list) <- purrr::map_chr(column_combos, paste, collapse = " ~ ")
  
  results_df <- result_list |>
    dplyr::bind_rows(.id = "Column Combination") |>
    dplyr::rename(
      "Total Count" = total_count,
      "Non-unique Count" = non_unique_count,
      "Proportion Unique" = proportion_unique
    ) |>
    as.data.frame()
  
  if (return_non_unique && !is.null(id_columns)) {
    non_unique_ids <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(all_columns))) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup() |>
      dplyr::pull(dplyr::all_of(id_columns)) |>
      unique()
    
    return(non_unique_ids)
  } else {
    return(results_df)
  }
  
  # Summarize results row by row
  cli::cli_h1("Summary of Administrative Hierarchy Validation")
  
  for (i in seq_len(nrow(results_df))) {
    row <- results_df[i, ]
    
    cli::cli_h2(paste("Column Combination:", row$`Column Combination`))
    
    if (row$`Non-unique Count` > 0) {
      cli::cli_alert_warning(paste0(
        "Found ", crayon::red(row$`Non-unique Count`),
        " non-unique combination(s) out of ", row$`Total Count`, " total."
      ))
    } else {
      cli::cli_alert_success("All combinations are unique.")
    }
    
    cli::cli_text("")
  }
}

#' Check for Mismatches Between Data and Shapefile
#'
#' This function compares geographical data with a shapefile to identify
#' mismatches in names and IDs. It provides both a summary of mismatches and
#' detailed information about specific mismatches.
#'
#' @param data A data frame containing the geographical data to be checked.
#' @param shapefile_data A data frame containing the shapefile data. If NULL,
#'        poliprep::shp_global will be used.
#' @param geo_name_cols A character vector of column names in 'data' containing
#'        geographical names.
#' @param geo_id_cols A character vector of column names in 'data' containing
#'        geographical IDs. The first element (typically ADM0_GUID) will be
#'        removed.
#' @param shapefile_name_cols A character vector of column names in
#'        'shapefile_data' containing geographical names. If NULL, default
#'        values will be used.
#' @param shapefile_id_cols A character vector of column names in
#'        'shapefile_data' containing geographical IDs. If NULL, default values
#'        will be used.
#' @param id_col A string specifying the name of the ID column in 'data'.
#'
#' @return A list containing two data frames:
#'   \item{summary_table}{A summary of mismatches for each geographical column}
#'   \item{detailed_mismatches}{Detailed information about each mismatch}
#'
#' @examples
#' # Sample data
#' sample_data <- data.frame(
#'   EPID = 1:5,
#'   Admin0GUID = c("A1", "A1", "A2", "A2", "A3"),
#'   Admin0Name = c("Country1", "Country1", "Country2", "Country2", "Country3"),
#'   Admin1GUID = c("B1", "B2", "B3", "B3", "B4"),
#'   Admin1Name = c(
#'     "Province1", "Province2", "Province3", "Province3",
#'     "Province4"
#'   ),
#'   Admin2GUID = c("C1", "C2", "C3", "C4", "C5"),
#'   Admin2Name = c(
#'     "District1", "District2", "District3", "District4",
#'     "District5"
#'   )
#' )
#'
#' # Sample shapefile data
#' shapefile_data <- data.frame(
#'   ADM0_NAME = c("Country1", "Country2"),
#'   ADM1_NAME = c("Province1", "Province3"),
#'   ADM2_NAME = c("District1", "District3"),
#'   ADM1_GUID = c("B1", "B3"),
#'   ADM2_GUID = c("C1", "C3")
#' )
#'
#' # Define column names
#' geo_name_cols <- c("Admin0Name", "Admin1Name", "Admin2Name")
#' geo_id_cols <- c("Admin0GUID", "Admin1GUID", "Admin2GUID")
#' shapefile_name_cols <- c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME")
#' shapefile_id_cols <- c("ADM1_GUID", "ADM2_GUID")
#' id_col <- "EPID"
#'
#' # Run the function
#' results <- join_and_check_mismatches(
#'   sample_data, shapefile_data,
#'   geo_name_cols, geo_id_cols,
#'   shapefile_name_cols, shapefile_id_cols,
#'   id_col
#' )
#'
#' # View results
#' print(results$summary_table)
#' print(results$detailed_mismatches)
#' @importFrom stats setNames
#' @export
join_and_check_mismatches <- function(data, shapefile_data = NULL,
                                      geo_name_cols, geo_id_cols,
                                      shapefile_name_cols = NULL,
                                      shapefile_id_cols = NULL,
                                      id_col) {
  # Remove the first element from geo_id_cols
  geo_id_cols <- geo_id_cols[-1]
  
  # Use poliprep::shp_global if shapefile_data is not provided
  if (is.null(shapefile_data)) {
    shapefile_data <- poliprep::shp_global
    shapefile_name_cols <- c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME")
    shapefile_id_cols <- c("ADM1_GUID", "ADM2_GUID")
  }
  
  # Ensure input integrity
  if (length(geo_name_cols) != length(shapefile_name_cols) ||
      length(geo_id_cols) != length(shapefile_id_cols)) {
    stop(
      paste0(
        "The number of name columns must match, ",
        "and ID columns must match except for ADM0_GUID."
      )
    )
  }
  
  # Ensure distinct combinations in both datasets
  data <- data |>
    dplyr::distinct(dplyr::across(
      dplyr::all_of(c(geo_name_cols, geo_id_cols, id_col))
    ))
  shapefile_data <- shapefile_data |>
    dplyr::distinct(dplyr::across(
      dplyr::all_of(c(shapefile_name_cols, shapefile_id_cols))
    ))
  
  # Initialize results list
  results <- list()
  
  # Summary table to keep track of mismatches
  summary_table <- data.frame(
    Column = c(geo_name_cols, geo_id_cols),
    Missing_in_Shapefile = numeric(length(c(geo_name_cols, geo_id_cols))),
    stringsAsFactors = FALSE
  )
  
  # Detailed mismatches (long list)
  detailed_mismatches <- data.frame(
    stringsAsFactors = FALSE
  )
  detailed_mismatches[[id_col]] <- character()
  detailed_mismatches$Geo_Column <- character()
  detailed_mismatches$Data_Value <- character()
  
  # Check names and IDs for mismatches
  for (i in seq_along(geo_name_cols)) {
    # Check if names from data exist in shapefile
    col <- geo_name_cols[i]
    shapefile_col <- shapefile_name_cols[i]
    
    # Mismatches where data values are not found in the shapefile
    missing_in_shapefile <- dplyr::anti_join(
      data |> dplyr::filter(!is.na(.data[[col]])),
      shapefile_data |> dplyr::filter(!is.na(.data[[shapefile_col]])),
      by = setNames(shapefile_col, col)
    )
    
    # Update the summary table
    summary_table$Missing_in_Shapefile[i] <- nrow(missing_in_shapefile)
    
    # If there are mismatches, add them to the detailed mismatch table
    if (nrow(missing_in_shapefile) > 0) {
      detailed_mismatches <- rbind(detailed_mismatches, data.frame(
        id_col = missing_in_shapefile[[id_col]],
        Geo_Column = col,
        Data_Value = missing_in_shapefile[[col]],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  for (i in seq_along(geo_id_cols)) {
    # Check if IDs from data exist in shapefile
    col <- geo_id_cols[i]
    shapefile_col <- shapefile_id_cols[i]
    
    # Ensure the shapefile ID column is character and lowercase
    shapefile_data <- shapefile_data |>
      dplyr::mutate(
        !!rlang::sym(shapefile_id_cols[i]) := as.character(
          !!rlang::sym(shapefile_id_cols[i])
        ),
        !!rlang::sym(shapefile_id_cols[i]) := stringr::str_to_lower(
          stringr::str_replace_all(
            !!rlang::sym(shapefile_id_cols[i]), "[{}]", ""
          )
        )
      )
    
    # Ensure the data ID column is character and lowercase
    data <- data |>
      dplyr::mutate(
        !!rlang::sym(geo_id_cols[i]) := as.character(
          !!rlang::sym(geo_id_cols[i])
        ),
        !!rlang::sym(geo_id_cols[i]) := stringr::str_to_lower(
          stringr::str_replace_all(!!rlang::sym(geo_id_cols[i]), "[{}]", "")
        )
      )
    
    # Mismatches where data values are not found in the shapefile
    missing_in_shapefile <- dplyr::anti_join(
      data |> dplyr::filter(!is.na(.data[[col]])),
      shapefile_data |> dplyr::filter(!is.na(.data[[shapefile_col]])),
      by = setNames(shapefile_col, col)
    )
    
    # Update the summary table
    summary_table$Missing_in_Shapefile[length(
      geo_name_cols
    ) + i] <- nrow(missing_in_shapefile)
    
    # If there are mismatches, add them to the detailed mismatch table
    if (nrow(missing_in_shapefile) > 0) {
      detailed_mismatches <- rbind(detailed_mismatches, data.frame(
        id_col = missing_in_shapefile[[id_col]],
        Geo_Column = col,
        Data_Value = missing_in_shapefile[[col]],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Rename the columns and add to results
  results$detailed_mismatches <- detailed_mismatches |>
    dplyr::rename(
      `Geo Column Type` = Geo_Column,
      `Name/ID  Missing in Shapefile` = Data_Value
    )
  names(results$detailed_mismatches)[1] <- id_col
  
  # Rename the columns and add to results
  results$summary_table <- summary_table |>
    dplyr::rename(
      `Geo Column Type` = Column,
      `Missing in Shapefile` = Missing_in_Shapefile
    )
  
  # Summarize results row by row
  cli::cli_h1("Summary of Geoname and ID Mismatches")
  
  for (i in seq_len(nrow(results$summary_table))) {
    row <- results$summary_table[i, ]
    
    cli::cli_h2(paste("Geographic Column:", row$`Geo Column Type`))
    
    if (row$`Missing in Shapefile` > 0) {
      cli::cli_alert_warning(paste0(
        "Found ", crayon::red(row$`Missing in Shapefile`),
        " value(s) in the data not present in the shapefile."
      ))
    } else {
      cli::cli_alert_success("All values in the data match the shapefile.")
    }
    
    cli::cli_text("") # Add a blank line for readability
  }
  
  return(results)
}



#' Check and validate data across multiple dimensions
#'
#' This function performs comprehensive checks on a dataset, including missing
#' data, data types, date validations, geographic hierarchy, geo-name and ID
#' mismatches, and coordinate checks.
#'
#' @param data A data frame containing the dataset to be checked.
#' @param id_col Character string specifying the name of the ID column.
#' @param geo_name_cols Character vector of column names containing geographic
#'   names.
#' @param geo_id_cols Character vector of column names containing geographic
#'   IDs.
#' @param date_cols Character vector of column names containing dates.
#' @param date_pair_cols List of character vectors, each containing a pair of
#'   date column names to be compared.
#' @param lat_long_cols Character vector of two column names containing
#'   latitude and longitude.
#' @param shapefile_data Optional data frame containing shapefile data for
#'   geographic comparisons.
#' @param shapefile_join_key Optional character string specifying the join key
#'   for the shapefile data.
#' @param correct_lat_col Optional character string specifying the column name
#'   for correct latitude in shapefile data.
#' @param correct_lon_col Optional character string specifying the column name
#'   for correct longitude in shapefile data.
#' @param shapefile_name_cols Optional character vector of column names in
#'   shapefile data containing geographic names.
#' @param shapefile_id_cols Optional character vector of column names in
#'   shapefile data containing geographic IDs.
#' @param run_missing_check Logical, whether to run missing data check.
#' @param run_date_check Logical, whether to run date validation checks.
#' @param run_geo_hierarchy_check Logical, whether to run geographic hierarchy
#'    check.
#' @param run_geo_mismatch_check Logical, whether to run geographic mismatch
#'    check.
#' @param run_coordinate_checks Logical, whether to run coordinate checks.
#' @param run_detections Logical, whether to run virus detections checks.
#' @param coordinate_checks Character vector specifying which coordinate checks
#'    to run.
#'
#' @return A list containing the following elements:
#'   \item{missing_data}{Data frame summarizing missing data and data types for
#'     all columns}
#'   \item{date_results}{Data frame summarizing date validation results}
#'   \item{date_results_pairs}{Data frame summarizing date pair validation
#'     results}
#'   \item{geo_hierarchy}{List containing results of geographic hierarchy
#'     validation}
#'   \item{geo_mismatches}{Data frame summarizing mismatches between data and
#'     shapefile}
#'   \item{coordinate_checks}{Data frame summarizing coordinate validation
#'     results}
#'
#' @examples
#' # Basic usage:
#' # results <- check_data(
#' #   data = my_data,
#' #   id_col = "ID",
#' #   geo_name_cols = c("country", "region", "district"),
#' #   geo_id_cols = c("country_id", "region_id", "district_id"),
#' #   date_cols = c("start_date", "end_date"),
#' #   lat_long_cols = c("latitude", "longitude")
#' # )
#' #
#' # With shapefile data and date pairs:
#' # results_with_shapefile <- check_data(
#' #   data = my_data,
#' #   id_col = "ID",
#' #   geo_name_cols = c("country", "region", "district"),
#' #   geo_id_cols = c("country_id", "region_id", "district_id"),
#' #   date_cols = c("start_date", "end_date"),
#' #   date_pair_cols = list(c("start_date", "end_date")),
#' #   lat_long_cols = c("latitude", "longitude"),
#' #   shapefile_data = my_shapefile_data,
#' #   shapefile_join_key = "district_id",
#' #   correct_lat_col = "correct_lat",
#' #   correct_lon_col = "correct_lon",
#' #   shapefile_name_cols = c("country_name", "region_name", "district_name"),
#' #   shapefile_id_cols = c("country_code", "region_code", "district_code")
#' # )
#'
#' @export
check_data <- function(data,
                       id_col,
                       geo_name_cols,
                       geo_id_cols,
                       date_cols,
                       date_pair_cols = NULL,
                       lat_long_cols,
                       shapefile_data = NULL,
                       shapefile_join_key = NULL,
                       correct_lat_col = NULL,
                       correct_lon_col = NULL,
                       shapefile_name_cols = NULL,
                       shapefile_id_cols = NULL,
                       run_missing_check = TRUE,
                       run_date_check = TRUE,
                       run_geo_hierarchy_check = TRUE,
                       run_geo_mismatch_check = TRUE,
                       run_coordinate_checks = TRUE,
                       run_detections = TRUE,
                       coordinate_checks = c(
                         "flip", "on_water", "missing",
                         "out_of_bounds", "precision",
                         "null_coords", "parse"
                       )) {
  # Initialize results list
  full_results <- list()
  
  # Get all columns
  all_cols <- names(data)
  
  # 1. Base data information ---------------------------------------------
  
  # 1a. Missing data
  if (run_missing_check) {
    full_results$missing_data <- check_missing(
      data,
      cols_to_check = all_cols,
      key_columns = c(id_col, geo_id_cols, date_cols, lat_long_cols)
    ) |>
      dplyr::select(Column, `Missing Count`, `Is Completely Null`) |>
      dplyr::mutate(
        `Missing Count` = stringr::str_replace_all(`Missing Count`, ",", ""),
        `Missing Count` = as.integer(`Missing Count`)
      ) |>
      as.data.frame()
    
    # 1b. Data type check for all columns
    data_type_check <- data.frame(
      Column = names(data),
      DataType = sapply(data, function(x) class(x)[1])
    )
    
    # add data type check to missing data
    full_results$missing_data <- full_results$missing_data |>
      dplyr::left_join(data_type_check, by = "Column") |>
      dplyr::select(Column, DataType, `Missing Count`, `Is Completely Null`)
    
    full_results$geo_missing_data <- full_results$missing_data |>
      dplyr::filter(Column %in% c(geo_name_cols, geo_id_cols))
    
    full_results$geo_missing_id <- data |>
      dplyr::select(!!rlang::sym(id_col), dplyr::all_of(
        c(geo_name_cols, geo_id_cols)
      )) |>
      tidyr::pivot_longer(
        cols = -!!rlang::sym(id_col),
        names_to = "Column",
        values_to = "Value"
      ) |>
      dplyr::filter(is.na(Value)) |>
      dplyr::distinct(!!rlang::sym(id_col), Column) |>
      dplyr::mutate(`Column Type` = "Geo") |>
      dplyr::mutate(Test = glue::glue("{Column} is missing")) |>
      dplyr::select(-Column) |>
      as.data.frame()
  }
  
  # 2a Check date columns if provided ------------------------------------
  
  if (run_date_check) {
    date_results <- data.frame(Column = date_cols)
    
    for (i in seq_along(date_cols)) {
      col <- date_cols[i]
      validated_data <- validate_date(data, col)
      
      date_results[i, "Missing"] <- sum(
        validated_data[[paste0(col, "_missing")]],
        na.rm = TRUE
      )
      date_results[i, "Non-date"] <- sum(
        validated_data[[paste0(col, "_non_date")]],
        na.rm = TRUE
      )
      date_results[i, "Invalid"] <- sum(
        validated_data[[paste0(col, "_invalid")]],
        na.rm = TRUE
      )
      date_results[i, "Future"] <- sum(
        validated_data[[paste0(col, "_future")]],
        na.rm = TRUE
      )
      date_results[i, "Leap Issue"] <- sum(
        validated_data[[paste0(col, "_leap_issue")]],
        na.rm = TRUE
      )
      date_results[i, "Format Issue"] <- sum(
        validated_data[[paste0(col, "_format_issue")]],
        na.rm = TRUE
      )
      
      # Get IDs
      ids <- validated_data |>
        dplyr::filter(!!rlang::sym(paste0(col, "_missing"))) |>
        dplyr::distinct(!!rlang::sym(id_col)) |>
        dplyr::mutate(Test = glue::glue("{col} is missing"))
      
      ids_non_date <- validated_data |>
        dplyr::filter(!!rlang::sym(paste0(col, "_non_date"))) |>
        dplyr::distinct(!!rlang::sym(id_col)) |>
        dplyr::mutate(Test = glue::glue("{col} is non-date"))
      
      ids_invalid <- validated_data |>
        dplyr::filter(!!rlang::sym(paste0(col, "_invalid"))) |>
        dplyr::distinct(!!rlang::sym(id_col)) |>
        dplyr::mutate(Test = glue::glue("{col} is invalid"))
      
      ids_future <- validated_data |>
        dplyr::filter(!!rlang::sym(paste0(col, "_future"))) |>
        dplyr::distinct(!!rlang::sym(id_col)) |>
        dplyr::mutate(Test = glue::glue("{col} is in the future"))
      
      ids_leap_issue <- validated_data |>
        dplyr::filter(!!rlang::sym(paste0(col, "_leap_issue"))) |>
        dplyr::distinct(!!rlang::sym(id_col)) |>
        dplyr::mutate(Test = glue::glue("{col} has leap year issue"))
      
      ids_format_issue <- validated_data |>
        dplyr::filter(!!rlang::sym(paste0(col, "_format_issue"))) |>
        dplyr::distinct(!!rlang::sym(id_col)) |>
        dplyr::mutate(Test = glue::glue("{col} has format issue"))
      
      full_results$date_issue_id <- dplyr::bind_rows(
        ids, ids_non_date, ids_invalid, ids_future,
        ids_leap_issue, ids_format_issue
      ) |>
        dplyr::mutate(`Column Type` = "Date")
    }
    
    # add date results pairs to results
    full_results$date_results <- date_results
    
    # 2b Check date pairs if provided
    if (!is.null(date_pair_cols)) {
      date_results_pairs <- data.frame()
      
      for (pair in date_pair_cols) {
        start_col <- pair[1]
        end_col <- pair[2]
        validated_data <- validate_dates(
          data, start_col, end_col,
          tests = c("order")
        )
        pair_name <- paste(start_col, end_col, sep = " ~ ")
        date_results_pairs[nrow(date_results_pairs) + 1, "Variable"] <-
          pair_name
        date_results_pairs[nrow(date_results_pairs), "Invalid Order"] <- sum(
          validated_data[[paste0(start_col, "_invalid_order")]],
          na.rm = TRUE
        )
        
        # Get IDs for invalid order
        ids_invalid_order <- validated_data |>
          dplyr::filter(!!rlang::sym(paste0(start_col, "_invalid_order"))) |>
          dplyr::distinct(!!rlang::sym(id_col)) |>
          dplyr::mutate(Test = glue::glue("{pair_name} has invalid order"))
        
        full_results$date_pair_issue_id <- dplyr::bind_rows(
          full_results$date_pair_issue_id, ids_invalid_order
        ) |>
          dplyr::mutate(`Column Type` = "Date")
      }
      
      # add date results pairs to results
      full_results$date_results_pairs <- date_results_pairs
    }
  }
  
  if (run_geo_hierarchy_check) {
    geo_hierarchy_results <- validate_admin_hierarchy(
      data,
      column_combos = list(geo_id_cols, geo_name_cols),
      id_columns = id_col
    )
    
    # add geo_hierarchy to results
    full_results$geo_hierarchy <- geo_hierarchy_results
  }
  
  # 4. Check geoname and ID -------------------------------------------
  
  if (run_geo_mismatch_check) {
    geo_mismatches <- join_and_check_mismatches(
      data, shapefile_data,
      geo_name_cols, geo_id_cols,
      shapefile_name_cols, shapefile_id_cols,
      id_col
    )
    
    # get ID and detAILS
    full_results$geo_mismatches_id <- geo_mismatches$detailed_mismatches |>
      dplyr::select(
        !!rlang::sym(id_col),
        Test = "Geo Column Type"
      ) |>
      dplyr::mutate(`Column Type` = "Geo")
    
    # add geo_mismatches to results
    full_results$geo_mismatches <- geo_mismatches$summary_table
  }
  
  # 5. Coordinate Checks -----------------------------------------------
  
  if (run_coordinate_checks) {
    coord_check_results <- check_coords(
      data = data,
      shapefile_data = shapefile_data,
      join_key_a = geo_id_cols[length(geo_id_cols)],
      join_key_b = shapefile_join_key,
      lat_col = lat_long_cols[1],
      lon_col = lat_long_cols[2],
      correct_lat_col = correct_lat_col,
      correct_lon_col = correct_lon_col,
      summary_table = TRUE,
      checks = coordinate_checks
    )
    coord_check_results$var1
    # add coordinate_checks to results
    full_results$coordinate_checks <- coord_check_results
    
    coord_check_results <- check_coords(
      data = data,
      shapefile_data = shapefile_data,
      join_key_a = geo_id_cols[length(geo_id_cols)],
      join_key_b = shapefile_join_key,
      lat_col = lat_long_cols[1],
      lon_col = lat_long_cols[2],
      correct_lat_col = correct_lat_col,
      correct_lon_col = correct_lon_col,
      summary_table = FALSE,
      checks = coordinate_checks
    )
    
    full_results$coord_issue_id <- dplyr::bind_rows(
      if ("parse" %in% coordinate_checks) {
        coord_check_results |>
          dplyr::filter(parse_failed == TRUE) |>
          dplyr::distinct(!!rlang::sym(id_col)) |>
          dplyr::mutate(Test = "parse_failed")
      },
      if ("on_water" %in% coordinate_checks) {
        coord_check_results |>
          dplyr::filter(on_water != "Land") |>
          dplyr::distinct(!!rlang::sym(id_col)) |>
          dplyr::mutate(Test = "on_water")
      },
      if ("flip" %in% coordinate_checks) {
        coord_check_results |>
          dplyr::filter(potentially_flipped == TRUE) |>
          dplyr::distinct(!!rlang::sym(id_col)) |>
          dplyr::mutate(Test = "potentially_flipped")
      },
      if ("null_coords" %in% coordinate_checks) {
        coord_check_results |>
          dplyr::filter(null_count == TRUE) |>
          dplyr::distinct(!!rlang::sym(id_col)) |>
          dplyr::mutate(Test = "null_count")
      },
      if ("out_of_bounds" %in% coordinate_checks) {
        coord_check_results |>
          dplyr::filter(out_of_bounds == TRUE) |>
          dplyr::distinct(!!rlang::sym(id_col)) |>
          dplyr::mutate(Test = "out_of_bounds")
      },
      if ("precision" %in% coordinate_checks) {
        coord_check_results |>
          dplyr::filter(low_precision == TRUE) |>
          dplyr::distinct(!!rlang::sym(id_col)) |>
          dplyr::mutate(Test = "low_precision")
      },
      if ("missing" %in% coordinate_checks) {
        coord_check_results |>
          dplyr::filter(missing_coords == TRUE) |>
          dplyr::distinct(!!rlang::sym(id_col)) |>
          dplyr::mutate(Test = "missing_coords")
      }
    ) |>
      dplyr::mutate(`Column Type` = "Coordinate")
  }
  
  if (run_detections) {
    # set up data identify detections
    data2 <- data |>
      dplyr::mutate(
        cVDPV1 = if ("VDPV1" %in% names(data)) {
          dplyr::if_else(VDPV1 == TRUE & VdpvClassifications == "Circulating",
                         TRUE, FALSE, missing = FALSE
          )
        },
        cVDPV2 = if ("VDPV2" %in% names(data)) {
          dplyr::if_else(VDPV2 == TRUE & VdpvClassifications == "Circulating",
                         TRUE, FALSE, missing = FALSE
          )
        },
        WPV1 = if ("WILD1" %in% names(data)) {
          dplyr::if_else(WILD1 == TRUE, TRUE, FALSE, missing = FALSE)
        } else {
          FALSE
        }
      )
    
    full_results$wpv1_count <- if ("WPV1" %in% names(data2)) {
      sum(data2$WPV1, na.rm = TRUE)
    } else {
      0
    }
    full_results$cvdpv1_count <- if ("cVDPV1" %in% names(data2)) {
      sum(data2$cVDPV1, na.rm = TRUE)
    } else {
      0
    }
    full_results$cvdpv2_count <- if ("cVDPV2" %in% names(data2)) {
      sum(data2$cVDPV2, na.rm = TRUE)
    } else {
      0
    }
  }
  
  full_results$total_columns <- ncol(data)
  full_results$total_rows <- nrow(data)
  full_results$duplicated_rows <- sum(duplicated(data[, id_col, drop = FALSE]))
  full_results$total_null_columns <- sum(
    full_results$missing_data$`Is Completely Null`,
    na.rm = T
  )
  
  return(full_results)
}

#' Create a Summary Table from Data Quality Check Results
#'
#' This function takes the results of various data quality checks and creates a
#' unified summary table. It processes different types of checks including
#' overall data summary, coordinate checks, geographical mismatches,
#' geographical hierarchy, date checks, and date pair checks.
#'
#' @param data A list containing the results of various data quality checks.
#'   The list may include the following components:
#'   - total_rows: Total number of rows in the dataset
#'   - total_columns: Total number of columns in the dataset
#'   - duplicated_rows: Number of duplicated rows
#'   - coordinate_checks: Results of coordinate validity checks
#'   - geo_mismatches: Results of geographical mismatches with shapefile
#'   - geo_hierarchy: Results of geographical hierarchy checks
#'   - date_results: Results of individual date column checks
#'   - date_results_pairs: Results of date pair checks
#'
#' @return A data frame with columns:
#'   - header: The category of the check (factor)
#'   - Column: The specific item being checked
#'   - count: The count or measure of the item (numeric)
#'
#' @details
#' The function processes each type of check separately and then combines them
#' into a single table. It also renames and recodes some of the column names
#' and values for better readability.
#'
#' @examples
#' # Assuming 'results' is the output from a data quality check function
#' # summary_table <- create_summary_table(results)
#' # print(summary_table)
#'
#' @export
create_summary_table <- function(data) {
  all_summaries <- list()
  
  # Main summary
  if (!is.null(data$total_rows) && !is.null(data$total_columns) &&
      !is.null(data$duplicated_rows)) {
    main_summary <- data.frame(
      Column = c(
        "Total Rows", "Total Columns", "Total Duplicate",
        "Total Null Columns"
      ),
      count = c(
        data$total_rows, data$total_columns, data$duplicated_rows,
        data$total_null_columns
      )
    ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::mutate(header = "Overall Data Summary") |>
      dplyr::filter(!Column %in% c("Total Columns", "Total Null Columns"))
    all_summaries$main_summary <- main_summary
  }
  
  # Detections summary
  if (!is.null(data$wpv1_count) && !is.null(data$cvdpv1_count) &&
      !is.null(data$cvdpv2_count)) {
    detections <- data.frame(
      Column = c("WPV1", "cVDPV1", "cVDPV2"),
      count = c(data$wpv1_count, data$cvdpv1_count, data$cvdpv2_count)
    ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::mutate(header = "Virus Detections")
    
    all_summaries$detections <- detections
  }
  
  # Coordinate checks
  if (!is.null(data$coordinate_checks)) {
    coordinate_checks <- data$coordinate_checks |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "Column",
        values_to = "count"
      ) |>
      dplyr::mutate(header = "Coordinate Checks") |>
      dplyr::filter(Column != "Total Coords")
    all_summaries$coordinate_checks <- coordinate_checks
  }
  
  # Geo missing checks
  if (!is.null(data$geo_missing_data)) {
    geo_missing_checks <- data$geo_missing_data |>
      dplyr::mutate(header = "Coordinate Checks") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::select(Column, count = "Missing Count") |>
      dplyr::mutate(header = "Geographical Names & ID Missing")
    
    all_summaries$geo_missing_checks <- geo_missing_checks
  }
  
  # Geographical mismatches
  if (!is.null(data$geo_mismatches)) {
    geo_mismatches <- data$geo_mismatches |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::mutate(header = "Geographical Mismatches") |>
      dplyr::rename(
        Column = "Geo Column Type",
        count = `Missing in Shapefile`
      )
    all_summaries$geo_mismatches <- geo_mismatches
  }
  
  # Geographical hierarchy
  if (!is.null(data$geo_hierarchy)) {
    geo_hierarchy <- data$geo_hierarchy |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::mutate(header = "Geographical Hierarchy") |>
      dplyr::select(
        Column = "Column Combination",
        count = `Non-unique Count`, header
      )
    all_summaries$geo_hierarchy <- geo_hierarchy
  }
  
  # Date results
  if (!is.null(data$date_results)) {
    date_results <- data$date_results |>
      dplyr::rename(Variable = Column) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(
        cols = -Variable,
        names_to = "Column",
        values_to = "count"
      ) |>
      dplyr::mutate(header = paste("Date Check:", Variable)) |>
      dplyr::select(Column, count, header)
    all_summaries$date_results <- date_results
  }
  
  # Date results pairs
  if (!is.null(data$date_results_pairs)) {
    date_results_pairs <- data$date_results_pairs |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(
        cols = -Variable,
        names_to = "Column",
        values_to = "count"
      ) |>
      dplyr::mutate(header = paste("Date Pair Check:", Variable)) |>
      dplyr::select(Column, count, header)
    all_summaries$date_results_pairs <- date_results_pairs
  }
  
  # Combine all summaries
  all_summaries <- dplyr::bind_rows(all_summaries)
  
  # Create a unified summary table
  summary_table <- all_summaries |>
    dplyr::mutate(header = factor(header,
                                  levels = unique(all_summaries$header)
    )) |>
    dplyr::select(header, Column, count) |>
    dplyr::mutate(count = as.numeric(
      stringr::str_replace_all(count, ",", "")
    )) |>
    dplyr::arrange(
      header,
      dplyr::desc(stringr::str_detect(Column, "Name")),
      Column
    ) |>
    dplyr::mutate(
      Column = dplyr::case_when(
        Column == "Total Rows" ~ "Total Rows",
        Column == "Total Columns" ~ "Total Columns",
        Column == "Total Duplicate" ~ "Total Duplicate Rows",
        Column == "Missing Coords" ~ "Missing Coordinates",
        Column == "Parse Failed" ~ "Coordinates Failed to Parse",
        Column == "Null Coords" ~
          "Null Coordinates (Latitude and Longitude are 0, 0)",
        Column == "Out of Bounds" ~
          "Coordinates Out of Bounds (-90 to 90, -180 to 180)",
        Column == "Low Precision" ~
          "Low Precision Coordinates (less than 5 decimal places)",
        Column == "Potentially Flipped" ~
          "Flipped Coordinates (Latitude and Longitude are switched)",
        Column == "On Water" ~
          "Coordinates On Water (Sea, Ocean, Lake or River)",
        header == "Geographical Names & ID Missing" & Column == "Admin0Name" ~
          "Admin0Name",
        header == "Geographical Names & ID Missing" & Column == "Admin1Name" ~
          "Admin1Name",
        header == "Geographical Names & ID Missing" & Column == "Admin2Name" ~
          "Admin2Name",
        header == "Geographical Names & ID Missing" & Column == "Admin0GUID" ~
          "Admin0GUID",
        header == "Geographical Names & ID Missing" & Column == "Admin1GUID" ~
          "Admin1GUID",
        header == "Geographical Names & ID Missing" & Column == "Admin2GUID" ~
          "Admin2GUID",
        header == "Geographical Mismatches" & Column == "Admin0Name" ~
          "Admin0Name not in Shapefile",
        header == "Geographical Mismatches" & Column == "Admin1Name" ~
          "Admin1Name not in Shapefile",
        header == "Geographical Mismatches" & Column == "Admin2Name" ~
          "Admin2Name not in Shapefile",
        header == "Geographical Mismatches" & Column == "Admin1GUID" ~
          "Admin1GUID not in Shapefile",
        header == "Geographical Mismatches" & Column == "Admin2GUID" ~
          "Admin2GUID not in Shapefile",
        header == "Geographical Hierarchy" &
          Column == "Admin0Name ~ Admin1Name ~ Admin2Name" ~
          "Non-unique name Hierarchy (Admin0Name ~ Admin1Name ~ Admin2Name)",
        header == "Geographical Hierarchy" &
          Column == "Admin0GUID ~ Admin1GUID ~ Admin2GUID" ~
          "Non-unique GUID Hierarchy (Admin0GUID ~ Admin1GUID ~ Admin2GUID)",
        Column == "Missing" ~ "Date is Missing",
        Column == "Non-date" ~
          "Non-date Value (Cannot be converted to date format)",
        Column == "Invalid" ~ "Invalid Date (Not starting with '20')",
        Column == "Future" ~ glue::glue(
          "Date into the future (beyond {lubridate::year(Sys.Date())})"
        ),
        Column == "Leap Issue" ~
          "Leap date (Feb 29th) appearing in Non-leap Year",
        Column == "Format Issue" ~
          "Date Format Issue (Not YYYY-MM-DD)",
        Column == "Invalid Order" ~
          "Invalid Date Order (First date is after second date)",
        TRUE ~ Column
      )
    )
  
  
  
  if (!is.null(data$date_issue_id) ||
      !is.null(data$geo_missing_id) ||
      !is.null(data$date_pair_issue_id) ||
      !is.null(data$geo_mismatches_id) ||
      !is.null(data$coord_issue_id)) {
    # resolve ID datasets
    id_data <- dplyr::bind_rows(
      data$date_issue_id,
      data$geo_missing_id,
      data$date_issue_id,
      data$date_pair_issue_id,
      data$geo_mismatches_id,
      data$coord_issue_id
    ) |>
      dplyr::mutate(
        Test = dplyr::case_when(
          Test == "parse_failed" ~ "Coordinates Failed to Parse",
          Test == "missing_coords" ~ "Missing Coordinate Values",
          Test == "null_count" ~
            "Null Coordinates (Latitude And Longitude Are 0, 0)",
          Test == "out_of_bounds" ~
            "Coordinates Out Of Bounds (Not Within -90 To 90, -180 To 180)",
          Test == "low_precision" ~
            "Low Precision Coordinates (Less Than 5 Decimal Places)",
          Test == "potentially_flipped" ~
            "Potentially Flipped Coordinates (Latitude And Longitude Switched)",
          Test == "on_water" ~
            "Coordinates On Water Bodies (Sea, Ocean, Lake Or River)",
          TRUE ~ Test
        )
      ) |>
      dplyr::mutate(
        Test = stringr::str_replace(
          Test, "future",
          glue::glue("Future Dates (Beyond {lubridate::year(Sys.Date())})")
        ),
        Test = stringr::str_replace(
          Test, "has invalid order",
          "Invalid Date Order (First Date Is After Second Date)"
        ),
        Test = stringr::str_replace(
          Test, "is invalid",
          "Invalid Date (Not Starting With '20')"
        ),
        Test = stringr::str_replace(
          Test, "is missing",
          "Missing Value"
        )
      ) |>
      dplyr::select(1, 3, 2)
  }
  
  return(
    list(
      summary_table = summary_table,
      id_data = id_data
    )
  )
}

#' Create Summary by Group
#'
#' This function creates a summary of data quality checks grouped by a specified
#' variable.
#'
#' @param data A data frame containing the dataset to be summarized.
#' @param group_var The name of the column to group by.
#' @param id_col The name of the column containing unique identifiers.
#' @param geo_name_cols A vector of column names containing geographic names.
#' @param geo_id_cols A vector of column names containing geographic IDs.
#' @param lat_long_cols A vector of column names for latitude and longitude.
#' @param date_cols A vector of column names containing dates.
#' @param date_pair_cols A list of date column pairs to be checked together.
#' @param shapefile_data Optional data frame containing shapefile data for
#'   geographic comparisons.
#' @param shapefile_join_key Optional character string specifying the join key
#'   for the shapefile data.
#' @param correct_lat_col Optional character string specifying the column name
#'   for correct latitude in shapefile data.
#' @param correct_lon_col Optional character string specifying the column name
#'   for correct longitude in shapefile data.
#' @param shapefile_name_cols Optional character vector of column names in
#'   shapefile data containing geographic names.
#' @param shapefile_id_cols Optional character vector of column names in
#'   shapefile data containing geographic IDs.
#' @param run_missing_check Logical; if TRUE, runs missing data check (default
#'   is TRUE).
#' @param run_date_check Logical; if TRUE, runs date validation check (default
#'   is TRUE).
#' @param run_geo_hierarchy_check Logical; if TRUE, runs geographic hierarchy
#'   check (default is TRUE).
#' @param run_geo_mismatch_check Logical; if TRUE, runs geographic mismatch
#'   check (default is TRUE).
#' @param run_coordinate_checks Logical; if TRUE, runs coordinate validation
#'   checks (default is TRUE).
#' @param run_detections Logical, whether to run virus detections checks.
#' @param coordinate_checks Character vector specifying which coordinate checks
#'   to run.
#' @param n_groups The number of groups to include in the summary (def is 4).
#' @param decreasing Logical; if TRUE, sorts groups in decreasing order (default
#'   is TRUE).
#'
#' @return A data frame containing the combined summary of data quality checks
#'   for each group.
#'
#' @details
#' This function performs the following steps:
#' 1. Selects unique groups based on the specified group variable.
#' 2. For each group, it runs data quality checks using `check_data`.
#' 3. Creates a summary table for each group using the `create_summary_table`
#'    function.
#' 4. Combines the results into a single data frame, with groups as columns.
#'
#' @examples
#' # summary <- create_summary_by_group(
#' #   data = my_data,
#' #   group_var = "Region",
#' #   id_col = "ID",
#' #   geo_name_cols = c("Country", "State", "City"),
#' #   geo_id_cols = c("CountryID", "StateID", "CityID"),
#' #   lat_long_cols = c("Latitude", "Longitude"),
#' #   date_cols = c("Date1", "Date2"),
#' #   date_pair_cols = list(c("Date1", "Date2")),
#' #   n_groups = 3,
#' #   decreasing = TRUE
#' # )
#'
#' @export
create_summary_by_group <- function(data, group_var, id_col, geo_name_cols,
                                    geo_id_cols, lat_long_cols, date_cols,
                                    date_pair_cols, shapefile_data = NULL,
                                    shapefile_join_key = NULL,
                                    correct_lat_col = NULL,
                                    correct_lon_col = NULL,
                                    shapefile_name_cols = NULL,
                                    shapefile_id_cols = NULL,
                                    run_missing_check = TRUE,
                                    run_date_check = TRUE,
                                    run_geo_hierarchy_check = TRUE,
                                    run_geo_mismatch_check = TRUE,
                                    run_coordinate_checks = TRUE,
                                    run_detections = FALSE,
                                    coordinate_checks = c(
                                      "flip", "on_water",
                                      "missing",
                                      "out_of_bounds",
                                      "precision",
                                      "null_coords", "parse"
                                    ),
                                    n_groups = 4, decreasing = FALSE) {
  # Get unique groups and sort them
  unique_groups <- unique(data[[group_var]]) |>
    sort(decreasing = decreasing) |>
    utils::tail(n_groups)
  
  res_list <- list()
  
  for (group in unique_groups) {
    group_data <- dplyr::filter(data, .data[[group_var]] == group)
    
    res_list[[as.character(group)]] <- check_data(
      data = group_data,
      id_col = id_col,
      geo_name_cols = geo_name_cols,
      geo_id_cols = geo_id_cols,
      lat_long_cols = lat_long_cols,
      date_cols = date_cols,
      date_pair_cols = date_pair_cols,
      shapefile_data = shapefile_data,
      shapefile_join_key = shapefile_join_key,
      correct_lat_col = correct_lat_col,
      correct_lon_col = correct_lon_col,
      shapefile_name_cols = shapefile_name_cols,
      shapefile_id_cols = shapefile_id_cols,
      run_missing_check = run_missing_check,
      run_date_check = run_date_check,
      run_geo_hierarchy_check = run_geo_hierarchy_check,
      run_geo_mismatch_check = run_geo_mismatch_check,
      run_coordinate_checks = run_coordinate_checks,
      coordinate_checks = coordinate_checks,
      run_detections = run_detections
    ) |>
      create_summary_table()
  }
  
  
  # Process summary table
  summary_table <- dplyr::bind_rows(
    lapply(res_list, `[[`, "summary_table"),
    .id = group_var
  ) |>
    tidyr::pivot_wider(
      id_cols = c(header, Column),
      names_from = all_of(group_var),
      values_from = count
    ) |>
    dplyr::arrange(header) |>
    dplyr::group_by(header) |>
    dplyr::mutate(
      header = ifelse(
        dplyr::row_number() > 1, "", as.character(header)
      )
    ) |>
    dplyr::ungroup()
  
  # Process id data
  id_data <- dplyr::bind_rows(
    lapply(res_list, `[[`, "id_data"),
    .id = group_var
  )
  
  return(list(summary_table = summary_table, id_data = id_data))
}

#' Create a GT Table for Data Quality Summary
#'
#' This function creates a formatted GT table from a summary of data quality
#' checks.
#'
#' @param summary_data A data frame containing the summary of data quality
#'   checks.
#' @param title A string for the table title. Default is "Summary of POLIS Data
#'   Quality Checks".
#' @param add_nanoplot A logical indicating whether to add a nanoplot column.
#'   Default is TRUE.
#' @param row_start An integer indicating the starting row for data color
#'   formatting. Default is 6.
#' @param autoscale_nanoplot Logical. Whether to autoscale the nanoplot.
#'    Default is FALSE.
#'
#' @return A GT table object with formatted data quality summary.
#'
#' @details
#' The function applies various formatting and styling to the summary data:
#' - Applies color scaling to numeric columns based on their values.
#' - Sets custom labels for columns.
#' - Adds borders to specific rows.
#' - Formats numbers to remove decimal places.
#' - Optionally adds a nanoplot column for visualizing trends.
#'
#' @examples
#' #  summary_data <- create_summary_by_group(my_data, "Region", "ID", ...)
#' # gt_table <- create_gt_table(summary_data,
#' # title = "My Data Quality Summary",
#' #  add_nanoplot = TRUE,
#' #  row_start = 6
#' # )
#'
#' @export
create_gt_table <- function(summary_data,
                            title = "Summary of POLIS Data Quality Checks",
                            add_nanoplot = TRUE, row_start = 6,
                            autoscale_nanoplot = FALSE) {
  # Conditional loading for packages
  required_packages <- c("scales", "gt", "glue")
  
  missing_packages <- required_packages[!sapply(
    required_packages, requireNamespace,
    quietly = TRUE
  )]
  
  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "Package(s) ", paste(missing_packages, collapse = ", "),
        " required but not installed. Please install them."
      ),
      call. = FALSE
    )
  }
  
  gt_table <- gt::gt(summary_data) |>
    gt::data_color(
      columns = -c(Column, header),
      rows = row_start:nrow(summary_data),
      fn = function(x) {
        # Handle potential negative values or NAs for this row
        x_clean <- pmax(x, 0, na.rm = TRUE)
        row_max <- max(x_clean, na.rm = TRUE) * 1.3
        
        if (row_max == 0) {
          return(rep("white", length(x)))
        }
        
        scales::col_numeric(
          palette = c("#FFFFFF", "#FF9999", "#FF0000"),
          domain = c(0, row_max)
        )(x_clean)
      }
    ) |>
    gt::tab_header(title = title) |>
    gt::cols_label(Column = "Validation Test") |>
    gt::cols_label(header = "Grouping") |>
    gt::cols_align(align = "left", columns = 2) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        color = "black",
        weight = gt::px(3)
      ),
      locations = gt::cells_body(
        rows = summary_data$Column %in% c(
          "Missing Values",
          "Data Types",
          "Date Validations",
          "Geographic Validations"
        )
      )
    ) |>
    gt::fmt_number(
      columns = dplyr::where(is.numeric),
      decimals = 0
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(everything())
    )
  
  if (add_nanoplot) {
    gt_table <- gt_table |> gt::cols_nanoplot(
      columns = -c(1, 2),
      new_col_name = "nanoplot",
      new_col_label = gt::md("**Quality Trend**"),
      autohide = FALSE, autoscale = autoscale_nanoplot,
      options = gt::nanoplot_options(
        data_bar_stroke_width = 10,
        vertical_guide_stroke_width = 51
      )
    )
  }
  
  return(gt_table)
}

#' Validate POLIS Data
#'
#' This function performs data quality checks on POLIS (Polio Information
#' System) data.
#'
#' @param data A dataframe containing the POLIS data to be validated.
#' @param type Character string specifying the data type. Either "AFP" or "ES".
#' @param group_var Optional. The column name to group the data by. If NULL,
#'    uses default.
#' @param id_col Optional. The column name for the unique identifier. If NULL,
#'    uses default.
#' @param geo_name_cols Optional. A vector of column names for geographic names.
#'    If NULL, uses default.
#' @param geo_id_cols Optional. A vector of column names for geographic IDs.
#'    If NULL, uses default.
#' @param lat_long_cols Optional. A vector of column names for latitude and
#'    longitude. If NULL, uses default.
#' @param date_cols Optional. A vector of column names for date fields. If NULL,
#'    uses default.
#' @param date_pair_cols Optional. A list of date column pairs for comparison.
#'    If NULL, uses default.
#' @param n_groups Integer. The number of groups to display in the summary.
#'    Default is 8.
#' @param decreasing Logical. Whether to sort the groups in decreasing order.
#'    Default is FALSE.
#' @param plots_path Optional. The file path to save output plots. Required if
#'    save_output is TRUE.
#' @param polis_version Character string. The version of POLIS being used.
#'    Default is "2.37.1".
#' @param custom_title Optional. A custom title for the output table.
#' @param save_output Logical. Whether to save the output as HTML and PNG.
#'    Default is FALSE.
#' @param vheight Integer. The height of the output image in pixels.
#'    Default is 1400.
#' @param vwidth Integer. The width of the output image in pixels. Default
#'    is 1550.
#' @param autoscale_nanoplot Logical. Whether to autoscale the nanoplot.
#'    Default is FALSE.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A list containing two elements:
#'   \item{gt_table}{A gt table object with the validation summary}
#'   \item{id_data}{A dataframe with detailed information for each unique
#'    identifier}
#'
#' @details
#' This function performs various data quality checks on POLIS data, including
#'  checks for missing values, geographic data consistency, date field validity,
#'  and more. It allows for customization of column names and grouping
#'  variables, making it flexible for different data structures within the
#'  POLIS system.
#'
#' The function will use default parameters based on the specified data type
#' (AFP or ES) if custom parameters are not provided. It also checks if all
#' specified columns exist in the dataset before proceeding with the analysis.
#'
#' @examples
#' # Assuming polis_data is your dataset and you have the necessary dependencies
#' # result <- validate_polis(polis_data, type = "AFP",
#' #                        group_var = "ReportingYear",
#' #                        n_groups = 8,
#' #                        decreasing = FALSE,
#' #                        plots_path = "/path/to/save/plots",
#' #                        polis_version = "2.37.1",
#' #                        save_output = TRUE,
#' #                        vheight = 1500, vwidth = 1600)
#'
#' # Access the GT table and ID data
#' # gt_table <- result$gt_table
#' # id_data <- result$id_data
#'
#' @export
validate_polis <- function(data, type = "AFP",
                           group_var = NULL,
                           id_col = NULL,
                           geo_name_cols = NULL,
                           geo_id_cols = NULL,
                           lat_long_cols = NULL,
                           date_cols = NULL,
                           date_pair_cols = NULL,
                           n_groups = 8,
                           decreasing = FALSE,
                           plots_path = NULL,
                           polis_version = "2.37.1",
                           autoscale_nanoplot = FALSE,
                           custom_title = NULL, save_output = FALSE,
                           vheight = 1400, vwidth = 1550, ...) {
  # Conditional loading for packages
  required_packages <- c("scales", "gt", "glue", "webshot")
  
  missing_packages <- required_packages[!sapply(
    required_packages, requireNamespace,
    quietly = TRUE
  )]
  
  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "Package(s) ", paste(missing_packages, collapse = ", "),
        " required but not installed. Please install them."
      ),
      call. = FALSE
    )
  }
  
  # Define parameters based on data type
  default_params <- list(
    AFP = list(
      group_var = group_var,
      id_col = "EPID",
      geo_name_cols = c("Admin0Name", "Admin1Name", "Admin2Name"),
      geo_id_cols = c("Admin0GUID", "Admin1GUID", "Admin2GUID"),
      lat_long_cols = c("Latitude", "Longitude"),
      date_cols = c(
        "CaseDate", "InvestigationDate",
        "NotificationDate", "Stool1CollectionDate",
        "Stool2CollectionDate"
      ),
      date_pair_cols = list(
        c("NotificationDate", "InvestigationDate"),
        c("NotificationDate", "Stool1CollectionDate"),
        c("Stool1CollectionDate", "Stool2CollectionDate")
      )
    ),
    ES = list(
      group_var = group_var,
      id_col = "SampleId",
      geo_name_cols = c("Admin0Name", "Admin1Name", "Admin2Name"),
      geo_id_cols = c("Admin0GUID", "Admin1GUID", "Admin2GUID"),
      lat_long_cols = c("SiteYCoordinate", "SiteXCoordinate"),
      date_cols = c(
        "CollectionDate", "DateShippedToRefLab", "DateReceivedInLab",
        "DateFinalCultureResult", "DateFinalResultsReported"
      ),
      date_pair_cols = list(
        c("CollectionDate", "DateShippedToRefLab"),
        c("CollectionDate", "DateReceivedInLab"),
        c("CollectionDate", "DateFinalResultsReported")
      )
    )
  )
  
  if (!type %in% names(default_params)) {
    stop(
      "Invalid data type. Supported types are: ",
      paste(names(default_params), collapse = ", ")
    )
  }
  
  # Use provided parameters if not NULL, otherwise use defaults
  params <- list(
    group_var = if (!is.null(group_var)) {
      group_var
    } else {
      default_params[[type]]$group_var
    },
    id_col = if (!is.null(id_col)) {
      id_col
    } else {
      default_params[[type]]$id_col
    },
    geo_name_cols = if (!is.null(geo_name_cols)) {
      geo_name_cols
    } else {
      default_params[[type]]$geo_name_cols
    },
    geo_id_cols = if (!is.null(geo_id_cols)) {
      geo_id_cols
    } else {
      default_params[[type]]$geo_id_cols
    },
    lat_long_cols = if (!is.null(lat_long_cols)) {
      lat_long_cols
    } else {
      default_params[[type]]$lat_long_cols
    },
    date_cols = if (!is.null(date_cols)) {
      date_cols
    } else {
      default_params[[type]]$date_cols
    },
    date_pair_cols = if (!is.null(date_pair_cols)) {
      date_pair_cols
    } else {
      default_params[[type]]$date_pair_cols
    }
  )
  
  # Check if all specified columns exist in the dataset
  all_cols <- c(
    params$group_var, params$id_col, params$geo_name_cols,
    params$geo_id_cols, params$lat_long_cols, params$date_cols,
    unlist(params$date_pair_cols)
  )
  missing_cols <- setdiff(all_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(paste(
      "The following columns are not present in the dataset:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  
  summary <- create_summary_by_group(
    data = data,
    group_var = params$group_var,
    id_col = params$id_col,
    geo_name_cols = params$geo_name_cols,
    geo_id_cols = params$geo_id_cols,
    lat_long_cols = params$lat_long_cols,
    date_cols = params$date_cols,
    date_pair_cols = params$date_pair_cols,
    n_groups = n_groups,
    decreasing = decreasing,
    ...
  )
  
  title <- if (is.null(custom_title)) {
    glue::glue(
      "POLIS {type} Data Quality Checks ",
      "(POLIS Version: {polis_version})"
    )
  } else {
    custom_title
  }
  
  gt_table <- create_gt_table(
    summary$summary_table |> dplyr::filter(
      Column != "Total Null Columns"
    ),
    title = title,
    autoscale_nanoplot = autoscale_nanoplot
  )
  
  if (save_output) {
    if (is.null(plots_path)) {
      stop("plots_path must be provided when save_output is TRUE")
    }
    
    today <- format(Sys.Date(), "%Y%m%d")
    file_prefix <- glue::glue("polis_{type}_validation_{today}_{polis_version}")
    
    html_path <- file.path(plots_path, glue::glue("{file_prefix}.html"))
    png_path <- file.path(plots_path, glue::glue("{file_prefix}.png"))
    
    gt_table |> gt::gtsave(html_path)
    
    webshot::webshot(html_path, png_path, vheight = vheight, vwidth = vwidth)
    
    file.remove(html_path)
  }
  
  return(list(gt_table = gt_table, id_data = summary$id_data))
}

#' Validate POLIS Data Snapshots
#'
#' This function validates POLIS data snapshots for a specific year and data
#' type.
#'
#' @param path Character. Directory path containing POLIS snapshot files.
#' @param data_type Character. Type of data ("AFP" or "ES"). Default is "AFP".
#' @param plots_path Character. Directory to save output plots.
#' @param custom_title Character. Custom title for the output. Default is NULL.
#' @param data_type_string String of data list name
#' @param year_of_interest Numeric. Year of interest for validation. Default is
#'    2024.
#' @param pattern Character. Pattern to match snapshot files. Default is NULL.
#' @param snapshots Numeric. Number of snapshots to validate. Default is 6.
#' @param vheight Integer. The height of the output image in pixels.
#'    Default is 1400.  Corresponds to the default 6 snapshots.
#' @param vwidth Integer. The width of the output image in pixels. Default
#'    is 1760. Corresponds to the default 6 snapshots.
#' @param ... Additional arguments passed to validate_polis function.
#'
#' @return A list containing validation results and GT table.
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter mutate
#' @importFrom lubridate year
#' @importFrom rlang sym
#'
#' @export
validate_polis_snapshots <- function(path, data_type = "AFP",
                                     year_of_interest = 2024,
                                     pattern = NULL,
                                     data_type_string = NULL,
                                     snapshots = 6,
                                     plots_path,
                                     custom_title = NULL,
                                     vheight = 1400,
                                     vwidth = 1760,
                                     ...) {
  default_title <- sprintf(
    "POLIS %s Data Quality Checks For Data Snapshots (%d-%d)",
    data_type, year_of_interest - 1, year_of_interest
  )
  
  if (is.null(pattern)) {
    pattern <- paste0("^polis_raw_data_", year_of_interest, "_")
  }
  
  files <- list.files(
    path,
    pattern = pattern,
    full.names = TRUE
  )
  
  weeks <- sort(
    as.numeric(gsub(".*_([0-9]+)\\.rds$", "\\1", files)),
    decreasing = TRUE
  )[1:snapshots]
  
  date_col <- if (data_type == "AFP") "CaseDate" else "CollectionDate"
  
  if (is.null(data_type_string)) {
    data_type_string <- if (data_type == "AFP") "human" else "env"
  }
  
  df <- purrr::map_dfr(weeks, function(week) {
    file_path <- file.path(
      path,
      sprintf("polis_raw_data_%d_%d.rds", year_of_interest, week)
    )
    
    poliprep::read(file_path)[[data_type_string]] |>
      dplyr::filter(
        lubridate::year(!!rlang::sym(date_col)) >= year_of_interest - 1
      ) |>
      dplyr::mutate(snapshot = sprintf("%d W%d", year_of_interest, week))
  }) |>
    dplyr::mutate(
      snapshot = factor(
        snapshot,
        levels = sprintf("%d W%d", year_of_interest, rev(weeks))
      )
    )
  
  results <- poliprep::validate_polis(
    data = df,
    group_var = "snapshot",
    type = data_type,
    n_groups = Inf,
    plots_path = plots_path,
    custom_title = if (is.null(custom_title)) default_title else custom_title,
    save_output = TRUE,
    vheight = vheight,
    vwidth = vwidth,
    ...
  )
  
  today <- format(Sys.Date(), "%Y%m%d")
  save_path <- file.path(
    plots_path,
    paste0(
      "polis_",
      tolower(data_type), "_snapshots_", year_of_interest, "_", today, ".RData"
    )
  )
  
  base::save(results, file = save_path)
  
  return(results)
  }

#' Validate AFRO Data
#'
#' This function performs data quality checks on AFRO (African Regional Office)
#' data before it is sent to POLIS (Polio Information System).
#'
#' @param data A dataframe containing the AFRO data to be validated.
#' @param type Character string specifying the data type. Either "AFP" or "ES".
#' @param group_var Optional. The column name to group the data by. If NULL,
#'    uses default.
#' @param id_col Optional. The column name for the unique identifier. If NULL,
#'    uses default.
#' @param geo_name_cols Optional. A vector of column names for geographic names.
#'    If NULL, uses default.
#' @param geo_id_cols Optional. A vector of column names for geographic IDs.
#'    If NULL, uses default.
#' @param lat_long_cols Optional. A vector of column names for latitude and
#'    longitude. If NULL, uses default.
#' @param date_cols Optional. A vector of column names for date fields. If NULL,
#'    uses default.
#' @param date_pair_cols Optional. A list of date column pairs for comparison.
#'    If NULL, uses default.
#' @param n_groups Integer. The number of groups to display in the summary.
#'    Default is 8.
#' @param decreasing Logical. Whether to sort the groups in decreasing order.
#'    Default is FALSE.
#' @param plots_path Optional. The file path to save output plots. Required if
#'    save_output is TRUE.
#' @param custom_title Optional. A custom title for the output table.
#' @param save_output Logical. Whether to save the output as HTML and PNG.
#'    Default is FALSE.
#' @param vheight Integer. The height of the output image in pixels.
#'    Default is 1400.
#' @param vwidth Integer. The width of the output image in pixels. Default
#'    is 1550.
#' @param autoscale_nanoplot Logical. Whether to autoscale the nanoplot.
#'    Default is FALSE.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A list containing two elements:
#'   \item{gt_table}{A gt table object with the validation summary}
#'   \item{id_data}{A dataframe with detailed information for each unique
#'    identifier}
#'
#' @details
#' This function performs various data quality checks on AFRO data before it is
#' sent to POLIS, including checks for missing values, geographic data
#' consistency, date field validity, and more. It allows for customization of
#' column names and grouping variables, making it flexible for different data
#' structures within the AFRO system.
#'
#' The function will use default parameters based on the specified data type
#' (AFP or ES) if custom parameters are not provided. It also checks if all
#' specified columns exist in the dataset before proceeding with the analysis.
#'
#' @examples
#' # Assuming afro_data is your dataset and you have the necessary dependencies
#' # result <- validate_afro(afro_data, type = "AFP",
#' #                        group_var = "ReportingYear",
#' #                        n_groups = 8,
#' #                        decreasing = FALSE,
#' #                        plots_path = "/path/to/save/plots",
#' #                        save_output = TRUE,
#' #                        vheight = 1500, vwidth = 1600)
#'
#' # Access the GT table and ID data
#' # gt_table <- result$gt_table
#' # id_data <- result$id_data
#'
#' @export
validate_afro <- function(data, type = "AFP",
                          group_var = NULL,
                          id_col = NULL,
                          geo_name_cols = NULL,
                          geo_id_cols = NULL,
                          lat_long_cols = NULL,
                          date_cols = NULL,
                          date_pair_cols = NULL,
                          n_groups = 8,
                          decreasing = FALSE,
                          plots_path = NULL,
                          custom_title = NULL, save_output = FALSE,
                          vheight = 1400, vwidth = 1550,
                          autoscale_nanoplot = FALSE, ...) {
  # Conditional loading for packages
  required_packages <- c(
    "scales", "zoo", "gt", "glue", "webshot"
  )
  
  missing_packages <- required_packages[!sapply(
    required_packages, requireNamespace,
    quietly = TRUE
  )]
  
  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "Package(s) ", paste(missing_packages, collapse = ", "),
        " required but not installed. Please install them."
      ),
      call. = FALSE
    )
  }
  
  # Define parameters based on data type
  default_params <- list(
    AFP = list(
      group_var = group_var,
      id_col = "EpidNumber",
      geo_name_cols = c("ctry", "Province", "District"),
      geo_id_cols = NULL,
      date_cols = c(
        "DateReceived", "DateOfOnset",
        "DateNotified", "DateCaseinvestigated",
        "Date1stStool", "Date2ndStool",
        "DateStoolSentolab", "DateSpecRecbyNatLab",
        "DateFinalCellcultureResults"
      ),
      date_pair_cols = list(
        c("DateOfOnset", "DateNotified"),
        c("DateNotified", "DateCaseinvestigated"),
        c("DateOfOnset", "DateCaseinvestigated"),
        c("DateOfOnset", "Date1stStool"),
        c("DateOfOnset", "Date2ndStool"),
        c("Date1stStool", "Date2ndStool"),
        c("DateSpecRecbyNatLab", "DateStoolSentolab"),
        c("DateSpecRecbyNatLab", "DateOfOnset"),
        c("DateFinalCellcultureResults", "DateOfOnset"),
        c("DateSpecRecbyNatLab", "DateCaseinvestigated"),
        c("DateFinalCellcultureResults", "DateCaseinvestigated")
      ),
      lat_long_cols = c("Latitude", "Longitude")
    )
  )
  
  if (!type %in% names(default_params)) {
    stop(
      "Invalid data type. Supported types are: ",
      paste(names(default_params), collapse = ", ")
    )
  }
  
  # Use provided parameters if not NULL, otherwise use defaults
  params <- list(
    group_var = if (!is.null(group_var)) {
      group_var
    } else {
      default_params[[type]]$group_var
    },
    id_col = if (!is.null(id_col)) {
      id_col
    } else {
      default_params[[type]]$id_col
    },
    geo_name_cols = if (!is.null(geo_name_cols)) {
      geo_name_cols
    } else {
      default_params[[type]]$geo_name_cols
    },
    geo_id_cols = if (!is.null(geo_id_cols)) {
      geo_id_cols
    } else {
      default_params[[type]]$geo_id_cols
    },
    lat_long_cols = if (!is.null(lat_long_cols)) {
      lat_long_cols
    } else {
      default_params[[type]]$lat_long_cols
    },
    date_cols = if (!is.null(date_cols)) {
      date_cols
    } else {
      default_params[[type]]$date_cols
    },
    date_pair_cols = if (!is.null(date_pair_cols)) {
      date_pair_cols
    } else {
      default_params[[type]]$date_pair_cols
    }
  )
  
  # Check if all specified columns exist in the dataset
  all_cols <- c(
    params$group_var, params$id_col, params$geo_name_cols,
    params$geo_id_cols, params$lat_long_cols, params$date_cols,
    unlist(params$date_pair_cols)
  )
  missing_cols <- setdiff(all_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(paste(
      "The following columns are not present in the dataset:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  
  summary <- create_summary_by_group(
    data = data,
    group_var = params$group_var,
    id_col = params$id_col,
    geo_name_cols = params$geo_name_cols,
    geo_id_cols = params$geo_id_cols,
    lat_long_cols = params$lat_long_cols,
    date_cols = params$date_cols,
    date_pair_cols = params$date_pair_cols,
    n_groups = n_groups,
    decreasing = decreasing,
    ...
  )
  
  # set up time lab
  time_labs <- paste0(
    "For ", zoo::as.yearmon(Sys.Date()),
    " in Epiweek ",
    lubridate::epiweek(Sys.Date())
  )
  
  title <- if (is.null(custom_title)) {
    glue::glue(
      "AFRO {type} Data Quality Checks ",
      "{time_labs}"
    )
  } else {
    custom_title
  }
  
  gt_table <- create_gt_table(
    summary$summary_table |> dplyr::filter(
      Column != "Total Null Columns"
    ),
    autoscale_nanoplot = autoscale_nanoplot,
    title = title
  )
  
  if (save_output) {
    if (is.null(plots_path)) {
      stop("plots_path must be provided when save_output is TRUE")
    }
    
    today <- format(Sys.Date(), "%Y%m%d")
    file_prefix <- glue::glue(
      "afro_quality_check_{type}_validation_{today}}"
    )
    
    html_path <- file.path(plots_path, glue::glue("{file_prefix}.html"))
    png_path <- file.path(plots_path, glue::glue("{file_prefix}.png"))
    
    gt_table |> gt::gtsave(html_path)
    
    webshot::webshot(html_path, png_path, vheight = vheight, vwidth = vwidth)
    
    file.remove(html_path)
  }
  
  return(list(gt_table = gt_table, id_data = summary$id_data))
}

#' Summarize Validation Results
#'
#' Summarizes validation results and optionally creates a plot.
#'
#' @param data Data frame containing validation results.
#' @param metadata Data frame with test metadata.
#' @param id_col Character. Column name for unique identifiers.
#' @param test_type_name Character. Name of test type to summarize.
#' @param agg_vars Character vector. Column(s) to aggregate results by.
#' @param create_plot Logical. Whether to create a summary plot (default FALSE).
#' @param return_cols Character vector. Additional columns to return for culprit
#'    identification (default NULL).
#'
#' @return List with:
#'   \item{summary}{Data frame of summarized results, grouped by agg_vars.}
#'   \item{plot}{ggplot object if create_plot is TRUE, else NULL.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- summarize_validation_results(
#'   data, metadata, "ID", "missing coord",
#'   c("Region", "Year"),
#'   create_plot = TRUE
#' )
#' print(result$summary)
#' if (!is.null(result$plot)) print(result$plot)
#' }
summarize_validation_results <- function(data,
                                         metadata, id_col,
                                         test_type_name, agg_vars,
                                         create_plot = FALSE,
                                         return_cols = NULL) {
  # Validate inputs
  if (!id_col %in% names(data)) {
    stop("id_col not found in the dataset")
  }
  
  if (length(agg_vars) == 0) {
    stop("At least one aggregation variable must be provided")
  }
  for (var in agg_vars) {
    if (!var %in% names(data)) {
      stop(paste("Aggregation variable", var, "not found in the dataset"))
    }
  }
  if (!test_type_name %in% metadata$Test) {
    stop("test_type not found in metadata")
  }
  
  # Get relevant IDs for the specified test type
  ids <- metadata |>
    dplyr::filter(Test %in% test_type_name) |>
    dplyr::pull(!!rlang::sym(id_col))
  
  # Summarize test results
  summary <- data |>
    dplyr::filter(!!rlang::sym(id_col) %in% ids) |>
    dplyr::select(!!!rlang::syms(c(agg_vars, id_col))) |>
    dplyr::group_by(!!!rlang::syms(agg_vars)) |>
    dplyr::summarise(Total = dplyr::n_distinct(!!rlang::sym(id_col))) |>
    dplyr::arrange(dplyr::desc(Total)) |>
    dplyr::ungroup()
  
  # Get culprit columns if return_cols is provided
  culprits <- NULL
  if (!is.null(return_cols)) {
    culprits <- data |>
      dplyr::filter(!!rlang::sym(id_col) %in% ids) |>
      dplyr::select(!!rlang::sym(id_col), !!!rlang::syms(return_cols))
  }
  
  plot <- NULL
  if (create_plot) {
    if (length(agg_vars) == 1) {
      plot <- summary |>
        dplyr::arrange(dplyr::desc(Total)) |>
        ggplot2::ggplot(ggplot2::aes(
          x = stats::reorder(!!rlang::sym(agg_vars), Total),
          y = Total,
          fill = !!rlang::sym(agg_vars)
        )) +
        ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Summary of", test_type_name),
          x = NULL,
          y = "Total"
        ) +
        ggplot2::scale_y_continuous(labels = scales::comma)
    } else if (length(agg_vars) == 2) {
      plot <- summary |>
        dplyr::group_by(!!rlang::sym(agg_vars[2])) |>
        dplyr::arrange(dplyr::desc(Total), .by_group = TRUE) |>
        dplyr::ungroup() |>
        ggplot2::ggplot(ggplot2::aes(
          x = stats::reorder(!!rlang::sym(agg_vars[1]), Total),
          y = Total,
          fill = !!rlang::sym(agg_vars[1])
        )) +
        ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +
        ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(agg_vars[2])),
          scales = "free_y"
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Summary of", test_type_name),
          x = NULL,
          y = "Total"
        ) +
        ggplot2::scale_y_continuous(labels = scales::comma)
    } else {
      warning("Plot can only be created for one or two aggregation variables.")
    }
  }
  
  list(summary = summary, plot = plot, culprits = culprits)
}
