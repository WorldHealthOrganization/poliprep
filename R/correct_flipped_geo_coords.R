#' Correct Flipped Geographical Coordinates
#'
#' This function corrects potentially flipped latitude and longitude coordinates
#' by comparing them with a reference shapefile.
#'
#' @param data A dataframe containing the geographical coordinates to be 
#'        corrected.
#' @param shapefile_data Optional. A dataframe containing reference shapefile 
#'        data. If NULL, `poliprep::shp_global` will be used.
#' @param join_key_a The column name in `data` to join with `shapefile_data`.
#' @param join_key_b Optional. The column name in `shapefile_data` to join with 
#'        `data`.
#'   If NULL and `shapefile_data` is NULL, defaults to "ADM2_GUID".
#' @param lat_col The name of the latitude column in `data`.
#' @param lon_col The name of the longitude column in `data`.
#' @param correct_lat_col Optional. The name of the correct latitude column in 
#'      `shapefile_data`.
#'   If NULL and `shapefile_data` is NULL, defaults to "CENTER_LAT".
#' @param correct_lon_col Optional. The name of the correct longitude column 
#'        in `shapefile_data`.
#'   If NULL and `shapefile_data` is NULL, defaults to "CENTER_LON".
#'
#' @return A dataframe with corrected latitude and longitude coordinates.
#'
#' @examples
#' # Using default global shapefile
#' data <- data.frame(
#'   id = c("a1b2c3", "d4e5f6"),
#'   lat = c(40.7128, -74.0060),
#'   lon = c(-74.0060, 40.7128)
#' )
#' corrected_data <- correct_flipped_geo_coords(
#'   data,
#'   join_key_a = "id",
#'   lat_col = "lat",
#'   lon_col = "lon"
#' )
#'
#' # Using custom shapefile
#' custom_shapefile <- data.frame(
#'   region_id = c("a1b2c3", "d4e5f6"),
#'   correct_lat = c(40.7128, 41.8781),
#'   correct_lon = c(-74.0060, -87.6298)
#' )
#' corrected_data <- correct_flipped_geo_coords(
#'   data,
#'   shapefile_data = custom_shapefile,
#'   join_key_a = "id",
#'   join_key_b = "region_id",
#'   lat_col = "lat",
#'   lon_col = "lon",
#'   correct_lat_col = "correct_lat",
#'   correct_lon_col = "correct_lon"
#' )
#'
#' @export
correct_flipped_geo_coords <- function(data, shapefile_data = NULL,
                                       join_key_a, join_key_b = NULL,
                                       lat_col, lon_col,
                                       correct_lat_col = NULL, 
                                       correct_lon_col = NULL) {
  
  # Use poliprep::shp_global if shapefile_data is not provided
  if (is.null(shapefile_data)) {
    # get global shapefile
    shapefile_data <- poliprep::shp_global
    join_key_b <- "ADM2_GUID"
    correct_lat_col <- "CENTER_LAT"
    correct_lon_col <- "CENTER_LON"
  }
  
  # Check if join_key_a exists in data
  if (!join_key_a %in% names(data)) {
    stop(glue::glue(
      "The join key '{join_key_a}' is not present in the data dataframe."
    ))
  }
  
  # Check if join_key_b exists in shapefile_data
  if (is.null(join_key_b) || !join_key_b %in% names(shapefile_data)) {
    stop(glue::glue(
      "The join key '{join_key_b}' is not present in ",
      "the shapefile_data dataframe."
    ))
  }
  
  # Check if correct_lat_col and correct_lon_col exist in shapefile_data
  if (is.null(correct_lat_col) || !correct_lat_col %in% names(shapefile_data)) {
    stop(glue::glue(
      "The column '{correct_lat_col}' ",
      "is not present in the shapefile_data dataframe."
    ))
  }
  if (is.null(correct_lon_col) || !correct_lon_col %in% names(shapefile_data)) {
    stop(glue::glue(
      "The column '{correct_lon_col}' is ",
      "not present in the shapefile_data dataframe."
    ))
  }
  
  # Convert GUIDs to lowercase and remove braces using dplyr style
  data <- data |>
    dplyr::mutate(
      !!rlang::sym(join_key_a) := as.character(!!rlang::sym(join_key_a)),
      !!rlang::sym(join_key_a) := stringr::str_to_lower(
        stringr::str_replace_all(!!rlang::sym(join_key_a), "[{}]", "")
      )
    )
  
  shapefile_data <- shapefile_data |>
    dplyr::mutate(
      !!rlang::sym(join_key_b) := as.character(!!rlang::sym(join_key_b)),
      !!rlang::sym(join_key_b) := stringr::str_to_lower(
        stringr::str_replace_all(!!rlang::sym(join_key_b), "[{}]", "")
      )
    )
  
  data <- data |>
    dplyr::left_join(
      shapefile_data |>
        dplyr::select(
          !!rlang::sym(join_key_b),
          !!rlang::sym(correct_lat_col),
          !!rlang::sym(correct_lon_col)
        ),
      by = stats::setNames(join_key_b, join_key_a)
    ) |>
    dplyr::mutate(
      dist_correct = ifelse(
        is.na(!!rlang::sym(correct_lat_col)) | is.na(!!rlang::sym(correct_lon_col)),
        NA,
        geosphere::distHaversine(
          cbind(!!rlang::sym(lon_col), !!rlang::sym(lat_col)),
          cbind(!!rlang::sym(correct_lon_col), !!rlang::sym(correct_lat_col))
        )
      ),
      dist_flipped = ifelse(
        is.na(!!rlang::sym(correct_lat_col)) | is.na(!!rlang::sym(correct_lon_col)),
        NA,
        geosphere::distHaversine(
          cbind(!!rlang::sym(lon_col), !!rlang::sym(lat_col)),
          cbind(!!rlang::sym(correct_lat_col), !!rlang::sym(correct_lon_col))
        )
      ),
      flipped = ifelse(is.na(dist_correct) | is.na(dist_flipped),
                       FALSE,
                       dist_flipped < dist_correct
      )
    ) |>
    dplyr::mutate(
      temp_lat = ifelse(flipped, !!rlang::sym(lon_col), !!rlang::sym(lat_col)),
      temp_lon = ifelse(flipped, !!rlang::sym(lat_col), !!rlang::sym(lon_col))
    ) |>
    dplyr::select(-dist_correct, 
                  -dist_flipped, 
                  -!!rlang::sym(lat_col), -!!rlang::sym(lon_col)) |>
    dplyr::rename(!!lat_col := temp_lat, !!lon_col := temp_lon) |>
    dplyr::select(
      -!!rlang::sym(correct_lat_col), -!!rlang::sym(correct_lon_col)) |>
    as.data.frame() |>
    dplyr::select(-dplyr::any_of("geometry"))
  
  # Calculate the number of coordinates flipped
  num_flipped <- sum(data$flipped, na.rm = TRUE)
  total_coords <- nrow(data)
  
  # Use cli to give a report
  if (num_flipped > 0) {
    cli::cli_alert_success(
      "{num_flipped} out of {total_coords} coordinates were flipped.")
  } else {
    cli::cli_alert_info("No coordinates were flipped.")
  }
  
  return(data)
}