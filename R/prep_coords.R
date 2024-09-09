#' Format Numbers with Thousand Separator
#'
#' This function formats numbers by adding a thousand separator (big mark)
#' and optionally rounding to a specified number of decimal places.
#'
#' @param x A numeric vector to be formatted.
#' @param decimals An integer specifying the number of decimal places to
#'         round to. Default is NULL, which means no rounding is performed.
#' @param big_mark A character to use as the thousand separator. 
#'        Default is ",".
#' @return A character vector of formatted numbers.
#'
#' @examples
#' big_mark(1234567.89)
#' big_mark(c(1234.56, 7890123.45), decimals = 2, big_mark = ",")
#'
#' @export
big_mark <- function(x, decimals = NULL, big_mark = ",") {
  if (!is.null(decimals)) {
    x <- round(x, decimals)
  }
  base::format(x, big.mark = big_mark, scientific = FALSE, trim = TRUE,
               nsmall = if (is.null(decimals)) 0 else decimals)
}

#' Common Setup for Coordinate Operations
#'
#' This helper function performs common setup tasks for coordinate operations.
#'
#' @param data A dataframe containing the geographical coordinates.
#' @param shapefile_data Optional. A dataframe containing reference shapefile 
#'        data.
#' @param join_key_a The column name in `data` to join with `shapefile_data`.
#' @param join_key_b Optional. The column name in `shapefile_data` to join with 
#'        `data`.
#' @param lat_col The name of the latitude column in `data`.
#' @param lon_col The name of the longitude column in `data`.
#' @param correct_lat_col Optional. The name of the correct latitude column in 
#'      `shapefile_data`.
#' @param correct_lon_col Optional. The name of the correct longitude column in 
#'        `shapefile_data`.
#'
#' @return A list containing processed data and shapefile data.
setup_coord_operation <- function(data, shapefile_data = NULL,
                                  join_key_a, join_key_b = NULL,
                                  lat_col, lon_col,
                                  correct_lat_col = NULL,
                                  correct_lon_col = NULL) {
  # Use poliprep::shp_global if shapefile_data is not provided
  if (is.null(shapefile_data)) {
    shapefile_data <- poliprep::shp_global
    join_key_b <- "ADM2_GUID"
    correct_lat_col <- "CENTER_LAT"
    correct_lon_col <- "CENTER_LON"
  }
  
  # Input validation
  if (!join_key_a %in% names(data)) {
    stop(glue::glue("The join key '{join_key_a}' ",
                    "is not present in the data dataframe."))
  }
  if (is.null(join_key_b) || !join_key_b %in% names(shapefile_data)) {
    stop(glue::glue("The join key '{join_key_b}' is not ",
                    "present in the shapefile_data dataframe."))
  }
  if (is.null(correct_lat_col) || !correct_lat_col %in% names(shapefile_data)) {
    stop(glue::glue("The column '{correct_lat_col}' is not present ",
                    "in the shapefile_data dataframe."))
  }
  if (is.null(correct_lon_col) || !correct_lon_col %in% names(shapefile_data)) {
    stop(glue::glue("The column '{correct_lon_col}' is not present ",
                    "in the shapefile_data dataframe."))
  }
  
  # Process data
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
  
  # Join data with shapefile_data
  joined_data <- data |>
    dplyr::left_join(
      shapefile_data |>
        dplyr::select(
          !!rlang::sym(join_key_b),
          !!rlang::sym(correct_lat_col),
          !!rlang::sym(correct_lon_col)
        ),
      by = stats::setNames(join_key_b, join_key_a)
    )
  
  return(list(data = joined_data, shapefile_data = shapefile_data))
}

#' Check for Potentially Flipped Geographical Coordinates
#'
#' This function checks for potentially flipped latitude and longitude 
#' coordinates by comparing them with a reference shapefile, without actually 
#' correcting them.
#'
#' @inheritParams setup_coord_operation
#'
#' @return A dataframe with additional columns indicating whether coordinates 
#'         are potentially flipped and the distances to the correct coordinates.
check_coord_flip <- function(data, shapefile_data = NULL,
                             join_key_a, join_key_b = NULL,
                             lat_col, lon_col,
                             correct_lat_col = NULL,
                             correct_lon_col = NULL) {

  # Use poliprep::shp_global if shapefile_data is not provided
  if (is.null(shapefile_data)) {
    shapefile_data <- poliprep::shp_global
    join_key_b <- "ADM2_GUID"
    correct_lat_col <- "CENTER_LAT"
    correct_lon_col <- "CENTER_LON"
  }
  
  setup_result <- setup_coord_operation(
    data, shapefile_data, join_key_a, join_key_b,
    lat_col, lon_col, correct_lat_col, correct_lon_col
  )
  
  # Use poliprep::shp_global if shapefile_data is not provided
  if (is.null(shapefile_data)) {
    shapefile_data <- poliprep::shp_global
    join_key_b <- "ADM2_GUID"
    correct_lat_col <- "CENTER_LAT"
    correct_lon_col <- "CENTER_LON"
  }
  
  joined_data <- setup_result$data
  
  result <- joined_data |>
    dplyr::mutate(
      dist_correct = calculate_distance(
        !!rlang::sym(lon_col),
        !!rlang::sym(lat_col),
        !!rlang::sym(correct_lon_col),
        !!rlang::sym(correct_lat_col)
      ),
      dist_flipped = calculate_distance(
        !!rlang::sym(lat_col),
        !!rlang::sym(lon_col),
        !!rlang::sym(correct_lon_col),
        !!rlang::sym(correct_lat_col)
      ),
      coord_diff_input = abs(
        !!rlang::sym(lat_col) - !!rlang::sym(lon_col)),
      coord_diff_correct = abs(
        !!rlang::sym(correct_lat_col) - !!rlang::sym(correct_lon_col)),
      potentially_flipped = dplyr::case_when(
        abs(dist_flipped - dist_correct) < 1e-6 ~ FALSE,
        coord_diff_input > 0.5 &  # Lowered threshold
          coord_diff_correct > 0.5 &  # Lowered threshold
          dist_flipped < dist_correct * 0.9 ~ TRUE,  # Adjusted ratio
        TRUE ~ FALSE
      ),
      flipped_confidence_score = pmin(
        100, pmax(0, 100 * (dist_correct - dist_flipped) / dist_correct * 
                    (1 + log10(coord_diff_input + 1)) * 
                    (1 + log10(coord_diff_correct + 1))))
    ) |>
    dplyr::select(
      -coord_diff_input, -coord_diff_correct)
  
  # Calculate the number of potentially flipped coordinates
  num_flipped <- sum(result$potentially_flipped, na.rm = TRUE)
  total_coords <- nrow(result)
  
  # Use cli to give a report
  if (num_flipped > 0) {
    num_flipped <- big_mark(num_flipped)
    total_coords <- big_mark(total_coords)
    cli::cli_alert_warning(
      "{num_flipped} out of {total_coords} coordinates are potentially flipped."
    )
  } else {
    cli::cli_alert_success("No coordinates appear to be flipped.")
  }
  
  return(result)
}

#' Calculate Haversine Distance Between Two Geographic Points
#'
#' This function calculates the Haversine distance between two points on the 
#' Earth's surface, given their longitude and latitude coordinates. It includes 
#' input validation and error handling.
#'
#' @param lon1 Numeric. Longitude of the first point in decimal degrees.
#' @param lat1 Numeric. Latitude of the first point in decimal degrees.
#' @param lon2 Numeric. Longitude of the second point in decimal degrees.
#' @param lat2 Numeric. Latitude of the second point in decimal degrees.
#'
#' @return Numeric. The Haversine distance between the two points in meters.
#'         Returns NA if any of the coordinates are invalid or if an error 
#'        occurs during calculation.
#'
#' @details
#' The function first checks if any of the input coordinates are outside the 
#' valid range (latitude between -90 and 90 degrees, longitude between -180 and 
#' 180 degrees). If any coordinate is invalid, it returns NA.
#' 
#' If all coordinates are valid, it uses the geosphere::distHaversine function 
#' to calculate the distance. Any errors during this calculation are caught, 
#' and NA is returned in such cases.
#'
#' @examples
#' calculate_distance(0, 0, 1, 1)
#' calculate_distance(-180, 90, 180, -90)
#' calculate_distance(0, 91, 0, 0)  # Returns NA due to invalid latitude
#'
#' @importFrom geosphere distHaversine
#'
#' @export
calculate_distance <- function(lon1, lat1, lon2, lat2) {
  
  # Check for NA values
  na_mask <- is.na(lon1) | is.na(lat1) | is.na(lon2) | is.na(lat2)
  
  # Check for out-of-range values
  invalid_mask <- abs(lat1) > 90 | abs(lat2) > 90 | 
    abs(lon1) > 180 | abs(lon2) > 180
  
  # Combine NA and invalid masks
  mask <- na_mask | invalid_mask
  
  # Initialize result vector
  result <- rep(NA_real_, length(lon1))
  
  # Calculate distances only for valid coordinates
  valid_indices <- which(!mask)
  if (length(valid_indices) > 0) {
    result[valid_indices] <- geosphere::distHaversine(
      cbind(lon1[valid_indices], lat1[valid_indices]),
      cbind(lon2[valid_indices], lat2[valid_indices])
    )
  }
  
  return(result)
}

#' Check if geographic coordinates are on land or water
#'
#' This function takes vectors of longitude and latitude coordinates 
#' and determines whether each point is located on land or water using 
#' Natural Earth data included in poliprep. It handles missing values gracefully.
#'
#' @param lon Numeric vector of longitude coordinates
#' @param lat Numeric vector of latitude coordinates
#'
#' @return A character vector with "land", "water", or NA for each input 
#'        coordinate pair
#'
#' @examples
#' # Create a data frame with test coordinates, known locations, and missing 
#' # values
#' coords <- data.frame(
#'    lat = c(40.7772, 40.2987, -1.1466, 1.7656, 4.6282, 5.5880, NA, 45.5017),
#'    long = c(-74.1613, -70.7210, 32.9333, 22.8465, -0.6796,
#'          -0.0420, -123.5673, NA),
#'    actual_location = c(
#'            "Land", "Ocean", "Inland Water", "Land", "Ocean", "Ocean", NA, NA)
#'  )
#'
#' # Apply the check_land_water function to the coordinates
#' coords$predicted_location <- check_land_water(coords$long, coords$lat)
#'
#' # Print the results, showing actual and predicted locations
#' print(coords)
#' 
#' @export
check_land_water <- function(lon, lat) {
  
  sf::sf_use_s2(FALSE)
  
  # Get world land polygons and lakes
  world <- poliprep::world
  lakes <- poliprep::lakes
  
  # Create a data frame with input coordinates
  input_df <- data.frame(lon = lon, lat = lat)
  
  # Identify rows with missing values
  missing_mask <- is.na(input_df$lon) | is.na(input_df$lat)
  
  # Create points from valid input coordinates
  valid_points <- sf::st_as_sf(
    input_df[!missing_mask, ], 
    coords = c("lon", "lat"), 
    crs = 4326
  )
  
  # Initialize result vector with NAs
  result <- rep(NA_character_, length(lon))
  
  if (nrow(valid_points) > 0) {
    suppressMessages({
      # Check if points intersect with land
      intersects_land <- sf::st_intersects(valid_points, world, sparse = FALSE)
      
      # Check if points intersect with lakes
      intersects_lakes <- sf::st_intersects(valid_points, lakes, sparse = FALSE)
    })
    
    # Determine if points are on land, in lakes, or in ocean
    valid_result <- ifelse(apply(intersects_land, 1, any), 
                           ifelse(apply(intersects_lakes, 1, any), 
                                  "Inland water", "Land"), "Ocean")
    
    # Assign results to non-missing coordinates
    result[!missing_mask] <- valid_result
  }
  
  return(result)
}

#' Comprehensive Geographic Data Check
#'
#' This function performs various checks on geographic data, including 
#' coordinate flipping, land/water checks, and missing coordinate checks. It 
#' provides CLI output and an optional table summary.
#'
#' @param data A dataframe containing the geographic data to be checked.
#' @param shapefile_data Optional. A dataframe containing reference shapefile 
#'        data. If NULL, `poliprep::shp_global` will be used.
#' @param join_key_a The column name in data to join with the shapefile data.
#' @param join_key_b Optional. The column name in shapefile_data to join with 
#'        data. If NULL and shapefile_data is NULL, defaults to "ADM2_GUID".
#' @param lat_col The name of the latitude column in the data.
#' @param lon_col The name of the longitude column in the data.
#' @param correct_lat_col Optional. The name of the correct latitude column in 
#'        shapefile_data. If NULL and shapefile_data is NULL, defaults to 
#'        "CENTER_LAT".
#' @param correct_lon_col Optional. The name of the correct longitude column in 
#'        shapefile_data. If NULL and shapefile_data is NULL, defaults to 
#'        "CENTER_LON".
#' @param checks A character vector specifying which checks to perform.
#'        Options are "flip", "on_water", "missing", "out_of_bounds", 
#'        "precision", and "null_coords". Default is all checks.
#' @param aggregate_by Optional. A column name to aggregate results by (e.g., 
#'                      "country" or "year").
#' @param summary_table Logical. If TRUE, returns a summary table instead of CLI 
#'                      output.
#' @param min_precision Numeric. The minimum number of decimal places required 
#'                      for coordinate precision. Default is 4.
#'
#' @return If summary_table is FALSE, returns the input data with additional 
#'         check columns. If summary_table is TRUE, returns a summary table of 
#'         the checks.
#'
#' @export
check_coords <- function(data, 
                        shapefile_data = NULL,
                        join_key_a, join_key_b = NULL,
                        lat_col, lon_col,
                        correct_lat_col = NULL,
                        correct_lon_col = NULL,
                        checks = c("flip", "on_water", "missing", 
                                    "out_of_bounds", "precision", 
                                    "null_coords"),
                         aggregate_by = NULL, summary_table = FALSE,
                         min_precision = 4) {
  
    # Use poliprep::shp_global if shapefile_data is not provided
  if (is.null(shapefile_data)) {
    shapefile_data <- poliprep::shp_global
    join_key_b <- "ADM2_GUID"
    correct_lat_col <- "CENTER_LAT"
    correct_lon_col <- "CENTER_LON"
  }
  
  # Initialize results
  results <- data
  total_count <- big_mark(nrow(results))
  
  # Check for missing coordinates
  if ("missing" %in% checks) {
    results <- results |>
      dplyr::mutate(
        missing_coords = is.na(
          !!rlang::sym(lat_col)) | is.na(!!rlang::sym(lon_col))
      )
    
    missing_count <- sum(results$missing_coords, na.rm = TRUE)
    
    cli::cli_h1("Missing Coordinates Check")
    if (missing_count > 0) {
      cli::cli_alert_danger(
        "Found {crayon::red(missing_count)} out of {total_count} coordinates missing.")
    } else {
      cli::cli_alert_success("No missing coordinates found.")
    }
  }
  
  # Check for out-of-bounds coordinates
  if ("out_of_bounds" %in% checks) {
    results <- results |>
      dplyr::mutate(
        out_of_bounds = (
          !!rlang::sym(lat_col) < -90 | !!rlang::sym(lat_col) > 90 |
            !!rlang::sym(lon_col) < -180 | !!rlang::sym(lon_col) > 180)
      )
    
    out_of_bounds_count <- sum(results$out_of_bounds, na.rm = TRUE)
    
    cli::cli_h1("Out-of-Bounds Coordinates Check")
    if (out_of_bounds_count > 0) {
      cli::cli_alert_danger(
        "Found {crayon::red(out_of_bounds_count)} out of {total_count} coordinates out ",
        "of bounds. Valid lat range is -90 to 90, and valid log range is -180 to 180.")
      cli::cli_alert_info(
        "Out-of-bounds coordinates are those where:",
        "\n  - Lat is less than -90 or greater than 90",
        "\n  - Long is less than -180 or greater than 180")
      cli::cli_alert_warning(
        "These coordinates may indicate data entry errors or projection issues.")
    } else {
      cli::cli_alert_success(
        "No out-of-bounds coordinates found. All coordinates are within valid ranges:",
        "\n  - Latitude: -90 to 90",
        "\n  - Longitude: -180 to 180")
    }
  }
  
  # Check for coordinate precision
  if ("precision" %in% checks) {
    results <- results |>
      dplyr::mutate(
        lat_precision = nchar(
          sub("^-?\\d+\\.", "", as.character(!!rlang::sym(lat_col)))),
        lon_precision = nchar(
          sub("^-?\\d+\\.", "", as.character(!!rlang::sym(lon_col)))),
        low_precision = (
          lat_precision < min_precision | lon_precision < min_precision)
      )
    
    low_precision_count <- sum(results$low_precision, na.rm = TRUE)
    
    cli::cli_h1("Coordinate Precision Check")
    if (low_precision_count > 0) {
      cli::cli_alert_warning(
        paste0("Found {crayon::red(low_precision_count)} coordinates with precision",
        " below {min_precision} decimal places."))
    }
    



    if (low_precision_count == 0) {
      cli::cli_alert_success(
        paste0(
        "All coordinates have appropriate precision ",
        "(minimum {min_precision} decimal places)."))
    }
  }
  
  # Check for Null Island (0,0 coordinates)
  if ("null_coords" %in% checks) {
    results <- results |>
      dplyr::mutate(
        null_count = (!!rlang::sym(lat_col) == 0 & !!rlang::sym(lon_col) == 0)
      )
    
    null_count <- sum(results$null_count, na.rm = TRUE)
    
    cli::cli_h1("Null Coordinates Check")
    if (null_count > 0) {
      cli::cli_alert_danger(
        "Found {crayon::red(null_count)} coordinates that are Null (0,0).")
    } else {
      cli::cli_alert_success("No coordinates that were Null (0,0).")
    }
  }
 
  # Check for flipped coordinates
  if ("flip" %in% checks) {
    tryCatch(
      {
        suppressMessages({
          results <- check_coord_flip(
            data = results,
            shapefile_data = shapefile_data,
            join_key_a = join_key_a,
            join_key_b = join_key_b,
            lat_col = lat_col,
            lon_col = lon_col,
            correct_lat_col = correct_lat_col,
            correct_lon_col = correct_lon_col
          )
        })
        
        if ("dist_correct" %in% names(results) && "dist_flipped" %in% names(results)) {
          results <- results |>
            dplyr::select(-dist_correct, -dist_flipped)
        }
        
        cli::cli_h1("Flipped Coordinates Check")
        flipped_count <- sum(results$potentially_flipped, na.rm = TRUE)
        total_count <- nrow(results)
        flipped_count_formatted <- big_mark(flipped_count)
        total_count_formatted <- big_mark(total_count)
        if (flipped_count > 0) {
          cli::cli_alert_warning(
            "Found {crayon::red(flipped_count_formatted)} out of {total_count_formatted} ",
            "coordinates potentially flipped."
          )
        } else {
          cli::cli_alert_success("No potentially flipped coordinates found.")
        }
      },
      error = function(e) {
        cli::cli_alert_danger("Error in flipped coordinates check: {e$message}")
        results$potentially_flipped <- NA
      }
    )
  }
  
  # Check for land/water
  if ("on_water" %in% checks) {
    results$on_water <- check_land_water(
      results[[lon_col]],
      results[[lat_col]]
    )
    
    cli::cli_h1("Land/Water Check")
    land_count <- sum(results$on_water == "Land", na.rm = TRUE)
    water_count <- sum(results$on_water != "Land", na.rm = TRUE)
    land_count <- big_mark(land_count)
    water_count <- big_mark(water_count)
    
    if (water_count == 0) {
      cli::cli_alert_success("All {land_count} coordinates are on land.")
    } else {
      cli::cli_alert_warning(
        "Found {crayon::red(water_count)} coordinates on water, the rest are on land."
      )
    }
  }
  
  
  # Generate summary table if requested
  if (summary_table) {
    summary_data <- results
    if (!is.null(aggregate_by)) {
      summary_data <- summary_data |>
        dplyr::select(
          dplyr::any_of(
            c(
              "missing_coords",
              "out_of_bounds",
              "low_precision",
              "high_precision",
              "null_count",
              "potentially_flipped",
              "on_water", aggregate_by
            )
          )
        ) |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(aggregate_by)))
    }
    
    summary_data  <- summary_data |>
      dplyr::reframe(
        total_coords = dplyr::n(),
        missing_coords = if (
          "missing_coords" %in% names(summary_data)) sum(
            missing_coords, na.rm = TRUE) else NA_real_,
        potentially_flipped = if (
          "potentially_flipped" %in% names(summary_data)) sum(
            potentially_flipped, na.rm = TRUE) else NA_real_,
        on_water = if (
          "on_water" %in% names(summary_data)) sum(
            on_water %in% c("Ocean", "Inland water"), na.rm = TRUE) else NA_real_,
        out_of_bounds = if (
          "out_of_bounds" %in% names(summary_data)) sum(
            out_of_bounds, na.rm = TRUE) else NA_real_,
        low_precision = if (
          "low_precision" %in% names(summary_data)) sum(
            low_precision, na.rm = TRUE) else NA_real_,
        null_count = if (
          "null_count" %in% names(summary_data)) sum(
            null_count, na.rm = TRUE) else NA_real_,
        total_coords = big_mark(total_coords),
        null_count = big_mark(null_count),
        low_precision = big_mark(low_precision),
        on_water = big_mark(on_water),
        potentially_flipped = big_mark(potentially_flipped),
        missing_coords = big_mark(missing_coords)
      ) |> 
      dplyr::select(
        dplyr::all_of(c(
          aggregate_by,
          "total_coords",
          if ("missing" %in% checks) "missing_coords" else NULL,
          if ("null_coords" %in% checks) "null_count" else NULL,
          if ("out_of_bounds" %in% checks) "out_of_bounds" else NULL,
          if ("precision" %in% checks) "low_precision" else NULL,
          if ("flip" %in% checks) "potentially_flipped" else NULL,
          if ("on_water" %in% checks) "on_water" else NULL
        ))
      )
    
    # Conditionally rename columns
    new_names <- c(
      "Total Coords" = "total_coords",
      "Missing Coords" = "missing_coords",
      "Null Coords" = "null_count",
      "Potentially Flipped" = "potentially_flipped",
      "On Water" = "on_water",
      "Out of Bounds" = "out_of_bounds",
      "Low Precision" = "low_precision"
    )
    
    for (new_name in names(new_names)) {
      if (new_names[new_name] %in% names(summary_data)) {
        summary_data <- summary_data |>
          dplyr::rename(!!new_name := !!rlang::sym(new_names[new_name]))
      }
    }
    
    cat("\n")
    return(as.data.frame(summary_data))
  } else {
    return(results)
  }
}


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
#'    id = c("38E294F7-508F-46A2-B5CF-B1D2FD6B2103", 
#'           "1965B8ED-CD56-4784-BEB4-6924A245F533"),
#'    lat = c(40.7128, -74.0060),
#'    lon = c(-74.0060, 40.7128)
#'  )
#'  
#'  corrected_data <- correct_flipped_geo_coords(
#'    data,
#'    join_key_a = "id",
#'    lat_col = "lat",
#'    lon_col = "lon"
#'  )
#' 
#'  # Using custom shapefile
#'  custom_shapefile <- data.frame(
#'    region_id = c("38E294F7-508F-46A2-B5CF-B1D2FD6B2103", 
#'                  "1965B8ED-CD56-4784-BEB4-6924A245F533"),
#'    correct_lat = c(40.7128, 41.8781),
#'    correct_lon = c(-74.0060, -87.6298)
#'  )
#'  corrected_data <- correct_flipped_geo_coords(
#'    data,
#'    shapefile_data = custom_shapefile,
#'    join_key_a = "id",
#'    join_key_b = "region_id",
#'    lat_col = "lat",
#'    lon_col = "lon",
#'    correct_lat_col = "correct_lat",
#'    correct_lon_col = "correct_lon"
#'  )
#'  
#' @export
correct_flipped_geo_coords <- function(data, shapefile_data = NULL,
                                       join_key_a, join_key_b = NULL,
                                       lat_col, lon_col,
                                       correct_lat_col = NULL,
                                       correct_lon_col = NULL) {
  
  # Use poliprep::shp_global if shapefile_data is not provided
  if (is.null(shapefile_data)) {
    shapefile_data <- poliprep::shp_global
    join_key_b <- "ADM2_GUID"
    correct_lat_col <- "CENTER_LAT"
    correct_lon_col <- "CENTER_LON"
  }
  
  suppressMessages({
    checked_data <- check_coord_flip(
      data, shapefile_data, join_key_a, join_key_b,
      lat_col, lon_col, correct_lat_col, correct_lon_col
    )
  })
  
  corrected_data <- checked_data |>
    dplyr::mutate(
      temp_lat = dplyr::if_else(
        potentially_flipped, !!rlang::sym(lon_col), !!rlang::sym(lat_col)
      ),
      temp_lon = dplyr::if_else(
        potentially_flipped, !!rlang::sym(lat_col), !!rlang::sym(lon_col)
      )
    ) |>
    dplyr::select(
      -dist_correct, -dist_flipped,
      -!!rlang::sym(lat_col), -!!rlang::sym(lon_col),
      -!!rlang::sym(correct_lat_col),
      -!!rlang::sym(correct_lon_col),
      -dplyr::any_of("geometry")
    ) |>
    dplyr::rename(!!lat_col := temp_lat, !!lon_col := temp_lon) |>
    as.data.frame()
  
  # Calculate the number of coordinates flipped
  num_flipped <- sum(corrected_data$potentially_flipped, na.rm = TRUE)
  total_coords <- nrow(corrected_data)
  
  # Use cli to give a report
  if (num_flipped > 0) {
    num_flipped <- big_mark(num_flipped)
    total_coords <- big_mark(total_coords)
    cli::cli_alert_success(
      "{num_flipped} out of {total_coords} coordinates were flipped."
    )
  } else {
    cli::cli_alert_info("No coordinates were flipped.")
  }
  
  return(corrected_data)
}