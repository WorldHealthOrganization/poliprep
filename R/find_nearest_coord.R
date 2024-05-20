#' Find Nearest Coordinate from shapefile Dataset
#'
#' This function takes a pair of coordinates (longitude and latitude) and finds
#' the nearest coordinate from the `shp_global` dataset.
#'
#' The shapefile dataset should contain longitude and latitude information
#' with columns named `CENTER_LON` and `CENTER_LAT`. The function calculates the
#' distance between the target point and all points in the dataset, returning
#' the nearest coordinate.
#'
#' @param lon Numeric. Longitude of the target point.
#' @param lat Numeric. Latitude of the target point.
#' @param shapefile An optional `sf` object containing the shapefile data. 
#'      Defaults to `poliprep::shp_global`.
#' @param level_to_return The admin level to return.
#' @return A vector of admin level names containing the nearest coordinate from
#'   the provided shapefile.
#' @examples
#' # Example usage
#' # Assuming `poliprep::shp_global` is already loaded in the environment
#' nearest_coord <- find_nearest_coord(15.28172, -4.271301)
#' print(nearest_coord)
#' @export
# Define the function
find_nearest_coord <- function(lon, lat, 
                               shapefile = NULL,
                               level_to_return = "adm2") {
  
  # Use default shapefile if none provided
  if (is.null(shapefile)) {
    shapefile <- poliprep::shp_global
  }
  
  # Prepare the `shp_global` dataset
  res <- shapefile |>
    dplyr::filter(ENDDATE > as.Date("9900-12-31")) |> 
    dplyr::rename(adm0 = ADM0_NAME,
                  adm1 = ADM1_NAME,
                  adm2 = ADM2_NAME) |> 
    dplyr::mutate(
      CENTER_LON = as.numeric(CENTER_LON),
      CENTER_LAT = as.numeric(CENTER_LAT)
    ) |>
    sf::st_as_sf(coords = c("CENTER_LON", "CENTER_LAT"), crs = 4326)
  
  # Define the target point
  target_point <- sf::st_sfc(
    sf::st_point(c(lon, lat)), crs = 4326
  ) |>
    sf::st_sf()
  
  # Find the nearest coordinate
  distances <- sf::st_distance(target_point, res)
  nearest_index <- which.min(distances)
  nearest_coord <- res[nearest_index, ][[level_to_return]]
  
  return(as.vector(nearest_coord))
}
