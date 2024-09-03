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
#' # nearest_coord <- find_nearest_coord(15.28172, -4.271301)
#' # print(nearest_coord)
#' @export
# Define the function
find_nearest_coord <- function(lon, lat, 
                               shapefile = NULL, 
                               level_to_return = "adm2") {
  if (is.null(shapefile)) {
    shapefile <- poliprep::shp_global
  }
  
  shapefile_prepared <- shapefile |>
    dplyr::filter(ENDDATE > as.Date("9900-12-31")) |>
    dplyr::rename(adm0 = ADM0_NAME, 
                  adm1 = ADM1_NAME, 
                  lon = CENTER_LON,
                  lat = CENTER_LAT,
                  adm2 = ADM2_NAME) |>
    dplyr::mutate(lon = as.numeric(lon), 
                  lat = as.numeric(lat)) |> 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) 
  
  target_points <- sf::st_as_sf(data.frame(lon = lon, lat = lat), 
                                coords = c("lon", "lat"), crs = 4326)
  
  nearest_indices <- sf::st_nearest_feature(target_points, shapefile_prepared)
  nearest_coords <- shapefile_prepared[nearest_indices, ][[level_to_return]]
  
  return(nearest_coords)
}
