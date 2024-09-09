
testthat::test_that("correct_flipped_geo_coords works correctly", {
  # Test data
  data <- data.frame(
    id = c("{0CDA1C45-9529-4188-BE2F-863E287EDA71}",
           "{387991C9-EA43-4C8C-8C96-E7F69F068F16}"),
    lat = c(40.7128, -74.0060),
    lon = c(-74.0060, 40.7128)
  )
  
  custom_shapefile <- data.frame(
    region_id = c("{0CDA1C45-9529-4188-BE2F-863E287EDA71}",
                  "{387991C9-EA43-4C8C-8C96-E7F69F068F16}"),
    correct_lat = c(40.7128, 41.8781),
    correct_lon = c(-74.0060, -87.6298)
  )
  
  suppressMessages(
    # Test without shapefile
    result <- correct_flipped_geo_coords(
      data,
      join_key_a = "id",
      lat_col = "lat",
      lon_col = "lon"
    )
  )
  
  # Check if coordinates are corrected
  testthat::expect_equal(
    round(result$lat), c(41, 41), tolerance = 1e-4)
  testthat::expect_equal(
    round(result$lon), c(-74, -74), tolerance = 1e-4)
  
  suppressMessages(
    # Test with custom shapefile
    result2 <- correct_flipped_geo_coords(
      data,
      shapefile_data = custom_shapefile,
      join_key_a = "id",
      join_key_b = "region_id",
      lat_col = "lat",
      lon_col = "lon",
      correct_lat_col = "correct_lat",
      correct_lon_col = "correct_lon"
    )
  )
  
  # Check if coordinates are corrected
  testthat::expect_equal(
    round(result2$lat), c(41, 41), tolerance = 1e-4)
  testthat::expect_equal(
    round(result2$lon), c(-74, -74), tolerance = 1e-4)
  
  # Check if flipped column is correct
  testthat::expect_equal(result2$potentially_flipped, c(FALSE, TRUE))
  
  # Test error messages
  testthat::expect_error(
    correct_flipped_geo_coords(
      data,
      shapefile_data = custom_shapefile,
      join_key_a = "non_existent_key",
      join_key_b = "region_id",
      lat_col = "lat",
      lon_col = "lon",
      correct_lat_col = "correct_lat",
      correct_lon_col = "correct_lon"
    ),
    "The join key 'non_existent_key' is not present in the data dataframe."
  )
  
  testthat::expect_error(
    correct_flipped_geo_coords(
      data,
      shapefile_data = custom_shapefile,
      join_key_a = "id",
      join_key_b = "non_existent_key",
      lat_col = "lat",
      lon_col = "lon",
      correct_lat_col = "correct_lat",
      correct_lon_col = "correct_lon"
    ),
    paste0("The join key 'non_existent_key' is not ",
           "present in the shapefile_data dataframe.")
  )
  
  testthat::expect_error(
    correct_flipped_geo_coords(
      data,
      shapefile_data = custom_shapefile,
      join_key_a = "id",
      join_key_b = "region_id",
      lat_col = "lat",
      lon_col = "lon",
      correct_lat_col = "correct_lat2",
      correct_lon_col = "correct_lon"
    ),
    paste0("The column 'correct_lat2' is not present in ",
           "the shapefile_data dataframe.")
  )
  
  testthat::expect_error(
    correct_flipped_geo_coords(
      data,
      shapefile_data = custom_shapefile,
      join_key_a = "id",
      join_key_b = "region_id",
      lat_col = "lat",
      lon_col = "lon",
      correct_lat_col = "correct_lat",
      correct_lon_col = "correct_lon2"
    ),
    paste0("The column 'correct_lon2' is not present in ",
           "the shapefile_data dataframe.")
  )
  
  
  # Test data2
  data2 <- data.frame(
    id = c("{0CDA1C45-9529-4188-BE2F-863E287EDA71}",
           "{387991C9-EA43-4C8C-8C96-E7F69F068F16}"),
    lat =  c(40.7128, 41.8781),
    lon = c(-74.0060, -87.6298)
  )
  
  
  suppressMessages(
    res <- correct_flipped_geo_coords(
      data2,
      shapefile_data = custom_shapefile,
      join_key_a = "id",
      join_key_b = "region_id",
      lat_col = "lat",
      lon_col = "lon",
      correct_lat_col = "correct_lat",
      correct_lon_col = "correct_lon"
    ))
  
  # Check if coordinates are corrected
  testthat::expect_equal(
    round(res$lat), c(41, 42), tolerance = 1e-4)
  testthat::expect_equal(
    round(res$lon), c(-74, -88), tolerance = 1e-4)
})

testthat::test_that("check_land_water function works correctly", {
  # Test known locations
  test_coords <- data.frame(
    lon = c(-74.1613, -70.7210, 32.9333, 22.8465, 
            -0.6796, -0.0420, NA, 45.5017, -180, 180),
    lat = c(40.7772, 40.2987, -1.1466, 1.7656, 
            4.6282, 5.5880, NA, NA, 90, -90)
  )
  
  expected_results <- c(
    "Land",         # New York City (Land)
    "Ocean",        # Atlantic Ocean
    "Inland water", # Lake Victoria
    "Land",         # Central African Republic (Land)
    "Ocean",        # Gulf of Guinea
    "Ocean",        # Gulf of Guinea (close to coast)
    NA,             # Missing longitude
    NA,             # Missing latitude
    "Ocean",        # North Pole
    "Ocean"         # South Pole
  )
  
  results <- check_land_water(test_coords$lon, test_coords$lat)
  
  testthat::expect_equal(results, expected_results)
  
  # # Test edge cases
  # edge_cases <- data.frame(
  #   lon = c(0, 0, 180, -180, 179.99, -179.99),
  #   lat = c(0, 90, 0, 0, 89.99, -89.99)
  # )
  # 
  # suppressWarnings(
  # edge_results <- check_land_water(edge_cases$lon, edge_cases$lon))
  # 
  # testthat::expect_true(all(!is.na(edge_results)))
  # 
  # # Test invalid coordinates
  # invalid_coords <- data.frame(
  #   lon = c(181, -181, 0, 0),
  #   lat = c(0, 0, 91, -91)
  # )
  # 
  # invalid_results <- check_land_water(invalid_coords$lon, invalid_coords$lat)
  # testthat::expect_true(all(is.na(invalid_results)))
  # 
  # Test empty input
  # testthat::expect_equal(length(check_land_water(numeric(0), numeric(0))), 0)
  
  # Test large number of coordinates
  set.seed(123)
  large_coords <- data.frame(
    lon = runif(10000, -180, 180),
    lat = runif(10000, -90, 90)
  )
  
  large_results <- check_land_water(large_coords$lon, large_coords$lat)
  testthat::expect_equal(length(large_results), 10000)
  testthat::expect_true(all(large_results %in% c(
    "Land", "Ocean", "Inland water")))
  
  # Test coordinates on country borders
  border_coords <- data.frame(
    lon = c(6.1466, -3.0053, 14.5962),
    lat = c(49.8153, 16.9742, 35.9025)
  )
  
  border_results <- check_land_water(border_coords$lon, border_coords$lat)
  testthat::expect_equal(border_results, c("Land", "Land", "Ocean"))
  
  # Test coordinates in small islands
  island_coords <- data.frame(
    lon = c(-157.8583, 55.4515, 166.6186),
    lat = c(21.3069, -20.9043, -0.5228)
  )
  
  island_results <- check_land_water(
    island_coords$lon, island_coords$lat)
  testthat::expect_equal(island_results, c("Land", "Land", "Ocean"))
})

testthat::test_that("check_land_water handles NA values correctly", {
  na_coords <- data.frame(
    lon = c(NA, 0, NA, 100),
    lat = c(0, NA, NA, 50)
  )
  
  na_results <- check_land_water(na_coords$lon, na_coords$lat)
  testthat::expect_equal(na_results, c(NA, NA, NA, "Land"))
})

testthat::test_that("check_land_water is consistent for repeated calls", {
  set.seed(456)
  test_coords <- data.frame(
    lon = runif(100, -180, 180),
    lat = runif(100, -90, 90)
  )
  
  results1 <- check_land_water(test_coords$lon, test_coords$lat)
  results2 <- check_land_water(test_coords$lon, test_coords$lat)
  
  testthat::expect_equal(results1, results2)
})

# Mock data for testing
mock_data <- data.frame(
  id = c("A1", "B2", "C3", "D4", "E5"),
  lat = c(40.7128, -74.0060, NA, 51.5074, 48.8566),
  lon = c(-74.0060, 40.7128, -0.1278, NA, 2.3522),
  country = c("USA", "USA", "UK", "UK", "France")
)

testthat::test_that("check_coords performs all checks correctly", {
  
  suppressMessages(
    result <- check_coords(mock_data, lat_col = "lat",  
                           lon_col = "lon", aggregate_by =  "id")
  )
  
  testthat::expect_true("missing_coords" %in% names(result))
  testthat::expect_true("lat_precision" %in% names(result))
  testthat::expect_true("on_water" %in% names(result))
  
  testthat::expect_equal(sum(result$missing_coords, na.rm = TRUE), 2)
  testthat::expect_false(any(result$potentially_flipped, na.rm = TRUE))
  testthat::expect_true(any(result$on_water != "Land", na.rm = TRUE))
})

testthat::test_that("check_coords handles aggregate_by correctly", {
  
  suppressMessages(
    result <- check_coords(mock_data, lat_col = "lat",  
                           lon_col = "lon", 
                           aggregate_by = "country", summary_table = TRUE))
  
  testthat::expect_equal(nrow(result), 3)  # 3 unique countries
  testthat::expect_true("country" %in% names(result))
  testthat::expect_true(any(c(
    "Potentially Flipped",
    "Missing Coords", 
    "On Water") %in% names(result)))
})

testthat::test_that("check_coords produces correct summary table", {
  
  
  suppressMessages(
    result <- check_coords(mock_data, lat_col = "lat",  
                           lon_col = "lon", 
                           summary_table = TRUE))
  
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(nrow(result), 1)  # One summary row
  testthat::expect_true(
    all(c(
      "Potentially Flipped",
      "Missing Coords", 
      "On Water") %in% names(result)))
})

