
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
    round(result$lat), c(-74, 41), tolerance = 1e-4)
  testthat::expect_equal(
    round(result$lon), c(41, -74), tolerance = 1e-4)
  
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
    round(result2$lat,), c(41, 41), tolerance = 1e-4)
  testthat::expect_equal(
    round(result2$lon), c(-74, -74), tolerance = 1e-4)
  
  # Check if flipped column is correct
  testthat::expect_equal(result2$flipped, c(FALSE, TRUE))
  
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
