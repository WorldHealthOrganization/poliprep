
# set up mocek shapefile
mock_data <- data.frame(
  CENTER_LON = c(-4.222, -3.2, -24.5),
  CENTER_LAT = c(15.02222, 43.2, 23.5),
  ENDDATE = c("9999-12-31 01:00:00", "9999-12-31 01:00:00", 
              "9999-12-31 01:00:00"),
  ADM0_NAME = c("Mock Area 1", "Mock Area 2", "Mock Area 3"),
  ADM1_NAME = c("Mock Area 1", "Mock Area 2", "Mock Area 3"),
  ADM2_NAME = c("Mock Area 1", "Mock Area 2", "Mock Area 3") 
) 

# Test cases
testthat::test_that("find_nearest_coord returns correct area", {
  result <- find_nearest_coord(shapefile = mock_data,  -24.0, 15.9)
  testthat::expect_equal(result, "Mock Area 3")

  result2 <- find_nearest_coord(
    lon = -24.0324, lat = 15.93214, level_to_return = "adm2")
  testthat::expect_equal(as.vector(result2), "RIBERIA BRAVA")
  
})
