# Example test for calculate_string_distance function
testthat::test_that("calculate_string_distance works correctly", {
  
  # get function output
  result <- calculate_string_distance(
    c("New York", "Los Angeles"), 
    c("New York", "Los Angeles", "Chicago"), "lv")
  
  # expected outputs based on function results
  expected <- data.frame(
    Algorithm = rep("lv", 6),
    AdminToClean = rep(c("New York", "Los Angeles"), each = 3),
    MatchedNames = c("New York", "Chicago", 
                     "Los Angeles", "Los Angeles", "New York", "Chicago"),
    Distance = c(0, 8, 10, 0, 10, 10), 
    MatchRank = rep(1:3, 2)
  )
  
  # Test if the output matches expected data frame
  testthat::expect_equal(result, expected)
})


testthat::test_that("Test administrative matching stats output", {
  
  # setup mock data
  data <- data.frame(
    country = c("Country1", "Country2", "Country1", "Country3"),
    province = c("State1", "State2", "State1", "State3"),
    district = c("City1", "City2", "City1", "City4")
  )
  
  lookup_data <- data.frame(
    country = c("Country1", "Country2", "Country1", "Country3"),
    province = c("State1", "State2", "State1", "State3"),
    district = c("City1", "City2", "City1", "City4")
  )
  
  suppressMessages({
    testthat::expect_invisible(
      calculate_match_stats(
        data = data, lookup_data = lookup_data, adm0 = "country",
        adm1 = "province", adm2 = "district")
    )
  })
})