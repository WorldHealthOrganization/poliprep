testthat::test_that("assign_round_number works with basic functionality", {
  df <- data.frame(
    today = as.Date(c("2024-04-28", "2024-05-03", "2024-05-04", "2024-06-01"))
  )
  
  result <- assign_round_number(df, date_col = "today", threshold = 7)
  
  expected_groups <- c("Apr 2024", "Apr 2024", "Apr 2024", "Jun 2024")
  
  testthat::expect_equal(levels(result$round_group), 
                         c("Apr 2024", "May 2024","Jun 2024"))
})


testthat::test_that("assign_round_number works with basic functionality", {
  
  df <- data.frame(
    today = as.Date(c("2024-04-28", "2024-05-03", "2024-05-04", "2024-06-01",
                      "2024-04-29", "2024-05-02")),
    states = c("State1", "State1", "State1", "State1", "State2", "State2")
  )
  
  result <- assign_round_number(df, date_col = "today", threshold = 7,
                                additional_grouping = "states")
  
  expected_groups <- c("Apr 2024", "Apr 2024", "Apr 2024", 
                       "Jun 2024", "Apr 2024", "Apr 2024", "Apr 2024")
  
  testthat::expect_equal(levels(result$round_group), 
                         c("Apr 2024", "May 2024","Jun 2024"))
})


