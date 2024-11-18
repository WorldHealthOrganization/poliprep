mock_data <- data.frame(
   virus_date = as.Date(c("2021-05-01", "2021-11-30", "2022-01-15")))

# Test 1: Correct Categorization
testthat::test_that("Dates are categorized correctly", {
  categorized_data <- categorize_biannual(mock_data, "virus_date")
  testthat::expect_true(
    all(categorized_data$date_categ %in% levels(categorized_data$date_categ)))
  testthat::expect_equal(as.vector(categorized_data$date_categ[1]), "Jan 21 / Jun 21")
})
