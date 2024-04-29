testthat::test_that("Data types match correctly", {
  # set up sample ref target data
  ref_df <- tibble::tibble(
    integer_col = 1:3,
    logical_col = c(TRUE, TRUE, FALSE), 
    character_col = c("a", "b", "c"),
    numeric_col = c(1.1, 2.2, 3.3),
    date_col = as.Date(c(
      "2021-01-01", "2021-01-02", "2021-01-03"
    )),
    posixct_date_col = as.POSIXct(c("2024-04-17",
                                    "2024-04-18",
                                    "2024-04-19"),
                                  format = "%Y-%m-%d")
  )
  
  # set up sample target data
  target_df <- tibble::tibble(
    integer_col = c("1", "2", "3"),
    logical_col = c("TRUE", "TRUE", "FALSE"), 
    character_col = 1:3,
    numeric_col = c("1.1", "2.2", "3.3"),
    date_col = c("2021-01-01", "2021-01-02", "2021-01-03"),
    posixct_date_col = c("2024-04-17",
                         "2024-04-18",
                         "2024-04-19")
  )
  
  # match datatypes
  matched_df <- prep_match_datatypes(ref_df, target_df)
  
  # check if prep_match_datatypes works
  testthat::expect_type(matched_df$integer_col, "integer")
  testthat::expect_type(matched_df$character_col, "character")
  testthat::expect_type(matched_df$numeric_col, "double")
  testthat::expect_type(matched_df$logical_col, "logical")
  testthat::expect_equal(class(matched_df$date_col), "Date")
})
