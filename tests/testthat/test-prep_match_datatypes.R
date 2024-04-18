testthat::test_that("Data types match correctly", {
    
    # set up sample ref target data
    ref_df <- tibble::tibble(
        integer_col = 1:3,
        character_col = c("a", "b", "c"),
        numeric_col = c(1.1, 2.2, 3.3),
        date_col = as.Date(
            c("2021-01-01", "2021-01-02", "2021-01-03"))
    )

    # set up sample target data
    target_df <- tibble::tibble(
        integer_col = c("1", "2", "3"),
        character_col = 1:3,
        numeric_col = c("1.1", "2.2", "3.3"),
        date_col = c("2021-01-01", "2021-01-02", "2021-01-03")
    )

    # match datatypes
    matched_df <- prep_match_datatypes(ref_df, target_df)

    # check if prep_match_datatypes works
    testthat::expect_type(matched_df$integer_col, "integer")
    testthat::expect_type(matched_df$character_col, "character")
    testthat::expect_type(matched_df$numeric_col, "double")
    testthat::expect_equal(class(matched_df$date_col), "Date")
})


