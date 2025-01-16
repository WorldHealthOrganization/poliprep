testthat::test_that("check_leap_issue correctly identifies leap year issues", {
  # Test valid leap year date
  dates <- c("2020-02-29")
  testthat::expect_equal(check_leap_issue(dates), c(FALSE))

  # Test invalid leap year date
  dates <- c("2019-02-29")
  testthat::expect_equal(check_leap_issue(dates), c(TRUE))

  # Test non-leap year date
  dates <- c("2021-03-01")
  testthat::expect_equal(check_leap_issue(dates), c(FALSE))

  # Test mixed dates
  dates <- c("2020-02-29", "2019-02-29", "2021-03-01")
  testthat::expect_equal(check_leap_issue(dates), c(FALSE, TRUE, FALSE))

  # Test invalid formats
  dates <- c("2020/02/29", "2019/02/29", "2021/03/01")
  testthat::expect_equal(check_leap_issue(dates), c(FALSE, TRUE, FALSE))

  # Test NA date
  dates <- c(NA)
  testthat::expect_equal(check_leap_issue(dates), c(FALSE))

  # Test mixed with NA
  dates <- c("2020-02-29", NA, "2019-02-29")
  testthat::expect_equal(check_leap_issue(dates), c(FALSE, FALSE, TRUE))

  # Test invalid month and day
  dates <- c("2020-13-29", "2019-02-30")
  testthat::expect_equal(check_leap_issue(dates), c(FALSE, FALSE))

  # Test only invalid formats
  dates <- c("2020-02-30", "2019-13-29")
  testthat::expect_equal(check_leap_issue(dates), c(FALSE, FALSE))
})

testthat::test_that("validate_date correctly identifies various date issues", {
  suppressMessages({
    data <- data.frame(
      date = c(
        "2023-06-15", "2024-07-20", NA, "1999-12-31", "2025-08-22",
        "2020/23/10", "2020-02-29", "2019-02-29"
      )
    )

    result <- validate_date(data, "date")

    # Check missing dates
    testthat::expect_equal(
      result$date_missing,
      c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )

    # Check non-date values
    testthat::expect_equal(
      result$date_non_date,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
    )

    # Check invalid dates
    testthat::expect_equal(
      result$date_invalid,
      c(FALSE, FALSE, NA, TRUE, FALSE, FALSE, FALSE, FALSE)
    )

    # Check future dates
    testthat::expect_equal(
      result$date_future,
      c(FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, NA)
    )

    # Check leap year issues
    testthat::expect_equal(
      result$date_leap_issue,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
    )

    # Check date formatting issues
    testthat::expect_equal(
      result$date_format_issue,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )
  })
})

testthat::test_that("validate_dates correctly identifies various date issues", {
  suppressMessages({
    data <- data.frame(
      date1 = c(
        "2023-06-15", "2024-07-20", NA, "1999-12-31", "2025-08-22",
        "2020/23/10", "2020-02-29", "2019-02-29"
      ),
      date2 = c(
        "2023-06-15", "2024-07-20", "2022-05-10", "2024-02-29",
        "2026-09-23", "2020/23/10", "2020-02-29", "2019-02-29"
      )
    )

    result <- validate_dates(data,
      date_col1 = "date1", date_col2 = "date2"
    )

    # Check missing dates
    testthat::expect_equal(
      result$date1_missing,
      c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )
    testthat::expect_equal(
      result$date2_missing,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )

    # Check non-date values
    testthat::expect_equal(
      result$date1_non_date,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_equal(
      result$date2_non_date,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
    )

    # Check invalid dates
    testthat::expect_equal(
      result$date1_invalid,
      c(FALSE, FALSE, NA, TRUE, FALSE, NA, FALSE, NA)
    )
    testthat::expect_equal(
      result$date2_invalid,
      c(FALSE, FALSE, FALSE, FALSE, TRUE, NA, FALSE, NA)
    )

    # Check future dates
    testthat::expect_equal(
      result$date1_future,
      c(FALSE, FALSE, NA, FALSE, FALSE, NA, FALSE, NA)
    )
    
    testthat::expect_equal(
      result$date2_future,
      c(FALSE, FALSE, FALSE, FALSE, TRUE, NA, FALSE, NA)
    )

    # Check leap year issues
    testthat::expect_equal(
      result$date1_leap_issue,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
    )

    testthat::expect_equal(
      result$date2_leap_issue,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
    )

    # Check date formatting issues
    testthat::expect_equal(
      result$date1_format_issue,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
    )

    testthat::expect_equal(
      result$date2_format_issue,
      c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
    )

    # Check invalid order
    testthat::expect_equal(
      result$date1_invalid_order,
      c(FALSE, FALSE, NA, FALSE, FALSE, NA, FALSE, NA)
    )
  })
})

