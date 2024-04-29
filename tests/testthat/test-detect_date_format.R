testthat::test_that("detects simple date formats correctly", {
  testthat::expect_equal(
    detect_date_format(c("01-02-2021", "02-03-2021")), "%d-%m-%Y")
  testthat::expect_equal(
    detect_date_format(c("2021/02/01", "2021/03/02")), "%Y/%m/%d")
})

testthat::test_that("handles mixed format vectors", {
  dates <- c("01-02-2021", "2021/03/02", "04.05.2021")
  testthat::expect_equal(detect_date_format(dates), "%d-%m-%Y")
})

testthat::test_that("returns NA when no dates are parsed", {
  dates <- c("99-99-9999", "99/99/9999")
  testthat::expect_true(is.na(detect_date_format(dates)))
})

testthat::test_that("handles additional arguments correctly", {
  dates <- c("01-02-21", "02-03-21")  # Ambiguous year format
  testthat::expect_equal(detect_date_format(
    dates, common_formats = "%d-%m-%y"), "%d-%m-%y")
})
