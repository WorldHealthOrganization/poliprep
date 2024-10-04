testthat::test_that("get_ona_page Successfully API call processes correctly", {
  suppressMessages(
    result <- get_ona_page(
      api_url = "https://fakerapi.it/api/v1/addresses?_quantity=10",
      api_token = NULL
    )
  )
  
  testthat::expect_type(result$data, "list")
  testthat::expect_equal(result$status, "OK")
  testthat::expect_equal(result$code, 200)
  testthat::expect_equal(result$total, 10)
} 
)

testthat::test_that("check_status_api when its success", {
  
  # get response
  response <- httr::HEAD(
    "https://fakerapi.it/api/v1/addresses?_quantity=10")
  
  # test response
  testthat::expect_invisible(check_status_api(response))
} 
)

testthat::test_that("check_status_api when it fails", {
  
  # get response
  response <- httr::HEAD(
    "https://fakerapi.it/api/add")
  
  # test response
  testthat::expect_error(check_status_api(response))
} 
)
