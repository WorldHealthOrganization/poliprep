testthat::test_that("Check Status API Responses", {
  # Testing for successful status code 200
  testthat::expect_null(check_status_api(200))
  # Testing for various error status codes
  testthat::expect_error(check_status_api(413))
  # Testing for an unspecified status code
  testthat::expect_error(check_status_api(999))
})


testthat::test_that("API URL Construction without selection", {
  
  base = "https://api.example.com/"
  
  actual_url <- construct_api_url(
    base, "data", "2020-01-01",
    "2020-12-31", "dateField", NULL, "regionField",  "AFRO",
    select_vars = NULL)
  
  expected_url <- paste0(
    base, "data?$filter=dateField%20ge%20DateTime",
    "'2020-01-01'%20and%20dateField%20le%20DateTime",
    "'2020-12-31'%20and%20regionField%20eq%20'AFRO'")
  
  testthat::expect_equal(actual_url, expected_url)
  
}
)

testthat::test_that("API URL Construction without selection", {
  
  base = "https://api.example.com/"
  
  actual_url <- construct_api_url(
    base, "data", "2020-01-01",
    "2020-12-31", "dateField", NULL, "regionField","AFRO",
    c("field1", "field2"))
  
  expected_url <- paste0(
    base, "data?$filter=dateField%20ge%20DateTime",
    "'2020-01-01'%20and%20dateField%20le%20DateTime",
    "'2020-12-31'%20and%20regionField%20eq%20'AFRO'",
    "&$select=field1,field2")
  
  testthat::expect_equal(actual_url, expected_url)
  
}
)

testthat::test_that("Get API Date Suffix Functionality", {
  # Test for valid data types
  testthat::expect_equal(get_api_date_suffix("cases"),
                         list(endpoint_suffix = "Case",
                              date_fields_initial = "CaseDate",
                              date_field = "LastUpdateDate"))
  testthat::expect_equal(get_api_date_suffix("virus"),
                         list(endpoint_suffix = "Virus",
                              date_fields_initial = "VirusDate",
                              date_field = "UpdatedDate"))
  # Add more tests for other valid data types
  
  # Test for an invalid data type
  testthat::expect_error(
    get_api_date_suffix("invalid_type"), "Invalid data_type specified")
})

# Mock the iterative_api_call function
mock_iterative_api_call <- function(api_url, token = NULL) {
  # Mock response data
  mock_data <- data.frame(
    id = 1:5,
    date = seq(as.Date('2021-01-01'), as.Date('2021-01-05'), by="day"),
    cases = sample(100:200, 5)
  )
  jsonlite::toJSON(list(data = mock_data))
}

# Mock the process_api_response function
mock_process_api_response <- function(response) {
  data <- jsonlite::fromJSON(response)
  data$data
}

testthat::test_that("get_polis_api_data returns correct data structure", {
  # Stub the external functions with mocks
  mockery::stub(
    get_polis_api_data, "iterative_api_call", mock_iterative_api_call)
  mockery::stub(
    get_polis_api_data, "process_api_response", mock_process_api_response)
  
  # Test the function
  result <- get_polis_api_data("2021-01-01",
                               "2021-01-31", "cases", "AFRO")
  
  # Assertions
  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("id", "date", "cases") %in% names(result)))
  testthat::expect_equal(nrow(result), 5)
})



testthat::test_that("Get the correct response and status code form API call", {
  url <- "https://fakerapi.it/api/v1/addresses?_quantity=1"
  
  status_code <- iterative_api_call(url) |>
    head(1) |>
    httr2::resps_data(\(resp) httr2::resp_status(resp))
  
  testthat::expect_equal(status_code, 200)
})


testthat::test_that("Test functionality of process_api_response", {
  url <- "https://fakerapi.it/api/v1/addresses?_quantity=10"
  
  response <- iterative_api_call(url)  |>
    process_api_response()
  
  testthat::expect_type(response, 'list')
})


# Assuming necessary functions are defined or mocked here

# Setup: Mock functions
mock_file_exists <- function(path) TRUE
mock_import_data <- function(path) {
  data.frame(Date = seq(as.Date("2021-01-01"),
                        as.Date("2021-01-10"), by = "day"))
}
mock_get_polis_api_data <- function(...) {
  data.frame(Date = seq(as.Date("2021-01-11"),
                        as.Date("2021-01-20"), by = "day"))
}
mock_export_data <- function(data, path) TRUE
mock_write_log_file_api <- function(log_file_name, log_message) TRUE


test_that("Main update_polis_api_data functionality", {
  mockery::stub(
    update_polis_api_data, "file.exists", mock_file_exists)
  mockery::stub(
    update_polis_api_data, "readRDS", mock_import_data)
  mockery::stub(
    update_polis_api_data, "get_polis_api_data", mock_get_polis_api_data)
  mockery::stub(
    update_polis_api_data, "saveRDS", mock_export_data)
  mockery::stub(
    update_polis_api_data, "write_log_file_api", mock_write_log_file_api)
  
  # Test 1: When data is being update
  suppressWarnings(
    result <- update_polis_api_data("2021-01-01", "2021-01-20")
  )
  testthat::expect_equal(nrow(result), 20)
  
})

#
# test_that("Functionality with save_directly and log_results", {
#
#   withr::with_tempdir({
#
#     # Set up the temporary file path
#     temp_file_path <- tempdir()
#
#     data_file_name <- file.path(temp_file_path, "cases_polis_data.rds")
#     log_file_name <- file.path(temp_file_path, "polis_data_update_log.xlsx")
#
#     # Set up mock files
#     mockery::stub(
#       update_polis_api_data, "file.exists", function(path) FALSE)
#     mockery::stub(
#       update_polis_api_data, "readRDS", function(path) NULL)
#     mockery::stub(
#       update_polis_api_data, "get_polis_api_data", function(...) {
#         data.frame(
#           Date = seq(as.Date("2021-01-11", format = "%Y-%m-%d"),
#                      as.Date("2021-01-20", format = "%Y-%m-%d"), by = "day"))
#       })
#     mockery::stub(
#       update_polis_api_data,
#       "saveRDS", function(data, data_file_name) TRUE)
#     mockery::stub(
#       update_polis_api_data, "write_log_file_api",
#       function(log_file_name, log_message) TRUE)
#
#     # Execute the function with save_directly and log_results set to TRUE
#     suppressWarnings(
#       result <- update_polis_api_data(
#         min_date = "2021-01-01", max_date = "2021-01-20",
#         data_type = "cases", region = "AFRO",
#         file_path = NULL,
#         save_directly = TRUE, log_results = TRUE
#       )
#     )
#
#     # Check if data and log files were created in the temporary directory
#     expect_true(
#       file.exists(data_file_name))
#     expect_true(
#       file.exists(file.path(temp_file_path, "polis_data_update_log.xlsx")))
#
#   })
# })

# testthat::test_that("Log File Handling in write_log_file_api", {
#   withr::with_tempdir({
#     full_data <- new_data <- data.frame(
#       date_field = as.Date('2023-01-01'),
#       other_field = 1:10
#     )
#     data_type <- "test_type"
#     last_date_in_chunk <- min_date <- max_date <- "2023-01-01"
#     session_end_date <- "2023-01-02"
#
#     # Setup: Creating a new file
#     log_file <- file.path(tempfile(), "log_file.rds")
#
#     # Test: New file creation
#     write_log_file_api(
#       log_file, data_type, full_data, new_data, 'date_field',
#       last_date_in_chunk, min_date, max_date, session_end_date)
#     file_content <- readRDS(log_file)
#
#     testthat::expect_equal(nrow(file_content), 1)
#     testthat::expect_equal(file_content$DataType, data_type)
#
#     # Test: Appending to existing file
#     write_log_file_api(
#       log_file, data_type, full_data, new_data, 'date_field',
#       last_date_in_chunk, min_date, max_date, session_end_date)
#     updated_file_content <- readRDS(log_file)
#     testthat::expect_equal(nrow(updated_file_content), 1)
#   })
# })
#
#
#
#
