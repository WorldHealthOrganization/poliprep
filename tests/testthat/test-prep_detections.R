# tests/testthat/test-virus_functions.R

library(testthat)

# Test get_detections function
testthat::test_that("get_detections returns correct format and counts", {
  # Setup test data
  test_data <- tibble::tibble(
    VirusTypeName = c("WPV1", "WPV1", "cVDPV2", "VDPV1"),
    Detections = c(2, 3, 1, 4)
  )
  
  virus_types <- c("WPV1", "cVDPV2", "VDPV1")
  
  result <- get_detections(test_data, virus_types)
  
  # Tests
  testthat::expect_type(result, "character")
  testthat::expect_length(result, 4)
  testthat::expect_true(all(stringr::str_detect(result, "^\\d+ \\w+\\d?$")))
  testthat::expect_equal(
    as.character(result[1]), "2 WPV1") 
  testthat::expect_equal(
    as.character(result[2]), "3 WPV1") 
  testthat::expect_equal(
    as.character(result[3]), "1 cVDPV2")
})

# Test format_date_ord function
testthat::test_that("format_date_ord handles various date cases correctly", {
  # Setup test dates
  dates <- as.Date(c(
    "2023-01-01",  # 1st
    "2023-01-02",  # 2nd
    "2023-01-03",  # 3rd
    "2023-01-04",  # 4th
    "2023-01-11",  # 11th
    "2023-01-12",  # 12th
    "2023-01-13",  # 13th
    "2023-01-21",  # 21st
    "2023-01-22",  # 22nd
    "2023-01-23"   # 23rd
  ))
  
  results <- format_date_ord(dates)
  
  # Tests
  testthat::expect_type(results, "character")
  testthat::expect_length(results, 10)
  
  # Test specific ordinal cases
  testthat::expect_equal(results[1], "1st Jan 2023")
  testthat::expect_equal(results[2], "2nd Jan 2023")
  testthat::expect_equal(results[3], "3rd Jan 2023")
  testthat::expect_equal(results[4], "4th Jan 2023")
  testthat::expect_equal(results[5], "11th Jan 2023")  # Special case
  testthat::expect_equal(results[6], "12th Jan 2023")  # Special case
  testthat::expect_equal(results[7], "13th Jan 2023")  # Special case
  testthat::expect_equal(results[8], "21st Jan 2023")
  testthat::expect_equal(results[9], "22nd Jan 2023")
  testthat::expect_equal(results[10], "23rd Jan 2023")
  
  # Test NA handling
  testthat::expect_equal(
    format_date_ord(as.Date(NA)), 
    "NAth NA NA"
  )
})

# Test prep_new_detections_table function
testthat::test_that("prep_new_detections_table processes data correctly", {
  # Setup mock data
  polis_df_old <- tibble::tibble(
    EPID = c("OLD1", "OLD2"),
    Admin0Name = c("COUNTRY1", "COUNTRY2"),
    VirusTypeName = c("WPV1", "cVDPV2"),
    VirusDate = as.Date(c("2023-01-01", "2023-01-02")),
    UpdatedDate = as.Date(c("2023-01-01", "2023-01-02")),
    SurveillanceTypeName = c("AFP", "Environmental"),
    VdpvEmergenceGroupName = c(NA, "EMG-1")
  )
  
  polis_df_new <- tibble::tibble(
    EPID = c("NEW1", "NEW2"),
    Admin0Name = c("COUNTRY1", "COUNTRY2"),
    VirusTypeName = c("WPV1", "cVDPV2"),
    VirusDate = as.Date(c("2023-02-01", "2023-02-02")),
    UpdatedDate = as.Date(c("2023-02-01", "2023-02-02")),
    SurveillanceTypeName = c("AFP", "Environmental"),
    VdpvEmergenceGroupName = c(NA, "EMG-2"),
    VdpvClassificationChangeDate = as.Date(c(NA, "2023-02-02")),
    VdpvReportedToHQDate = as.Date(c(NA, "2023-02-15")),
    VdpvNtChangesClosestMatch = c(NA, "10"),
    VdpvNtChangesFromSabin = c(NA, "12"),
    VaccineOrigin = c(NA, "Sabin")
  )
  
  polis_sia <- tibble::tibble(
    Admin0Name = c("COUNTRY1", "COUNTRY2"),
    ActivityStatus = c("Done", "Done"),
    ActivityDateFrom = as.Date(c("2023-01-15", "2023-01-16")),
    ActivityVaccineType = c("bOPV", "nOPV2")
  )
  
  # Run function
  result <- prep_new_detections_table(
    polis_df_old = polis_df_old,
    polis_df_new = polis_df_new,
    polis_sia = polis_sia,
    include_sia = TRUE,
    save_table = FALSE
  )
  
  # Tests
  testthat::expect_s3_class(result, "gt_tbl")
  
  # Test table structure
  testthat::expect_true("Country2" %in% result$`_data`$Country)
  testthat::expect_true("Virus" %in% names(result$`_data`))
  testthat::expect_true("Emergence Group" %in% names(result$`_data`))
  
  # Test data processing
  table_data <- result$`_data`
  testthat::expect_equal(nrow(table_data), 1) 
  
  # Test specific calculations
  testthat::expect_true(any(table_data$`New Emergence`))  
  testthat::expect_true(all(!is.na(table_data$`Latest Detection`))) 
})

# Test error handling
testthat::test_that("functions handle errors appropriately", {
  # Test get_detections with invalid input
  testthat::expect_error(get_detections(NULL, c("WPV1")))
  testthat::expect_null(get_detections(tibble::tibble(), NULL))
  
  # Test format_date_ord with invalid input
  testthat::expect_error(format_date_ord("invalid date"))
  testthat::expect_error(format_date_ord("2023/13/01"))  # Invalid month
  
  # Test prep_new_detections_table with invalid input
  testthat::expect_error(prep_new_detections_table(NULL, NULL, NULL))
  testthat::expect_error(
    prep_new_detections_table(
      tibble::tibble(),
      tibble::tibble(),
      tibble::tibble()
    )
  )
})

# Test edge cases
testthat::test_that("functions handle edge cases correctly", {
  
  # Test get_detections with empty data
  empty_data <- tibble::tibble(
    VirusTypeName = character(),
    Detections = numeric()
  )
  testthat::expect_length(get_detections(empty_data, c("WPV1")), 0)
  
  # Test format_date_ord with edge dates
  testthat::expect_match(
    format_date_ord(as.Date("2000-12-31")),
    "31st Dec 2000"
  )
  testthat::expect_match(
    format_date_ord(as.Date("2100-01-01")),
    "1st Jan 2100"
  )
  
})
