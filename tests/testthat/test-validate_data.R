testthat::test_that("check_missing works correctly", {
  
  suppressMessages({
    # Create a sample dataset
    df <- data.frame(
      a = c(1, 2, NA, 4, 5),
      b = c("x", NA, "z", NA, "w"),
      c = c(TRUE, FALSE, TRUE, TRUE, NA),
      year = c(2020, 2021, 2020, 2021, 2020),
      country = c("USA", "Canada", "USA", "Canada", "Mexico")
    )
    
    # Test 1: Basic functionality
    result <- check_missing(df)
    testthat::expect_equal(nrow(result), 5)
    testthat::expect_equal(colnames(result), c(
      "Column", "Missing Count", "Missing Percent",
      "Is Completely Null"
    ))
    testthat::expect_equal(result$`Missing Count`, c("1", "2", "1", "0", "0"))
    testthat::expect_equal(result$`Missing Percent`, c(20, 40, 20, 0, 0))
    
    # Test 2: Checking specific columns
    result <- check_missing(df, cols_to_check = c("a", "b"))
    testthat::expect_equal(nrow(result), 2)
    testthat::expect_equal(as.vector(result$Column), c("a", "b"))
    
    # Test 3: Using key_columns
    result <- check_missing(df, key_columns = c("a", "b"))
    testthat::expect_equal(result$Priority, c(
      "High Priority", "High Priority", "Standard",
      "Standard", "Standard"
    ))
    
    # Test 4: Using group_by
    result <- check_missing(df, group_by = c("year", "country"))
    testthat::expect_equal(nrow(result), 9) # 5 columns * 3 groups
    expect_true(all(c("year", "country") %in% colnames(result)))
    
    # Test 5: Completely null column
    df$d <- NA
    result <- check_missing(df)
    expect_true(any(result$`Is Completely Null`))
    testthat::expect_equal(sum(result$`Is Completely Null`), 1)
    
  })
  
})


testthat::test_that("get_missing_ids works correctly", {
  # Create a sample dataset
  df <- data.frame(
    id = 1:5,
    a = c(1, NA, 3, NA, 5),
    b = c("x", "y", NA, "w", "z"),
    c = c(TRUE, FALSE, TRUE, NA, TRUE)
  )
  
  # Test 1: Check missing values in column 'a'
  result_a <- get_missing_ids(df, "id", "a")
  testthat::expect_equal(result_a, c(2, 4))
  
  # Test 2: Check missing values in column 'b'
  result_b <- get_missing_ids(df, "id", "b")
  testthat::expect_equal(result_b, 3)
  
  # Test 3: Check missing values in column 'c'
  result_c <- get_missing_ids(df, "id", "c")
  testthat::expect_equal(result_c, 4)
  
  # Test 4: Check a column with no missing values
  result_no_missing <- get_missing_ids(df, "id", "id")
  testthat::expect_equal(length(result_no_missing), 0)
  
  # Test 5: Check with a different id column
  df_alt_id <- df |> dplyr::rename(alt_id = id)
  result_alt_id <- get_missing_ids(df_alt_id, "alt_id", "a")
  testthat::expect_equal(result_alt_id, c(2, 4))
  
  # Test 6: Check with all missing values
  df$all_na <- NA
  result_all_na <- get_missing_ids(df, "id", "all_na")
  testthat::expect_equal(result_all_na, 1:5)
  
  # Test 7: Check with no missing values
  df$no_na <- 1:5
  result_no_na <- get_missing_ids(df, "id", "no_na")
  testthat::expect_equal(length(result_no_na), 0)
  
  # Test 8: Error handling for non-existent columns
  testthat::expect_error(get_missing_ids(df, "id", "non_existent_column"))
  testthat::expect_error(get_missing_ids(df, "non_existent_id", "a"))
})

testthat::test_that("join_and_check_mismatches works correctly", {
  
  suppressMessages({ 
    
  # Sample data
  sample_data <- data.frame(
    EPID = 1:5,
    Admin0GUID = c("A1", "A1", "A2", "A2", "A3"),
    Admin0Name = c("Country1", "Country1", "Country2", "Country2", 
                   "Country3"),
    Admin1GUID = c("B1", "B2", "B3", "B3", "B4"),
    Admin1Name = c("Province1", "Province2", "Province3", "Province3", 
                   "Province4"),
    Admin2GUID = c("C1", "C2", "C3", "C4", "C5"),
    Admin2Name = c("District1", "District2", "District3", "District4", 
                   "District5")
  )
  
  # Sample shapefile data
  shapefile_data <- data.frame(
    ADM0_NAME = c("Country1", "Country2"),
    ADM1_NAME = c("Province1", "Province3"),
    ADM2_NAME = c("District1", "District3"),
    ADM1_GUID = c("B1", "B3"),
    ADM2_GUID = c("C1", "C3")
  )
  
  # Define column names
  geo_name_cols <- c("Admin0Name", "Admin1Name", "Admin2Name")
  geo_id_cols <- c("Admin0GUID", "Admin1GUID", "Admin2GUID")
  shapefile_name_cols <- c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME")
  shapefile_id_cols <- c("ADM1_GUID", "ADM2_GUID")
  id_col <- "EPID"
  
  # Run the function
  results <- join_and_check_mismatches(
    sample_data, shapefile_data,
    geo_name_cols, geo_id_cols,
    shapefile_name_cols, shapefile_id_cols,
    id_col
  )
  
  # Test summary table
  testthat::expect_equal(nrow(results$summary_table), 5)
  testthat::expect_equal(
    results$summary_table$`Geo Column Type`,
    c("Admin0Name", "Admin1Name", "Admin2Name", "Admin1GUID", "Admin2GUID")
  )
  testthat::expect_equal(
    results$summary_table$`Missing in Shapefile`, c(1, 2, 3, 2, 3))
  
  # Test detailed mismatches
  testthat::expect_equal(nrow(results$detailed_mismatches), 11)
  testthat::expect_true(
    all(results$detailed_mismatches$ID %in% sample_data$EPID))
  testthat::expect_true(all(results$detailed_mismatches$`Geo Column Type` %in%
                              c("Admin0Name", "Admin1Name", "Admin2Name", 
                                "Admin1GUID", "Admin2GUID")))
  
  # Test error handling
  testthat::expect_error(
    join_and_check_mismatches(
      sample_data, shapefile_data,
      geo_name_cols = c("Admin0Name", "Admin1Name"),
      geo_id_cols = geo_id_cols,
      shapefile_name_cols = shapefile_name_cols,
      shapefile_id_cols = shapefile_id_cols,
      id_col = id_col
    ),
    paste0("The number of name columns must match, ",
           "and ID columns must match except for ADM0_GUID.")
  )
  
})
})


testthat::test_that("check_data function works correctly", {
  
  suppressMessages({ 
    
    # Create a sample dataset
    sample_data <- data.frame(
      ID = 1:5,
      Admin0Name = c("Country1", "Country2", "Country1", "Country2", NA),
      Admin1Name = c("State1", "State2", "State1", "State2", "State3"),
      Admin2Name = c("City1", "City2", "City3", "City4", "City5"),
      Admin0GUID = c("A1", "A2", "A1", "A2", "A3"),
      Admin1GUID = c("B1", "B2", "B1", "B2", "B3"),
      Admin2GUID = c("C1", "C2", "C3", "C4", "C5"),
      Latitude = c(10.1234, 20.5678, 30.9012, 40.3456, 50.7890),
      Longitude = c(-5.4321, -15.8765, -25.2109, -35.6543, -45.0987),
      Date1 = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", 
                        "2023-05-01")),
      Date2 = as.Date(c("2023-06-01", "2023-07-01", "2023-08-01", "2023-09-01", 
                        "2023-10-01"))
    )
    
    # Run the check_data function
    result <- check_data(
      data = sample_data,
      id_col = "ID",
      geo_name_cols = c("Admin0Name", "Admin1Name", "Admin2Name"),
      geo_id_cols = c("Admin0GUID", "Admin1GUID", "Admin2GUID"),
      date_cols = c("Date1", "Date2"),
      date_pair_cols = list(c("Date1", "Date2")),
      lat_long_cols = c("Latitude", "Longitude")
    )
    
    # Test the structure of the result
    testthat::expect_type(result, "list")
    testthat::expect_true("missing_data" %in% base::names(result))
    testthat::expect_true("date_results" %in% base::names(result))
    testthat::expect_true("date_results_pairs" %in% base::names(result))
    testthat::expect_true("geo_hierarchy" %in% base::names(result))
    testthat::expect_true("coordinate_checks" %in% base::names(result))
    
    # Test specific results
    testthat::expect_equal(result$total_rows, 5)
    testthat::expect_equal(result$total_columns, 11)
    testthat::expect_equal(result$duplicated_rows, 0)
    
    # Test missing data results
    testthat::expect_equal(base::sum(result$missing_data$`Missing Count`), 1)
    testthat::expect_equal(
      result$missing_data$Column[base::which(
        result$missing_data$`Missing Count` == 1)], "Admin0Name")
    
    # Test date results
    testthat::expect_equal(base::nrow(result$date_results), 2)
    testthat::expect_equal(base::sum(result$date_results$Missing), 0)
    
    # Test geo hierarchy results
    testthat::expect_equal(base::nrow(result$geo_hierarchy), 2)
    
    # Test coordinate checks
    testthat::expect_true(base::nrow(result$coordinate_checks) == 1)
    
  })
  
})
# 
# testthat::test_that("create_summary_table works correctly", {
#   
#   # Create mock data
#   mock_data$summary_data <- list(
#     total_rows = 100,
#     total_columns = 10,
#     duplicated_rows = 5,
#     total_null_columns = 1,
#     wpv1_count = 20,
#     cvdpv1_count = 15,
#     cvdpv2_count = 10,
#     coordinate_checks = dplyr::tibble(
#       Missing = 2,
#       `Out of Bounds` = 1,
#       `Low Precision` = 3
#     ) |> as.data.frame(),
#     geo_mismatches = dplyr::tibble(
#       `Geo Column Type` = c("Admin0Name", "Admin1Name"),
#       `Missing in Shapefile` = c(5, 10)
#     ) |> as.data.frame(),
#     geo_hierarchy = dplyr::tibble(
#       `Column Combination` = c("Admin0Name ~ Admin1Name", 
#                                "Admin0GUID ~ Admin1GUID"),
#       `Non-unique Count` = c(2, 1)
#     ) |> as.data.frame(),
#     date_results = dplyr::tibble(
#       Column = c("Date1", "Date2"),
#       Missing = c(3, 2),
#       `Non-date` = c(1, 0),
#       Invalid = c(2, 1)
#     ) |> as.data.frame(),
#     date_results_pairs = dplyr::tibble(
#       Variable = "Date1 - Date2",
#       `Invalid Order` = 4
#     ) |> as.data.frame()
#   )
#   
#   
#   mock_data$id_data <- "ID"
#   
#   # Run the function
#   result <- create_summary_table(mock_data)
#   
#   # Test the structure of the result
#   testthat::expect_s3_class(result, "data.frame")
#   testthat::expect_true(
#     all(c("header", "Column", "count") %in% base::colnames(result)))
#   
#   # Test specific results
#   testthat::expect_equal(base::nrow(result), 19)
#   
#   # Test Overall Data Summary
#   overall_summary <- result[result$header == "Overall Data Summary", ]
#   testthat::expect_equal(overall_summary$count, c(5, 100))
#   
#   # Test Virus Detections
#   virus_detections <- result[result$header == "Virus Detections", ]
#   testthat::expect_equal(virus_detections$count, c(20, 15, 10))
#   
#   # Test Coordinate Checks
#   coordinate_checks <- result[result$header == "Coordinate Checks", ]
#   testthat::expect_equal(coordinate_checks$count, c(3, 2, 1))
#   
#   # Test Geographical Mismatches
#   geo_mismatches <- result[result$header == "Geographical Mismatches", ]
#   testthat::expect_equal(geo_mismatches$count, c(5, 10))
#   
#   # Test Geographical Hierarchy
#   geo_hierarchy <- result[result$header == "Geographical Hierarchy", ]
#   testthat::expect_equal(geo_hierarchy$count, c(2, 1))
#   
#   # Test Date Checks
#   date_checks <- result[base::startsWith(
#     as.vector(result$header[13]), "Date Check:"), ]
#   testthat::expect_equal(base::nrow(date_checks), 19)
#   
# })
