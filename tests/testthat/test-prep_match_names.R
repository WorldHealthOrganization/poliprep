# test the function
testthat::test_that("Test prep_match_names with different names", {
  # reference df with more detailed information
  ref_dataframe <- tibble::tibble(
    CountryName = c("Cameroon", "Nigeria", "Tchad"),
    NameOfCapital = c("Yaounde", "Abuja", "Ndjamena"),
    ProvinceName = c("Centre", "FCT Abuja", "Guera"),
    DistrictName = c("Mfoundi", "Abuja", "Chari-Baguirmi"),
    SiteName = c("Alpha", "Beta", "Gamma")
  )
  
  # target df with similar content but varied naming conventions
  target_dataframe <- tibble::tibble(
    COUNTRYNAME = c("Cameroon", "Nigeria", "Tchad"), # ALLCAPS
    nameofcapital = c("Yaounde", "Abuja", "Ndjamena"), # sentence
    province_Name = c("Centre", "FCT Abuja", "Guera"), # lower_UPPER
    DISTRICT_name = c("Mfoundi", "Abuja", "Chari-Baguirmi"), # UPPERlower
    siteName = c("Alpha", "Beta", "Gamma") # lowerCamel
  )
  
  # match the naming conventions
  improved_targetd_df <- prep_match_names(ref_dataframe, target_dataframe)
  
  # Test whether prep_match_names can handle the different naming conventions
  testthat::expect_true(
    "CountryName" %in% colnames(improved_targetd_df),
    label = "Doesn't handle ALL_CAPS naming convention"
  )
  
  testthat::expect_true(
    "NameOfCapital" %in% colnames(improved_targetd_df),
    label = "Doesn't handle sentence naming convention"
  )
  
  testthat::expect_true(
    "ProvinceName" %in% colnames(improved_targetd_df),
    label = "Doesn't handle lowerUPPER naming convention"
  )
  
  testthat::expect_true(
    "DistrictName" %in% colnames(improved_targetd_df),
    label = "Doesn't handle UPPERlower naming convention"
  )
  
  testthat::expect_true(
    "SiteName" %in% colnames(improved_targetd_df),
    label = "Doesn't handle UPPERlower naming convention")
  
  
  # match the naming conventions
  improved_targetd_df2 <- prep_match_names(
    ref_dataframe, target_dataframe, report = FALSE)
  
})

testthat::test_that("Unmatched and matched columns are correctly reported", {
  # Define reference and target dataframes with partial match
  ref_df <- tibble::tibble(
    CountryName = c("Cameroon", "Nigeria"),
    Population = c(25, 200)
  )
  
  target_df <- tibble::tibble(
    country_name = c("South Sudan", "Kenya"),
    population = c(11, 53),
    GDP = c(10, 95)
  )
  
  # Expected output messages
  expected_messages <- c(
    "No match found for the following 1 column(s) in the tarrget df: GDP"
  )
  
  # Run the function and capture the output
  output <- testthat::capture_output_lines(
    prep_match_names(ref_df, target_df)
  )
  
  # Check if the expected messages are present in the output when 
  # all names are not matched
  expect_equal(output, expected_messages)
  
  
  expected_messages_2 <- c(
    "All columns were successfully matched and renamed!"
  )
  
  output_2 <- testthat::capture_output_lines(
    prep_match_names(ref_df, target_df[, - 3])
  )
  
  # Check if the expected messages are present in the output when 
  # all names are matched
  testthat::expect_equal(output_2, expected_messages_2)
  
}
)