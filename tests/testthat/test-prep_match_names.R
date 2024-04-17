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
    label = "Doesn't handle UPPERlower naming convention"
  )
})
