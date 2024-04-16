# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# test the function
test_that("Match variables naming convention within two dataframes", {
  # Create reference and target dataframes
  ref_dataframe <- tibble::tibble(CountryName = c("Cameroon", 
                                                  "Nigeria", 
                                                  "Tchad", 
                                                  "Niger"), 
                                  NameOfCapital = c("Yaounde", 
                                                    "Abuja", 
                                                    "Ndjamena", 
                                                    "Niamey"))
  
  target_dataframe <- tibble::tibble(countryname = c("South Sudan", 
                                                     "Kenya", 
                                                     "Ethiopia", 
                                                     "CAR"), 
                                     nameofcapital = c("Juba", 
                                                       "Nairobi", 
                                                       "Addis Ababa", 
                                                       "Bangui"))
  # match the naming conventions
  target_dataframe <- prep_match_names(ref_dataframe, target_dataframe)
  
  # Test the names matching. Expect c(TRUE, TRUE)
  expect_equal(names(ref_dataframe) == names(target_dataframe), c(TRUE, TRUE))
})
