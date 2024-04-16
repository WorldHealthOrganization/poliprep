#' Match naming conventions
#'
#' Given two dataframes with variables in common but with diffrent naming 
#' conventions (ex: CamelCase vs camelcase), this function will find the common 
#' varibles and rename those in the target table with those from the reference 
#' table.
#' 
#' @param ref_dataframe reference dataframe to be matched to
#' @param target_dataframe target dataframe that needs to be matched to
#'        ref_dataframe
#'
#' @return returns the target dataframe with common names matching the reference
#'        dataframe
#' @export
#'
#' @examples
#'
#' # Create reference and target dataframes
#' ref_dataframe <- tibble::tibble(CountryName = c("Cameroon",
#'                                                 "Nigeria",
#'                                                "Tchad",
#'                                                "Niger"),
#'                                NameOfCapital = c("Yaounde",
#'                                                  "Abuja",
#'                                                  "Ndjamena",
#'                                                  "Niamey"))
#'
#' target_dataframe <- tibble::tibble(countryname = c("South Sudan",
#'                                                    "Kenya",
#'                                                    "Ethiopia",
#'                                                    "CAR"),
#'                                    nameofcapital = c("Juba",
#'                                                      "Nairobi",
#'                                                      "Addis Ababa",
#'                                                      "Bangui"))
#' # Check the names matching. Expect False
#' names(ref_dataframe) == names(target_dataframe)
#'
#' # Match the dataframes names
#' target_dataframe <- prep_match_names(ref_dataframe, target_dataframe)
#'
#' # Check the names matching. Expect True
#' names(ref_dataframe) == names(target_dataframe)


prep_match_names <- function(ref_dataframe, target_dataframe) {
  # get the column names of the reference dataframe
  refNames <- names(ref_dataframe)
  # get the column names of the target dataframe
  targetNames <- names(target_dataframe)
  # find the common columns between the reference and the target dataframes
  common_cols <- intersect(toupper(targetNames), toupper(refNames))
  # loop through the common columns and rename the target dataframe columns
  for (col in common_cols) {
    # get the index of the col in the target dataframe column names
    col_index1 <- which(tolower(targetNames) == tolower(col))
    # get the index of the col in the target reference column names
    col_index2 <- which(tolower(refNames) == tolower(col))
    # get the corresponding column name from the reference dataframe
    new_col_name <- refNames[col_index2]
    # rename the target dataframe column using the new name from the reference
    # dataframe
    names(target_dataframe)[col_index1] <- new_col_name
  }
  # Return the target dataframe
  return(target_dataframe)
}