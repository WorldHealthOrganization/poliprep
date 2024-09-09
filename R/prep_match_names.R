#' Match Naming Conventions Between df
#'
#' Standardizes variable names between two df by renaming variables in
#' the target dataframe to match the naming conventions in the reference
#' dataframe. This function is particularly useful for ensuring seamless
#' integration and minimizing errors during data merging processes, especially
#' when dealing with data from different sources that use different naming
#' conventions (e.g., CamelCase vs snake_case).
#'
#' @param ref_dataframe A dataframe with reference variable names.
#' @param target_dataframe A dataframe to modify variable names to match
#'        `ref_dataframe`.
#' @param report Boolean value indicating if processing report should be printed
#'               or not. Defaults to TRUE 
#'
#' @return A modified copy of `target_dataframe` with standardized variable
#'        names.
#'
#' @examples
#' # Define reference and target df with different naming conventions
#' ref_dataframe <- tibble::tibble(
#'   CountryName = c("Cameroon", "Nigeria", "Tchad", "Niger"),
#'   NameOfCapital = c("Yaounde", "Abuja", "Ndjamena", "Niamey")
#' )
#'
#' target_dataframe <- tibble::tibble(
#'   countryname = c("South Sudan", "Kenya", "Ethiopia", "CAR"),
#'   nameofcapital = c("Juba", "Nairobi", "Addis Ababa", "Bangui")
#' )
#'
#' # Before matching: expect FALSE
#' all(names(ref_dataframe) == names(target_dataframe))
#'
#' # Apply the name matching function
#' target_dataframe <- prep_match_names(ref_dataframe, target_dataframe)
#'
#' # After matching: expect TRUE
#' all(names(ref_dataframe) == names(target_dataframe))
#' @export
prep_match_names <- function(ref_dataframe, target_dataframe, report = TRUE) {
  
  # Step 1): get the column names of the ref and target df
  ref_names <- names(ref_dataframe)
  target_names <- names(target_dataframe)
  
  # Step 2): check if reference names contain spaces or punctuation
  # if so names are clean, clean both ref and target names
  if (all(grepl("^[[:alnum:]]+$", ref_names))) {
    ref_names <- gsub("[[:punct:][:space:]]", "", ref_names)
    target_names <- gsub("[[:punct:][:space:]]", "", target_names)
  }
  
  # Step 3): find the common columns between the ref and the target df
  common_cols <- intersect(toupper(target_names), toupper(ref_names))
  
  # Step 4): loop through the common columns and rename the target
  # dataframe columns
  for (col in common_cols) {
    # get the index of the col in the target dataframe column names
    col_index1 <- which(tolower(target_names) == tolower(col))
    # get the index of the col in the target reference column names
    col_index2 <- which(tolower(ref_names) == tolower(col))
    # get the corresponding column name from the reference dataframe
    new_col_name <- ref_names[col_index2]
    # rename the target dataframe column using the new name from the reference
    # dataframe
    names(target_dataframe)[col_index1] <- new_col_name
  }
  
  # Step 5): Return processing report to user
  # target columns with no match in ref columns
  if (report == TRUE) {
    no_match_cols <-
      setdiff(names(target_dataframe), names(ref_dataframe))
    if (length(no_match_cols) > 0) {
        cat(glue::glue(
          "No match found for the following {length(no_match_cols)} column(s)",
          " in the tarrget df: {crayon::red(no_match_cols)}\n"))
    } else {
      cat(glue::glue(
        "All columns were successfully matched and renamed!\n"))
      
    }
  }
  
  # Return the target dataframe
  return(target_dataframe)
}
