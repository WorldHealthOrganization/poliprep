#' Match Data Types Between Dataframes
#'
#' This function aligns the data types of columns in the target dataframe
#' to match those in the reference dataframe. It ensures consistency in
#' data types across dataframes, facilitating accurate data analysis and
#' manipulation.
#'
#' @param ref_dataframe A dataframe serving as the reference for data types.
#' @param target_dataframe A dataframe whose columns' data types will be
#'        modified to match those of `ref_dataframe`.
#'
#' @return A modified copy of `target_dataframe` with data types aligned
#'         to those of `ref_dataframe`.
#' @export
#'
#' @examples
#' ref_df <- tibble::tibble(
#'   integer_col = 1:3,
#'   character_col = c("a", "b", "c"),
#'   numeric_col = c(1.1, 2.2, 3.3)
#' )
#' target_df <- tibble::tibble(
#'   integer_col = c("1", "2", "3"), # should be integer
#'   character_col = 1:3, # should be character
#'   numeric_col = c("1.1", "2.2", "3.3") # should be numeric
#' )
#' matched_df <- prep_match_datatypes(ref_df, target_df)
#'
prep_match_datatypes <- function(ref_dataframe, target_dataframe) {
  data_types <- tibble::tibble(
    Col_names = names(ref_dataframe),
    data_type = sapply(ref_dataframe, class)
  )
  for (col in names(target_dataframe)) {
    if (col %in% data_types$Col_names) {
      col_type <- data_types$data_type[data_types$Col_names == col]
      if (col_type == "integer") {
        target_dataframe <- target_dataframe |>
          dplyr::mutate(!!col := as.integer(!!rlang::sym(col)))
      } else if (col_type == "character") {
        target_dataframe <- target_dataframe |>
          dplyr::mutate(!!col := as.character(!!rlang::sym(col)))
      } else if (col_type == "numeric") {
        target_dataframe <- target_dataframe |>
          dplyr::mutate(!!col := as.numeric(!!rlang::sym(col)))
      } else if (col_type == "logical") {
        target_dataframe <- target_dataframe |>
          dplyr::mutate(!!col := as.logical(!!rlang::sym(col)))
      } else if (col_type == "Date") {
        target_dataframe <- target_dataframe |>
          dplyr::mutate(!!col := as.Date(!!rlang::sym(col)
          ))
      } else if (col_type == 'c("POSIXct", "POSIXt")') {
        target_dataframe <- target_dataframe |>
          dplyr::mutate(!!col == as.POSIXct(!!rlang::sym(col),
                                            format = "YYYY-MM-DD"
          ))
      }
    }
  }
  return(target_dataframe)
}
