#' Title
#'
#' @param ref_dataframe
#' @param target_dataframe
#'
#' @return
#' @export
#'
#' @examples
#'
#'
prep_match_datatypes <- function(ref_dataframe, target_dataframe) {
  data_types <- tibble::tibble(
    Col_names = names(ref_dataframe),
    data_type = sapply(ref_dataframe, class)
  )
  # Loop through the columns in AFP Central
  for (col in names(target_dataframe)) {
    # check that the column is in the esa_data_type tibble
    if (col %in% data_types$Col_names) {
      # get the datatype from the tibble
      col_type <- data_types$data_type[data_types$Col_names == col]
      # check what the data type is and use that to convert the AFP
      # central datatype
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
      } else if (col_type == 'c("POSIXct", "POSIXt")') {
        target_dataframe <- target_dataframe |>
          dplyr::mutate(!!col == as.POSIXct(!!rlang::sym(col),
                                            format = "YYYY-MM-DD"))
      }
    }
  }
  # Return the target dataframe
  return(target_dataframe)
}
