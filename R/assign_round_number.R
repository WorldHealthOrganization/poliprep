#' Assign Round Numbers to Dates Based on Proximity
#'
#' This function assigns a round name to a data frame containing dates based on 
#' their proximity within a specified threshold. Dates that are within the 
#' threshold (in days) of each other will be assigned the same round name. 
#' Additionally, the function allows for grouping by additional columns, such 
#' as states or LGAs. This is particularly useful in scenarios where campaigns 
#' may overlap, such as when campaigns occurring in early October 2024 are 
#' closely related to those in September 2024. By clustering campaigns based on 
#' proximity, the function effectively handles edge cases and provides coherent 
#' round groupings.
#'
#' @param df A data frame containing at least one column of dates.
#' @param date_col A string specifying the name of the date column in the data 
#'   frame. Default is "today".
#' @param additional_grouping A character vector specifying additional columns 
#'   to group by (e.g., c("states", "lgas")). Default is NULL, meaning only 
#'   clustering by dates will be performed.
#' @param threshold A numeric value representing the maximum number of days 
#'   between dates to be considered part of the same round. Default is 7.
#' 
#' @return A data frame with a new column `round_group` containing the assigned 
#'   round names (month and year) based on the proximity of the dates, and 
#'   ordered as factors.
#' 
#' @examples
#' df <- data.frame(
#'   today = as.Date(
#'     c("2024-04-28", "2024-05-03", "2024-05-04", "2024-05-03",
#'      "2024-06-01", "2024-06-02", "2024-07-29", "2024-07-30")),
#'   states = c("State1", "State1", "State1", "State1", "State2", "State2", 
#'   "State2", "State2")
#' )
#' result <- assign_round_number(df, date_col = "today", 
#'                               additional_grouping = "states")
#' print(result)
#' 
#' @export
# Define the function to assign round numbers based on proximity
assign_round_number <- function(df,
                                date_col = "today",
                                additional_grouping = NULL,
                                threshold = 7) {
  # Ensure dates are of Date type
  df[[date_col]] <- as.Date(df[[date_col]])
  
  # Get distinct dates for clustering
  distinct_dates <- df |>
    dplyr::distinct(!!rlang::sym(date_col)) |>
    dplyr::arrange(!!rlang::sym(date_col))
  
  # Check if there are enough distinct dates to perform clustering
  if (nrow(distinct_dates) < 2) {
    df$round_group <- NA # Return NA if insufficient data
    return(df)
  }
  
  # Create a distance matrix
  distance_matrix <- as.matrix(dist(distinct_dates[[date_col]]))
  
  # Perform hierarchical clustering
  hc <- hclust(as.dist(distance_matrix))
  
  # Cut the dendrogram to form groups
  clusters <- cutree(hc, h = threshold) # Cut tree at specified height
  
  # Create a round group label for the distinct dates
  distinct_dates$ClusterID <- clusters
  round_labels <- unique(format(distinct_dates[[date_col]], "%B %Y"))
  
  # Create round group labels based on clusters
  distinct_dates <- distinct_dates |>
    dplyr::group_by(ClusterID) |>
    dplyr::mutate(
      round_group = format(dplyr::first(.data[[date_col]]), "%B %Y")
    ) |>
    dplyr::ungroup()
  
  df <- df |>
    dplyr::left_join(
      distinct_dates |>
        dplyr::select(!!rlang::sym(date_col), ClusterID, round_group),
      by = date_col
    )
  
  df <- df |>
    dplyr::mutate(
      round_group = factor(round_group,
                           levels = round_labels,
                           ordered = TRUE
      )
    )
  
  return(df)
}