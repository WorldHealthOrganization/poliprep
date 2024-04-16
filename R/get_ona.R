#' Get Data from ONA API
#'
#' This function getes data for a specified form from the ONA API using a 
#' provided API token and form ID. It returns the data in a structured format 
#' if the request is successful. If the request fails, the function stops and 
#' returns an error message indicating the failure reason.
#'
#' @param api_url A string specifying the URL for the ONA API, 
#'                could be https://esurv.afro.who.int/api/v1/data eSurv.
#'                Default is https://api.whonghub.org/api/v1/data
#' @param form_id A string or numeric value as the dataset id. 
#'                Could be found by looking at the URL of the form on ONA.
#' @param api_token A string specifying the API token for ONA
#'
#' @return A list containing the data geted from the ONA API.
#'
#' @examples
#' # api_url <- https://api.ona.io/api/v1/data
#' # api_token <- "your_api_token_here"
#' # form_id <- 123456
#' # data <- get_ona_data(api_url, form_id, api_token)
#' @export
#'
#' @seealso \url{https://api.ona.io/api/v1/data/} for more info on ONA API
get_ona_data <- function(
    base_url = "https://api.whonghub.org", form_id, api_token) {
  
  base_url_pattern <- "^(https?://[^/]+).*"
  # check base url validity
  try(
    if(grepl(base_url_pattern, base_url)){
      base_url <- sub(base_url_pattern, "\\1", base_url)
    }else{
      stop(paste("Error: ", base_url, " is not a valid base url"))
    }
  )
  
  # set up URL
  api_url <- paste0(base_url,"/api/v1/data/", form_id)
  
  # api page limit
  api_limit <- 100000
  
  process_id <- cli::cli_process_start(
    paste0("geting form '", form_id, "' from ONA server")); cat("\n")
  
  results <- list()
  get_next_page <- TRUE
  start <- 0
  
  #' function to get a Page of Data from an API
  get_ona_page <- function(start = 0, api_limit) {
    
    resp <- httr::RETRY(
      verb = "GET",
      url = api_url,
      config = httr::add_headers(Authorization = paste("Token", api_token)),
      query = list(
        start = start,
        limit = api_limit
      ),
      times = 12,
      pause_cap = 180,
      httr::progress(type = "down")
    )
    
    if (!(resp$status_code %in% 200:299)) {
      stop("Unsuccessful response from server")
    }
    
    content <- httr::content(resp, "text", encoding = "UTF-8")
    if ("results" %in% names(content)) content <- content$results
    
    jsonlite::fromJSON(content)
  }
  
  # look through get_next_page function to deal with pagination
  while (get_next_page) {
    current_page <- get_ona_page(start, api_limit = api_limit)  |>
      as.data.frame() |> 
      dplyr::mutate(
        dplyr::across(
          tidyselect::everything(), as.character))
    
    # join the paginated results
    results <- dplyr::bind_rows(results, current_page)
    if (nrow(current_page) < api_limit) {
      get_next_page <- FALSE
    } else {
      start <- start + api_limit
    }
  }
  
  # Check for unique submission IDs
  if (dplyr::n_distinct(results$`_id`) != nrow(results)) {
    stop("Number of submission IDs is not the same as the number of records.")
  }
  
  # praise for successful results
  praise_emoji <- function() {
    if (!cli::is_utf8_output()) {
      return("")
    }
    
    emoji <- c(
      "\U0001f600", 
      "\U0001f973", 
      "\U0001f638",
      "\U0001f308", 
      "\U0001f947", 
      "\U0001f389", 
      "\U0001f38a" 
    )
    sample(emoji, 1)
  }
  
  cat("\n"); cli::cli_process_done(
    process_id, 
    msg_done = "Download complete! {praise_emoji()}")
  
  return(results)
}