#' Check Status of API Response
#'
#' This function checks the API response status code. It handles various HTTP
#' status codes by providing specific error messages. It returns `TRUE` for
#' successful responses (status code 200). For error conditions like 400, 403,
#' 500, etc., it stops execution with an error message, aiding in debugging and
#' issue diagnosis.
#'
#' @param response The API response.
#'
#' @return `TRUE` for a successful response (status code 200). If the status
#'         code indicates an error (e.g., 400, 403, 500), the function stops
#'         and returns a specific error message.
#'
#' @examples
#' # response <- check_status_api(response)
check_status_api <- function(response) {
  
  # get resoinse code 
  response_status_code <- httr::status_code(response)
  # get http code
  code <- httpcode::http_code(response_status_code)
  
  if (code$status_code != 200) {
    stop(
      glue::glue(
        "{code$status_code}: {code$explanation}",
        "\n For a full explanation of this error call: ",
        "httpcode::http_code({code$status_code}, verbose = TRUE)"
      )
    )
  }
}

#' Fetch ONA API Data
#'
#' Retrieves data from the ONA API endpoints to display available forms and 
#' datasets. It initially, uses given API token for authorization and checks the 
#' initial API response status. Then it validates the base URL and constructs 
#' the full API endpoint URL to retrieve a dataframe of all the available for 
#' forms/data.
#'
#' @param base_url The base URL for the ONA API; defaults to 
#'            'https://api.whonghub.org'.
#' @param api_token API token for authentication.
#'
#' @return Data frame of the API endpoint data.
#' 
#' @export  
#' @examples
#' # prep_ona_data_endpoints(api_token = "your_api_token_here")
prep_ona_data_endpoints <- function(
    base_url = "https://api.whonghub.org", api_token) {
  
  # check base url validity
  base_url_pattern <- "^(https?://[^/]+).*"
  if ( grepl(base_url_pattern, base_url)){
    base_url <- sub(base_url_pattern, "\\1", base_url)
  } else {
    stop(paste("Error: ", base_url, " is not a valid base url"))
  }
  
  # set up URL
  api_url <- paste0(base_url,"/api/v1/data")
  
  # Validate base url link first -----------------------------------------------
  
  # get head before download
  response <- httr::HEAD(
    base_url, 
    config = httr::add_headers(Authorization = paste("Token", api_token)))
  
  # check status of call
  check_status_api(response)
  
  # Validate url link before downloading ---------------------------------------
  
  # get one data endpoint df 
  response <- httr::GET(
    api_url, 
    config = httr::add_headers(Authorization = paste("Token", api_token))) |> 
    httr::content("text", encoding = "UTF-8") |> 
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
  
  return(response)
}

#' Get a Page of Data from an API
#'
#' This function retrieves a single page of data from a specified API endpoint.
#'
#' @param api_url The base URL of the API endpoint.
#' @param api_token Authentication token for API access, prefixed with "Token".
#' @param start An integer specifying the starting point for data retrieval.
#'              Defaults to 0.
#' @param api_limit An integer specifying the maximum number of items to 
#'                  retrieve per page.
#' @param times The number of attempts to retry the request in case of failure.
#'              Defaults to 12.
#' @return A list containing the retrieved data parsed from JSON format. If
#'         the specified content is not found or an error occurs, the function
#'         stops and returns an error message.
get_ona_page <- function(api_url, api_token, start = 0, api_limit, times = 12) {
  tryCatch({
    resp <- httr::RETRY(
      verb = "GET",
      url = api_url,
      config = httr::add_headers(Authorization = paste("Token", api_token)),
      query = list(
        start = start,
        limit = api_limit
      ),
      times = times,  
      pause_cap = 180,
      httr::progress(type = "down")
    )
    
    content <- httr::content(resp, "text", encoding = "UTF-8")
    jsonlite::fromJSON(content, simplifyDataFrame = TRUE)
  }, error = function(e) {
    message("Error encountered: ", e$message)
    NULL
  })
}

#' Get Data from ONA API
#'
#' This function getes data for a specified form from the ONA API using a 
#' provided API token and form ID. It returns the data in a structured format 
#' if the request is successful. If the request fails, the function stops and 
#' returns an error message indicating the failure reason.
#'
#' @param base_url A string specifying the URL for the ONA API, 
#'                could be https://esurv.afro.who.int/api/v1/data eSurv.
#'                Default is https://api.whonghub.org/api/v1/data
#' @param form_id A string or numeric value as the dataset id. 
#'                Could be found by looking at the URL of the form on ONA.
#' @param api_token A string specifying the API token for ONA
#'
#' @return A list containing the data geted from the ONA API.
#'
#' @examples
#' # base_url <- https://api.ona.io/api/v1/data
#' # api_token <- "your_api_token_here"
#' # form_id <- 123456
#' # data <- get_ona_data(base_url, form_id, api_token)
#' @export
#'
#' @seealso \url{https://api.ona.io/api/v1/data/} for more info on ONA API
get_ona_data <- function(
    base_url = "https://api.whonghub.org", form_id, api_token) {
  
  # check base url validity
  base_url_pattern <- "^(https?://[^/]+).*"
  if ( grepl(base_url_pattern, base_url)){
    base_url <- sub(base_url_pattern, "\\1", base_url)
  } else {
    stop(paste("Error: ", base_url, " is not a valid base url"))
  }
  
  # set up URL
  api_url <- paste0(base_url,"/api/v1/data/", form_id)
  
  # api page limit
  api_limit <- 100000
  
  process_id <- cli::cli_process_start(
    paste0("geting form '", form_id, "' from ONA server")); cat("\n")
  
  results <- list()
  get_next_page <- TRUE
  start <- 0
  
  # Check if the form id is available for download -----------------------------
  
  resp_data <- prep_ona_data_endpoints(
    base_url = base_url,
    api_token = api_token)
  
  if (!(form_id %in% unique(resp_data$id))) {
    cli::cli_abort(
      paste0("Form IDs ", 
             toString(form_id), 
             " not found. Use `prep_ona_data_endpoints()` ",
             "to check available forms for download.")
    )
  }
  
  # Download data (use pagination if necessary) --------------------------------
  
  # look through get_next_page function to deal with pagination
  while (get_next_page) {
    current_page <- get_ona_page(
      api_url, api_token, start, api_limit = api_limit)  |>
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

