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

#' Validate and Normalize a Base URL
#'
#' This function checks if the provided `base_url` adheres to a standard URL 
#' format, specifically ensuring it starts with either `http` or `https` and 
#' contains no path elements. 
#'
#' @param base_url A string representing the base URL to be validated.
#'
#' @return A normalized base URL containing only the protocol and the domain.
#' If the URL is not valid, the function stops and returns an error message.
validate_base_url <- function(base_url) {
  base_url_pattern <- "^(https?://[^/]+).*"
  if (!grepl(base_url_pattern, base_url)) {
    stop(paste("Error: ", base_url, " is not a valid base URL"))
  }
  sub(base_url_pattern, "\\1", base_url)
}

#' Retrieve Paginated Data from API
#'
#' Fetches data from a specified API URL in a paginated manner using the 
#' provided API token and page limit. Continues to request data until all pages 
#' are retrieved.
#'
#' @param api_url The API URL from which data should be fetched.
#' @param api_token Authorization token for API access.
#'
#' @return A data frame combining all pages of data retrieved from the API.
#'
#' @examples
#' # api_url <- "https://api.example.com/data"
#' # api_token <- "your_api_token_here"
#' # data <- get_paginated_data(api_url, api_token)
get_paginated_data <- function(api_url, api_token) {
  api_limit = 100000
  results <- list()
  get_next_page <- TRUE
  start <- 0
  while (get_next_page) {
    current_page <- get_ona_page(api_url, api_token, start, api_limit) |> 
      as.data.frame() |> 
      dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
    
    results <- dplyr::bind_rows(results, current_page)
    if (nrow(current_page) < api_limit) {
      get_next_page <- FALSE
    } else {
      start <- start + api_limit
    }
  }
  results
}

#' Generate a Random Emoji for Successful Results
#'
#' Selects a random emoji from a predefined list to use as a celebratory message
#' in user interfaces or logs where UTF-8 output is supported. 
#' Taken from testthat package.
#'
#' @return A single emoji character if UTF-8 output is supported; otherwise, 
#' an empty string.
#'
#' @examples
#' # emoji <- praise_emoji()
#'
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
#' @param selected_columns Selected columns to download. Default is NULL.
#'    It's in stringed list like c("year", "form_id")
#' @return A list containing the data geted from the ONA API.
#'
#' @examples
#' # base_url <- https://api.ona.io/api/v1/data
#' # api_token <- "your_api_token_here"
#' # form_id <- 123456
#' # data <- get_ona_data(base_url, form_id, api_token)
#' @export
#' @seealso \url{https://api.ona.io/api/v1/data/} for more info on ONA API
get_ona_data <- function(base_url = "https://api.whonghub.org", form_id, 
                         api_token, selected_columns = NULL) {
  
  
  # check base url validity
  base_url <- validate_base_url(base_url)
  
  # set up URL
  api_url <- paste0(base_url,"/api/v1/data/", form_id)
  
  process_id <- cli::cli_process_start(
    paste0("geting form '", form_id, "' from ONA server")); cat("\n")
  
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
  
  # If getting multiple columns, include these in url ---------------------------
  
  if (!is.null(selected_columns)) {
    # Construct query parameters with quotes around column names
    query_params <- list(
      fields = paste0(
        '[', 
        paste(
          sapply(
            selected_columns, 
            function(col) paste0('"', col, '"')), 
          collapse = ','), ']'))
    
    # Build the full URL with query parameters using modify_url from httr
    api_url <- httr::modify_url(api_url, query = query_params) 
  }
  
  # Download data (use pagination if necessary) --------------------------------
  
  # Retrieve data handling pagination
  results <- get_paginated_data(api_url, api_token) |> 
    # drop any empty columns
    dplyr::select(
      dplyr::where(
        ~ any(!is.na(.))))
  
  # Return output message and save results -------------------------------------
  
  # praise for successful results
  cat("\n"); cli::cli_process_done(
    process_id, 
    msg_done = "Download complete! {praise_emoji()}")
  
  return(results)
}

#' Get Data from ONA for Multiple Forms
#'
#' This function retrieves data for a specified form from the ONA API using a 
#' provided API token and constructs a unique key for each dataset. It returns 
#' the data in a structured format if the request is successful.
#'
#' @param base_url The base URL for the ONA API; defaults to 
#'            'https://api.whonghub.org'.
#' @param form_ids A vector containing form id number to identify each form.
#' @param api_token A string specifying the API token for ONA.
#' @param selected_columns Selected columns to download. Default is NULL. 
#'    It's in stringed list like c("year", "form_id")
#' 
#' @return A data frame containing the combined data from all specified form 
#'        IDs, and includes from_id column.
#' @examples
#' # api_token <- "your_api_token_here"
#' # data <- get_multi_ona_data(form_ids = c(623, 432, 643), api_token)
#' @importFrom foreach %dopar%
#' @export
get_multi_ona_data <- function(base_url = "https://api.whonghub.org", 
                               form_ids, api_token, selected_columns = NULL) {
  
  # Check if the form IDs are available for download ---------------------------
  resp_data <- prep_ona_data_endpoints(
    base_url = base_url,
    api_token = api_token
  )
  
  if (!all(form_ids %in% unique(resp_data$id))) {
    missing_ids <- form_ids[!form_ids %in% unique(resp_data$id)]
    cli::cli_abort(
      paste0("Form IDs ", 
             toString(missing_ids), 
             " not found. Use `prep_ona_data_endpoints()` ",
             "to check available forms for download.")
    )
  }
  
  # Fetch data in parallel for each form ID ------------------------------------
  
  # register cluster 
  doParallel::registerDoParallel(cores = (parallel::detectCores() - 2)) 
  future::plan(future::multisession) 
  
  combined_data <- 
    foreach::foreach(
      form_id = form_ids, .combine = 'bind_rows') %dopar% {
        data <- get_ona_data(form_id = form_id, 
                             api_token = api_token, 
                             selected_columns = selected_columns)
        # add form_id as a new column in each df
        dplyr::mutate(data, form_id_num = form_id)
      } 
  
  # drop any empty columns
  combined_data <- combined_data|> 
    dplyr::select(
      dplyr::where(
        ~ any(!is.na(.))))
  
  # stop the parallel backend when done
  doParallel::stopImplicitCluster()		
  
  return(combined_data)
}

#' Generate URLs for Data Retrieval
#'
#' This function generates a list of URLs based on a given set of form IDs. 
#' If data exists for a form ID, the URL will include the last edited date 
#' parameters; otherwise, the URL will only contain the form ID.
#'
#' @param full_data The full dataset containing form IDs and last edited dates.
#' @param file_path The path to the directory containing the data file.
#' @param data_file_name The name of the data file (without extension).
#' @param base_url The base URL of the API endpoint.
#' @param form_ids A vector of form IDs for which URLs need to be generated.
#'
#' @return A list of URLs corresponding to the provided form IDs.
#' 
#' @examples
#' \dontrun{
#' generate_urls(file_path = "/path/to/data", 
#'               data_file_name = "data_file", 
#'               base_url = "https://example.com", 
#'               form_ids = c(8417, 8734, 9056))
#' }
#' 
generate_urls <- function(full_data, file_path, 
                          data_file_name, base_url, form_ids) {
  
  urls <- lapply(form_ids, function(form_id) {
    # Check if form_id exists in the dataset
    if (form_id %in% full_data$form_id_num) {
      # date of last update
      last_date_in_chunk <- full_data |> 
        dplyr::filter(form_id_num == form_id) |>
        dplyr::summarise(
          date_last_updated = as.Date(
            max(date_last_updated, na.rm = TRUE), 
            format = "%Y-%m-%d")
        ) |> 
        dplyr::pull(date_last_updated)
      
      # Construct URL with last edited parts
      paste0(
        base_url,"/api/v1/data/", form_id,
        "?last_edited__year=", lubridate::year(last_date_in_chunk),
        "&last_edited__month=", lubridate::month(last_date_in_chunk),
        "&last_edited__day__gte=", lubridate::day(last_date_in_chunk)
      )
    } else {
      # Construct URL without last edited parts
      paste0(
        base_url,"/api/v1/data/", form_id
      )
    }
  })
  
  urls
}

#' Call multiple URLs
#' 
#' Function to pull multiple URLS in parallel and with progress bar displayed.
#' Taken from Nishant Kishores `tidypolis`.
#' @param api_token A string specifying the API token for ONA
#' @param urls A list of URLS to perform call on
#' @description Call multiple URLs
#' @import dplyr foreach future doFuture
#' @param urls array of url strings
#' @return tibble with all data
call_urls <- function(urls, api_token) {
  ## use futures for parallel operations
  doParallel::registerDoParallel(cores = (parallel::detectCores() - 2)) 
  future::plan(future::multisession) 
  options(doFuture.rng.onMisuse = "ignore")
  
  progressr::handlers("cli")
  
  progressr::with_progress({
    p <- progressr::progressor(along = urls)
    
    results <- foreach::`%dopar%`(
      foreach::foreach(
        url = urls
      ), {
        p()  # Update progress
        # Retrieve data from the API
        data <- get_paginated_data(api_url = url, api_token = api_token)
        
        # Extract form ID from the URL using regex
        form_id <- gsub(".*data/(\\d+).*", "\\1", url)
        
        # If data is non-empty, add the form_id as a new column
        if (nrow(data) > 0) {
          data <- dplyr::mutate(
            data, form_id_num = form_id, 
            date_last_updated = Sys.Date())
        }
        
        data
      })
  })
  
  # Combine all the results into one tibble
  combined_data <- dplyr::bind_rows(results)
  gc()  # Clean up memory
  
  combined_data
}

#' Update ONA Data
#'
#' This function manages and updates a dataset by downloading new data from 
#' specified form IDs using an API endpoint, and integrating this with existing
#' data. The function checks for the availability of form IDs, downloads new 
#' data, combines it with existing data if present, logs the results if 
#' specified, and saves the updated dataset.
#'
#' @param base_url The base URL for the API calls, defaults to 
#'        "https://api.whonghub.org".
#' @param form_ids A vector of form IDs to download data for.
#' @param api_token The API token used for authentication with the API.
#' @param log_results Boolean flag to indicate whether to log results of the 
#'        data updates, defaults to TRUE.
#' @param file_path The file path where data files will be stored, can be NULL.
#' @param selected_columns Selected columns to download. Default is NULL.
#'    It's in stringed list like c("year", "form_id")
#' @param data_file_name The base name for the data file, defaults to 
#'      "my_ona_data".
#'
#' @return Returns a data frame containing the combined new and existing data.
#'
#' @examples
#' # get_updated_ona_data(
#' #       form_ids = c(123, 456), api_token = "your_api_token_here")
#'
#' @export
get_updated_ona_data <- function(base_url = "https://api.whonghub.org", 
                                 form_ids, api_token, 
                                 log_results = TRUE,  file_path = NULL, 
                                 selected_columns = NULL,
                                 data_file_name = "my_ona_data") {
  
  # check base url validity
  base_url <- validate_base_url(base_url)
  
  if (is.null(file_path)) {
    # Prompt the user to enter the file path
    file_path <- readline(
      "Enter the file path (or press Enter to use the current directory): ")
    
    # If the entered file path is empty or incorrect, default to 
    # the current working directory
    if (nchar(file_path) == 0 || !file.exists(file_path)) {
      file_path <- getwd()
      message("Using the current working directory as the file path.")
    } else {
      message("Using the provided file path.")
    }
  }
  
  # Check if the form id is available for download -----------------------------
  
  resp_data <- prep_ona_data_endpoints(
    base_url = base_url,
    api_token = api_token)
  
  # Check if all form_ids are present in resp_data$id
  if (!all(form_ids %in% unique(resp_data$id))) {
    cli::cli_abort(
      paste0(
        "Form IDs ", toString(form_ids),
        " not found. Use `prep_ona_data_endpoints()`", 
        " to check available forms for download.")
    )
  }
  # Process if to update data --------------------------------------------------
  
  # construct file names for data
  file_name <- paste0(file_path, "/", data_file_name, ".rds")
  
  # Load existing data if it exists
  if (file.exists(file_name)) {
    
    # get data
    full_data_orig <- poliprep::read(file_name) 
  } else {
    full_data_orig <- data.frame()
  }
  
  urls <- generate_urls(full_data_orig, 
                        file_path, data_file_name, base_url, form_ids)
  
  
  # If getting multiple columns, include these in url ---------------------------
  
  if (!is.null(selected_columns)) {
    # Construct query parameters with quotes around column names
    query_params <- list(
      fields = paste0(
        '[', 
        paste(
          sapply(
            selected_columns, 
            function(col) paste0('"', col, '"')), 
          collapse = ','), ']'))
    
    url_list <-  NULL
    
    for (url in urls) {
      
      # Build the full URL for each form id
      results <- httr::modify_url(url, query = query_params) 
      
      url_list[[url]] <- results
    }
    
  } else {url_list = urls}
  
  # Download data  -------------------------------------------------------------
  
  new_data <- call_urls(url_list, api_token = api_token) |> 
    # drop any empty columns
    dplyr::select(
      dplyr::where(
        ~ any(!is.na(.))))
  
  # update the existing data ---------------------------------------------------
  
  # Combine new data with existing data
    full_data <- dplyr::bind_rows(full_data_orig, new_data) |> 
      dplyr::arrange(
        `_id`,
        dplyr::desc(date_last_updated),
        dplyr::desc(date_last_updated)) |>
      dplyr::group_by(`_id`, form_id_num) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
  
  # log results ----------------------------------------------------------------
  
  if (log_results) {
  
  logs = NULL
  
  for (form_id in unique(full_data$form_id_num)) {
    
    if (nrow(full_data_orig) != 0) {
      df <- full_data_orig |> 
        dplyr::filter(form_id_num == form_id) |> 
        janitor::remove_empty(which = "cols") } else {df <- data.frame()}
    
    df_new <- new_data |> 
      dplyr::filter(form_id_num == form_id) |> 
      janitor::remove_empty(which = "cols")
    
    # Construct the log message
    log_message <- data.frame(
      form_id = form_id,
      update_date = Sys.Date(),
      total_columns = ncol(df_new),
      total_rows = format(nrow(df) + nrow(df_new), big.mark = ","),
      new_columns = ncol(df_new) - ncol(df),
      new_rows = format(nrow(df_new), big.mark = ",")
    )
    
    logs[[form_id]] <- log_message |> 
      dplyr::mutate(
        new_rows = ifelse(
          new_rows == total_rows, " Initial Download", new_rows
        ),
        new_columns = ifelse(
          new_columns == total_columns, " Initial Download", new_columns
        )
      )
    
  }
  
  log_messages <- do.call(rbind, logs)
  
    # construct file names for logging
    log_file_name <- paste0(file_path, "/", "ona_data_update_log.rds")
    
    if (file.exists(log_file_name)) {
      log_data <- poliprep::read(log_file_name)
      log_data <- dplyr::bind_rows(log_data, log_messages) |> 
        dplyr::distinct()
    } else {
      log_data <- log_messages
    }
    
    # Save log file
    poliprep::save(
      janitor::clean_names(log_data), log_file_name)
  }
  
  # Return output message and save results -------------------------------------
  
  # save full data
  poliprep::save(full_data, file_name)
  
  return(full_data)
}