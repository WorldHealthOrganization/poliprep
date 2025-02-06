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
  # Conditional loading for packages
  if (!requireNamespace("httpcode", quietly = TRUE)) {
    stop(
      "Package 'httpcode' is required but is not installed. Please install it.",
      call. = FALSE
    )
  }

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
prep_ona_data_endpoints <- function(
    base_url = "https://api.whonghub.org", api_token) {
  # check base url validity
  base_url_pattern <- "^(https?://[^/]+).*"
  if (grepl(base_url_pattern, base_url)) {
    base_url <- sub(base_url_pattern, "\\1", base_url)
  } else {
    stop(paste("Error: ", base_url, " is not a valid base url"))
  }

  # set up URL
  api_url <- paste0(base_url, "/api/v1/data")

  # Validate base url link first -----------------------------------------------

  # get head before download
  response <- httr::HEAD(
    base_url,
    config = httr::add_headers(Authorization = paste("Token", api_token))
  )

  # check status of call
  check_status_api(response)

  # Validate url link before downloading ---------------------------------------

  # get one data endpoint df
  response <- httr::GET(
    api_url,
    config = httr::add_headers(Authorization = paste("Token", api_token))
  ) |>
    httr::content("text", encoding = "UTF-8") |>
    jsonlite::fromJSON(simplifyDataFrame = TRUE)

  return(response)
}

#' Process and Convert R Filters to MongoDB Query Format
#'
#' @description
#' Converts R filtering expressions into MongoDB query format and returns a
#' JSON string. Handles single conditions, multiple conditions combined with &,
#' and vector filters.
#'
#' @param ... One or more filter expressions. Can be formulas (e.g.,
#'   ~field > value), vectors, or single values
#'
#' @return A JSON string containing the MongoDB query filters
#'
#' @details
#' Supports the following R operators that are mapped to MongoDB operators:
#' \itemize{
#'   \item > maps to $gt
#'   \item >= maps to $gte
#'   \item < maps to $lt
#'   \item <= maps to $lte
#'   \item == maps to $eq
#'   \item != maps to $ne
#' }
#'
#' @examples
#' # Single condition
#' # process_comparison_filters(~age >= 21)
#'
#' # Multiple conditions
#' # process_comparison_filters(~age >= 21 & age <= 65)
#'
#' # Vector filter
#' # process_comparison_filters(status = c("active", "pending"))
process_comparison_filters <- function(...) {
  filters <- list(...)
  converted_filters <- list()

  for (filter in filters) {
    # Check if the filter is a formula
    if (inherits(filter, "formula")) {
      filter_expr <- filter[[2]]

      # If the formula contains multiple conditions combined with &
      if (filter_expr[[1]] == as.name("&")) {
        # Initialize a temporary list to hold conditions for the same field
        field_conditions <- list()

        # Process each condition individually
        sub_conditions <- as.list(filter_expr)[-1]
        for (cond in sub_conditions) {
          op <- as.character(cond[[1]])
          field_name <- as.character(cond[[2]])
          value <- as.character(cond[[3]])

          # Map R operators to MongoDB operators
          operator_map <- list(
            ">" = "$gt",
            ">=" = "$gte",
            "<" = "$lt",
            "<=" = "$lte",
            "==" = "$eq",
            "!=" = "$ne"
          )
          mongo_op <- operator_map[[op]]

          # Add each condition to the field's condition list
          field_conditions[[mongo_op]] <- value
        }

        # Assign combined conditions to the field name in the main filter
        converted_filters[[field_name]] <- field_conditions
      } else {
        # Handle single comparison formulas
        operator <- as.character(filter_expr[[1]])
        field_name <- as.character(filter_expr[[2]])
        value <- as.character(filter_expr[[3]])

        # Map operators
        operator_map <- list(
          ">" = "$gt",
          ">=" = "$gte",
          "<" = "$lt",
          "<=" = "$lte",
          "==" = "$eq",
          "!=" = "$ne"
        )
        mongo_operator <- operator_map[[operator]]

        # Single condition with correct field name
        converted_filters[[field_name]] <- setNames(list(value), mongo_operator)
      }

      # Handle vector filters as simple arrays without $in
    } else if (is.vector(filter) && length(filter) > 1) {
      field_name <- names(filter)[1]
      converted_filters[[field_name]] <- filter[[1]]

      # Handle single-value fields
    } else {
      field_name <- names(filter)[1]
      converted_filters[[field_name]] <- filter[[1]]
    }
  }

  # Convert to JSON format
  jsonlite::toJSON(converted_filters, auto_unbox = TRUE)
}

#' Process Logical Filters for MongoDB-style Queries
#'
#' @description
#' Processes a list of filters and converts them into MongoDB-style logical
#' queries. If multiple values exist for a field, they are combined with an OR
#' operator. Single value conditions are combined with AND operator if OR
#' conditions exist.
#'
#' @param filters A list containing field-value pairs for filtering
#'
#' @return A list with MongoDB-style query operators ($and, $or) or simple
#'   conditions
#'
#' @examples
#' # filters <- list(
#' #   status = c("active", "pending"),
#' #   type = "user"
#' # )
#' # process_logical_filters(filters)
process_logical_filters <- function(filters) {
  # List of logical operators
  logical_operators <- c("$and", "$or", "$nor", "$not")

  # Check if filters already contain logical operators
  if (any(names(filters) %in% logical_operators)) {
    # Return filters as-is
    return(filters)
  }

  single_conditions <- list()
  or_conditions <- list()

  for (field_name in names(filters)) {
    field_values <- filters[[field_name]]

    if (length(field_values) == 1) {
      # Single value, add directly
      single_conditions[[field_name]] <- field_values
    } else {
      # Multiple values, create list of conditions for '$or'
      for (value in field_values) {
        condition <- list()
        condition[[field_name]] <- value
        or_conditions[[length(or_conditions) + 1]] <- condition
      }
    }
  }

  if (length(or_conditions) > 0 && length(single_conditions) > 0) {
    # Combine single conditions and or conditions with '$and'
    final_query <- list("$and" = c(
      list(single_conditions),
      list("$or" = or_conditions)
    ))
  } else if (length(or_conditions) > 0) {
    # Only '$or' conditions
    final_query <- list("$or" = or_conditions)
  } else {
    # Only single conditions
    final_query <- single_conditions
  }

  jsonlite::toJSON(final_query,
    auto_unbox = TRUE
  )
}

#' Get Form from ONA API
#'
#' This function gets data for a specified form from the ONA API using a
#' provided API token and form ID. It returns the data in a structured format
#' if the request is successful. If the request fails, the function stops and
#' returns an error message indicating the failure reason.
#'
#' @param base_url A string specifying the URL for the ONA API.
#'                Default is "https://api.whonghub.org"
#' @param form_id A string or numeric value specifying the dataset ID.
#'                Can be found in the URL of the form on ONA.
#' @param api_token A string specifying the API token for authentication
#' @param selected_columns Optional. A character vector of column names to
#'                download. Default is NULL which downloads all columns.
#'                Example: c("year", "form_id")
#' @param logical_filters Optional. A named list for filtering with logical
#'                operators. Names are column names, values are vectors to
#'                filter by. Uses OR within groups and AND between groups.
#' @param comparison_filters Optional. A named list for filtering with
#'                comparisons. Names are column names, values are comparison
#'                conditions. Supports >, <, >=, <=, =, != operators.
#'
#' @return A data frame containing the filtered data downloaded from the ONA
#'         API, with empty columns removed.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' data <- get_ona_form(
#'   form_id = "123456",
#'   api_token = "your_api_token_here"
#' )
#'
#' # Basic usage with selected columns and filters
#' ona_eim_ipds_ih_epm <- get_ona_form(
#'   form_id = 7178,
#'   api_token = ONA_TOKEN,
#'   selected_columns = c("states", "endtime", "today", "_duration"),
#'   logical_filters = list(states = c("BORNO", "KANO")),
#'   comparison_filters = (~ `today` >= "2023-02-04" & `today` <= "2025-02-04")
#' )
#' }
#'
#' @seealso \url{https://api.ona.io/api/v1/data/} for ONA API documentation
get_ona_form <- function(base_url = "https://api.whonghub.org",
                         form_id,
                         api_token,
                         selected_columns = NULL,
                         logical_filters = NULL,
                         comparison_filters = NULL) {
  # Check base URL validity
  base_url <- validate_base_url(base_url)

  # Set up API URL
  api_url <- paste0(base_url, "/api/v1/data/", form_id)

  # Start the CLI process
  process_id <- cli::cli_process_start(
    paste0("Getting form '", form_id, "' from ONA server")
  )
  cat("\n")

  # Check if the form id is available for download
  resp_data <- prep_ona_data_endpoints(
    base_url = base_url,
    api_token = api_token
  )

  if (!(form_id %in% unique(resp_data$id))) {
    cli::cli_abort(
      paste0(
        "Form ID ",
        form_id,
        " not found. Use `prep_ona_data_endpoints()` ",
        "to check available forms for download."
      )
    )
  }

  # Initialize query parameters list
  query_params <- list()

  # Convert selected_columns to JSON array string
  if (!is.null(selected_columns)) {
    fields_json <- jsonlite::toJSON(selected_columns, auto_unbox = FALSE)
  } else {
    fields_json <- NULL
  }

  # Process filters and combine them if both are present
  query_json <- if (!is.null(logical_filters) && !is.null(comparison_filters)) {
    comparison <- process_comparison_filters(comparison_filters) |>
      jsonlite::minify() |>
      gsub("^\\{|\\}$", "", x = _)

    logical <- process_logical_filters(logical_filters) |>
      jsonlite::minify() |>
      gsub("^\\{|\\}$", "", x = _)

    paste0("{", comparison, ",", logical, "}") |>
      jsonlite::minify()
  } else if (!is.null(comparison_filters)) {
    process_comparison_filters(comparison_filters)
  } else if (!is.null(logical_filters)) {
    process_logical_filters(logical_filters)
  } else {
    NULL
  }

  # Build the full URL with query parameters
  full_url <- httr::modify_url(
    api_url,
    query = list(
      fields = fields_json,
      query = query_json
    )
  )

  # Download data (use pagination if necessary) --------------------------------

  results <- get_paginated_data(full_url, api_token) |>
    # Drop any empty columns
    dplyr::select(
      dplyr::where(
        ~ any(!is.na(.))
      )
    )

  # Finish the CLI process
  cat("\n")
  cli::cli_process_done(
    process_id,
    msg_done = "Download complete! {praise_emoji()}"
  )

  return(results)
}

#' Get a Page of Data from an API
#'
#' This function retrieves a single page of data from a specified API endpoint.
#'
#' @param api_url The base URL of the API endpoint.
#' @param api_token Authentication token for API access, prefixed with "Token".
#' @param times The number of attempts to retry the request in case of failure.
#'              Defaults to 12.
#' @return A list containing the retrieved data parsed from JSON format. If
#'         the specified content is not found or an error occurs, the function
#'         stops and returns an error message.
get_ona_page <- function(api_url, api_token, times = 12) {
  tryCatch(
    httr::RETRY(
      "GET", api_url,
      httr::add_headers(Authorization = paste("Token", api_token)),
      times = times, pause_cap = 180, httr::progress("down")
    ) |>
      httr::content("text", encoding = "UTF-8") |>
      jsonlite::fromJSON(simplifyDataFrame = TRUE),
    error = function(e) {
      message("Error encountered: ", e$message)
      NULL
    }
  )
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
  api_limit <- 100000
  results <- list()
  page_number <- 1

  repeat {
    # Append page and page_size parameters
    paged_url <- paste0(
      api_url,
      ifelse(grepl("\\?", api_url), "&", "?"),
      "page=", page_number,
      "&page_size=100000"
    )

    current_page <- get_ona_page(paged_url, api_token) |>
      as.data.frame() |>
      dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))

    results <- dplyr::bind_rows(results, current_page)

    if (nrow(current_page) < api_limit || nrow(current_page) == 0) break

    page_number <- page_number + 1
  }

  return(dplyr::distinct(results))
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

#' Get Data from ONA for Multiple Forms (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `get_ona_data()` instead, which
#' supports both single and multiple form IDs.
#'
#' @inheritParams get_ona_data
#' @keywords internal
get_multi_ona_data <- function(base_url = "https://api.whonghub.org",
                               form_ids,
                               api_token,
                               selected_columns = NULL,
                               logical_filters = NULL,
                               comparison_filters = NULL) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_multi_ona_data()",
    "get_ona_data()",
    details =
      "Use `get_ona_data()` which now supports both single and multiple form IDs"
  )

  get_ona_data(
    base_url = base_url,
    form_ids = form_ids,
    api_token = api_token,
    selected_columns = selected_columns,
    logical_filters = logical_filters,
    comparison_filters = comparison_filters
  )
}

#' Get Data from ONA API
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
#' @param logical_filters Optional. A named list for filtering with logical
#'                operators. Names are column names, values are vectors to
#'                filter by. Uses OR within groups and AND between groups.
#' @param comparison_filters Optional. A named list for filtering with
#'                comparisons. Names are column names, values are comparison
#'                conditions. Supports >, <, >=, <=, =, != operators.
#' @return A data frame containing the combined data from all specified form
#'        IDs, and includes from_id column.
#' @examples
#' # api_token <- "your_api_token_here"
#' @importFrom foreach %dopar%
#' @export
get_ona_data <- function(base_url = "https://api.whonghub.org",
                         form_ids, api_token,
                         selected_columns = NULL,
                         logical_filters = NULL,
                         comparison_filters = NULL) {
  
  # Check if the form IDs are available for download ---------------------------
  resp_data <- prep_ona_data_endpoints(
    base_url = base_url,
    api_token = api_token
  )
  
  if (!all(form_ids %in% unique(resp_data$id))) {
    missing_ids <- form_ids[!form_ids %in% unique(resp_data$id)]
    cli::cli_abort(
      paste0(
        "Form IDs ",
        toString(missing_ids),
        " not found. Use `prep_ona_data_endpoints()` ",
        "to check available forms for download."
      )
    )
  }
  
  # Fetch data sequentially for each form ID -----------------------------------
  combined_data <- purrr::map_dfr(
    form_ids,
    function(form_id) {
      data <- get_ona_form(
        form_id = form_id,
        api_token = api_token,
        selected_columns = selected_columns,
        logical_filters = logical_filters,
        comparison_filters = comparison_filters
      )
      dplyr::mutate(data, form_id_num = form_id)
    }
  )
  
  # drop any empty columns
  combined_data <- combined_data |>
    dplyr::select(
      dplyr::where(
        ~ any(!is.na(.))
      )
    )
  
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
#' generate_urls(
#'   file_path = "/path/to/data",
#'   data_file_name = "data_file",
#'   base_url = "https://example.com",
#'   form_ids = c(8417, 8734, 9056)
#' )
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
            format = "%Y-%m-%d"
          )
        ) |>
        dplyr::pull(date_last_updated)

      # Construct URL with last edited parts
      paste0(
        base_url, "/api/v1/data/", form_id,
        "?last_edited__year=", lubridate::year(last_date_in_chunk),
        "&last_edited__month=", lubridate::month(last_date_in_chunk),
        "&last_edited__day__gte=", lubridate::day(last_date_in_chunk)
      )
    } else {
      # Construct URL without last edited parts
      paste0(
        base_url, "/api/v1/data/", form_id
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
  progressr::handlers("cli")
  
  progressr::with_progress({
    p <- progressr::progressor(along = urls)
    
    results <- purrr::map_dfr(
      urls,
      function(url) {
        p() # Update progress
        # Retrieve data from the API
        data <- get_paginated_data(api_url = url, api_token = api_token)
        
        # Extract form ID from the URL using regex
        form_id <- gsub(".*data/(\\d+).*", "\\1", url)
        
        # If data is non-empty, add the form_id as a new column
        if (nrow(data) > 0) {
          data <- dplyr::mutate(
            data,
            form_id_num = form_id,
            date_last_updated = Sys.Date()
          )
        }
        
        data
      }
    )
  })
  
  gc() # Clean up memory
  results
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
                                 log_results = TRUE, file_path = NULL,
                                 selected_columns = NULL,
                                 data_file_name = "my_ona_data") {
  # check base url validity
  base_url <- validate_base_url(base_url)
  
  if (is.null(file_path)) {
    # Prompt the user to enter the file path
    file_path <- readline(
      "Enter the file path (or press Enter to use the current directory): "
    )
    
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
    api_token = api_token
  )
  
  # Check if all form_ids are present in resp_data$id
  if (!all(form_ids %in% unique(resp_data$id))) {
    cli::cli_abort(
      paste0(
        "Form IDs ", toString(form_ids),
        " not found. Use `prep_ona_data_endpoints()`",
        " to check available forms for download."
      )
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
  
  urls <- generate_urls(
    full_data_orig,
    file_path, data_file_name, base_url, form_ids
  )
  
  
  # If getting multiple columns, include these in url --------------------------
  
  if (!is.null(selected_columns)) {
    # Convert selected_columns to JSON array string
    if (!is.null(selected_columns)) {
      # selected_columns <- c("_id", selected_columns) # ensure there is id col
      fields_json <- jsonlite::toJSON(selected_columns, auto_unbox = FALSE)
    } else {
      fields_json <- NULL
    }
    
    url_list <- NULL
    
    for (url in urls) {
      # Build the full URL for each form id
      results <- httr::modify_url(url, query = fields_json)
      
      url_list[[url]] <- results
    }
  } else {
    url_list <- urls
  }
  
  # Download data  -------------------------------------------------------------
  
  new_data <- call_urls(url_list, api_token = api_token) |>
    # drop any empty columns
    dplyr::select(
      dplyr::where(
        ~ any(!is.na(.))
      )
    )
  
  # update the existing data ---------------------------------------------------
  
  # Combine new data with existing data
  full_data <- dplyr::bind_rows(full_data_orig, new_data) |>
    dplyr::arrange(
      `_id`,
      dplyr::desc(date_last_updated),
      dplyr::desc(date_last_updated)
    ) |>
    dplyr::group_by(`_id`, form_id_num) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  # log results ----------------------------------------------------------------
  
  if (log_results) {
    logs <- NULL
    
    for (form_id in unique(full_data$form_id_num)) {
      if (nrow(full_data_orig) != 0) {
        df <- full_data_orig |>
          dplyr::filter(form_id_num == form_id) |>
          janitor::remove_empty(which = "cols")
      } else {
        df <- data.frame()
      }
      
      
      if (nrow(new_data) > 0) {
        df_new <- new_data |>
          dplyr::filter(form_id_num == form_id) |>
          janitor::remove_empty(which = "cols")
        
        df_new_tot_cols <- ncol(df_new)
        df_new_tot_rows <- nrow(df_new)
      } else {
        df_new <- data.frame()
        df_new_tot_cols <- 0
        df_new_tot_rows <- 0
      }
      
      log_message <- data.frame(
        form_id = form_id,
        update_date = Sys.Date(),
        total_columns = length(union(colnames(df), colnames(df_new))),
        total_rows = format(nrow(df) + df_new_tot_rows, big.mark = ","),
        new_columns = length(setdiff(colnames(df_new), colnames(df))),
        new_rows = if (
          df_new_tot_rows == 0) "No new data" else format(
            df_new_tot_rows, big.mark = ",")
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
    
    log_messages <- do.call(rbind, logs) |> 
      dplyr::mutate(
        new_columns = as.character(new_columns),
        new_rows = as.character(new_rows)
      )
    
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
      janitor::clean_names(log_data), log_file_name
    )
    
    
    cat("\n") 
    if (nrow(new_data) > 0) {
      cli::cli_alert_success(
        paste0(
          "Data update completed. \n",
          "Total new rows added: ",
          crayon::yellow(format(nrow(new_data), big.mark = ","))
        )
      )
    } else {
      cli::cli_alert_info("No new data available. Everything is up to date.")
    }
    
  }
  
  # Return output message and save results -------------------------------------
  
  # save full data
  poliprep::save(full_data, file_name)
  
  return(full_data)
}
