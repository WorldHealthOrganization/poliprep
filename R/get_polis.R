#' Extract Entity Sets from the POLIS API Metadata
#'
#' This function retrieves and extracts entity sets from the POLIS API metadata
#' URL provided. It parses the XML content to extract the names and entity
#' types of the available data sets.
#'
#' @param url A character string specifying the URL of the POLIS API metadata.
#'
#' @return A data frame containing two columns:
#' \describe{
#'   \item{DataName}{The name of the data set.}
#'   \item{EntityType}{The entity type associated with the data set.}
#' }
#'
#' @examples
#' \dontrun{
#' # Define the URL for the POLIS API metadata
#' polis_api_root_url <-
#'              "https://extranet.who.int/polis/api/v2/$metadata?token="
#' api_token <- Sys.getenv("POLIS_API_KEY")
#' url <- paste0(polis_api_root_url, api_token)
#'
#' # Extract the entity sets
#' xml_df <- extract_entity_sets(url)
#' }
extract_entity_sets <- function(url) {
  response <- httr::GET(url)
  content <- httr::content(response, as = "text")
  xml_content <- xml2::read_xml(content)
  
  # Get the namespaces
  ns <- xml2::xml_ns(xml_content)
  
  # Extract EntitySet nodes using the correct namespaces
  entity_sets <- xml_content |>
    xml2::xml_find_all("//d4:EntityContainer/d4:EntitySet", ns = ns)
  
  # Extract attributes from EntitySet nodes and convert to a data frame
  data.frame(
    `DataName` = xml2::xml_attr(entity_sets, "Name"),
    `EntityType` = xml2::xml_attr(entity_sets, "EntityType"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(`EntityType`) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

#' Get HTTP Status Code for a POLIS API Table
#'
#' This function sends a GET request to a specified POLIS API table and returns
#' the HTTP status code. It includes error handling to manage timeouts and other
#' potential request errors.
#'
#' @param table A character string specifying the name of the table to check.
#' @param api_token A character string of the POLIS API token. Defaults to
#'      `Sys.getenv("POLIS_API_KEY")`.
#'
#' @return An integer representing the HTTP status code, or a character string
#'      ("Timeout" or "Error") if an exception occurs.
#'
#' @examples
#' \dontrun{
#' # Get the status code for the "countries" table
#' status_code <- get_status_code("countries")
#' }
get_status_code <- function(table, api_token = Sys.getenv("POLIS_API_KEY")) {
  url <- paste0("https://extranet.who.int/polis/api/v2/", table, "?$top=5")
  tryCatch({
    response <- httr::GET(
      url,
      httr::add_headers("authorization-token" = api_token),
      httr::timeout(10)
    )
    httr::status_code(response)
  }, error = function(e) {
    if (grepl("Timeout was reached", e$message)) {
      return("Timeout")
    } else {
      return("Error")
    }
  })
}

#' Check Availability of POLIS API Tables
#'
#' This function checks the availability of specified tables in the POLIS API
#' by sending GET requests. It reports the HTTP status codes and provides
#' success or error messages using the `cli` package.
#'
#' @param api_token A character string of the POLIS API token. Defaults to
#'    `Sys.getenv("POLIS_API_KEY")`.
#' @param tables_to_check An optional character vector of table names to check.
#'     If `NULL`, all available tables are checked.
#'
#' @return No return value. The function outputs messages to the console
#'    indicating the availability of each table.
#'
#' @examples
#' \dontrun{
#' # Check the availability of specific tables
#' check_tables_availability(
#'   api_token = Sys.getenv("POLIS_API_KEY"),
#'   tables_to_check = c("virus", "case", "population", "humanspecimenviruses",
#'                       "envsample", "synonym", "geography", "lqas",
#'                       "activity", "subactivity", "envirosamplesite",
#'                       "im", "labspecimen")
#' )
#' }
#'@export
check_tables_availability <- function(
    api_token = Sys.getenv("POLIS_API_KEY"),
    tables_to_check = NULL) {
  
  # Get the list of tables
  polis_api_root_url <- "https://extranet.who.int/polis/api/v2/$metadata?token="
  url <- paste0(polis_api_root_url, api_token)
  
  xml_df <- extract_entity_sets(url)
  tables <- tolower(xml_df$DataName)
  
  # If specific tables are requested, filter the list
  if (!is.null(tables_to_check)) {
    # Ensure the provided table names are in lowercase
    tables_to_check <- tolower(tables_to_check)
    # Filter tables to only include those specified
    tables <- tables[tables %in% tables_to_check]
    
    # Warn if any requested tables are not found
    missing_tables <- setdiff(tables_to_check, tables)
    if (length(missing_tables) > 0) {
      cli::cli_alert_warning(
        glue::glue(
          "The following tables were not found and will be skipped:",
          " {paste(missing_tables, collapse = ', ')}")
      )
    }
  }
  
  # Disable SSL verification and set a timeout
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  options(timeout = 5)
  
  # Loop through tables and check availability
  for (table in tables) {
    status_code <- get_status_code(table, api_token = api_token)
    if (status_code == 200) {
      cli::cli_alert_success(
        "POLIS table `{table}` is available to download."
      )
    } else {
      cli::cli_alert_danger(
        "POLIS table `{table}` is not available. Status code: {status_code}"
      )
    }
  }
}

#' Construct API URL
#'
#' This function constructs a URL for making an API call. It takes various
#' parameters such as the API endpoint, endpoint suffix, date range, region, and
#' selected variables, and combines them to form a complete and well-formatted
#' API URL. The function handles the inclusion of date filters, region filters,
#' and selection of specific fields in the query, and ensures proper URL
#' encoding.
#'
#' @param endpoint The base URL of the API endpoint.
#' @param suffix Additional path appended to the endpoint to specify the
#'        API resource.
#' @param min_date The minimum date for the date range filter.
#' @param max_date The maximum date for the date range filter.
#' @param country_code ISO3 country code to filter the data. Default is NULL.
#' @param date_field The field name in the API corresponding to the date.
#' @param region_field The field name in the API corresponding to the region.
#' @param region The specific region to filter the data. If NULL or empty,
#'        no region filter is applied.
#' @param select_vars A vector of field names to be included in the API
#'        response. If NULL or empty, no selective fields are applied.
#'
#' @return A string containing the fully constructed API URL.
#'
#' @examples
#' construct_api_url(
#'   "https://api.example.com/", "data", "2020-01-01", "2020-12-31",
#'   "dateField", "NGA", "regionField", "AFRO", c("field1", "field2")
#' )
#' @export

construct_api_url <- function(endpoint, suffix, min_date, max_date,
                              date_field, country_code,
                              region_field, region, select_vars) {
  # Base URL construction
  base_url <- paste0(endpoint, suffix)
  
  # Date filter
  date_filter <- glue::glue(
    "{date_field} ge DateTime'{min_date}' and ",
    "{date_field} le DateTime'{max_date}'"
  )
  
  # Region filter
  region_filter <- ""
  if (!is.null(region) && region != "" &&
      !(suffix %in% c("HumanSpecimenViruses", "Im"))) {
    region_filter <- glue::glue(" and {region_field} eq '{region}'")
  }
  
  # country code filter
  country_code_filter <- ""
  if (!is.null(country_code) && country_code != "" ) {
    country_code_filter <- glue::glue(
      " and CountryISO3Code eq '{country_code}'")
  }
  
  # Combine date and region filters
  filter_query <- paste(date_filter, country_code_filter,
                        region_filter, sep = "")
  
  # Select query for additional fields
  select_query <- ""
  if (!is.null(select_vars) && length(select_vars) > 0) {
    select_query <- paste0("$select=", paste(select_vars, collapse = ","))
  }
  
  # Construct final query string
  query_string <- ""
  if (select_query != "") {
    query_string <- paste(filter_query, select_query, sep = "&")
  } else {
    query_string <- filter_query
  }
  
  # Construct the full API URL
  api_url <- paste0(base_url, "?$filter=", utils::URLencode(query_string))
  
  return(api_url)
}

#' Get Date Field and Endpoint Suffix Based on Data Type
#'
#' This function returns the appropriate endpoint suffix and date field name
#' for a given data type. It is specifically designed to work with the POLIS API
#' data retrieval system. The function takes a data type as input and returns a
#' list containing the corresponding endpoint suffix and date field. It ensures
#' that the data type provided is valid and returns an error message if not.
#'
#' @param data_type A string specifying the type of data for which information
#'                  is needed. Valid data types include 'cases', 'virus',
#'                  'population', 'env', 'geo', 'geo_synonym', 'im', 'activity',
#'                  'lab_specimen', 'lab_specimen_virus', 'lab_env',
#'                  'sub_activ', and lqas'.
#'
#' @return A list with two elements: 'endpoint_suffix', which is the suffix for
#'         the API endpoint corresponding to the data type, and 'date_field',
#'         which is the name of the date field relevant to the data type.
#'
#' @examples
#' result <- get_api_date_suffix("cases")
#' endpoint_suffix <- result$endpoint_suffix
#' date_field <- result$date_field
#' @export

get_api_date_suffix <- function(data_type) {
  # Define endpoint suffixes and date fields in named lists
  endpoint_suffixes <- c(
    cases = "Case", virus = "Virus",
    population = "Population", env = "EnvSample",
    geo = "Geography", geo_synonym = "Synonym",
    lab_specimen = "LabSpecimen",
    lab_specimen_virus = "HumanSpecimenViruses",
    lab_env = "EnvSample",
    im = "Im", activity = "Activity",
    sub_activ = "SubActivity", lqas = "Lqas"
  )
  
  # Define date fields for each data type
  # for initial data
  date_fields_initial <- c(
    cases = "CaseDate",
    virus = "VirusDate",
    population = "CreatedDate",
    env = "CollectionDate",
    geo = "CreatedDate",
    geo_synonym = "UpdatedDate",
    im = "PublishDate",
    activity = "ActivityDateFrom",
    lab_specimen_virus = "PublishDate",
    lab_specimen = "CreatedDate",
    lab_env = "CollectionDate",
    sub_activ = "DateFrom",
    lqas = "Start"
  )
  
  # Define date fields for each data type
  # for updated data
  date_fields <- c(
    cases = "LastUpdateDate",
    virus = "UpdatedDate", population = "UpdatedDate",
    env = "LastUpdateDate", geo = "UpdatedDate",
    geo_synonym = "UpdatedDate",
    im = "PublishDate", activity = "LastUpdateDate",
    lab_specimen_virus = "PublishDate",
    lab_specimen = "LastUpdateDate",
    lab_env = "LastUpdateDate",
    sub_activ = "UpdatedDate", lqas = "Start"
  )
  
  # Check if the provided data type is valid
  if (!data_type %in% names(endpoint_suffixes)) {
    stop("Invalid data_type specified")
  }
  
  # Return endpoint suffix and date field
  list(
    endpoint_suffix = endpoint_suffixes[[data_type]],
    date_fields_initial = date_fields_initial[[data_type]],
    date_field = date_fields[[data_type]]
  )
}

#' Retrieve Data from POLIS API
#'
#' This function serves as a gateway to retrieve various types of health-related
#' data from the POLIS API. It simplifies the process of querying the API by
#' handling authentication, constructing the request URL, and iterating over
#' paginated results. The function is versatile, allowing for data retrieval
#' based on a range of parameters such as date range, data type, region, and
#' specific variables. It automates the handling of API responses, including
#' status checking and data aggregation, and returns the results in a
#' convenient data frame format. The function is designed to be robust,
#' providing informative error messages in case of missing API keys or
#' unsuccessful API calls.
#'
#' @param min_date Start date for data retrieval in 'YYYY-MM-DD' format.
#'                 Specifies the earliest date of the data to be fetched.
#'                 Required parameter.
#' @param max_date End date for data retrieval in 'YYYY-MM-DD' format.
#'                 Defaults to the current system date. Specifies the latest
#'                 date of the data to be fetched.
#' @param data_type Type of data to retrieve.
#'                  Supported types include 'cases', 'virus', 'population',
#'                  'env' (Environmental), 'geo' (Geographical), 'geo_synonym',
#'                  'im' (Independent Monitoring), 'activity', 'sub_activ'
#'                  (Sub-activities), and 'lqas' (Lot Quality Assurance
#'                  Sampling), 'lab_specimen' (Human Specimen),
#'                  'lab_env' (Environmental Sample), 'lab_specimen_virus'
#'                  (Human Specimen Viruses). Default is 'cases'.
#' @param region Region code for data filtering.
#'               Represents the WHO region from which to retrieve the data.
#'               Possible values are AFRO; AMRO; EMRO; EURO; SEARO; WPRO Use
#'               'Global' to retrieve global data. Default is 'AFRO'.
#' @param country_code ISO3 country code to filter the data. Default is NULL.
#' @param select_vars Vector of variables to select from the API response.
#'                    If NULL (default), all variables are selected.
#' @param polis_api_key API key for authentication.
#'                        Default is retrieved from the environment variable
#'                        'POLIS_API_KEY'. An explicit API key can be provided
#'                        if required.
#' @param log_results Logical indicating whether to log the results of the API
#' @param log_file_path Path to the directory where the log file will be saved
#' @param updated_dates Logical indicating whether to use the 'LastUpdateDate'
#' @return A data frame containing the requested data aggregated from all pages
#'         of the API response. Each row represents a record, and columns
#'         correspond to the variables in the dataset.
#'
#' @examples
#' \dontrun{
#' data <- get_polis_api_data("2021-01-01", "2021-01-31", "cases", "AFRO")
#' }
#' @export

get_polis_api_data <- function(min_date = "2021-01-01",
                               max_date = Sys.Date(),
                               data_type = "cases",
                               region = "AFRO",
                               country_code = NULL,
                               select_vars = NULL,
                               updated_dates = FALSE,
                               polis_api_key,
                               log_results = FALSE,
                               log_file_path) {
  
  # API Endpoint and URL Construction
  api_endpoint <- "https://extranet.who.int/polis/api/v2/"
  endpoint_suffix <- get_api_date_suffix(data_type)$endpoint_suffix
  
  # set up the dates
  if (updated_dates) {
    date_field <- get_api_date_suffix(data_type)$date_field
  } else {
    date_field <- get_api_date_suffix(data_type)$date_fields_initial
  }
  
  # set up region field
  if (tolower(region) == "global"  || is.null(region)) {
    region_field <- NULL
  } else {
    region_field <- get_api_date_suffix(data_type)$region_field
    # set up region field name
    region_field <- if (data_type == "virus") "RegionName" else "WHORegion"
  }
  
  # Construct the full API URL
  api_url <- construct_api_url(
    api_endpoint, endpoint_suffix, min_date, max_date,
    date_field, country_code, region_field, region, select_vars
  )
  
  # all API iteratively
  response <- iterative_api_call(api_url, token = polis_api_key)
  
  # process API response
  full_data <- process_api_response(response)
  
  # log results
  if (log_results) {
    
    # Check if log file name is provided
    if (is.null(log_file_path)) {
      warning("No log file name provided. Logging is disabled.")
      return(invisible(NULL))
    }
    
    # set up log file name
    log_file_name <- paste0(log_file_path, "/", "polis_data_update_log.rds")
    
    # Construct the log message
    log_message <- data.frame(
      Region = tools::toTitleCase(region),
      QueryStartDate = as.Date(min_date, format = "%Y-%m-%d"),
      QueryEndDate = as.Date(max_date, format = "%Y-%m-%d"),
      DataStartDate = min(as.Date(full_data[[date_field]])),
      DataEndDate = max(as.Date(full_data[[date_field]])),
      PolisDataType = as.character(endpoint_suffix),
      NumberOfVariables = ncol(full_data),
      NumberOfRows = format(nrow(full_data), big.mark = ",")
    )
    
    if (file.exists(log_file_name)) {
      log_data <- readRDS(log_file_name)
      log_data <- rbind(log_data, log_message)
    } else {
      log_data <- log_message
    }
    
    # Save log file
    saveRDS(log_data, log_file_name)
    
  }
  
  return(full_data)
}

#' Try API Call with Retry Logic
#'
#' This function attempts to make an API call to a specified URL using the GET
#' method. It includes a retry mechanism, where the request is attempted up to a
#' specified number of times (`max_attempts`) before giving up. If the request
#' is successful and returns a 200 status code, the response is returned. If any
#' errors occur or if a non-200 status code is received, the function will wait
#' for 5 seconds and retry the request. After the maximum number of attempts, if
#' the request still fails, the function stops with an error message.
#'
#' @param url The URL for the API endpoint to which the GET request is made.
#' @param token The authorization token required for the API request.
#' @param max_attempts The maximum number of attempts to make the API call.
#'        Default is 3 attempts.
#'
#' @return The response from the API call if successful, or an error message
#'         if all attempts fail.
#'
#' @examples
#' \dontrun{
#' iterative_api_call("https://api.example.com/data", "your_api_token")
#' }
#' @export

iterative_api_call <- function(url, token = NULL, max_attempts = 3) {
  # Configure the initial request
  req <- httr2::request(url) |>
    httr2::req_headers(`authorization-token` = token) |>
    httr2::req_retry(max_tries = max_attempts) |>
    httr2::req_progress()
  
  # Checking that the API call is valid
  resp <- req |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  
  # check status of call
  check_status_api(
    httr2::resp_status(resp)
  )
  
  # Perform iterative requests with a lambda function for next_req
  httr2::req_perform_iterative(
    req,
    next_req = \(req, resp) {
      next_link <- httr2::resp_body_json(resp)$odata.nextLink
      if (is.null(next_link)) {
        return(NULL)
      }
      req |> httr2::req_url(next_link)
    },
    max_reqs = Inf,
    progress = list(
      name = "Downloading POLIS pages:",
      format_done = "POLIS data downloaded :)",
      clear = FALSE
    )
  )
}

#' Process API Response
#'
#' This function processes the response from an API call. It checks the status
#' code of the response; if the status code is 200, indicating a successful
#' response, it parses the content of the response using.
#'
#' @param response The response object from an API call made using the `httr2`
#'       package.
#'
#' @return A `dataframe`, which is the parsed data from the response If the
#'         response is unsuccessful, both elements of the list are NULL.
#'
#' @examples
#' \dontrun{
#' # Example usage within a function that makes API calls:
#' response <- iterative_api_call(api_url, token)
#' processed_response <- process_response(response)
#' }
#' @export

process_api_response <- function(response) {
  # extract the main data
  content <- response |>
    httr2::resps_successes() |>
    httr2::resps_data(\(resp) httr2::resp_body_json(resp)$value)
  
  dplyr::bind_rows(content)
}

#' Save Polis Data to compressed RDS File
#'
#' This function saves a given POLIS data object to an RDS file using a
#' specific naming convention based on the current date. It also manages the
#' retention of only the 5 most recent datasets in the specified directory,
#' removing older datasets if necessary.
#'
#' @param polis_data The POLIS data object to be saved.
#' @param polis_path The directory path where the RDS file will be saved. This
#'   function will check this directory for existing datasets and will maintain
#'   only the 5 most recent datasets, deleting older ones.
#' @param filname The name of the file to be saved.
#' @param max_datasets The max number of datasets to retain in the directory.
#'
#' @return Invisible NULL. This function is used for its side effect of
#'   saving a file and potentially deleting older files, rather than for
#'   returning a value.
#'
#' @examples
#' # Assume `polis_data` is your dataset and `./polis_datasets` is your
#' # target directory
#' # save_polis_data(polis_data, "./polis_datasets")
#'
#' @export
#' @export
save_polis_data <- function(polis_data, polis_path, filname, max_datasets = 5) {
  cli::cli_process_start("Saving POLIS data into a compressed RDS file.")
  
  # Generate the file name based on the ISO week year and week number
  suffix_name <- sprintf("_%s_%s.rds",
                         format(Sys.Date(), "%G"), format(Sys.Date(), "%V"))
  full_path <- file.path(polis_path, paste0(filname, suffix_name))
  
  # Save POLIS list
  saveRDS(polis_data, full_path, compress = "xz")
  
  cli::cli_process_done()
  
  # Check existing datasets and keep only the 5 most recent
  existing_files <- list.files(
    polis_path, pattern = "\\.rds$", full.names = TRUE)
  
  # Exclude files that contain 'polis_data_update_log' in the name
  existing_files <- grep(
    "polis_data_update_log", existing_files, value = TRUE, invert = TRUE)
  
  # Keep only files starting with the specified filname
  existing_files <- grep(
    paste0("^", filname), basename(existing_files), value = TRUE)
  
  if (length(existing_files) > max_datasets) {
    # Sort files by date, assuming the naming convention holds the date info
    file_dates <- sapply(existing_files, function(x) {
      as.Date(stringr::str_extract(x, "\\d{4}_\\d{2}"), "%G_%V")
    })
    
    oldest_files <- existing_files[order(file_dates)][1:(
      length(existing_files) - max_datasets)]
    
    cli::cli_alert_success(
      "Removing {length(oldest_files)} old file(s) to keep top {max_datasets}.")
    
    suppressMessages(file.remove(file.path(polis_path, oldest_files)))
  }
  cli::cli_process_done()
}

#' Retrieve and Update POLIS Data
#'
#' This function is designed for both the initial retrieval and subsequent
#' updates of POLIS datasets. It intelligently checks for existing data,
#' fetches only new data since the last update, integrates this with the
#' existing data, logs update sessions, and saves the updated dataset to a
#' specified location. It interacts with the POLIS API and allows filtering
#' and selection based on various criteria.
#'
#' @param min_date Start date for data retrieval in 'YYYY-MM-DD' format.
#'                 This date marks the beginning of the dataset to be retrieved.
#' @param max_date End date for data retrieval, default is the current date.
#'                 This date marks the end of the dataset to be retrieved.
#' @param data_type Type of data to retrieve.
#'                  Supported types include 'cases', 'virus', 'population',
#'                  'env' (Environmental), 'geo' (Geographical), 'geo_synonym',
#'                  'im' (Independent Monitoring), 'activity', 'sub_activ'
#'                  (Sub-activities), and 'lqas' (Lot Quality Assurance
#'                  Sampling), 'lab_specimen' (Human Specimen),
#'                  'lab_env' (Environmental Sample), 'lab_specimen_virus'
#'                  (Human Specimen Viruses). Default is 'cases'.
#' @param region Region code for data filtering, default is 'AFRO'.
#'               This parameter filters the data by the specified WHO region.
#' @param country_code ISO3 country code to filter the data. Default is NULL.
#' @param select_vars Vector of variables to select, default is NULL (all vars).
#'                    This parameter allows for the selection of specific
#'                    variables from the API response.
#' @param file_path Path for the data and log files, default is a preset
#'                  directory. This defines the storage location for data
#'                  and logs.
#' @param save_directly Boolean indicating whether data should be saved directly.
#'                      Default is TRUE. If FALSE, the function returns a
#'                      data frame of the aggregated data.
#' @param log_results Boolean indicating whether to log update sessions.
#'                    Default is TRUE. This controls the logging of updates.
#' @param polis_api_key API key for authentication, default from environment.
#'                        This is used for accessing and authenticating with
#'                        the API.
#'
#' @return If save_directly is FALSE, returns a data frame of the aggregated
#'         data. Otherwise, the data is saved to the specified file path and
#'         nothing is returned.
#'
#' @examples
#' \dontrun{
#' update_polis_api_data("2021-01-01", "2021-01-31", "cases", "AFRO")
#' }
#' @export

update_polis_api_data <- function(min_date,
                                  max_date = Sys.Date(),
                                  data_type = "cases",
                                  region = "AFRO",
                                  country_code = NULL,
                                  select_vars = NULL,
                                  file_path = NULL,
                                  save_directly = FALSE,
                                  log_results = FALSE,
                                  polis_api_key = NULL) {
  
  # Construct file names for data and log
  data_file_name <- paste0(file_path, "/", data_type, "_polis_data.rds")
  log_file_name <- paste0(file_path, "/", "polis_data_update_log.rds")
  
  # set up the dates
  date_field <- get_api_date_suffix(data_type)$date_field
  
  # Load existing data if it exists
  if (file.exists(data_file_name)) {
    full_data <- readRDS(data_file_name)
    last_date_in_chunk <- as.Date(
      max(full_data[[date_field]], na.rm = T),
      format = "%Y-%m-%d"
    )
  } else {
    full_data <- data.frame()
    last_date_in_chunk <- as.Date(min_date) - 1
  }
  
  # Retrieve data from the API starting from the day after the last date in
  # the existing data
  min_date <- last_date_in_chunk + 1
  
  # Retrieve data from the API
  new_data <- get_polis_api_data(
    min_date = min_date, max_date = max_date,
    data_type = data_type, region = region, country_code = country_code,
    select_vars = select_vars, updated_dates = TRUE,
    log_results = FALSE, polis_api_key = polis_api_key,
  )
  
  # Combine new data with existing data
  if (nrow(new_data) > 0) {
    full_data <- dplyr::bind_rows(full_data, new_data)
  }
  
  # Log the session details to an Excel file
  if (nrow(full_data) > 0) {
    session_end_date <- min(
      as.Date(max_date),
      max(as.Date(full_data[[date_field]],
                  format = "%Y-%m-%d"
      ), na.rm = TRUE)
    )
    
    if (save_directly && log_results) {
      write_log_file_api(
        log_file_name, data_type, full_data, new_data, date_field,
        last_date_in_chunk, min_date, max_date, session_end_date
      )
    }
    
    n_rows <- format(sum(nrow(full_data)), big.mark = ",")
    
    if (file.exists(data_file_name) & session_end_date > min_date) {
      n_rows <- format(sum(nrow(full_data)), big.mark = ",")
      
      cat(glue::glue(
        "Hooray! You have updated the {data_type} ",
        "data (N = {n_rows}) from {as.Date(last_date_in_chunk)}",
        " to {session_end_date}.\n"
      ))
    } else if (session_end_date > min_date) {
      cat(glue::glue(
        "Hooray! You have downloaded the complete {data_type} ",
        "data (N = {n_rows}) from {as.Date(min_date)} to ",
        "{session_end_date}.\n"
      ))
    }
  }
  
  # Save the updated full dataset
  if (save_directly) {
    saveRDS(full_data, data_file_name, compress = "xz")
  } else {
    return(full_data)
  }
}

#' Write Update Log File
#'
#' This function serves as a wrapper for updating a log file. It constructs a
#' log  message based on the provided parameters and appends it to an existing
#' log file or creates a new one if it doesn't exist. The function ensures that
#' date-timecolumns are correctly formatted and removes duplicate entries before
#' saving the file.
#'
#' @param log_file_name The name (and path) of the log file to be updated.
#' @param data_type The type of data being logged.
#' @param full_data A data frame representing the full dataset being processed.
#' @param new_data A data frame representing the new data being added.
#' @param date_field The name of the date field in the full_data and new_data
#'        data frames.
#' @param last_date_in_chunk The last date in the current data chunk being
#'        processed.
#' @param min_date The earliest date in the data being processed.
#' @param max_date The latest date in the data being processed.
#' @param session_end_date The end date of the current session.
#'
#' @details The function checks for the existence of the log file. If the file
#'          exists, it appends the new log message. If not, it creates a new log
#'          file. The function constructs the log message from the given
#'          parameters, including data types and date ranges. It also handles
#'          the conversion of specific columns to date format and ensures that
#'          duplicate entries are removed.
#'
#' @examples
#' \dontrun{
#' new_log_message <- data.frame(...) # example log message
#' write_log_file_api(
#'   "api_log.csv", "myDataType", fullData, newData, "dateCol",
#'   "2023-01-01", "2023-01-01", "2023-01-10", "2023-01-10"
#' )
#' }
#' @export

write_log_file_api <- function(log_file_name,
                               data_type,
                               full_data,
                               new_data,
                               date_field,
                               last_date_in_chunk,
                               min_date,
                               max_date,
                               session_end_date) {
  # Construct the log message
  log_message <- data.frame(
    UpdateDate = as.Date(Sys.Date(), "%Y-%m-%d"),
    UpdateTime = format(Sys.time(), "%H:%M:%S %Z"),
    OverallStartDate = min(as.Date(full_data[[date_field]],
                                   format = "%Y-%m-%d"
    ), na.rm = TRUE),
    SessionStartDate = as.Date(min_date, format = "%Y-%m-%d"),
    SessionEndDate = as.Date(session_end_date, format = "%Y-%m-%d"),
    DataType = as.character(data_type),
    NewRowsAdded = format(nrow(new_data), big.mark = ","),
    stringsAsFactors = FALSE
  )
  
  # Append or create log file
  if (file.exists(log_file_name)) {
    log_data <- readRDS(log_file_name)
    log_data <- rbind(log_data, log_message)
  } else {
    log_data <- log_message
  }
  
  # Convert date-time columns
  date_cols <- c(
    "UpdateDate", "OverallStartDate",
    "SessionStartDate", "SessionEndDate"
  )
  log_data <- log_data |>
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(date_cols), as.Date)
    ) |>
    dplyr::distinct(
      dplyr::across(
        tidyselect::all_of(c(date_cols, "NewRowsAdded"))
      ),
      .keep_all = TRUE
    )
  
  # Save log file
  saveRDS(log_data, log_file_name)
}





