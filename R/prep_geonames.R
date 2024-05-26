#' Save Dataframe to RDS File
#'
#' Prompts the user for confirmation before saving a dataframe to a RDS file.
#' If the specified file path does not exist or is NULL, the user is prompted to
#' provide a new path. A default file name is used if no valid path is provided.
#'
#' @param data_to_save DataFrame to be saved.
#' @param default_save_path Default path for saving the dataframe. If not
#'                     provided or invalid, the user is prompted for a new path.
#'                    Defaults to \code{NULL}.
#'
#' @return Invisible \code{NULL}. The function's primary purpose is to saving a
#'        file, not to return a value.
#'
#' @examples
#' # handle_file_save(data_to_save, "path/to/default/location.rds")
#'
#' @keywords internal
handle_file_save <- function(data_to_save, default_save_path = NULL) {
  cache_path <- default_save_path
  
  while (TRUE) {
    # prompt the user to confirm if they want to save the cleaned cache file
    confirm_save <- tolower(
      readline("Do you want to save the cleaned cache file? [y/n]: ")
    )
    
    if (confirm_save == "y") {
      if (is.null(cache_path) || !file.exists(cache_path)) {
        cli::cli_alert_warning(
          "The specified file path is null or the file does not exist."
        )
        
        # Ask for a new file path
        cache_path <- readline(
          prompt = "Enter the new file path for saving: "
        )
        
        # check if user wants to create a new file if path does not exist
        if (!file.exists(cache_path)) {
          # give alternative name for saving if none given
          cache_path <- paste0(getwd(), "/prepped_geoname_cache.rds")
          
          cli::cli_alert_info(paste0(
            "The specified path does not exist, a default path and",
            " name will be used."
          ))
        }
      }
      
      # save the file
      saveRDS(
        data_to_save, cache_path
      )
      cli::cli_alert_success(
        "File saved successfully to {cache_path}."
      )
      break
    } else if (confirm_save == "n") {
      cli::cli_alert_info(
        "File not saved. Proceeding without saving..."
      )
      break
    } else {
      cli::cli_alert_warning(
        "Invalid input. Please respond with 'y' for yes or 'n' for no."
      )
    }
  }
}

#' Calculate and Report Geo-naming Match Statistics
#'
#' Compares entries in a given dataset against a lookup dataset across specified
#' admn levels (e.g., countries, (province/state/region), districts) to 
#' calculate and report match statistics.
#'
#' @param data A dataframe containing the target data to be matched.
#' @param lookup_data A dataframe serving as the reference for matching.
#' @param level0 level0 col name (country) in both 'data' and 'lookup_data'.
#' @param level1 level1 col name (province/state/region) in both 'data' and 
#'            'lookup_data'.
#' @param level2 level2 col name (district) in both 'data' and 'lookup_data'.
#'
#' @details Calculates unique matches across administrative levels, reports
#'        match and mismatch counts.
#'
#' @return Invisible NULL. The primary purpose of this function is to reporting
#'        match stats rather than return a value.
#'
#' @examples
#' # calculate_match_stats(
#' #      my_data, my_lookup, "country", "province", "district")
#'
#' @keywords internal
calculate_match_stats <- function(data, lookup_data, level0 = NULL, 
                                  level1 = NULL, level2 = NULL) {
  
  # Calculate unique matches for each admin level
  results <- list()
  
  if (!is.null(level0)) {
    matches_level0 <- sum(unique(data[[level0]]) %in% 
                            unique(lookup_data[[level0]]))
    results$level0 <- c(
      "matches" = matches_level0, "total" = length(unique(data[[level0]])))
  }
  
  if (!is.null(level1)) {
    matches_level1 <- sum(
      unique(data[[level1]]) %in% unique(lookup_data[[level1]]))
    results$level1 <- c(
      "matches" = matches_level1, "total" = length(unique(data[[level1]])))
  }
  
  if (!is.null(level2)) {
    matches_level2 <- sum(
      unique(data[[level2]]) %in% unique(lookup_data[[level2]]))
    results$level2 <- c(
      "matches" = matches_level2, "total" = 
        length(unique(data[[level2]])))
  }
  
  # Presenting the results using cli
  cli::cli_alert_info("Match Summary:")
  cli::cli_ul()
  
  if (!is.null(level0)) {
    cli::cli_li(
      glue::glue(
        "{level0} (level 0): {results$level0['matches']} ",
        "out of {results$level0['total']} matched"))
  }
  
  if (!is.null(level1)) {
    cli::cli_li(glue::glue(
      "{level1} (level 1): {results$level1['matches']} ",
      "out of {results$level1['total']} matched"))
  }
  
  if (!is.null(level2)) {
    cli::cli_li(
      glue::glue(
        "{level2} (level 2): {results$level2['matches']} ",
        "out of {results$level2['total']} matched"))
  }
  
  cli::cli_end()
  invisible(NULL)
}

#' Display a Custom Menu and Capture User Choice
#'
#' An alternative to base R's `menu` function, using `cli` for enhanced
#' interactivity and style. Displays a menu with given options and special
#' actions, capturing user selection through a custom prompt.
#'
#'
#'
#' @param title The menu title.
#' @param choices_input Vector of option strings to display.
#' @param special_actions Named list of special actions with string identifiers.
#' @param prompt String to display for user input prompt.
#'
#' @return The selected option's identifier (numeric or special action key).
#' @importFrom cli cli_h2 cli_text cli_alert_warning
#' @examples
#' # display_custom_menu("Choose an option:", c("Option 1", "Option 2"),
#' #                   list(x = "Skip", y = "Save"), "Your choice:")
#'
#' @keywords internal
display_custom_menu <- function(title, main_header, choices_input,
                                special_actions, prompt) {
  cli::cli_h1(main_header)
  
  # display the title using cli
  cli::cli_h2(title)
  
  # display the replacement options with cli styling
  options_nums <- seq_along(choices_input)
  for (i in options_nums) {
    cli::cli_text(glue::glue("{i}: {choices_input[i]}"))
  }
  
  # display the special actions with a skipped line and non-numeric identifiers
  cli::cli_text("\n") # Skipped line for visual separation
  special_actions_keys <- names(special_actions)
  for (key in special_actions_keys) {
    cli::cli_text(glue::glue("{key}: {special_actions[[key]]}"))
  }
  
  cat("\n")
  # capture and handle user input
  choice <- NA
  repeat {
    choice <- tolower(readline(prompt = paste0(prompt)))
    if (choice %in% c(
      as.character(options_nums), tolower(special_actions_keys)
    )) {
      break
    }
    cli::cli_alert_warning("Invalid choice, please try again.")
  }
  
  choice
}

#' Calculate String distances Between Admin Names
#'
#' Computes the string distances between administrative names to be cleaned and
#' a set of lookup administrative names using the specified method. Returns the
#' top N closest matches for each name.
#'
#' @param admins_to_clean A vector of administrative names to be cleaned.
#' @param lookup_admins A vector of administrative names for lookup.
#' @param method The method used to calculate string distances (e.g., "jaro",
#'        "levenshtein"). It can take a number of alo
#'
#' @return A dataframe detailing top N matches for each name, including
#'        algorithm_name, name, matches, distances, and ranks.
#'
#'
#' @examples
#' # calculate_string_distance(c("New York", "Los Angeles"),
#' # c("New York", "Los Angeles", "Chicago"),
#' #   method = "lv"
#' # )
#'
#' @keywords internal
calculate_string_distance <- function(
    admins_to_clean, lookup_admins, method) {
  # calculate string distances between each admin name to
  # be cleaned and all lookup admin names
  suppressWarnings({
    scores <- stringdist::stringdistmatrix(
      admins_to_clean,
      lookup_admins,
      method = method
    )
  })
  
  
  n_matches <- length(unique(lookup_admins))
  
  # initialize a list to store results for each admin name
  results <- list()
  
  # iterate over each row (admin name to be cleaned)
  for (i in seq_len(nrow(scores))) {
    # sort distances and get indices ot top ones
    sorted_indices <- order(scores[i, ], decreasing = FALSE)[1:n_matches]
    
    # extract corresponding match details
    matched_names <- lookup_admins[sorted_indices]
    distances <- scores[i, sorted_indices]
    
    # store in results list
    results[[i]] <- data.frame(
      algorithm_name = method,
      name_to_match = admins_to_clean[i],
      matched_names = matched_names,
      distance = distances,
      match_rank = 1:n_matches
    )
  }
  
  # combine all results into a single data frame
  results_df <- do.call(rbind, results)
  
  results_df
}

#' Interact with Users for Data Cleaning Choices
#'
#' Presents an interactive CLI menu using `cli` for users to make selections on
#' data cleaning choices, particularly for administrative names like countries,
#' provinces, and districts. It allows users to replace, skip, save, or exit,
#' incorporating user feedback into the data cleaning process.
#'
#' @param input_data Data frame containing admin names etc.,.
#' @param levels The avaiable admin levels withnin the prep_geoname function
#' @param level The admins level being cleaned, i.ie level1 or even disrict.
#' @param clear_console Logical, whether to clear the console before showing
#'                  prompts; defaults to TRUE.
#'
#' @return Data frame of user-selected replacements if any; otherwise,
#'          provides feedback based on user actions.
#'
#' @examples
#' # handle_user_interaction(my_data, "level1", TRUE)
#'
#' @keywords internal
handle_user_interaction <- function(input_data, levels, level,
                                    clear_console = T, stratify) {
  # Interactivity --------------------------------------------------------------
  
  # set up the messaging prompts at the start of the function
  prompts <- c(
    "What'll it be?:",
    "Your next move?:",
    "How shall we proceed?:",
    "Pick your path:",
    "Let's make a choice!:",
    "Forge ahead to:",
    "Plot your trajectory:",
    "Navigate your destiny:",
    "Select and soar!:",
    "Decisions, decisions!:",
    "Where to next?:"
  )
  prompt <- sample(prompts)[1]
  
  # filter out missing cachees
  input_data <- input_data |>
    dplyr::filter(
      !is.na(matched_names) &  !is.na(name_to_match))
  
  # set cachees for looping
  unique_names <- unique(input_data$name_to_match)
  number_names <- length(unique_names)
  
  # initialize empty lists to store user choices
  user_choices <- list()
  user_choice <- NULL
  
  # loop through unmatched records in input_data
  # Initialize the index
  i <- 1
  while (i <= length(unique_names)) {
    # clear console
    if (clear_console) {
      cat("\014")
      cat("\033[2J", "\033[H")
    }
    
    # Define color using crayon function
    red <- crayon::red
    bl <- crayon::blue
    gr <- crayon::green
    b <- crayon::bold
    p <- crayon::underline
    
    # set up choices -----------------------------------------------------------
    # select the cache to clean and suggested replacements
    name_to_clean <- unique_names[i]
    replacement_name <- input_data |>
      dplyr::filter(
        name_to_match == name_to_clean
      ) |>
      dplyr::distinct(matched_names) |>
      dplyr::pull() |>
      stringr::str_to_title()
    
    # get unique long names 
    unique_geo_long <-  input_data |>
      dplyr::filter(
        name_to_match == name_to_clean) |>
      dplyr::distinct()
    
    # # apply red highlight only to the last choice if going back
    # if (!is.null(user_choice) && user_choice == "b") {
    #   replacement_name <- sapply(replacement_name, function(x) {
    #     if (x == stringr::str_to_title(last_choice)) b(red(x)) else x
    #   })
    # }
    
    # narrow down to top 25 if not stratified
    if (!stratify) {
      replacement_name <- replacement_name[1:20]
    }
    
    # set output title ---------------------------------------------------------
    
    # set up main header to keep track
    main_header <- glue::glue(
      "{stringr::str_to_title(level)} {i} of {length(unique_names)}"
    )
    
    if (!is.na(levels[2]) && stratify && level == levels[2]) {
      level_label = "level1"
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      str_long_geo <- stringr::str_to_title(long_geo)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " with in {bl(str_long_geo)}?"
      )
    } else if (!is.na(levels[3]) &&  stratify && level == levels[3]) {
      level_label = "level2"
      long_geo <- unique_geo_long$long_geo[1]
      long_geo_country <- stringr::str_to_title(
        strsplit(long_geo, "_")[[1]][[1]]
      )
      long_geo_province <- stringr::str_to_title(
        strsplit(long_geo, "_")[[1]][[2]]
      )
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " with in the {gr(long_geo_province)} province ",
        "of {bl(long_geo_country)}?"
      )
    } else if (!is.na(levels[1]) &&  stratify && level == levels[1]) {
      level_label = "level0"
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace ",
        "{b(red(str_cache))} with?"
      )
    } else if (!stratify) {
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace ",
        "{b(red(str_cache))} with?"
      )
    }
    
    # action set up ------------------------------------------------------------
    special_actions <- list(
      "B" = "Go Back",
      "S" = "Skip this one",
      "E" = "Save and exit",
      "Q" = "Exit without saving",
      "M" = "Enter name manually"
    )
    
    # present the menu to the user ---------------------------------------------
    user_choice <- display_custom_menu(
      title, main_header,
      replacement_name,
      special_actions,
      prompt = prompt
    )
    
    # handle user choices ------------------------------------------------------
    if (user_choice == "b") { # Go Back
      if (i > 1) {
        i <- i - 1
        next
      } else {
        cli::cli_alert_warning("You can't go back further.")
      }
    } else if (user_choice == "s") { # Skip this one
      cli::cli_alert_info("You are skipping this one...")
      i <- i + 1
      next
    } else if (user_choice == "e") { # Save and exit
      if (length(user_choices) > 0) {
        cli::cli_alert_success("Choices saved successfully. Exiting...")
      } else {
        cli::cli_alert_warning("No choices to save.")
      }
      break # exit the loop entirely
    } else if (user_choice == "q") { # Exit without saving
      confirm_exit <- tolower(
        readline("Are you sure you want to exit without saving? [y/n]: ")
      )
      if (confirm_exit == "y") {
        cli::cli_alert_danger("You have exited without saving...")
        return(NULL)
      } else {
        cli::cli_alert_info("Returning to menu...")
      }
    } else if (user_choice == "m") { # Enter name manually
      manual_name <- readline(prompt = "Enter the name manually: ")
      if (manual_name != "") {
        user_choices[[length(user_choices) + 1]] <- data.frame(
          level = level_label,
          name_to_match = name_to_clean,
          replacement = replace_int,
          longname_to_match = ifelse(
            level != levels[1], paste0(long_geo, "_", name_to_clean), 
            name_to_clean),
          longname_corrected = ifelse(
            level != levels[1], paste0(long_geo, "_", replace_int), long_geo),
          created_time = format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
        cli::cli_alert_success("Manual name entered successfully.")
      } else {
        cli::cli_alert_warning("No name entered. Returning to menu...")
      }
      i <- i + 1
    } else {
      suppressWarnings({
        replace_int <- toupper(replacement_name[as.integer(user_choice)])
        user_choices[[length(user_choices) + 1]] <- data.frame(
          level = level_label,
          name_to_match = name_to_clean,
          replacement = replace_int,
          longname_to_match = ifelse(
            level != levels[1], paste0(long_geo, "_", name_to_clean), 
            name_to_clean),
          longname_corrected = ifelse(
            level != levels[1], paste0(long_geo, "_", replace_int), long_geo),
          created_time = format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
      })
      i <- i + 1
    }
  }
  
  # Aggregation user-chosen replacements into df -------------------------------
  
  # clear console
  if (clear_console) {
    cat("\014")
    cat("\033[2J", "\033[H")
  }
  
  if (length(user_choices) != 0) {
    # Combine user choices into a single data frame
    user_choices_df <- dplyr::bind_rows(user_choices) |> 
      # fix longname_corrected for country
      dplyr::mutate(
        longname_to_match = dplyr::if_else(
          level == "level0", replacement, longname_to_match
        ),
        longname_corrected = dplyr::if_else(
          level == "level0", replacement,  longname_corrected
        )
      ) 
    
    cli::cli_alert_success(
      "Your selections have been successfully saved. Exiting..."
    )
    # return results
    return(user_choices_df)
  } else {
    cli::cli_alert_warning(
      "No selections were made to save. Exiting..."
    )
    data.frame(
      level = NULL,
      name_to_match = NULL, 
      replacement = NULL, 
      longname_to_match = NULL,
      longname_corrected = NULL,
      created_time = NULL
    )
    # return(NULL)
  }
}

#' Construct Long Geographic Names
#'
#' This function creates a composite geographic identifier by concatenating
#' values from specified administrative level columns within a dataframe. 
#' @param data A dataframe containing the geographic data.
#' @param level0 level0 col name (country) in both 'data' and 'lookup_data'.
#' @param level1 level1 col name (province) in both 'data' and 'lookup_data'.
#' @param level2 level2 col name (district) in both 'data' and 'lookup_data'.
#'
#' @return Returns the dataframe with an additional column `long_geo` that 
#'         contains the concatenated geographic identifiers.
#'
#' @examples
#' # Assuming `data` is a dataframe with columns 'country', 'state', and 'city':
#' # data <- data.frame(
#' # country = c("USA", "USA", "Canada"),
#' #  state = c("California", NA, "Ontario"),
#' #  city = c("Los Angeles", "New York", "Toronto")
#' #)
#' # result <- construct_geo_names(data, "country", "state", "city")
construct_geo_names <- function(data, level0, level1, level2) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate(long_geo = {
      non_null_adms <- NULL
      if (!is.null(level0) && !is.na(get(level0))) {
        non_null_adms <- c(non_null_adms, get(level0))
      }
      if (!is.null(level1) && !is.na(get(level1))) {
        non_null_adms <- c(non_null_adms, get(level1))
      }
      if (!is.null(level2) && !is.na(get(level2))) {
        non_null_adms <- c(non_null_adms, get(level2))
      }
      paste(non_null_adms, collapse = "_")
    }) |>
    dplyr::ungroup()
}

#' Interactive Admin Name Cleaning and Matching
#'
#' This function streamlines the admin name cleaning process, leveraging both
#' algorithmic approaches and interactive user decisions. It is inspired 
#' by the string distance matching of the
#' \code{\link[epiCleanr]{clean_admin_names}} function (Mo
#' Yusuf at WHO AFRO) and the interactivity of `cache_to_plias` in the `stanley`
#' package by Steve Kroiss from BMGF. It combines string distance 
#' algorithms for initial matching and offers user interactivity for final 
#' decision-making, which are then saved for future reference and sharing.
#' Although the function does not require limiting name matching exclusively to 
#' upper-level admins, optimal performance is achieved by confining to stricter
#' within-admin stratifications (through the use of stratify function option),
#' ensuring more accurate results. The function can also work with site names
#' or even any string matching that has lookup data.
#'
#' @param target_df Data frame containing the admin names to clean.
#' @param lookup_df Lookup data frame for verifying admin names. If this is not
#'                  provided, an internal version of WHO geoname data 
#'                  attached to poliprep is used. 
#' @param level0 level0 col name (country) in both 'data' and 'lookup_data'.
#' @param level1 level1 col name (province) in both 'data' and 'lookup_data'.
#' @param level2 level2 col name (district) in both 'data' and 'lookup_data'.
#' @param cache_path Optional; the path where the cache data frame is
#'        saved after user modifications. This path is also used to match and
#'        integrate previously established corrections into the current
#'        session. If NULL or the file does not exist at the provided path,
#'        users will be prompted to specify a new path or create a new cache
#'        data frame.
#' @param method The string distance calculation method(s) to be used. Users
#'        can specify one or more algorithms from the
#'        \code{\link[stringdist]{stringdist}} package to compute
#'        string distances between admin names. The function by
#'        default uses \code{"jw"} (Jaro-Winkler). Other options include: 
#'        \code{"lv"} (Levenshtein), \code{"dl"}
#'        (Damerau-Levenshtein), \code{"lcs"} (Longest Common Subsequence),
#'        \code{"qgram"} (Q-Gram), \code{"jw"} (Jaro-Winkler), and
#'        \code{"soundex"}.
#' @param stratify Logical; if TRUE, performs cleaning stratified by
#'        admin levels to maintain hierarchical consistency.
#' @param interactive Logical; if TRUE, prompts the user for interactive
#'        matching decisions. Defaults to FALSE.
#'
#' @details
#' The function performs the following steps:
#' 1. Prepares the data by ensuring administrative names are in uppercase for
#'    consistent matching.
#' 2. Attempts to load a previously saved cache file if available, or
#'    initializes the cleaning process.
#' 3. Matches administrative names between `target_df` and `lookup_df` using
#'    string distance algorithms, running in parallel. Results are ranked 
#'    by closeness.
#' 4. Engages the user through an interactive CLI menu to make decisions on
#'    ambiguous matches.
#' 5. Saves the user's decisions in a cache data frame, either to a specified
#'    path or by prompting the user for a location.
#' 6. Returns a cleaned data frame with updated administrative names based on
#'    user choices and algorithmic matching.
#'
#' @return A data frame with cleaned administrative names and saved data frame
#'        of user decisions.
#'
#' @examples
#' # Dummy target data
#' # target_df <- data.frame(
#' # country = c("ANGOLA", "UGA", "ZAMBIA"),
#' #province = c("CABONDA", "TESO", "LUSAKA"),
#' # district = c("BALIZE", "BOKEDEA", "RAFUNSA")
#' #)
#' 
#' # Interactively clean geonames
#' # prep_geonames(
#' # target_df,
#' # level0 ="country", level1 = 'province',
#' # level2 = "district"
#' # )
#'
#' @importFrom rlang :=
#' @importFrom foreach %dopar%
#' @export
prep_geonames <- function(target_df, lookup_df = NULL,
                          level0 = NULL, 
                          level1 = NULL, 
                          level2 = NULL,
                          cache_path = NULL,
                          method = "jw",
                          stratify = TRUE,
                          interactive = TRUE) {
  
  
  # Validation -----------------------------------------------------------------
  
  # Ensure higher levels cannot be used without corresponding lower levels
  if (stratify && !is.null(level1) && is.null(level0)) {
    stop("You cannot specify level1 without level0.")
  }
  if (stratify && !is.null(level2) && (is.null(level0) || is.null(level1))) {
    stop("You cannot specify level2 without both level0 and level1.")
  }
  
  # Ensure lookup_df contains necessary columns if provided
  if (!is.null(lookup_df)) {
    required_columns <- NULL
    if (!is.null(level0)) required_columns <- c(required_columns, level0)
    if (!is.null(level1)) required_columns <- c(required_columns, level1)
    if (!is.null(level2)) required_columns <- c(required_columns, level2)
    
    missing_columns <- setdiff(required_columns, colnames(lookup_df))
    if (length(missing_columns) > 0) {
      stop(
        paste("The following columns are missing in lookup_df:", 
              paste(missing_columns, collapse = ", ")))
    }
  }
  
  
  # Ensure target_df contains necessary columns
  required_columns <- NULL
  if (!is.null(level0)) required_columns <- c(required_columns, level0)
  if (!is.null(level1)) required_columns <- c(required_columns, level1)
  if (!is.null(level2)) required_columns <- c(required_columns, level2)
  
  missing_columns <- setdiff(required_columns, colnames(target_df))
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "The following columns are missing in target_df:", 
        paste(missing_columns, collapse = ", ")))
  }
  
  # Ensure method is supported
  supported_methods <- c(
    "jw", "osa", "lv", "dl", "hamming", "lcs", "qgram", 
    "cosine", "jaccard", "soundex")
  if (
    !(method %in% supported_methods)) {
    stop(
      paste(
        "Unsupported method:", method, ". 
        Supported methods are:", paste(supported_methods, collapse = ", ")))
  }
  
  # Ensure stratify is logical
  if (!is.logical(stratify)) {
    stop("stratify must be a logical value (TRUE or FALSE).")
  }
  
  # Ensure interactive is logical
  if (!is.logical(interactive)) {
    stop("interactive must be a logical value (TRUE or FALSE).")
  }
  
  # Ensure cache_path is a valid file path if provided
  if (!is.null(cache_path) && !dir.exists(dirname(cache_path))) {
    stop("The directory for cache_path does not exist.")
  }
  
  # Validation: Ensure lookup_df is not empty if provided
  if (!is.null(lookup_df) && nrow(lookup_df) == 0) {
    stop("The lookup_df is empty.")
  }
  
  # Ensure level0, level1, and level2 are valid column names
  if (!is.null(level0) && !(level0 %in% colnames(target_df))) {
    stop(paste("The column", level0, "is not in target_df."))
  }
  if (!is.null(level1) && !(level1 %in% colnames(target_df))) {
    stop(paste("The column", level1, "is not in target_df."))
  }
  if (!is.null(level2) && !(level2 %in% colnames(target_df))) {
    stop(paste("The column", level2, "is not in target_df."))
  }
  
  # Step 0: Setup target and lookup datasets -----------------------------------
  
  # Get the internal shapefile if lookup data is not provided
  if (is.null(lookup_df)) {
    lookup_df <- poliprep::shp_global
    
    if (!is.null(level0)) {
      lookup_df <- dplyr::rename(lookup_df, !!level0 := ADM0_NAME) 
    }
    if (!is.null(level1)) {
      lookup_df <- dplyr::rename(lookup_df, !!level1 := ADM1_NAME)
    }
    if (!is.null(level2)) {
      lookup_df <- dplyr::rename(lookup_df, !!level2 := ADM2_NAME)
    }
  }
  
  # Create the levels vector
  levels <- c(
    if (exists("level0")) level0 else NULL,
    if (exists("level1")) level1 else NULL,
    if (exists("level2")) level2 else NULL)
  
  # Ensure administrative names are uppercase
  target_df <- target_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(levels), toupper
      )
    )
  
  lookup_df <- lookup_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(levels), toupper
      )
    )
  
  # Step 1: Configure cache if saved cache file exists availabel ---------------
  
  # load saved cache file
  if (!is.null(cache_path) && file.exists(cache_path)) {
    saved_cache_df <- readRDS(
      cache_path
    ) |> 
      # harmonised column names incase using old version of cache file
      dplyr::rename(level0_prepped = any_of("country_prepped")) |>
      dplyr::rename(level1_prepped = any_of("province_prepped")) |>
      dplyr::rename(level2_prepped = any_of("district_prepped")) |>
      
      dplyr::mutate(
        dplyr::case_when(
          level == "country" ~ "level0",
          level == "province" ~ "level1",
          level == "district" ~ "level2",
          TRUE ~ level
        )
      )
    
  } else {
    saved_cache_df <- data.frame()
    target_todo <- target_df
  }
  
  # if the cache file exists, merge it with the target data and replace
  # incorrect names with correct ones.
  if (!is.null(saved_cache_df) && nrow(saved_cache_df) > 0) {
    # join with saved_cache_df based on cleaned names
    # a admin eval at time in case not all exist
    if (!is.null(level0)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_cache_df |>
            dplyr::filter(level == "level0") |>
            dplyr::distinct(name_to_match, level0_prepped),
          by = stats::setNames("name_to_match", level0)
        ) |>
        dplyr::mutate(
          !!level0 := dplyr::coalesce(level0_prepped, .data[[level0]])
        )
    }
    
    if (!is.null(level1)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_cache_df |>
            dplyr::filter(level == "level1") |>
            dplyr::distinct(name_to_match, level0_prepped, level1_prepped),
          by = stats::setNames(
            c("level0_prepped", "name_to_match"), c(level0, level1))
        ) |>
        dplyr::mutate(
          !!level1 := dplyr::coalesce(level1_prepped, .data[[level1]])
        )
    }
    
    if (!is.null(level2)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_cache_df |>
            dplyr::filter(level == "level2") |>
            dplyr::distinct(name_to_match, level0_prepped,
                            level1_prepped, level2_prepped),
          by = stats::setNames(
            c("level0_prepped", "level1_prepped", "name_to_match"), 
            c(level0, level1, level2))
        ) |>
        dplyr::mutate(
          !!level2 := dplyr::coalesce(level2_prepped, .data[[level2]])
        )
    }
    
    # remove prepped columns
    target_df <- target_df |>
      dplyr::select(-matches("_prepped$"))
  }
  
  # Step 2: Filter out for those where there is a match ------------------------
  
  # get the original data
  orig_df <- target_df
  
  # Dynamically filter for missing geolocations
  filter_na_expr <- purrr::map(levels, ~ rlang::expr(is.na(!!.x)))
  target_df_na <- target_df |> dplyr::filter(!!!filter_na_expr)
  
  # Dynamically filter for non-missing geolocations
  filter_not_na_expr <- purrr::map(levels, ~ rlang::expr(!is.na(!!.x)))
  target_df <- target_df |> dplyr::filter(!!!filter_not_na_expr)
  
  # dynamically construct the long geonames on target data
  target_df <- construct_geo_names(target_df, level0, level1, level2)
  lookup_df <- construct_geo_names(lookup_df, level0, level1, level2)
  
  # filter to matched rows
  target_done <- target_df |>
    dplyr::filter(
      (long_geo %in%
         unique(lookup_df[["long_geo"]]))
    )
  
  # reduce down to only unmatched rows
  target_todo <- target_df |>
    dplyr::filter(
      !(long_geo %in%
          unique(lookup_df[["long_geo"]]))
    )
  
  calculate_match_stats(
    target_df, lookup_df, level0, level1, level2
  )
  
  # Early return with finalised_df
  if (nrow(target_todo) == 0) {
    cli::cli_alert_success(
      "All records matched; process completed. Exiting..."
    )
    
    return(orig_df)
  }
  
  # return if non-interactive.
  if (!interactive) {
    cli::cli_alert_success(
      "In non-interactive mode. Exiting after matching with cache..."
    )
    
    return(orig_df)
  }
  
  cli::cli_alert_info(
    "Partial match completed. There are still matches to be made.")
  
  user_input <- 
    readline("Would you like to do interactive matching? (yes/no):")
  
  if (!(tolower(user_input) %in% c("yes", "y"))) {
    cli::cli_alert_info(
      "Exiting without interactive matching..."
    )
    return(orig_df)
  }
  
  # Step 3: String distance matching in interactivty ---------------------------
  
  # initialize empty lists to store results
  unmatched_df_group <- list()
  cleaned_dfs <- list()
  
  # Initialize flag variable
  skip_to_end <- FALSE
  
  for (level in levels) {
    top_res_list <- list()
    
    # Check if the current level should be stratified
    if (stratify && level %in% c(levels[2], levels[3])) {
      # Set up the grouping level (previous level in hierarchy)
      
      # set up grouping level
      grouping_level <- ifelse(
        level %in% c(levels[2]), level0, level1
      )
      
      for (group in unique(target_todo[[grouping_level]])) {
        if (!(group %in% unique(lookup_df[[grouping_level]]))) {
          skip_to_end <- TRUE
          break
        }
        
        lookup_df_group <- lookup_df |>
          dplyr::filter(.data[[grouping_level]] == group)
        
        unmatched_df_group <- target_todo |>
          dplyr::filter(.data[[grouping_level]] == group) |>
          dplyr::filter(!(.data[[level]] %in% unique(lookup_df_group[[level]])))
        
        if (nrow(unmatched_df_group) == 0) next
        
        # Dynamically create a long_geo from the previous levels
        if (level %in% c(levels[3])) {
          long_geo_group <- paste(
            unmatched_df_group[[level0]][1],
            group,
            sep = "_"
          )
        } else {
          long_geo_group <- group
        }
        
        top_res <-
          calculate_string_distance(
            unmatched_df_group[[level]], 
            lookup_df_group[[level]], 
            method = method) |>
          dplyr::select(-match_rank, -algorithm_name) |>
          dplyr::group_by(name_to_match, matched_names) |>
          dplyr::slice_min(distance, with_ties = FALSE) |>
          dplyr::distinct(name_to_match, .keep_all = T) |>
          dplyr::arrange(name_to_match, distance) |>
          dplyr::select(name_to_match, matched_names) |>
          dplyr::ungroup() |>
          dplyr::mutate(long_geo = long_geo_group)
        
        top_res_list[[group]] <- top_res
      }
      
      if (skip_to_end) {
        break
      }
      
      top_res <- do.call(rbind, top_res_list)
    } else {
      unmatched_df_group <- target_todo |>
        dplyr::filter(!(.data[[level]] %in% unique(lookup_df[[level]])))
      
      if (nrow(unmatched_df_group) == 0) next
      
      top_res <-
        calculate_string_distance(
          unmatched_df_group[[level]],
          lookup_df[[level]],
          method = method
        ) |>
        dplyr::select(-match_rank, -algorithm_name) |>
        dplyr::group_by(name_to_match, matched_names) |>
        dplyr::slice_min(distance, with_ties = FALSE) |>
        dplyr::distinct(name_to_match, .keep_all = T) |>
        dplyr::arrange(name_to_match, distance) |>
        dplyr::select(name_to_match, matched_names) |>
        dplyr::ungroup() |> 
        dplyr::mutate(long_geo = name_to_match)
    }
    
    if (!is.null(top_res)) {
      replacement_df <- handle_user_interaction(
        top_res, levels, level, 
        stratify = stratify)
      cleaned_dfs[[level]] <- replacement_df
    } else {
      cleaned_dfs <- NULL
      replacement_df <- data.frame()
    }
    
    if (length(replacement_df) > 0) {
      target_todo <- target_todo |>
        dplyr::left_join(
          replacement_df |>
            dplyr::select(
              !!level := name_to_match, replacement),
          by = level
        ) |>
        dplyr::mutate(
          !!level := ifelse(
            is.na(replacement), .data[[level]], replacement)
        ) |>
        dplyr::select(-replacement)
    }
    
    target_todo <- construct_geo_names(
      target_todo, level0, level1, level2)
    
    if (skip_to_end) {
      break
    }
  }
  
  # Step 4: clean up the cache file and save -----------------------------------
  
  if (length(cleaned_dfs) > 0 && any(sapply(cleaned_dfs, nrow) > 0)) {
    
    # clean up the cache df
    suppressWarnings(
      cleaned_cache_joined <- dplyr::bind_rows(cleaned_dfs) |>
        tidyr::separate(
          longname_corrected,
          into = c("level0_prepped", "level1_prepped", "level2_prepped"),
          sep = "_", extra = "drop"
        ) |>
        dplyr::mutate(
          level0_prepped = dplyr::if_else(
            level == "level0", replacement, level0_prepped),
          level1_prepped = dplyr::if_else(
            level == "level1", replacement, level1_prepped),
          level2_prepped = dplyr::if_else(
            level == "level2", replacement, level2_prepped),
          dplyr::across( .cols = -created_time, ~ dplyr::na_if(.x, ""))
        ) |> 
        # add username
        dplyr::mutate(name_of_creator = Sys.getenv("RSTUDIO_USER_IDENTITY")) 
    )
    
    # combine cleaned data frames
    final_cache_dfs <-
      dplyr::bind_rows(saved_cache_df, cleaned_cache_joined) |>
      dplyr::mutate(
        longname_to_match = NA,
        longname_to_match = dplyr::case_when(
          is.na(longname_to_match) & level == "level0" ~ name_to_match,
          is.na(longname_to_match) & level == "level1" ~ 
            paste(level0_prepped, name_to_match,  sep = "_"),
          is.na(longname_to_match) & level == "level2" ~ 
            paste(level0_prepped, level1_prepped, 
                  name_to_match,  sep = "_")
        )) |> 
      dplyr::select(
        level, name_to_match, replacement, 
        # longname_corrected
        longname_to_match,
        level0_prepped, level1_prepped, level2_prepped, 
        created_time, name_of_creator
      ) |>
      dplyr::arrange(created_time) |> 
      dplyr::distinct(longname_to_match,
                      .keep_all = TRUE) |> 
      dplyr::select(-longname_to_match)
    
    # file saving
    handle_file_save(final_cache_dfs, cache_path)
  }
  
  # Step 5: Combine the cleaned data frames ------------------------------------
  
  finalised_df <- dplyr::bind_rows(target_done, target_todo, target_df_na) |> 
    dplyr::select(-long_geo)
  
  # get stats
  calculate_match_stats(
    finalised_df, lookup_df, level0, level1, level2
  )
  
  # return the final data frame
  return(finalised_df)
  
}
