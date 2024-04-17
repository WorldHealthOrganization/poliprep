#' Save Dataframe to CSV File
#'
#' Prompts the user for confirmation before saving a dataframe to a CSV file.
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
#' # handle_file_save(data_to_save, "path/to/default/location.csv")
#'
#' 
#' @keywords internal
#' @export
handle_file_save <- function(data_to_save, default_save_path = NULL) {
  save_alias_df_path <- default_save_path
  
  while (TRUE) {
    # prompt the user to confirm if they want to save the cleaned alias file
    confirm_save <- tolower(
      readline("Do you want to save the cleaned alias file? [y/n]: ")
    )
    
    if (confirm_save == "y") {
      if (is.null(save_alias_df_path) || !file.exists(save_alias_df_path)) {
        cli::cli_alert_warning(
          "The specified file path is null or the file does not exist."
        )
        
        # Ask for a new file path
        save_alias_df_path <- readline(
          prompt = "Enter the new file path for saving: "
        )
        
        # check if user wants to create a new file if path does not exist
        if (!file.exists(save_alias_df_path)) {
          # give alternative name for saving if none given
          save_alias_df_path <- paste0(getwd(), "/geoname_alias_cache.csv")
          
          cli::cli_alert_info(paste0(
            "The specified path does not exist, a default path and",
            " name will be used."
          ))
        }
      }
      
      # cave the file
      utils::write.csv(
        data_to_save, save_alias_df_path,
        row.names = FALSE, fileEncoding = "UTF-8"
      )
      cli::cli_alert_success(
        "File saved successfully to {save_alias_df_path}."
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
#' admn levels (e.g., countries, provinces, districts) to calculate and report
#' match statistics.
#'
#' @param data A dataframe containing the target data to be matched.
#' @param lookup_data A dataframe serving as the reference for matching.
#' @param adm0 adm0 col name (country) in both 'data' and 'lookup_data'.
#' @param adm1 adm1 col name (province) in both 'data' and 'lookup_data'.
#' @param adm2 adm2 col name (district) in both 'data' and 'lookup_data'.
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
#' @export
calculate_match_stats <- function(data, lookup_data,
                                  adm0 = NULL, adm1 = NULL, adm2 = NULL) {
  
  # Calculate unique matches for each admn level -------------------------------
  if (!is.null(adm0)) {
    matches_adm0 <- sum(unique(data[[adm0]]) %in% unique(lookup_data[[adm0]]))
  }
  
  if (!is.null(adm1)) {
    matches_adm1 <- sum(unique(data[[adm1]]) %in% unique(lookup_data[[adm1]]))
  }
  
  if (!is.null(adm2)) {
    matches_adm2 <- sum(unique(data[[adm2]]) %in% unique(lookup_data[[adm2]]))
  }
  
  if (!is.null(adm0) & !is.null(adm1)) {
    adm1_unique <- data |>
      dplyr::mutate(
        joined_adm = paste(get(adm0), get(adm1), sep = "-")
      ) |>
      dplyr::distinct(joined_adm) |>
      dplyr::pull()
    
    adm1_unique_lookup <- lookup_data |>
      dplyr::mutate(
        joined_adm = paste(get(adm0), get(adm1), sep = "-")
      ) |>
      dplyr::distinct(joined_adm) |>
      dplyr::pull()
    
    matches_adm1 <- sum(
      unique(adm1_unique) %in% unique(adm1_unique_lookup)
    )
  }
  
  if (!is.null(adm1) & !is.null(adm2)) {
    adm2_unique <- data |>
      dplyr::mutate(
        joined_adm = paste(get(adm0), get(adm1), get(adm2), sep = "-")
      ) |>
      dplyr::distinct(joined_adm) |>
      dplyr::pull()
    
    adm2_unique_lookup <- lookup_data |>
      dplyr::mutate(
        joined_adm = paste(get(adm0), get(adm1), get(adm2), sep = "-")
      ) |>
      dplyr::distinct(joined_adm) |>
      dplyr::pull()
    
    matches_adm2 <- sum(
      unique(adm2_unique) %in% unique(adm2_unique_lookup)
    )
  }
  
  # Presenting the results using cli -------------------------------------------
  cli::cli_alert_info("Match Summary:")
  cli::cli_ul()
  
  if (!is.null(adm0)) {
    unique_adm0 <- length(unique(data[[adm0]]))
    cli::cli_li("{adm0} (adm0): {matches_adm0} out of {unique_adm0} matched")
  }
  
  if (!is.null(adm1)) {
    unique_adm1 <- length(unique(data[[adm1]]))
    cli::cli_li("{adm1}  (adm1): {matches_adm1} out of {unique_adm1} matched")
  }
  
  if (!is.null(adm2)) {
    unique_adm2 <- length(unique(data[[adm2]]))
    cli::cli_li("{adm2}  (adm2): {matches_adm2} out of {unique_adm2} matched")
  }
  cli::cli_end()
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
#' @param options Vector of option strings to display.
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
#' @export
display_custom_menu <- function(title, main_header, options,
                                special_actions, prompt) {
  cli::cli_h1(main_header)
  
  # display the title using cli
  cli::cli_h2(title)
  
  # display the replacement options with cli styling
  options_nums <- seq_along(options)
  for (i in options_nums) {
    cli::cli_text(glue::glue("{i}: {options[i]}"))
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

#' Calculate String Distances Between Admin Names
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
#'        algorithm, name, matches, distances, and ranks.
#'
#'
#' @examples
#' calculate_string_distance(c("New York", "Los Angeles"),
#'   c("New York", "Los Angeles", "Chicago"),
#'   method = "lv"
#' )
#'
#' @keywords internal
#' @export
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
  for (i in  seq_len(nrow(scores))) {
    # sort distances and get indices ot top ones
    sorted_indices <- order(scores[i, ], decreasing = FALSE)[1:n_matches]
    
    # extract corresponding match details
    matched_names <- lookup_admins[sorted_indices]
    distances <- scores[i, sorted_indices]
    
    # store in results list
    results[[i]] <- data.frame(
      Algorithm = method,
      AdminToClean = admins_to_clean[i],
      MatchedNames = matched_names,
      Distance = distances,
      MatchRank = 1:n_matches
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
#' @param adm_level The admins level being cleaned, i.ie adm1 or even disrict.
#' @param clear_console Logical, whether to clear the console before showing
#'                  prompts; defaults to TRUE.
#'
#' @return Data frame of user-selected replacements if any; otherwise,
#'          provides feedback based on user actions.
#'
#' @examples
#' # handle_user_interaction(my_data, "adm1", TRUE)
#'
#' @keywords internal
#' @export
handle_user_interaction <- function(input_data, adm_level,
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
  
  # set aliases for looping
  unique_aliases <- unique(input_data$AdminToClean)
  num_aliases <- length(unique_aliases)
  
  # initialize empty lists to store user choices
  user_choices <- list()
  user_choice <- NULL
  
  # loop through unmatched records in input_data
  # Initialize the index
  i <- 1
  while (i <= length(unique_aliases)) {
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
    # select the alias to clean and suggested replacements
    alias_to_clean <- unique_aliases[i]
    replacement_alias <- input_data |>
      dplyr::filter(
        AdminToClean == alias_to_clean &
          !is.na(MatchedNames)
      ) |>
      dplyr::distinct(MatchedNames) |>
      dplyr::pull() |>
      stringr::str_to_title()
    
    # # apply red highlight only to the last choice if going back
    # if (!is.null(user_choice) && user_choice == "b") {
    #   replacement_alias <- sapply(replacement_alias, function(x) {
    #     if (x == stringr::str_to_title(last_choice)) b(red(x)) else x
    #   })
    # }
    
    # narrow down to top 25 if not stratified
    if (!stratify) {
      replacement_alias <- replacement_alias[1:20]
    }
    
    # set output title ---------------------------------------------------------
    
    # set up main header to keep track
    main_header <- glue::glue(
      "{stringr::str_to_title(adm_level)} {i} of {length(unique_aliases)}"
    )
    
    if (adm_level %in% c("adm1", "province") && stratify) {
      long_geo <- input_data$long_geo[i]
      str_alias <- stringr::str_to_title(alias_to_clean)
      str_long_geo <- stringr::str_to_title(long_geo)
      title <- glue::glue(
        "Which province name would you like to replace {b(red(str_alias))}",
        " with in {bl(str_long_geo)}?"
      )
    } else if (adm_level %in% c("adm2", "district") && stratify) {
      long_geo <- input_data$long_geo[i]
      long_geo_country <- stringr::str_to_title(
        strsplit(long_geo, "_")[[1]][[1]]
      )
      long_geo_province <- stringr::str_to_title(
        strsplit(long_geo, "_")[[1]][[2]]
      )
      str_alias <- stringr::str_to_title(alias_to_clean)
      title <- glue::glue(
        "Which district name would you like to replace {b(red(str_alias))}",
        " with in the {gr(long_geo_province)} province ",
        "of {bl(long_geo_country)}?"
      )
    } else if (adm_level %in% c("adm0", "country") && stratify) {
      long_geo <- NULL
      str_alias <- stringr::str_to_title(alias_to_clean)
      title <- glue::glue(
        "Which country name would you like to replace ",
        "{b(red(str_alias))} with?"
      )
    } else if (!stratify) {
      long_geo <- NULL
      str_alias <- stringr::str_to_title(alias_to_clean)
      title <- glue::glue(
        "Which {adm_level} name would you like to replace ",
        "{b(red(str_alias))} with?"
      )
    }
    
    # action set up ------------------------------------------------------------
    special_actions <- list(
      "B" = "Go Back",
      "S" = "Skip this one",
      "E" = "Save and exit",
      "Q" = "Exit without saving"
    )
    
    # present the menu to the user ---------------------------------------------
    user_choice <- display_custom_menu(
      title, main_header,
      replacement_alias,
      special_actions,
      prompt = prompt
    )
    
    # handle user choices ------------------------------------------------------
    if (user_choice == "b") { # Go Back
      if (i > 1) {
        i <- i - 1
        
        # # set up last choice for highlighting (if applicable)
        # last_choice <- if (
        #   length(user_choices) > 0) user_choices[[i]]$replacement else NULL
        # 
        # # inform user if there are no previous choices
        # if (is.null(last_choice)) {
        #   cli::cli_alert_warning(
        #   "There are no previous choices to revert to.")
        # } else {
        #   cli::cli_alert_info("Reverted to previous decision.")
        # }
        next
      } else {
        cli::cli_alert_warning("You can't go back further.")
      }
    }  else if (user_choice == "s") { # Skip this one
      cli::cli_alert_info("You are skipping this one...")
      i <- i + 1
      next
    } else if (user_choice == "e") { # Save and exit
      if (length(user_choices) > 0) {
        # convert the list of user choices to a data frame before saving
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
        # break # exit the loop without saving
        return(NULL)
      } else {
        cli::cli_alert_info("Returning to menu...")
      }
    } else {
      suppressWarnings({
        # handle replacement choice
        replace_int <- toupper(replacement_alias[as.integer(user_choice)])
        user_choices[[length(user_choices) + 1]] <- data.frame(
          AdminToClean = alias_to_clean,
          replacement = replace_int,
          name_alias = paste(input_data$long_geo[i], alias_to_clean, sep = "_"),
          name_corrected = paste(input_data$long_geo[i], replace_int,sep = "_"),
          level = adm_level
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
      # fix long_geo for country
      dplyr::mutate(
        name_alias = dplyr::if_else(
          level == "country", alias_to_clean, name_alias
        ),
        name_corrected = dplyr::if_else(
          level == "country", replacement, name_corrected
        )
      )
    
    cli::cli_alert_success(
      "Your selections have been successfully saved. Exiting..."
    )
    # return results
    return(user_choices_df)
  } else if (tolower(user_choice) == "s") {
    cli::cli_alert_warning(
      "No selections were made to save. Exiting..."
    )
  } else if (options[user_choice] != "Exit without saving") {
    cli::cli_alert_info(paste0(
      "No selections were saved.",
      " It appears all available choices were skipped."
    ))
  }
}

#' Interactive Admin Name Cleaning and Matching
#'
#' This function streamlines the admin name cleaning process, leveraging both
#' algorithmic approaches and interactive user decisions. It is inspired by the
#' string distance matching of the
#' \code{\link[epiCleanr]{clean_admin_names}} function (by Mo
#' Yusuf at WHO AFRO) and the interactivity of `alias_to_plias` in the `stanley`
#' package by Steve Kroiss from BMGF. It combines string distance algorithms for
#' initial matching and offers user interactivity for final decision-making,
#' which are then saved for future reference and sharing. Although the function
#' does not require limiting name matching exclusively to upper-level admins,
#' optimal performance is achieved by confining to stricter
#' within-admin stratifications (through the use of stratify function option),
#' ensuring more accurate results.
#'
#'
#' @param target_df Data frame containing the admin names to clean.
#' @param lookup_df Lookup data frame for verifying admin names.
#' @param adm0 adm0 col name (country) in both 'data' and 'lookup_data'.
#' @param adm1 adm1 col name (province) in both 'data' and 'lookup_data'.
#' @param adm2 adm2 col name (district) in both 'data' and 'lookup_data'.
#' @param save_alias_df_path Optional; the path where the alias data frame is
#'        saved after user modifications. This path is also used to match and
#'        integrate previously established  corrections into the current
#'        session. If NULL or the file does not exist at the provided path,
#'        users will be prompted to specify a new path or create a new alias
#'        data frame.
#' @param method The string distance calculation method(s) to be used. Users
#'        can specify one or more algorithms from the
#'        \code{\link[stringdist]{stringdist}} package to compute
#'        string distances between admin names. If left NULL, the function
#'        defaults to using a comprehensive set of algorithms, applying them in
#'        parallel to identify and rank the best matches based on closeness.
#'        The default methods include: \code{"lv"} (Levenshtein), \code{"dl"}
#'        (Damerau-Levenshtein), \code{"lcs"} (Longest Common Subsequence),
#'        \code{"qgram"} (Q-Gram), \code{"jw"} (Jaro-Winkler), and
#'        \code{"soundex"}.
#' @param stratify Logical; if TRUE, performs cleaning stratified by
#'        admin levels to maintain hierarchical consistency.
#'
#' @details
#' The function performs the following steps:
#' 1. Prepares the data by ensuring administrative names are in uppercase for
#'    consistent matching.
#' 2. Attempts to load a previously saved alias file if available, or
#'    initializes the cleaning process.
#' 3. Matches administrative names between `target_df` and `lookup_df` using
#'    string distance algorithms, running in parallel. Results are ranked by
#'    closeness.
#' 4. Engages the user through an interactive CLI menu to make decisions on
#'    ambiguous matches, leveraging the `cli` package for enhanced usability.
#' 5. Saves the user's decisions in an alias data frame, either to a specified
#'    path or by prompting the user for a location.
#' 6. Returns a cleaned data frame with updated administrative names based on
#'    user choices and algorithmic matching.
#'
#' @return A data frame with cleaned administrative names and saved data frame
#'        of users decisions.
#'
#' @examples
#' # results <- prep_geonames(
#' #          target_df, lookup_df, "country", "province", "district")
#'
#' @importFrom rlang :=
#' @export
prep_geonames <- function(target_df, lookup_df,
                          adm0 = NULL, adm1 = NULL, adm2 = NULL,
                          save_alias_df_path = NULL,
                          method = NULL,
                          stratify = TRUE) {
  # set string distance methods if null
  if (is.null(method)) {
    methods <- c("lv", "dl", "lcs", "qgram", "jw", "soundex")
  }
  
  # set admin levels
  adm_levels <- c(adm0, adm1, adm2)
  
  # Ensure administrative names are uppercase
  target_df <- target_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(adm0, adm1, adm2)), toupper
      )
    )
  
  lookup_df <- lookup_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(adm0, adm1, adm2)), toupper
      )
    )
  
  # Step 1: Configure alias if saved alias file exists available ---------------
  
  # load saved alias file
  if (!is.null(save_alias_df_path) && file.exists(save_alias_df_path)) {
    saved_alias_df <- utils::read.csv(
      save_alias_df_path,
      fileEncoding = "UTF-8"
    )
  } else {
    saved_alias_df <- data.frame()
    target_todo <- target_df
  }
  
  # if the alias file exists, merge it with the target data and replace
  # incorrect names with correct ones.
  if (!is.null(saved_alias_df) && nrow(saved_alias_df) > 0) {
    # join with saved_alias_df based on cleaned names
    # a admin eval at time in case not all exist
    if (!is.null(adm0)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_alias_df |>
            dplyr::filter(level == adm0) |>
            dplyr::distinct(AdminToClean, country_prepped),
          relationship = "many-to-many",
          by = stats::setNames("AdminToClean", adm0)
        ) |>
        dplyr::mutate(
          country = dplyr::coalesce(country_prepped, country)
        )
    }
    
    if (!is.null(adm1)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_alias_df |>
            dplyr::filter(level == adm1) |>
            dplyr::distinct(AdminToClean, province_prepped),
          relationship = "many-to-many",
          by = stats::setNames("AdminToClean", adm1)
        ) |>
        dplyr::mutate(
          province = dplyr::coalesce(province_prepped, province)
        )
    }
    
    if (!is.null(adm2)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_alias_df |>
            dplyr::filter(level == adm2) |>
            dplyr::distinct(AdminToClean, district_prepped),
          relationship = "many-to-many",
          by = stats::setNames("AdminToClean", adm2)
        ) |>
        dplyr::mutate(
          district = dplyr::coalesce(district_prepped, district)
        )
    }
    
    # remove prepped columns
    target_df <- target_df |>
      dplyr::select(-matches("_prepped$"))
  }
  
  # Step 2: Filter out for those where there is a match ------------------------
  
  # dynamically construct the long geonames on target data
  target_df <- dplyr::rowwise(target_df)
  target_df <- target_df |>
    dplyr::mutate(long_geo = {
      non_null_adms <- NULL
      
      if (!is.null(adm0) &&
          !is.na(get(adm0))) {
        non_null_adms <- c(non_null_adms, get(adm0))
      }
      if (!is.null(adm1) &&
          !is.na(get(adm1))) {
        non_null_adms <- c(non_null_adms, get(adm1))
      }
      if (!is.null(adm2) &&
          !is.na(get(adm2))) {
        non_null_adms <- c(non_null_adms, get(adm2))
      }
      
      paste(non_null_adms, collapse = "_")
    })
  
  target_df <- dplyr::ungroup(target_df)
  
  # dynamically construct the prepped names on lookup data
  lookup_df <- dplyr::rowwise(lookup_df)
  lookup_df <- lookup_df |>
    dplyr::mutate(long_geo = {
      non_null_adms <- NULL
      
      if (!is.null(adm0) &&
          !is.na(get(adm0))) {
        non_null_adms <- c(non_null_adms, get(adm0))
      }
      if (!is.null(adm1) &&
          !is.na(get(adm1))) {
        non_null_adms <- c(non_null_adms, get(adm1))
      }
      if (!is.null(adm2) &&
          !is.na(get(adm2))) {
        non_null_adms <- c(non_null_adms, get(adm2))
      }
      
      paste(non_null_adms, collapse = "_")
    })
  
  lookup_df <- dplyr::ungroup(lookup_df)
  
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
    target_df, lookup_df, adm0, adm1, adm2
  )
  
  # Early return with finalised_df
  if (nrow(target_todo) == 0) {
    cli::cli_alert_success(
      "All records matched; process completed. Exiting..."
    )
    return(target_done) # Early return with finalised_df
  } else {
    cli::cli_alert_info(
      "Partial match completed. Now carrying out string distance matching..."
    )
  }
  
  # Step 3: String distance those that are unmatched ---------------------------
  
  # initialize empty lists to store results
  unmatched_dfs <- list()
  cleaned_dfs <- list()
  
  # loop through administrative levels
  for (adm_level in adm_levels) {
    # Initialize empty list for top results within each level
    top_res_list <- list()
    
    # nested loop for province and district levels (stratified)
    if (stratify & adm_level %in% c("adm1", "adm2", "province", "district")) {
      # set up grouping level
      grouping_level <- ifelse(
        adm_level %in% c("adm1", "province"), "country", "province"
      )
      
      # loop through unique groups within the previous level
      for (group in unique(target_todo[[grouping_level]])) {
        # Filter unmatched records for this group (country/province)
        lookup_df_group <- lookup_df |>
          dplyr::filter(.data[[grouping_level]] == group)
        
        unmatched_df_group <- target_todo |>
          dplyr::filter(.data[[grouping_level]] == group) |>
          dplyr::filter(
            !(.data[[adm_level]] %in%
                unique(lookup_df_group[[adm_level]]))
          )
        
        # skip if no unmatched records in this group
        if (nrow(unmatched_df_group) == 0) next
        
        # if adm_level is adm2 or district, create a long_geo from
        # country and province
        if (adm_level %in% c("adm2", "district")) {
          long_geo_group <- paste(
            unmatched_df_group[[adm0]][1],
            group,
            sep = "_"
          )
        } else {
          long_geo_group <- group
        }
        
        # calculate string distance for this group
        top_res <- parallel::mclapply(
          methods,
          function(method) {
            calculate_string_distance(
              unmatched_df_group[[adm_level]],
              lookup_df_group[[adm_level]],
              method
            )
          },
          mc.cores = parallel::detectCores() - 1
        ) |>
          dplyr::bind_rows() |>
          dplyr::select(-MatchRank, -Algorithm) |>
          dplyr::group_by(AdminToClean, MatchedNames) |>
          dplyr::slice_min(Distance, with_ties = FALSE) |>
          dplyr::distinct(AdminToClean, .keep_all = T) |>
          dplyr::arrange(AdminToClean, Distance) |>
          dplyr::select(AdminToClean, MatchedNames) |>
          dplyr::ungroup() |>
          dplyr::mutate(long_geo = long_geo_group)
        
        # store top results for this group
        top_res_list[[group]] <- top_res
      }
      
      # combine top results for all groups within this level
      top_res <- do.call(rbind, top_res_list)
    } else {
      #  filter to nonmatched countries
      unmatched_df_group <- target_todo |>
        dplyr::filter(
          !(.data[[adm_level]] %in%
              unique(lookup_df[[adm_level]]))
        )
      
      # skip if no unmatched records in this group
      if (nrow(unmatched_df_group) == 0) next
      
      # standard processing for all levels or non-strict province/district
      top_res <- parallel::mclapply(
        methods,
        function(method) {
          calculate_string_distance(
            target_todo[[adm_level]],
            lookup_df[[adm_level]],
            method
          )
        },
        mc.cores = parallel::detectCores() - 1
      ) |>
        dplyr::bind_rows() |>
        dplyr::select(-MatchRank, -Algorithm) |>
        dplyr::group_by(AdminToClean, MatchedNames) |>
        dplyr::slice_min(Distance, with_ties = FALSE) |>
        dplyr::distinct(AdminToClean, .keep_all = T) |>
        dplyr::arrange(AdminToClean, Distance) |>
        dplyr::select(AdminToClean, MatchedNames) |>
        dplyr::ungroup()
    }
    
    if (!is.null(top_res)) {
      # handle user interaction for replacements
      replacement_df <- handle_user_interaction(
        top_res, adm_level,
        stratify = stratify
      )
      
      # store cleaned data for this level
      cleaned_dfs[[adm_level]] <- replacement_df
      
    } else {cleaned_dfs = NULL}
    
    
    if (!is.null(cleaned_dfs)) {
      
      # lets update the dataset
      target_todo <- target_todo |>
        dplyr::left_join(
          replacement_df |>
            dplyr::select(
              !!adm_level := AdminToClean, replacement
            ),
          by = adm_level
        ) |>
        dplyr::mutate(
          !!adm_level := ifelse(
            is.na(replacement), .data[[adm_level]], replacement
          )
        ) |>
        dplyr::select(-replacement)
    }
  }
  # Step 4: clean up the alias file and save -----------------------------------
  
  if (!is.null(cleaned_dfs)) {
    # clean up the alias df
    suppressWarnings(
      cleaned_dfs_joined <- dplyr::bind_rows(cleaned_dfs) |>
        dplyr::mutate(name_corrected2 = name_corrected) |>
        tidyr::separate(
          name_corrected2,
          into = c("country_prepped", "province_prepped", "district_prepped"),
          sep = "_"
        ) |>
        dplyr::mutate(
          dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, ""))
        )
    )
    
    # combine cleaned data frames
    final_alias_dfs <-
      dplyr::bind_rows(saved_alias_df, cleaned_dfs_joined) |>
      dplyr::select(
        AdminToClean, replacement, name_alias,
        name_corrected, level, country_prepped,
        province_prepped, district_prepped
      ) |>
      dplyr::distinct()
    
    # file saving
    handle_file_save(final_alias_dfs, save_alias_df_path)
  }
  
  # Step 5: Combine the cleaned data frames ------------------------------------
  
  finalised_df <- dplyr::bind_rows(target_done, target_todo)
  
  # get stats
  calculate_match_stats(
    finalised_df, lookup_df, adm0, adm1, adm2
  )
  
  # return the final data frame
  return(finalised_df)
}