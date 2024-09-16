#' Get virus detections from data
#'
#' @param data A data frame containing virus detection data
#' @param virus_types A vector of virus types to filter
#' @return A character vector of detection counts by virus type
get_detections <- function(data, virus_types) {
    detections <- list()

    for (virus in virus_types) {
        count <- data |>
            dplyr::filter(VirusTypeName == virus) |>
            dplyr::pull(Detections)

        if (length(count) > 0) {
            detections[[virus]] <- paste(count, virus)
        }
    }

    unlist(detections)
}

#' Format Date with Ordinal Day
#'
#' This function converts a date column to a string with an ordinal day
#' (e.g., "1st", "2nd", "3rd") followed by the abbreviated month name and year.
#'
#' @param date_column A Date vector or a character vector that can be coerced
#'   to Date. The dates to be formatted.
#'
#' @return A character vector representing the formatted dates with ordinal day,
#'   abbreviated month, and year (e.g., "2nd Jan 2020").
#'
#' @examples
#' format_date_ord(as.Date("2020-01-02")) # Returns "2nd Jan 2020"
#' format_date_ord(c("2023-03-21", "2023-04-11"))
#' # Returns c("21st Mar 2023", "11th Apr 2023")
#'
#' @details
#' The function handles special cases for 11th, 12th, and 13th, which always
#' use "th" as the ordinal suffix regardless of their ones digit.
#'
#' @seealso \code{\link{as.Date}}, \code{\link{format}}
#'
#' @export
format_date_ord <- function(date_column) {
    date_column <- as.Date(date_column)
    day <- as.integer(format(date_column, "%d"))
    month <- format(date_column, "%b")
    year <- format(date_column, "%Y")

    ordinal_suffix <- function(day) {
        suffix <- rep("th", length(day))
        suffix[day %% 10 == 1 & day %% 100 != 11] <- "st"
        suffix[day %% 10 == 2 & day %% 100 != 12] <- "nd"
        suffix[day %% 10 == 3 & day %% 100 != 13] <- "rd"
        suffix
    }

    paste0(day, ordinal_suffix(day), " ", month, " ", year)
}

#' Prepare a new detections report for poliovirus cases
#'
#' This function processes and summarizes new poliovirus detections,
#' comparing them with previous data. It generates an overview and
#' country-specific details, with an option to output to a Word document.#'
#' @param polis_df_old A data frame containing previous poliovirus data
#' @param polis_df_new A data frame containing new poliovirus data
#' @param output_word Logical, whether to output results to a Word document
#' @param output_path String, path for the output Word document (optional)
#'
#' @return A list containing:
#'   \item{overview}{A list with summary statistics}
#'   \item{country_specific}{A character vector of country-specific details}
#'   \item{result}{A data frame of processed new detections}
#'
#' @details
#' The function performs the following main steps:
#' 1. Processes previous detection data
#' 2. Filters and processes new detection data
#' 3. Generates summary statistics and country-specific details
#' 4. Optionally creates a Word document report
#'
#' It uses various dplyr functions for data manipulation and summarization,
#' stringr for string operations, and officer for Word document creation.
#'
#' @examples
#' \dontrun{
#' report <- prep_new_detections_report(old_data, new_data, output_word = TRUE)
#' }
#'
#' @export
prep_new_detections_report <- function(polis_df_old,
                                       polis_df_new,
                                       output_word = FALSE,
                                       output_path = NULL) {
    # Conditional loading for packages
    required_packages <- c(
        "epoxy", "glue", "stringr", "officer"
    )

    missing_packages <- required_packages[!sapply(
        required_packages, requireNamespace,
        quietly = TRUE
    )]


    prev_detection <- polis_df_old |>
        dplyr::mutate(across(where(is.factor), as.character)) |>
        dplyr::mutate(
            VirusTypeName = ifelse(VirusTypeName == "WILD1", "WPV1", VirusTypeName)
        ) |>
        dplyr::filter(stringr::str_detect(VirusTypeName, "^WPV|^VDPV|^cVDPV")) |>
        dplyr::filter(SurveillanceTypeName %in% c("AFP", "Environmental")) |>
        dplyr::mutate(VirusDate = as.Date(VirusDate)) |>
        dplyr::group_by(Admin0Name, VirusTypeName) |>
        dplyr::reframe(`Previous Detection` = max(VirusDate))

    # get new epids
    new_epids <- polis_df_old |>
        dplyr::filter(
            lubridate::year(VirusDate) > lubridate::year(Sys.Date()) - 3
        ) |>
        dplyr::pull(EPID)

    # establish old emergence groups
    previous_emergence_group <- polis_df_old |>
        dplyr::filter(!is.na(VdpvEmergenceGroupName)) |>
        dplyr::mutate(
            unique_emerg_grp = sub("-\\d+$", "", VdpvEmergenceGroupName)
        ) |>
        dplyr::pull(unique_emerg_grp) |>
        unique()


    result <- polis_df_new |>
        dplyr::filter(
            lubridate::year(as.Date(VirusDate)) > lubridate::year(Sys.Date()) - 3 &
                !EPID %in% unique(new_epids),
            stringr::str_detect(VirusTypeName, "^WILD|^VDPV|^cVDPV"),
            SurveillanceTypeName %in% c("AFP", "Environmental")
        ) |>
        dplyr::mutate(dplyr::across(where(is.factor), as.character)) |>
        dplyr::transmute(
            Admin0Name, Admin1Name, Admin2Name,
            VirusDate = as.Date(VirusDate),
            VirusTypeName = ifelse(VirusTypeName == "WILD1", "WPV1",
                VirusTypeName
            ),
            BaseVdpvEmergenceGroup = sub("-\\d+$", "", VdpvEmergenceGroupName),
            SurveillanceTypeName,
            NtChanges = ifelse(
                stringr::str_detect(VirusTypeName, "^VDPV"), NtChanges, NA
            ),
            `Emergence/Cluster Group` = ifelse(is.na(VdpvEmergenceGroupName),
                WildClusterName,
                VdpvEmergenceGroupName
            )
        ) |>
        dplyr::count(
            Admin0Name, Admin1Name, Admin2Name, VirusDate, VirusTypeName,
            SurveillanceTypeName, NtChanges, `Emergence/Cluster Group`,
            BaseVdpvEmergenceGroup,
            name = "Detections"
        ) |>
        dplyr::left_join(
            prev_detection,
            by = c("Admin0Name", "VirusTypeName")
        ) |>
        dplyr::mutate(
            `Days Since Last Detections` =
                VirusDate - `Previous Detection`
        ) |>
        dplyr::mutate(
            `New Emergence` = ifelse(
                VirusTypeName %in% c("cVDPV1", "cVDPV2") &
                    !BaseVdpvEmergenceGroup %in% previous_emergence_group,
                TRUE, FALSE
            )
        )

    reslist <- result |>
        dplyr::mutate(
            `Emergence/Cluster Group` = ifelse(
                VirusTypeName == "WPV1",
                paste(`Emergence/Cluster Group`, "Cluster Group"),
                paste(`Emergence/Cluster Group`, "Emergence Group")
            ),
            `Emergence/Cluster Group` = dplyr::case_when(
                !is.na(NtChanges) & VirusTypeName %in% c("VDPV1", "VDPV2") ~
                    paste0(" with ", NtChanges, " Nucleotide changes from Sabin"),
                !is.na(NtChanges) & !VirusTypeName %in% c("VDPV1", "VDPV2") ~
                    paste0(" belonging to ", `Emergence/Cluster Group`),
                TRUE ~ ""
            ),
            `Emergence/Cluster Group` = ifelse(
                stringr::str_detect(`Emergence/Cluster Group`, " belonging to NA"), "",
                `Emergence/Cluster Group`
            ),
            `Emergence/Cluster Group` = ifelse(
                `New Emergence` == TRUE,
                paste0(`Emergence/Cluster Group`, " (this is a new Emergence Group)"),
                `Emergence/Cluster Group`
            ),
            VirusDate = format_date_ord(VirusDate),
            `Previous Detection` = ifelse(
                !is.na(`Previous Detection`),
                format_date_ord(`Previous Detection`),
                `Previous Detection`
            ),
            prev_det = ifelse(
                is.na(`Previous Detection`),
                #  "" # ,
                paste(
                    " This virus was reported for the first time in that country"
                ),
                paste0(
                    " With last known detection in that country being in ",
                    `Previous Detection`, "."
                )
            ),
            country = stringr::str_to_title(Admin0Name),
            province = stringr::str_to_title(Admin1Name),
            district = stringr::str_to_title(Admin2Name)
        ) |>
        as.list()

    country_result <- result |>
        dplyr::group_by(Admin0Name) |>
        dplyr::summarise(Detections = sum(Detections)) |>
        dplyr::arrange(dplyr::desc(Detections))

    countyr_n <- length(unique(country_result$Admin0Name))
    detections_n <- sum(country_result$Detections)

    virus_es_surv <- result |>
        dplyr::filter(SurveillanceTypeName == "Environmental") |>
        dplyr::group_by(VirusTypeName) |>
        dplyr::reframe(Detections = sum(Detections)) |>
        dplyr::arrange(dplyr::desc(Detections))

    virus_afp_surv <- result |>
        dplyr::filter(SurveillanceTypeName == "AFP") |>
        dplyr::group_by(VirusTypeName) |>
        dplyr::reframe(Detections = sum(Detections)) |>
        dplyr::arrange(dplyr::desc(Detections))

    virus_types <- c("WPV1", "cVDPV1", "cVDPV2", "VDPV1", "VDPV2")

    es_detections <- paste(
        get_detections(virus_es_surv, virus_types),
        collapse = ", "
    )
    afp_detections <- paste(
        get_detections(virus_afp_surv, virus_types),
        collapse = ", "
    )

    overview <- list(
        total = glue::glue(
            "{detections_n} positive isolates are ",
            "reported from {countyr_n} countries.\n"
        ),
        afp = glue::glue("AFP Cases: {afp_detections}"),
        es = glue::glue("Environmental Samples: {es_detections}.\n")
    )

    country_specific <- epoxy::epoxy(
        "{reslist$Admin0Name}\n \u2022 ",
        "{reslist$Detections} {reslist$VirusTypeName} ",
        "{reslist$SurveillanceTypeName}",
        " Sample in {reslist$province}, {reslist$district} from",
        " {reslist$VirusDate} is reported this week",
        "{reslist$`Emergence/Cluster Group`}.",
        "{reslist$prev_det}\n"
    )


    if (output_word) {
        doc <- officer::read_docx()
        doc <- doc |>
            officer::body_add_par("Overview of Virus \n", style = "heading 1") |>
            officer::body_add_par(overview$total, style = "Normal") |>
            officer::body_add_par(overview$afp, style = "Normal") |>
            officer::body_add_par(overview$es, style = "Normal") |>
            officer::body_add_par("Country Specific Details", style = "heading 1")

        for (message in country_specific) {
            doc <- officer::body_add_par(
                doc, message,
                style = "Normal"
            )
        }



        if (is.null(output_path)) {
            output_path <- "new_detections_report.docx"
        }

        print(doc, target = output_path)
    }

    list(
        detections_table = overview,
        country_specific = country_specific,
        result = result
    )
}
