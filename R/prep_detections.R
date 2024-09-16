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
        dplyr::filter(stringr::str_detect(VirusTypeName, "^WILD|^VDPV|^cVDPV")) |>
        dplyr::filter(SurveillanceTypeName %in% c("AFP", "Environmental")) |>
        dplyr::mutate(VirusDate = as.Date(VirusDate)) |>
        dplyr::group_by(Admin0Name, VirusTypeName) |>
        dplyr::reframe(`Previous Detection` = max(VirusDate))

    result <- polis_df_new |>
        dplyr::filter(
            !EPID %in% unique(polis_df_old$EPID),
            stringr::str_detect(VirusTypeName, "^WILD|^VDPV|^cVDPV"),
            SurveillanceTypeName %in% c("AFP", "Environmental")
        ) |>
        dplyr::transmute(
            Admin0Name, Admin1Name, Admin2Name,
            VirusDate = as.Date(VirusDate),
            VirusTypeName = ifelse(VirusTypeName == "WILD1", "WPV1", VirusTypeName),
            SurveillanceTypeName,
            NtChanges,
            `Emergence/Cluster Group` = ifelse(is.na(VdpvEmergenceGroupName),
                WildClusterName, VdpvEmergenceGroupName
            )
        ) |>
        dplyr::count(
            Admin0Name, Admin1Name, Admin2Name, VirusDate, VirusTypeName,
            SurveillanceTypeName, NtChanges, `Emergence/Cluster Group`,
            name = "Detections"
        ) |>
        dplyr::left_join(
            prev_detection,
            by = c("Admin0Name", "VirusTypeName")
        )

    reslist <- result |>
        dplyr::mutate(
            `Emergence/Cluster Group` = paste0(
                " with ", NtChanges, " Nucleotide changes from Sabin."
            ),
            VirusDate = format_date_ord(VirusDate),
            prev_det = ifelse(
                is.na(`Previous Detection`),
                " This virus was reported for the first time in that country",
                paste(
                    " With last known detection in that country being in",
                    format_date_ord(`Previous Detection`)
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
        "{reslist$`Emergence/Cluster Group`}",
        "{reslist$prev_det}.\n"
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
        overview = overview,
        country_specific = country_specific,
        result = result
    )
}
