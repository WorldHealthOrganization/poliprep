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
#' Prepare New Detections Table
#'
#' This function creates a summary table of poliovirus detections, comparing old
#' and new data, and optionally includes information about Supplementary
#' Immunization Activities (SIA).
#'
#' @param polis_df_old A data frame containing the old POLIS data.
#' @param polis_df_new A data frame containing the new POLIS data.
#' @param polis_sia A data frame containing SIA data.
#' @param include_sia Logical, whether to include SIA information in the output
#'   table. Default is TRUE.
#' @param save_table Logical, whether to save the table as an image file.
#'   Default is FALSE.
#' @param output_path Character string specifying the directory to save the
#'   output file. If NULL, uses the current working directory.
#' @param vheight Numeric, the height of the output image in pixels. Default is
#'   800.
#' @param vwidth Numeric, the width of the output image in pixels. Default is
#'   3260.
#'
#' @return A gt table object containing the summary of poliovirus detections.
#'
#' @details
#' The function performs the following main steps:
#' 1. Checks for required packages and loads them if necessary.
#' 2. Processes the old POLIS data to get previous detection dates.
#' 3. Filters and processes the new POLIS data.
#' 4. Combines and summarizes the data, including calculations for various
#'    metrics.
#' 5. Joins with SIA data if include_sia is TRUE.
#' 6. Creates a formatted gt table with conditional styling.
#' 7. Optionally saves the table as an HTML and PNG file.
#'
#' @note
#' - The function uses various dplyr, tidyr, and gt functions for data
#'   manipulation and table creation.
#' - It applies conditional formatting to highlight new emergences, delayed
#'   reporting, and other important information.
#' - If save_table is TRUE, it saves the table as both HTML and PNG files, then
#'   deletes the HTML file.
#'
#' @examples
#' # Basic usage:
#' # result_table <- prep_new_detections_table(
#' #   polis_df_old, polis_df_new,
#' #   polis_sia
#' # )
#'
#' @export

prep_new_detections_table <- function(polis_df_old,
                                      polis_df_new,
                                      polis_sia,
                                      include_sia = TRUE,
                                      save_table = FALSE,
                                      output_path = NULL,
                                      vheight = 800,
                                      vwidth = 3260) {
    # Conditional loading for packages
    required_packages <- c(
        "gt", "glue", "stringr", "webshot"
    )

    missing_packages <- required_packages[!sapply(
        required_packages, requireNamespace,
        quietly = TRUE
    )]


    prev_detection <- polis_df_old |>
        dplyr::mutate(across(where(is.factor), as.character)) |>
        dplyr::mutate(
            VirusTypeName = ifelse(
                VirusTypeName == "WILD1", "WPV1", VirusTypeName
            )
        ) |>
        dplyr::filter(
            stringr::str_detect(VirusTypeName, "^WPV|^VDPV|^cVDPV") &
                SurveillanceTypeName %in% c("AFP", "Environmental")
        ) |>
        dplyr::mutate(VirusDate = as.Date(VirusDate)) |>
        dplyr::group_by(Admin0Name, VirusTypeName) |>
        dplyr::reframe(`Previous Detection` = max(VirusDate))



    # get max date for old and new data
    polis_old_maxdate <- max(as.Date(polis_df_old$UpdatedDate))
    polis_new_maxdate <- max(as.Date(polis_df_new$UpdatedDate))

    res <- polis_df_new |>
        dplyr::mutate(
            year = lubridate::year(as.Date(VirusDate))
        ) |>
        dplyr::filter(
            year > lubridate::year(Sys.Date()) - 3 &
                !EPID %in% unique(polis_df_old$EPID),
            stringr::str_detect(VirusTypeName, "^WILD|^VDPV|^cVDPV"),
            SurveillanceTypeName %in% c("AFP", "Environmental")
        ) |>
        dplyr::mutate(dplyr::across(where(is.factor), as.character)) |>
        dplyr::mutate(
            # BaseVdpvEmergenceGroup = sub("-\\d+$", "", VdpvEmergenceGroupName),
            BaseVdpvEmergenceGroup = VdpvEmergenceGroupName,
            VdpvClassificationChangeDate = as.Date(VdpvClassificationChangeDate),
            VdpvReportedToHQDate = as.Date(VdpvReportedToHQDate),
            VirusDate = as.Date(VirusDate),
            VirusTypeName = ifelse(VirusTypeName == "WILD1", "WPV1", VirusTypeName),
            year = lubridate::year(VirusDate)
        )


    data <- res |>
        dplyr::group_by(Admin0Name, VirusTypeName, year) |>
        dplyr::summarise(
            Detections = dplyr::n(),
            MostRecentVirusDate = max(VirusDate, na.rm = TRUE),
            .groups = "drop"
        ) |>
        tidyr::pivot_wider(
            names_from = year,
            values_from = Detections,
            names_glue = "{year} Detections"
        ) |>
        dplyr::mutate(
            MostRecentVirusDate = format(MostRecentVirusDate, "%Y-%m-%d")
        ) |>
        dplyr::left_join(
            prev_detection,
            by = c("Admin0Name", "VirusTypeName")
        ) |>
        dplyr::select(
            Country = Admin0Name,
            Virus = VirusTypeName,
            dplyr::ends_with("Detections"),
            `Most Recent Detection` = MostRecentVirusDate,
            `Previous Detection`
        ) |>
        dplyr::left_join(
            res |>
                dplyr::mutate(
                    Ntchanges = ifelse(
                        is.na(VdpvNtChangesClosestMatch),
                        VdpvNtChangesFromSabin,
                        VdpvNtChangesClosestMatch
                    ),
                    Ntchanges = as.numeric(Ntchanges),
                    VdpvNtChangesClosestMatch = ifelse(
                        is.na(VdpvNtChangesClosestMatch),
                        paste0(VdpvNtChangesFromSabin, "**"),
                        VdpvNtChangesClosestMatch
                    ),
                ) |>
                dplyr::arrange(dplyr::desc(VirusDate)) |>
                dplyr::group_by(Admin0Name, VirusTypeName) |>
                dplyr::slice(1) |>
                dplyr::ungroup() |>
                dplyr::select(
                    Country = Admin0Name,
                    Virus = VirusTypeName,
                    `Emergence Group` = BaseVdpvEmergenceGroup,
                    `VDPV Classification Change Date` = VdpvClassificationChangeDate,
                    `Virus Reported to HQ Date` = VdpvReportedToHQDate,
                    `Nt Changes from Closest Match` = VdpvNtChangesClosestMatch,
                    Ntchanges,
                    `Vaccine Origin` = VaccineOrigin
                ),
            by = c("Country", "Virus")
        ) |>
        dplyr::mutate(
            `Most Recent Detection` = as.Date(`Most Recent Detection`),
            `Detections to Confirmation (days)` = `Virus Reported to HQ Date` - `Most Recent Detection`,
            `Delayed Reporting` = ifelse(`Detections to Confirmation (days)` > 90, TRUE, FALSE),
            `New Emergence` = ifelse(
                Virus %in% c("cVDPV1", "cVDPV2") &
                    !`Emergence Group` %in% unique(polis_df_old$VdpvEmergenceGroupName),
                TRUE, FALSE
            )
        ) |>
        dplyr::mutate(
            `New VDPV Classification` = ifelse(
                Virus %in% c("cVDPV1", "cVDPV2") &
                    `VDPV Classification Change Date` > polis_old_maxdate &
                    `VDPV Classification Change Date` <= polis_new_maxdate,
                TRUE, FALSE
            )
        ) |>
        dplyr::relocate(`Nt Changes from Closest Match`, `Vaccine Origin`, .after = dplyr::last_col()) |>
        as.data.frame() |>
        dplyr::left_join(
            polis_sia |>
                dplyr::filter(
                    Admin0Name %in% res$Admin0Name
                ) |>
                dplyr::filter(ActivityStatus %in% c("Done")) |>
                dplyr::mutate(
                    ActivityDateFrom = as.Date(ActivityDateFrom)
                ) |>
                dplyr::arrange(dplyr::desc(ActivityDateFrom)) |>
                dplyr::group_by(Admin0Name) |>
                dplyr::slice(1) |>
                dplyr::ungroup() |>
                dplyr::select(
                    Country = Admin0Name,
                    `Vaccine Type` = ActivityVaccineType,
                    `Date of Any Last SIA Campagin` = ActivityDateFrom
                ),
            by = "Country"
        ) |>
        dplyr::mutate(
            `Days Since SIA and Detection` = `Most Recent Detection` - `Date of Any Last SIA Campagin`,
            `Days between Previous and Most Recent Detection` = `Most Recent Detection` - `Previous Detection`,
            `Days between Previous and Most Recent Detection` = ifelse(
                !is.na(`Days between Previous and Most Recent Detection`) &
                    `Days between Previous and Most Recent Detection` < 0,
                paste0(`Days between Previous and Most Recent Detection`, "*"),
                `Days between Previous and Most Recent Detection`
            ),
            `Days between Previous and Most Recent Detection` = ifelse(
                is.na(`Days between Previous and Most Recent Detection`),
                "",
                `Days between Previous and Most Recent Detection`
            ),
            `Most Recent Detection` = poliprep::format_date_ord(`Most Recent Detection`),
            `Previous Detection` = poliprep::format_date_ord(`Previous Detection`),
            `Date of Any Last SIA Campagin` = poliprep::format_date_ord(`Date of Any Last SIA Campagin`),
            `Previous Detection` = ifelse(
                `Previous Detection` == "NAth NA NA",
                "", `Previous Detection`
            ),
            `Emergence Group` = ifelse(is.na(`Emergence Group`), "No Classification Given Yet", `Emergence Group`),
            `Vaccine Origin` = ifelse(is.na(`Vaccine Origin`), "", `Vaccine Origin`),
            Country = stringr::str_to_title(Country)
        ) |>
        dplyr::select(
            Country,
            `Virus` = Virus,
            dplyr::ends_with("Detections"),
            `Emergence Group` = `Emergence Group`,
            `New Emergence`,
            `Latest Detection` = `Most Recent Detection`,
            `Prior Detection` = `Previous Detection`,
            `Days Between` = `Days between Previous and Most Recent Detection`,
            `Days to Confirm` = `Detections to Confirmation (days)`,
            `Delayed Report` = `Delayed Reporting`,
            `Nt Changes` = `Nt Changes from Closest Match`,
            `New VDPV Class` = `New VDPV Classification`,
            `Vaccine Origin`,
            `Last SIA Date` = `Date of Any Last SIA Campagin`,
            `Days Since SIA` = `Days Since SIA and Detection`,
            `Vaccine Type`, Ntchanges
        )

    gt_table <- data |>
        gt::gt() |>
        gt::tab_style(
            style = list(
                gt::cell_fill(color = "#de2d26"),
                gt::cell_text(color = "white")
            ),
            locations = gt::cells_body(
                columns = `Emergence Group`,
                rows = `New Emergence` == TRUE
            )
        ) |>
        gt::tab_spanner(
            label = "Previous Campaigns",
            columns = c(`Last SIA Date`, `Vaccine Type`)
        ) |>
        gt::tab_spanner(
            label = "Virus Information",
            columns = c(
                "Virus",
                dplyr::ends_with("Detections"),
                "Emergence Group"
            )
        ) |>
        gt::tab_spanner(
            label = "Virus History",
            columns = c(
                "Days to Confirm",
                "Latest Detection", "Prior Detection",
                "Days Between",
                "Nt Changes", "Vaccine Origin"
            )
        ) |>
        gt::tab_style(
            style = list(
                gt::cell_fill(color = "#fee0d2")
            ),
            locations = gt::cells_body(
                columns = `Days to Confirm`,
                rows = `Delayed Report` == TRUE
            )
        ) |>
        gt::tab_style(
            style = list(
                gt::cell_fill(color = "#deebf7")
            ),
            locations = gt::cells_body(
                columns = `Virus`,
                rows = `New VDPV Class` == TRUE &
                    `Virus` != "WPV1"
            )
        ) |>
        gt::tab_style(
            style = list(
                gt::cell_fill(color = "#efedf5")
            ),
            locations = gt::cells_body(
                columns = `Nt Changes`,
                rows = Ntchanges > 11
            )
        ) |>
        gt::cols_hide(columns = c(
            "Ntchanges", "New VDPV Class", "Delayed Report",
            "New Emergence", "Days Since SIA"
        )) |>
        gt::tab_footnote(
            footnote = gt::md(
                "Latest Detection: Latest reported to HQ this week; may not be newest virus detection in country<br>
     Prior Detection: Last detection of this virus type in the country before current reporting week<br>
     * Due to lab delay, virus may have been detected earlier and not reported until this week<br>
     ** If VDPV and no Nt Changes from Closest Match, Nt Changes from Sabin is used."
            )
        ) |>
        gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_column_spanners(
                spanners = dplyr::everything()
            )
        ) |>
        gt::tab_source_note(
            gt::html(paste0(
                '<pre style="display: inline;font-family: Avenir, Verdana, sans-serif; font-size: 15px">',
                '<span style="background-color: #deebf7;">        </span> = New linkage to ongoing outbreak</pre>   ',
                '<pre style="display: inline;font-family: Avenir, Verdana, sans-serif; font-size: 15px">',
                '<span style="background-color: #de2d26;">        </span> = New Emergence</pre>   ',
                '<pre style="display: inline;font-family: Avenir, Verdana, sans-serif; font-size: 15px">',
                '<span style="background-color: #fee0d2;">        </span> = Days to Confirm > 90</pre>  ',
                '<pre style="display: inline;font-family: Avenir, Verdana, sans-serif; font-size: 15px">',
                '<span style="background-color: #efedf5;">        </span> = Nt Changes > 11</pre>  '
            ))
        ) |>
        gt::tab_header(
            title = "Poliovirus Detections Summary",
            subtitle = glue::glue(
                "Confirmed detections between {poliprep::format_date_ord(polis_old_maxdate)} and ",
                "{poliprep::format_date_ord(polis_new_maxdate)}"
            )
        ) |>
        gt::tab_options(
            table.font.size = gt::px(24),
            table.width = gt::pct(100)
        )

    if (!include_sia) {
        gt_table <- gt_table |>
            gt::cols_hide(
                columns = c("Last SIA Date", "Vaccine Type")
            )
    }

    if (save_table) {
        if (is.null(output_path)) {
            output_path <- getwd()
        }

        today <- format(Sys.Date(), "%Y%m%d")
        file_prefix <- glue::glue(
            "poliovirus_detections_summary_{today}"
        )

        html_path <- file.path(output_path, glue::glue("{file_prefix}.html"))
        png_path <- file.path(output_path, glue::glue("{file_prefix}.png"))

        gt_table |> gt::gtsave(html_path)

        webshot::webshot(html_path, png_path, vheight = vheight, vwidth = vwidth)

        file.remove(html_path)
    }

    return(gt_table)
}
