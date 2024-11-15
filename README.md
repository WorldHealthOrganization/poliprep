<!-- badges: start -->
  [![R-CMD-check](https://github.com/WorldHealthOrganization/poliprep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WorldHealthOrganization/poliprep/actions/workflows/R-CMD-check.yaml)  [![CodeFactor](https://www.codefactor.io/repository/github/WorldHealthOrganization/poliprep/badge)](https://www.codefactor.io/repository/github/WorldHealthOrganization/poliprep) [![codecov](https://codecov.io/gh/WorldHealthOrganization/poliprep/graph/badge.svg?token=PCYAMB2S6Y)](https://codecov.io/gh/WorldHealthOrganization/poliprep)
<!-- badges: end -->

# poliprep

## What is poliprep?

`poliprep` is an R package developed by the polio data science team at The World Health Organisation Regional Office for Africa ([WHO AFRO](https://www.afro.who.int/)). It is designed to assist users who have access to and work with datasets collected as part of the [Global Polio Eradication Initiative](https://polioeradication.org/).

The package offers a number of functions for various tasks, including importing and exporting data, pulling data from relevant APIs, data processing and cleaning, as well as performing validations and checks. It also features the capability to generate reports or scorecards highlighting areas of concern identified during the validation process.

## :wrench: Installation

The package can be installed directly using `devtools`. The steps are as follows:

```r
# 1) Install devtools if you haven't already
install.packages("devtools")

# 2) Install the poliprep package from GitHub
devtools::install_github("WorldHealthOrganization/poliprep")
```

## :globe_with_meridians: Roadmap

We plan to add a number of functions which do the following:

- [x] Import and export different datasets (tabular, shapefiles, etc.).
- [x] Pull data from ONA using an API call.
- [x] Update data from ONA API without re-downloading.
- [x] Clean and fix names of places including geolocations.
- [x] Check and clean geo-coordinates.
- [x] Match variables naming conventions & datatypes within two dataframes.
- [x] Validate AFP & ES POLIS data.
- [x] Validate AFP Surveillance data.
- [x] Validate A Surveillance data.
- [x] Create New Detections Table from POLIS data.


## Usage

### Data Management (Import, Export, and Manipulation)

#### `read` and `save` to Import and Export Data

Inspired by `rio`, the read function allows you to read data from a wide range of file formats. Additional reading options specific to each format can be passed through the ellipsis (...) argument. Similarly, the save function provides a simple way to export data into various formats.

``` r
# Load the epiCleanr package
library(epiCleanr)

# Reading a CSV file with a specific seperator
data_csv <- read("path/to/your/file.csv", sep = "\n")

# Import the first sheet from an Excel file
data_excel <- read("path/to/your/file.xlsx", sheet = 1)

# Import any shapefiles file (GeoJSON/shp)
data_geojson <- read(file_path = file.path(path, "test_data.GeoJSON"))
data_shp <- read(file_path = file.path(path, "test_data.shp"))

# Export a Stata DTA file
save(my_data, "path/to/your/file.dta")

# Export an RDS file
save(my_data, "path/to/your/file.rds")

# Export any shapefiles file (GeoJSON/shp)
save(my_shp, "path/to/your/file.GeoJson")
save(my_shp, "path/to/your/file.shp")

# Export an Excel file with sheets
save(
list(my_data = my_data1, my_data2 = my_data2), "path/to/your/file.xlsx")
```

### `prep_mdb_table_extract` for Extracting Tables from Microsoft Access Databases

The `prep_mdb_table_extract` function extracts a specific table from multiple .mdb files located in a specified folder. This is particularly useful when working with datasets stored in Microsoft Access databases that need to be aggregated or analyzed collectively.


```r
# Example setup
mdb_folder <- "path/to/mdb_folder"
target_table <- "target_table_name"

result_list <- prep_mdb_table_extract(mdb_folder, target_table)
```

#### Guidance on Saving API Keys

Before downloading anything using an API, it’s good practice to save your API key locally on your computer rather than embedding it directly within your code. This approach helps keep your credentials secure and ensures they aren’t accidentally shared or exposed.

To securely set your access token in the R environment, you can add it to your .Renviron file. This allows you to access the token across sessions without explicitly including it in your code.

Where the key is stored depends on the environment you’re working in—user or project. If you’re working within an RStudio project (which is generally recommended), you need to specify the scope as "project" so that the key is saved within your environment. Otherwise the default is 'user'.

``` r
usethis::edit_r_environ(scope = "project")

# add the following line to your .Renviron file:
MY_ONA_TOKEN = "your_access_token" 
```

After adding the token, you can retrieve it in your code using `Sys.getenv()`:

``` r
# Set up your token as an object for later use
api_token <- Sys.getenv("MY_ONA_TOKEN")
```

#### `get_ona_data` for Downloading Data from ONA

Before downloading any data from ONA using `poliprep`, you can double-check that your API token is working and verify the available form IDs accessible with your API token. The `prep_ona_data_endpoints` function helps with this:

``` r
available_froms <- prep_ona_data_endpoints(
                     api_token = Sys.getenv("MY_ONA_TOKEN")
                     )
```

Once you have a working API token and the form IDs of interest, you can seamlessly retrieve data from a specified form on the ONA API using the `get_ona_data` function.

``` r
data_ona <- get_ona_data(
               base_url = "https://api.whonghub.org", form_id = 7178,
               api_token = Sys.getenv("MY_ONA_TOKEN"))
```

The function also allows you to download selected columns using the `selected_columns` paramater, which takes vector(s).

``` r
data_ona <- get_ona_data(
              base_url = "https://api.whonghub.org", form_id = 7178,
              api_token = Sys.getenv("MY_ONA_TOKEN"),
              selected_columns = c("states", "endtime", "today", "_duration")
              )
```

You can also filter your dataset before downloading. There are two parameters to do this, `logical_filters` and `comparison_filters`. To select specific elements in a column, you can now use `logical_filters`. For filtering ranges, you can use `comparison_filters`, which always starts with a tilde (\~) and then the conditions are written in a standard R syntax. See below to see how this can be done:

``` r
data_ona <- get_ona_data(
              base_url = "https://api.whonghub.org", form_id = 7178,
              api_token = Sys.getenv("MY_ONA_TOKEN"),
              selected_columns = c("states", "endtime", "today", "_duration"),
              logical_filters = list(states = c("BORNO", "KANO")),
              comparison_filters = (~ `today` >= "2023-02-04" & `today` <= "2025-02-04")
              )
```

Before applying any of these filters, double-check both the column names and their contents to ensure they match your intended filters

#### `get_multi_ona_data` for Downloading Data from Multiple ONA Forms

In cases where you need to download data from multiple ONA forms simultaneously, `poliprep` provides the `get_multi_ona_data` function. This function uses parallel processing in the background to speed up the download process. It’s particularly useful when the forms have identical structures (e.g., the same survey conducted across different administrative levels or countries) but need to be downloaded together together. Ensure that any selected columns and filters exist in all forms of interest; otherwise, the function will fail. Also, the form ID for each downloaded row is appended to the dataset, allowing you to track its source for future checks.

```r
data_ona2 <- get_multi_ona_data(
  form_ids = c(7131, 7178), 
  selected_columns = c("states", "_duration"),
  api_token = Sys.getenv("MY_ONA_TOKEN"),
  logical_filters = list(states = c("BORNO", "KANO")),
  comparison_filters = (~ `_duration` >= "120" & `_duration` <= "2782")
)
```

#### `get_updated_ona_data` for Updating Existing ONA Data with New Data from ONA

When working with datasets from multiple ONA forms, you may prefer to fetch only new and necessary rows rather than repeatedly downloading all data. The `get_updated_ona_data` function in `poliprep` streamlines this process by enabling you to update existing datasets with new rows from specified forms, ensuring data continuity while avoiding duplication. It is particularly effective for managing updates across multiple forms, such as identical surveys conducted in different administrative regions, and includes an optional logging feature to track updates and maintain an audit trail of data changes.

**Key Considerations:**
-   The function supports downloading data from multiple form IDs simultaneously.
-   Do not change parameters for the filters (e.g., logical_filters or comparison_filters) when updating, as this may result in mismatched datasets.
-   Specify the file path where the previously downloaded data is stored. If this is the first run, the function downloads all matching data and saves it to the given location.

``` r
get_updated_ona_data(
  form_ids = c(7131, 7178), 
  selected_columns = c("states", "_duration"),
  api_token = Sys.getenv("MY_ONA_TOKEN"),
  logical_filters = list(states = c("BORNO", "KANO")),
  comparison_filters = (~ `_duration` >= "120" & `_duration` <= "2782"),
  file_path = "data/ona_updates",
  log_results = TRUE
)
```

#### `prep_match_names` for Matching Naming Conventions Between Dataframes

The `prep_match_names` function standardizes variable names in a target dataframe to match the naming conventions in a reference dataframe. This is especially useful when integrating datasets from different sources that use varying naming conventions, such as CamelCase and snake_case. 


```r
ref_dataframe <- tibble::tibble(
  CountryName = c("Cameroon", "Nigeria", "Tchad", "Niger"),
  NameOfCapital = c("Yaounde", "Abuja", "Ndjamena", "Niamey")
)

target_dataframe <- tibble::tibble(
  countryname = c("South Sudan", "Kenya", "Ethiopia", "CAR"),
  nameofcapital = c("Juba", "Nairobi", "Addis Ababa", "Bangui")
)
```
Check variable names before matching:

```r
# Expect FALSE since names are different
all(names(ref_dataframe) == names(target_dataframe))

#> [1] FALSE
```

Apply the function to match the column names of `target_dataframe` to be the same 
as those of `ref_dataframe`:

```r
target_dataframe <- prep_match_names(ref_dataframe, target_dataframe)
```

Verify variable names after matching

```r
colnames(target_dataframe)
#> [1] "CountryName"   "NameOfCapital"

colnames(target_dataframe)
#> [1] "CountryName"   "NameOfCapital"

# Expect TRUE since names now match
all(names(ref_dataframe) == names(target_dataframe))
#> [1] TRUE
```

#### `prep_match_datatypes` for Matching Data Types Between Dataframes

The `prep_match_datatypes` function aligns the data types of columns in a target dataframe to match those in a reference dataframe. This is especially useful when working with datasets from different sources that may have inconsistent data types, such as characters being stored as integers or vice versa. 


```r
ref_df <- tibble::tibble(
  integer_col = 1:3,
  character_col = c("a", "b", "c"),
  numeric_col = c(1.1, 2.2, 3.3)
)

target_df <- tibble::tibble(
  integer_col = c("1", "2", "3"),      # Should be integer
  character_col = 1:3,                 # Should be character
  numeric_col = c("1.1", "2.2", "3.3") # Should be numeric
)
```
Check column data types before matching:

```r
sapply(target_df, class)

#> $integer_col
#> [1] "character"
#> 
#> $character_col
#> [1] "integer"
#> 
#> $numeric_col
#> [1] "character"
```

Apply the function to match the column data types of `target_df` to be the same as those of `ref_df`:

```r
matched_df <- prep_match_datatypes(ref_df, target_df)
```

Verify variable names after matching

```r
sapply(matched_df, class)
#> $integer_col
#> [1] "integer"
#> 
#> $character_col
#> [1] "character"
#> 
#> $numeric_col
#> [1] "numeric"
```
Confirm the data types of `target_df` now match those of `ref_df`:

```r
all(sapply(ref_df, class) == sapply(matched_df, class))
#> [1] TRUE
```

### Geolocation Handling

#### `prep_geonames` for Interactive Admin Name Cleaning and Matching

The `prep_geonames` function combines algorithmic matching with user interactivity to clean and standardize administrative names. It uses string distance calculations for initial matching and allows users to make final corrections interactively, with all decisions saved for future use. The function supports a user-provided lookup dataset as a reference or defaults to internal WHO geonames data if no lookup is provided. Additionally, it supports hierarchical stratification across up to six administrative levels. Cached user decisions enhance consistency and efficiency in subsequent sessions. For users who prefer to run the code without interactivity, the function can be executed with `interactive = FALSE`.
 
```r
target_df <- data.frame(
  country = c("ANGOLA", "UGA", "ZAMBIA"),
  province = c("CABONDA", "TESO", "LUSAKA"),
  district = c("BALIZE", "BOKEDEA", "RAFUNSA")
)

cleaned_df <- prep_geonames(
  target_df,
  level0 = "country",
  level1 = "province",
  level2 = "district",
  interactive = TRUE
)
```
Here is a short video to demonstrate the full interactivity of `prep_geonames`:

[![Watch the video](https://raw.githubusercontent.com/WorldHealthOrganization/poliprep/feature_dev/inst/extdata/demo_screenshot.png)](https://raw.githubusercontent.com/WorldHealthOrganization/poliprep/feature_dev/inst/extdata/prep_geoname_demo.mov)

### Date Handling

#### `autoparse_dates` for Parsing Dates in a Data Frame

The `autoparse_dates` function parses and standardizes date columns in a data frame, ensuring consistency in date formats. This is particularly useful when working with datasets containing multiple date formats or ambiguous date entries.

```r
# Example setup
df <- data.frame(
  mixed_formats = c("2023-10-03", "11.09.2022", "25-12-21 23:59", "2020-08-15T00:00:00Z"),
  iso8601 = c("2021-03-20T00:01:00.513+01:00", "2022-11-05T23:15:59.123+01:00",
              "2023-06-15T12:30:45.789Z", "2020-01-01T00:00:00.000-05:00")
)
```

Check the initial date formats:

```r
df$mixed_formats
#> [1] "2023-10-03"           "11.09.2022"
#> [3] "25-12-21 23:59"       "2020-08-15T00:00:00Z"
```

```r
parsed_df <- autoparse_dates(
  data = df,
  date_cols = c("mixed_formats", "iso8601"),
  output_format = "%Y-%m-%d"
)
```

Verify the parsed date columns:

```r
parsed_df$mixed_formats
#> [1] "2023-10-03" "2022-09-11" "2021-12-25" "2020-08-15"

parsed_df$iso8601
#> [1] "2021-03-20" "2022-11-05" "2023-06-15" "2020-01-01"
```


#### `categorize_biannual` for Categorizing Dates into Biannual Intervals

The `categorize_biannual` function assigns dates in a dataset to non-overlapping six-month intervals, starting from the earliest date in the dataset and working backward from the latest date. This is particularly useful for summarizing or grouping data by biannual time periods.


```r
# Example setup
data <- data.frame(
  virus_date = as.Date(c("2021-05-01", "2021-11-30", "2022-01-15"))
)
```

Categorize the dates into six-month intervals:

```r
categorized_data <- categorize_biannual(data, "virus_date")
```

Verify the categorized intervals:

```r
categorized_data

#>   virus_date      date_categ
#> 1 2021-05-01 Jan 21 / Jun 21
#> 2 2021-11-30 Jul 21 / Dec 21
#> 3 2022-01-15 Jan 22 / Jun 22
```


#### `validate_date` for Checking and Validating Date Columns

```r
# Example setup
data <- data.frame(
  country = c("Rwanda", "Burundi", "Ethiopia", "Zambia", "Zambia", 
              "Chad", "Niger", "Angola"),
  date = c("2023-06-15", "2024-07-20", NA, "1999-12-31", "2025-08-22", 
           "2020/23/10", "2020-02-29", "2019-02-29")
)
```

Run the validation on the date column and check:

```r
validated_data <- validate_date(data, "date")

#> ── Check for missing dates ─────────────────────────────────────────────────────────────────────────────────
#> ℹ Date column date has 1 missing date(s)! Check column date_missing.

#> ── Check for non-date values ───────────────────────────────────────────────────────────────────────────────
#> ℹ Date column date has 1 non-date value(s)! Check column date_non_date.

#> ── Check for sensible dates ────────────────────────────────────────────────────────────────────────────────
#> ℹ Date column date has 1 non-sensible date(s) (not starting with '2000')! Check column date_invalid.
#> ℹ Date column date has 1 date(s) with a year greater than the current year! Check column date_future.

#> ── Check for leap year validity ────────────────────────────────────────────────────────────────────────────
#> ℹ Date column date has 1 invalid leap year date(s)! Check column date_leap_issue.

#> ── Check for improper date formatting ──────────────────────────────────────────────────────────────────────
#> ✔ Date column has no formatting issues!

validated_data

#>    country       date date_missing date_non_date date_invalid date_future date_leap_issue date_format_issue
#> 1   Rwanda 2023-06-15        FALSE         FALSE        FALSE       FALSE           FALSE             FALSE
#> 2  Burundi 2024-07-20        FALSE         FALSE        FALSE       FALSE           FALSE             FALSE
#> 3 Ethiopia       <NA>         TRUE         FALSE           NA          NA           FALSE             FALSE
#> 4   Zambia 1999-12-31        FALSE         FALSE         TRUE       FALSE           FALSE             FALSE
#> 5   Zambia 2025-08-22        FALSE         FALSE        FALSE        TRUE           FALSE             FALSE
#> 6     Chad 2020-10-23        FALSE         FALSE        FALSE       FALSE           FALSE             FALSE
#> 7    Niger 2020-02-29        FALSE         FALSE        FALSE       FALSE           FALSE             FALSE
#> 8   Angola 2019-02-29        FALSE          TRUE        FALSE          NA            TRUE             FALSE
```

Filter data to find specific issues:

```r
# Find rows with non-date values
validated_data |>
  dplyr::filter(date_non_date == TRUE) |>
  dplyr::count(country)
#>   country n
#> 1    Chad 1

# Find rows with leap year issues
validated_data |>
  dplyr::filter(date_leap_issue == TRUE) |>
  dplyr::count(country)
#>   country n
#> 1  Angola 1

# Find rows with formatting issues
validated_data |>
  dplyr::filter(date_format_issue == TRUE) |>
  dplyr::count(country)
#>   country n
#> 1    Chad 1
```

#### `validate_dates` for Validating Two Date Columns

The `validate_dates` function checks and validates two date columns in a data frame. It performs a series of tests to identify issues such as missing values, non-date entries, leap year validity, date formatting errors, differences in formatting between the columns, and whether one date precedes the other.

```r
# Example setup
data <- data.frame(
  country = c("Rwanda", "Burundi", "Ethiopia", "Zambia", "Zambia",
              "Chad", "Niger", "Angola"),
  date1 = c("2024-06-15", "2024-07-20", NA, "1999-12-31", "2025-08-22",
            "2020/23/10", "2020-02-29", "2024-02-29"),
  date2 = c("2023-06-15", "2024-07-20", "2022-05-10", "2019-02-29",
            "2026-09-23", "2020/23/10", "2020-02-29", "2022-02-29")
)
```

Run validation on the two date columns:

```r
validated_data <- validate_dates(data, "date1", "date2")


#> ── Check for missing dates ─────────────────────────────────────────────────────────────────────────────────────
#> ℹ Date column date1 has 1 missing date(s)! Check column date1_missing.

#> ── Check for non-date values ───────────────────────────────────────────────────────────────────────────────────
#> ℹ Date column date1 has 1 non-date value(s)! Check column date1_non_date.
#> ℹ Date column date2 has 3 non-date value(s)! Check column date2_non_date.

#> ── Check for sensible dates ─────────────────────────────────────────────────────────────────────────────────────
#> ℹ Date column date1 has 2 non-sensible date(s) (not between 2000 and 2024)! Check column date1_invalid.
#> ℹ Date column date2 has 1 non-sensible date(s) (not between 2000 and 2024)! Check column date2_invalid.
#> ℹ Date column date1 has 1 date(s) with a year greater than the current year! Check column date1_future.
#> ℹ Date column date2 has 1 date(s) with a year greater than the current year! Check column date2_future.

#> ── Check for leap year validity ─────────────────────────────────────────────────────────────────────────────────
#> ℹ Date column date2 has 2 invalid leap year date(s)! Check column date2_leap_issue.

#> ── Check for improper date formatting ───────────────────────────────────────────────────────────────────────────
#> ℹ Date column date1 has 1 improperly formatted date(s)! Check column date1_format_issue.
#> ℹ Date column date2 has 3 improperly formatted date(s)! Check column date2_format_issue.

#> ── Check similarity in date formatting ──────────────────────────────────────────────────────────────────────────
#> ✔ Both date columns have the same format!

#> ── Check if the first date is before the second date ────────────────────────────────────────────────────────────
#> ℹ There are 1 instances where the first date is not before the second date! Check column date1_invalid_order.
```

Inspect validation results:

```r
# Check for rows with missing values in `date1`
validated_data |>
  dplyr::filter(date1_missing == TRUE) |>
  dplyr::count(country)
#>   country n
#> 1 Ethiopia 1

# Check for rows where `date1` has a non-date value
validated_data |>
  dplyr::filter(date1_non_date == TRUE) |>
  dplyr::count(country)
#>   country n
#> 1    Chad 1

# Check for rows where `date1` has an invalid leap year date
validated_data |>
  dplyr::filter(date1_leap_issue == TRUE) |>
  dplyr::count(country)
#>   country n
#> 1  Angola 1

# Check for rows where `date1` is after `date2`
validated_data |>
  dplyr::filter(date1_invalid_order == TRUE) |>
  dplyr::count(country)
#>   country n
#> 1  Zambia 1
```

## :incoming_envelope: Contacting us

For any issues or questions about `poliprep`, please contact Mo at [moyusuf\@who.int](mailto:moyusuf@who.int).

## :handshake: Guidance on making contributions

We welcome contributions via forking and pull requests. For guidance and best practices, please follow [WHO's Contribution Guidelines](https://github.com/WorldHealthOrganization/open-source-communication-channel/blob/main/CONTRIBUTING.md). Additionally, before any contribution, please ensure you are familiar with WHO's [Contributor Covenant Code of Conduct](https://github.com/WorldHealthOrganization/open-source-communication-channel/tree/main?tab=coc-ov-file).
