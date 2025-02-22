# poliprep 1.4.0

# poliprep 1.3.0

## Improvements

- Refactored `prep_geonames()` to warn users about unsupported levels in the default look-up file.

## Bug Fixes

- Fixed issue where `handle_file_save()` overwrote existing cache data.
- Resolved incorrect merging behavior in `prep_geonames()`.

## Deprecations

- Deprecated `get_multi_ona_data()` in favor of `get_ona_data()`

### Added
- **`init_folders()`**: A new function to create a standardized project folder structure.
  - Automatically generates directories for organizing data, scripts, and outputs.
  - Ensures consistency and reproducibility in data-related workflows.
  - Provides clear feedback messages using `cli` for successful and existing folders.

#### Example Usage
```r
# Create the project folder structure in the current working directory
init_folders()

# Specify a custom directory
init_folders(base_path = "~/my_project")
```
