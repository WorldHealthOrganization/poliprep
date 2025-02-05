# poliprep 1.3.0

## Improvements

- Refactored `prep_geonames()` to warn users about unsupported levels in the default look-up file.

## Bug Fixes

- Fixed issue where `handle_file_save()` overwrote existing cache data.
- Resolved incorrect merging behavior in `prep_geonames()`.

## Deprecations

- Deprecated `get_multi_ona_data()` in favor of `get_ona_data()`
