testthat::test_that("init_folders handles existing directories correctly", {
  # Create temporary directory
  temp_dir <- withr::local_tempdir()
  
  # Create one of the directories beforehand
  dir.create(file.path(temp_dir, "01_data", "raw"), recursive = TRUE)
  
  # Capture messages
  messages <- capture.output(
    init_folders(base_path = temp_dir),
    type = "message"
  )
  
  # Should see both "Created" and "Exists" messages
  testthat::expect_true(any(grepl("Created:", messages)))
  testthat::expect_true(any(grepl("Exists:", messages)))
})


testthat::test_that("init_folders handles path normalization correctly", {
  # Create temporary directory
  temp_dir <- withr::local_tempdir()
  
  # Test with different path formats
  paths <- list(
    normalizePath(temp_dir),
    file.path(temp_dir, "//"),  # Extra slashes
    gsub("/", "\\", temp_dir, fixed = TRUE)  # Windows-style paths
  )
  
  for (path in paths) {
    testthat::expect_error(init_folders(base_path = path), NA)
  }
})

testthat::test_that("init_folders returns success message", {
  temp_dir <- withr::local_tempdir()
  
  # Capture messages
  messages <- capture.output(
    init_folders(base_path = temp_dir),
    type = "message"
  )
  
  # Check for success message
  testthat::expect_true(
    any(grepl("Folder structure created successfully", messages))
  )
})




