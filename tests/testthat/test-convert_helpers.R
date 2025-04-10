# testing validate_input_output() --------------------------------------
test_that("validate_input_output validates existing files with NULL output", {
  test_file <- test_path("fixtures", "example_dir", "penguins.R")
  result <- validate_input_output(test_file, NULL)

  expect_equal(result$input, result$output)
  expect_equal(basename(result$input), basename(test_file))
  expect_equal(basename(result$output), basename(test_file))
  expect_true(file.exists(result$input))
  expect_true(file.exists(result$output))
})

test_that("validate_input_output validates existing files with different output", {
  test_file <- test_path("fixtures", "example_dir", "penguins.R")

  # Test with output to a temporary file
  temp_output <- withr::local_tempfile(fileext = ".R")

  result <- validate_input_output(test_file, temp_output)
  expect_equal(basename(result$input), basename(test_file))
  expect_equal(result$output, normalizePath(temp_output))
  expect_true(file.exists(result$output))
})

test_that("validate_input_output handles incorrect input and output lengths", {
  valid_file <- test_path("fixtures", "example_dir", "penguins.R")
  expect_error(
    validate_input_output(c(valid_file, valid_file), output = NULL),
    "`input` must be a single character string \\(a path\\)"
  )

  expect_error(
    validate_input_output(valid_file, c(valid_file, valid_file)),
    "`output` must be a single character string \\(a path\\) or NULL"
  )
})

test_that("validate_input_output handles non-existent path", {
  non_existent_file <- test_path("fixtures", "example_dir", "no_file_here.R")
  if (file.exists(non_existent_file)) {
    file.remove(non_existent_file)
  }

  expect_error(
    validate_input_output(non_existent_file, output = NULL),
    "[Ff]ile|[Pp]ath"
  )

  expect_error(
    validate_input_output(123, output = NULL),
    "invalid 'path' argument"
  )
})

test_that("validate_input_output handles extension argument", {
  md_ext <- test_path("fixtures", "example_dir", "not_a_script.md")
  expect_error(
    validate_input_output(md_ext, NULL),
    "`input` does not have a valid file extension"
  )

  expect_no_error(
    validate_input_output(md_ext, NULL, extensions = c("R", "md"))
  )
})

test_that("validate_input_output creates output directory if needed", {
  input_file <- test_path("fixtures", "example_dir", "penguins.R")
  temp_dir <- withr::local_tempdir()
  nested_output <- file.path(temp_dir, "nested", "output.R")
  expect_false(dir.exists(file.path(temp_dir, "nested"))) # shouldn't, but check
  result <- validate_input_output(input_file, nested_output)
  expect_true(dir.exists(file.path(temp_dir, "nested")))
  expect_true(file.exists(nested_output))
})

# testing penguins_substitute() ------------------------------------------

test_that("penguins_substitute makes all expected substitutions", {
  # Create file content with all patterns
  file_content <- c(
    "library(palmerpenguins)",
    "library('palmerpenguins')",
    'library("palmerpenguins")',
    "penguins$bill_length_mm",
    "penguins$bill_depth_mm",
    "penguins$flipper_length_mm",
    "penguins$body_mass_g",
    'select(ends_with("_mm"))',
    'data("penguins", package = "palmerpenguins")',
    "data(penguins, package = 'palmerpenguins')"
  )

  result <- penguins_substitute(file_content, "test_output.R")

  # Verify that substitutions happened
  expect_equal(result$content[1], "")
  expect_equal(result$content[2], "")
  expect_equal(result$content[3], "")
  expect_equal(result$content[4], "penguins$bill_len")
  expect_equal(result$content[5], "penguins$bill_dep")
  expect_equal(result$content[6], "penguins$flipper_len")
  expect_equal(result$content[7], "penguins$body_mass")
  expect_equal(
    result$content[8],
    'select(starts_with("flipper_"), starts_with("bill_"))'
  )
  expect_equal(
    result$content[9],
    'data("penguins", package = "datasets")'
  )
  expect_equal(
    result$content[10],
    'data("penguins", package = "datasets")'
  )
  expect_true(result$matches)
})

test_that("penguins_substitute produces expected messages", {
  file_content <- readLines(test_path("fixtures", "example_dir", "penguins.R"))
  # one ends_with
  expect_message(
    penguins_substitute(file_content[-17], "penguins_new.R"),
    '- ends_with\\("_mm"\\) replaced on line 16 in penguins_new.R'
  )
  # multiple ends_with
  expect_message(
    penguins_substitute(file_content, "penguins_new.R"),
    '- ends_with\\("_mm"\\) replaced on lines 16, 17 in penguins_new.R'
  )
})

test_that("penguins_substitute handles empty content and no matches correctly", {
  # Test with empty content
  empty_result <- penguins_substitute(character(0), "test.R")
  expect_false(empty_result$matches)
  expect_equal(empty_result$content, character(0))

  # Test with content having no patterns
  no_match_content <- c("x <- 1", "y <- 2", "z <- x + y")
  no_match_result <- penguins_substitute(no_match_content, "test.R")
  expect_false(no_match_result$matches)
  expect_equal(no_match_result$content, no_match_content)
})

# testing penguins_convert() ---------------------------------------------
test_that("penguins_convert correctly converts fixture files with penguins references", {
  # Test files with penguins references
  penguin_files <- list(
    test_path("fixtures", "example_dir", "penguins.R"),
    test_path("fixtures", "example_dir", "penguins.qmd"),
    test_path("fixtures", "example_dir", "nested", "penguins.rmd")
  )

  for (file_path in penguin_files) {
    # Create temporary output file
    temp_output <- withr::local_tempfile(
      fileext = paste0(".", tools::file_ext(file_path))
    )

    # Run conversion
    result <- penguins_convert(file_path, temp_output)

    # Check that the function reported changes
    expect_true(result)

    # Read output file content
    output_content <- readLines(temp_output)

    # Check that conversions were made
    expect_false(any(grepl(
      "library\\(['\"]?palmerpenguins['\"]?\\)",
      output_content
    )))
    expect_false(any(grepl(
      'data("penguins", package = "palmerpenguins")',
      output_content
    )))
    expect_false(any(grepl(
      "data(penguins, package='palmerpenguins')",
      output_content
    )))
    expect_false(any(grepl(
      "data\\(['\"]?penguins['\"]?, package\\s?=\\s?['\"]palmerpenguins['\"]\\)",
      output_content
    )))
    expect_false(any(grepl("bill_length_mm", output_content)))
    expect_true(any(grepl("bill_len", output_content)))

    # Test for in-place modification
    if (file.info(file_path)$isdir == FALSE && file.access(file_path, 2) == 0) {
      # Make a temporary copy of the input file for in-place testing
      temp_copy <- withr::local_tempfile(
        fileext = paste0(".", tools::file_ext(file_path))
      )
      file.copy(file_path, temp_copy)

      # Run in-place conversion
      in_place_result <- penguins_convert(temp_copy, NULL)

      # Check that the function reported changes
      expect_true(in_place_result)

      # Read modified file content
      modified_content <- readLines(temp_copy)

      # Check that modifications were made
      expect_false(any(grepl(
        "library\\(['\"]?palmerpenguins['\"]?\\)",
        modified_content
      )))
    }
  }
})

test_that("penguins_convert correctly handles files without penguins references", {
  # Test files without penguins references
  no_penguin_files <- list(
    test_path("fixtures", "example_dir", "no_penguins.R"),
    test_path("fixtures", "example_dir", "nested", "no_penguins.Rmd")
  )

  for (file_path in no_penguin_files) {
    # Create temporary output file
    temp_output <- withr::local_tempfile(
      fileext = paste0(".", tools::file_ext(file_path))
    )

    # Run conversion
    result <- penguins_convert(file_path, temp_output)

    # Check that the function reported no changes
    expect_false(result)

    # Check that output content matches input content
    input_content <- readLines(file_path)
    output_content <- readLines(temp_output)
    expect_equal(output_content, input_content)
  }
})

test_that("penguins_convert handles edge cases correctly", {
  # Create an empty temporary file
  empty_file <- withr::local_tempfile(fileext = ".R")
  file.create(empty_file)

  # Create output file for empty file test
  empty_output <- withr::local_tempfile(fileext = ".R")

  # Run conversion on empty file
  empty_result <- penguins_convert(empty_file, empty_output)

  # Check that the function reported no changes
  expect_false(empty_result)

  # Check that output file exists but is empty
  expect_true(file.exists(empty_output))
  expect_equal(readLines(empty_output, warn = FALSE), character(0))
})

test_that("penguins_convert handles file writing correctly based on changes", {
  # Create a temporary file with no penguins references
  no_changes_file <- withr::local_tempfile(fileext = ".R")
  writeLines("x <- 1\ny <- 2", no_changes_file)

  # Case 1: No changes needed, output same as input
  # This should not modify the file at all
  original_content <- readLines(no_changes_file)

  result <- penguins_convert(no_changes_file, no_changes_file)

  # File should not be modified
  expect_false(result) # No changes were made
  expect_equal(readLines(no_changes_file), original_content)

  # Case 2: No changes needed but different output location
  # This should copy the file without modification
  different_output <- withr::local_tempfile(fileext = ".R")
  result <- penguins_convert(no_changes_file, different_output)

  expect_false(result) # No changes were made
  expect_equal(readLines(different_output), original_content)

  # Case 3: Changes needed (file with penguin references)
  penguin_file <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c("library(palmerpenguins)", "x <- penguins$bill_length_mm"),
    penguin_file
  )

  # Output to same location (in-place modification)
  result <- penguins_convert(penguin_file, penguin_file)

  expect_true(result) # Changes were made
  modified_content <- readLines(penguin_file)
  expect_false(any(grepl("palmerpenguins", modified_content)))
  expect_false(any(grepl("bill_length_mm", modified_content)))
  expect_true(any(grepl("bill_len", modified_content)))

  # Case 4: Changes needed with different output location
  penguin_file2 <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c("library(palmerpenguins)", "x <- penguins$bill_length_mm"),
    penguin_file2
  )
  different_output2 <- withr::local_tempfile(fileext = ".R")

  result <- penguins_convert(penguin_file2, different_output2)

  expect_true(result) # Changes were made
  expect_false(any(grepl("palmerpenguins", readLines(different_output2))))
  expect_true(any(grepl("bill_len", readLines(different_output2))))

  # Case 5: File with no EOL at end - test that we don't add one
  # when no changes are needed
  no_eol_file <- withr::local_tempfile(fileext = ".R")
  con <- file(no_eol_file, "wb")
  writeChar("x <- 1\ny <- 2", con, eos = NULL)
  close(con)

  # Get original file content as raw bytes to check exact line endings
  original_raw <- readBin(no_eol_file, "raw", file.info(no_eol_file)$size)

  different_output3 <- withr::local_tempfile(fileext = ".R")
  result <- penguins_convert(no_eol_file, different_output3)

  # Check that output file has same raw content (no EOL added)
  output_raw <- readBin(
    different_output3,
    "raw",
    file.info(different_output3)$size
  )
  expect_equal(output_raw, original_raw)
})
