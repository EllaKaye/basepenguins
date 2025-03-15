# testing validate_input_output() --------------------------------------
# EK_version
test_that("validate_input_output validates existing files with NULL output", {
  test_file <- test_path("fixtures", "example_dir", "penguins.R")
  result <- validate_input_output(test_file, NULL)

  expect_equal(result$input, result$output)
  expect_equal(basename(result$input), basename(test_file))
  expect_equal(basename(result$output), basename(test_file))
  expect_true(file.exists(result$input))
  expect_true(file.exists(result$output))
})

# EK_version
test_that("validate_input_output validates existing files with different output", {
  test_file <- test_path("fixtures", "example_dir", "penguins.R")

  # Test with output to a temporary file
  temp_output <- withr::local_tempfile(fileext = ".R")

  result <- validate_input_output(test_file, temp_output)
  expect_equal(basename(result$input), basename(test_file))
  expect_equal(result$output, normalizePath(temp_output))
  expect_true(file.exists(result$output))
})

test_that("validate_input_output creates output directory if needed", {
  # Use an existing fixture file for input
  input_file <- test_path("fixtures", "example_dir", "penguins.R")

  # Create temporary directory with nested output path
  temp_dir <- withr::local_tempdir()
  nested_output <- file.path(temp_dir, "nested", "output.R")

  # Directory should not exist yet
  expect_false(dir.exists(file.path(temp_dir, "nested")))

  # Function should create the directory
  result <- validate_input_output(input_file, nested_output)

  # Check that directory was created
  expect_true(dir.exists(file.path(temp_dir, "nested")))
  expect_true(file.exists(nested_output))
})

test_that("validate_input_output handles invalid inputs correctly", {
  # Use an existing fixture file for valid input reference
  valid_file <- test_path("fixtures", "example_dir", "penguins.R")

  # Test with invalid file extension
  invalid_ext_file <- test_path("fixtures", "example_dir", "not_a_script.md")
  expect_error(
    validate_input_output(invalid_ext_file, NULL),
    "`input` does not have a valid file extension"
  )

  # Test with non-existent file
  non_existent_file <- tempfile(fileext = ".R")
  # Make sure the file doesn't exist
  if (file.exists(non_existent_file)) {
    file.remove(non_existent_file)
  }
  expect_error(validate_input_output(non_existent_file, NULL))

  # Test with multiple inputs
  expect_error(
    validate_input_output(c(valid_file, valid_file), NULL),
    "`input` must be a single character string"
  )

  # Test with multiple outputs
  output1 <- withr::local_tempfile(fileext = ".R")
  output2 <- withr::local_tempfile(fileext = ".R")
  expect_error(
    validate_input_output(valid_file, c(output1, output2)),
    "`output` must be a single character string"
  )

  # Test with custom extensions allowed
  expect_no_error(
    validate_input_output(invalid_ext_file, NULL, extensions = c("R", "md"))
  )
})

# testing penguins_substitute() ------------------------------------------
test_that("penguins_substitute correctly processes fixture files", {
  # Test files with different characteristics
  test_files <- list(
    # File with penguins references
    list(
      path = test_path("fixtures", "example_dir", "penguins.R"),
      expected_match = TRUE,
      check_contains = "body_mass",
      check_not_contains = "body_mass_g"
    ),
    # File with penguins references and ends_with
    list(
      path = test_path("fixtures", "example_dir", "penguins.qmd"),
      expected_match = TRUE,
      check_contains = 'starts_with\\("flipper_"\\), starts_with\\("bill_"\\)',
      check_not_contains = 'ends_with\\(["\']_mm["\']\\)'
    ),
    # File with multiple penguins references
    list(
      path = test_path("fixtures", "example_dir", "nested", "penguins.rmd"),
      expected_match = TRUE,
      check_contains = "bill_len",
      check_not_contains = "bill_length_mm"
    ),
    # File with no penguins references
    list(
      path = test_path("fixtures", "example_dir", "no_penguins.R"),
      expected_match = FALSE,
      check_unchanged = TRUE
    )
  )

  for (file_info in test_files) {
    # Read file content
    file_content <- readLines(file_info$path)

    # Perform substitution
    result <- penguins_substitute(file_content, basename(file_info$path))

    # Check match status
    expect_equal(result$matches, file_info$expected_match)

    # Check content changes as expected
    if (file_info$expected_match) {
      if (!is.null(file_info$check_contains)) {
        expect_true(any(grepl(file_info$check_contains, result$content)))
      }
      if (!is.null(file_info$check_not_contains)) {
        expect_false(any(grepl(file_info$check_not_contains, result$content)))
      }
    } else if (
      !is.null(file_info$check_unchanged) && file_info$check_unchanged
    ) {
      expect_equal(result$content, file_content)
    }
  }
})

test_that("penguins_substitute handles all individual patterns correctly", {
  # Test each pattern individually with minimal test cases
  patterns_to_test <- list(
    # Library calls with different quote styles
    list(
      content = c(
        "library(palmerpenguins)",
        "library('palmerpenguins')",
        'library("palmerpenguins")'
      ),
      expected_content = c("", "", ""),
      expected_match = TRUE
    ),
    # Variable names
    list(
      content = c(
        "penguins$bill_length_mm",
        "penguins$bill_depth_mm",
        "penguins$flipper_length_mm",
        "penguins$body_mass_g"
      ),
      expected_content = c(
        "penguins$bill_len",
        "penguins$bill_dep",
        "penguins$flipper_len",
        "penguins$body_mass"
      ),
      expected_match = TRUE
    ),
    # ends_with pattern with multiple occurrences (should produce a message)
    list(
      content = c(
        'select(ends_with("_mm"))',
        'summarize(across(ends_with("_mm"), mean))'
      ),
      message_pattern = "lines 1, 2",
      expected_match = TRUE
    )
  )

  for (pattern in patterns_to_test) {
    if (!is.null(pattern$message_pattern)) {
      # Test for message when using ends_with multiple times
      expect_message(
        result <- penguins_substitute(pattern$content, "test.R"),
        pattern$message_pattern
      )
    } else {
      # Normal pattern test
      result <- penguins_substitute(pattern$content, "test.R")
    }

    # Check match status
    expect_equal(result$matches, pattern$expected_match)

    # Check content changes if expected_content provided
    if (!is.null(pattern$expected_content)) {
      expect_equal(result$content, pattern$expected_content)
    }
  }
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
    expect_false(any(grepl("bill_length_mm", output_content)))
    expect_false(any(grepl("body_mass_g", output_content)))

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
