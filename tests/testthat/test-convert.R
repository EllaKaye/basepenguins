# testing validate_penguins_input() --------------------------------------
test_that("validate_penguins_input validates input and output paths correctly", {
  # Create temporary R file for testing
  temp_file <- withr::local_tempfile(fileext = ".R")
  writeLines("test content", temp_file)

  # Test with valid input and NULL output (same as input)
  result <- validate_penguins_input(temp_file, NULL)
  expect_equal(result$input, normalizePath(temp_file))
  expect_equal(result$output, normalizePath(temp_file))

  # For macOS and some other systems, the actual path structure might differ
  # So we focus on testing that the file exists and is the same file
  expect_true(file.exists(result$input))
  expect_true(file.exists(result$output))

  # When output is NULL, output_short should match the input
  if (is.null(result$output_short) || result$output_short != temp_file) {
    # Test behavior rather than exact string
    expect_true(file.exists(result$output_short))
    expect_equal(basename(result$output_short), basename(temp_file))
  }

  # Test with valid input and valid output
  temp_output <- withr::local_tempfile(fileext = ".R")
  result <- validate_penguins_input(temp_file, temp_output)
  expect_equal(result$input, normalizePath(temp_file))
  expect_equal(result$output, normalizePath(temp_output))

  # Check that output_short corresponds to the output file
  expect_true(file.exists(result$output))
  expect_equal(basename(result$output_short), basename(temp_output))
})

test_that("validate_penguins_input creates output directory if needed", {
  # Create temporary R file for testing
  temp_file <- withr::local_tempfile(fileext = ".R")
  writeLines("test content", temp_file)

  # Create temporary directory with nested output path
  temp_dir <- withr::local_tempdir()
  nested_output <- file.path(temp_dir, "nested", "output.R")

  # Directory should not exist yet
  expect_false(dir.exists(file.path(temp_dir, "nested")))

  # Function should create the directory
  result <- validate_penguins_input(temp_file, nested_output)

  # Check that directory was created
  expect_true(dir.exists(file.path(temp_dir, "nested")))
  expect_true(file.exists(nested_output))
})

test_that("validate_penguins_input creates output file if needed", {
  # Create temporary R file for testing
  temp_file <- withr::local_tempfile(fileext = ".R")
  writeLines("test content", temp_file)

  # Create temporary output file path (doesn't exist yet)
  temp_output <- withr::local_tempfile(fileext = ".R")

  # File should not exist yet
  expect_false(file.exists(temp_output))

  # Function should create the file
  result <- validate_penguins_input(temp_file, temp_output)

  # Check that file was created
  expect_true(file.exists(temp_output))
})

test_that("validate_penguins_input handles different file extensions correctly", {
  # Create temporary files with different extensions
  extensions <- c("R", "qmd", "rmd", "Rmd")

  for (ext in extensions) {
    temp_file <- withr::local_tempfile(fileext = paste0(".", ext))
    writeLines("test content", temp_file)

    # Test that each extension is accepted
    expect_no_error(validate_penguins_input(temp_file, NULL))
  }

  # Test with invalid extension
  temp_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("test content", temp_file)

  expect_error(
    validate_penguins_input(temp_file, NULL),
    "`input` does not have a valid file extension"
  )
})

test_that("validate_penguins_input errors with multiple input paths", {
  # Create temporary files
  temp_file1 <- withr::local_tempfile(fileext = ".R")
  temp_file2 <- withr::local_tempfile(fileext = ".R")
  writeLines("test content", temp_file1)
  writeLines("test content", temp_file2)

  # Test with multiple inputs
  expect_error(
    validate_penguins_input(c(temp_file1, temp_file2), NULL),
    "`input` must be a single character string"
  )
})

test_that("validate_penguins_input errors with multiple output paths", {
  # Create temporary files
  temp_file <- withr::local_tempfile(fileext = ".R")
  temp_output1 <- withr::local_tempfile(fileext = ".R")
  temp_output2 <- withr::local_tempfile(fileext = ".R")
  writeLines("test content", temp_file)

  # Test with multiple outputs
  expect_error(
    validate_penguins_input(temp_file, c(temp_output1, temp_output2)),
    "`output` must be a single character string"
  )
})

test_that("validate_penguins_input errors when input file doesn't exist", {
  # Create a path to a non-existent file
  non_existent_file <- tempfile(fileext = ".R")

  # Make sure the file doesn't exist
  if (file.exists(non_existent_file)) {
    file.remove(non_existent_file)
  }

  # Test with non-existent input - error msg varies by OS, just check it errors
  expect_error(
    validate_penguins_input(non_existent_file, NULL)
  )
})

test_that("validate_penguins_input works with fixtures/example_dir files", {
  # Get path to fixtures/example_dir using test_path
  fixtures_dir <- test_path("fixtures", "example_dir")

  # Test with existing R file
  r_file <- file.path(fixtures_dir, "penguins.R")
  result <- validate_penguins_input(r_file, NULL)
  expect_equal(basename(result$input), "penguins.R")
  expect_equal(basename(result$output), "penguins.R")

  # Test with existing qmd file
  qmd_file <- file.path(fixtures_dir, "penguins.qmd")
  result <- validate_penguins_input(qmd_file, NULL)
  expect_equal(basename(result$input), "penguins.qmd")
  expect_equal(basename(result$output), "penguins.qmd")

  # Test with existing rmd file in nested directory
  rmd_file <- file.path(fixtures_dir, "nested", "penguins.rmd")
  result <- validate_penguins_input(rmd_file, NULL)
  expect_equal(basename(result$input), "penguins.rmd")
  expect_equal(basename(result$output), "penguins.rmd")
})

test_that("validate_penguins_input works with custom extensions", {
  # Create temporary R file
  temp_file <- withr::local_tempfile(fileext = ".custom")
  writeLines("test content", temp_file)

  # Test with custom extension
  expect_error(
    validate_penguins_input(temp_file, NULL)
  )

  # Test with custom extension allowed
  expect_no_error(
    validate_penguins_input(temp_file, NULL, extensions = c("R", "custom"))
  )
})
