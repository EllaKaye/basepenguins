# testing convert_files() ------------------------------------------------

test_that("convert_files correctly processes files with new output locations", {
  # Input files: one with penguins references, one without
  penguin_file <- test_path("fixtures", "example_dir", "penguins.R")
  no_penguin_file <- test_path("fixtures", "example_dir", "no_penguins.R")

  input_files <- c(penguin_file, no_penguin_file)

  # Create temporary output files
  temp_dir <- withr::local_tempdir()
  output_files <- file.path(temp_dir, basename(input_files))

  # Run conversion
  result <- convert_files(input_files, output_files)

  # Check that the function returned the expected structure
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Check that output files were created
  expect_true(file.exists(output_files[1]))
  expect_true(file.exists(output_files[2]))

  # Verify content of penguin output file (should have substitutions)
  penguin_out_content <- readLines(output_files[1])
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    penguin_out_content
  )))

  # These two lines need testing as a pair
  expect_false(any(grepl("bill_length_mm", penguin_out_content)))
  expect_true(any(grepl("bill_len", penguin_out_content)))

  # Verify content of no_penguin output file (should be unchanged)
  no_penguin_out_content <- readLines(output_files[2])
  expect_equal(no_penguin_out_content, readLines(no_penguin_file))

  # Check values and names of result
  expected_changed <- output_files[1]
  names(expected_changed) <- input_files[1]
  expected_not_changed <- output_files[2]
  names(expected_not_changed) <- input_files[2]
  expect_equal(result$changed, expected_changed)
  expect_equal(result$not_changed, expected_not_changed)
})

test_that("convert_files handles overwrite (output = NULL) correctly", {
  # Create temporary copies to test in-place modification
  temp_dir <- withr::local_tempdir()

  penguin_copy <- file.path(temp_dir, "penguins.R")
  no_penguin_copy <- file.path(temp_dir, "no_penguins.R")

  file.copy(test_path("fixtures", "example_dir", "penguins.R"), penguin_copy)
  file.copy(
    test_path("fixtures", "example_dir", "no_penguins.R"),
    no_penguin_copy
  )

  input_files <- c(penguin_copy, no_penguin_copy)

  # Run in-place conversion
  result <- convert_files(input_files, NULL)

  # Verify the penguin file was processed correctly
  modified_content <- readLines(penguin_copy)
  # Instead of comparing before/after content directly, check for specific changes
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    modified_content
  )))

  # These two lines need testing as a pair
  expect_false(any(grepl("bill_length_mm", modified_content)))
  expect_true(any(grepl("bill_len", modified_content)))

  # Verify the no_penguin file was not modified
  no_penguin_content <- readLines(no_penguin_copy)
  expect_equal(
    no_penguin_content,
    readLines(test_path("fixtures", "example_dir", "no_penguins.R"))
  )

  # Check the structure of the result
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Check values and names of result
  expected_changed <- input_files[1]
  names(expected_changed) <- input_files[1]
  expected_not_changed <- input_files[2]
  names(expected_not_changed) <- input_files[2]
  expect_equal(result$changed, expected_changed)
  expect_equal(result$not_changed, expected_not_changed)
})

test_that("convert_files creates output directories when needed", {
  input_file <- test_path("fixtures", "example_dir", "penguins.R")

  # Create output path with nested directories
  temp_dir <- withr::local_tempdir()
  nested_dir <- file.path(temp_dir, "level1", "level2")
  output_file <- file.path(nested_dir, "output_penguins.R")

  # Run conversion
  expect_false(dir.exists(nested_dir))
  result <- convert_files(input_file, output_file)

  # Check that directories were created
  expect_true(dir.exists(nested_dir))
  expect_true(file.exists(output_file))

  # Verify the file has the expected content
  output_content <- readLines(output_file)

  # These two lines need testing as a pair
  expect_false(any(grepl("bill_length_mm", output_content)))
  expect_true(any(grepl("bill_len", output_content)))
})

test_that("convert_files handles non-convertible files correctly", {
  # Use a mix of convertible and non-convertible files
  r_file <- test_path("fixtures", "example_dir", "penguins.R")
  md_file <- test_path("fixtures", "example_dir", "not_a_script.md")
  html_file <- test_path(
    "fixtures",
    "example_dir",
    "nested",
    "not_a_script.html"
  )

  input_files <- c(r_file, md_file, html_file)

  # Create output paths
  temp_dir <- withr::local_tempdir()
  output_files <- file.path(temp_dir, basename(input_files))

  # Ensure we need to create the nested directory for HTML file
  output_files[3] <- file.path(temp_dir, "nested", basename(html_file))

  # Run conversion
  result <- convert_files(input_files, output_files)

  # Check the structure of the result
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Verify non-convertible files were copied correctly
  expect_true(file.exists(output_files[2])) # md file
  expect_true(file.exists(output_files[3])) # html file

  # Content should match original
  expect_equal(readLines(output_files[2]), readLines(md_file))
  expect_equal(readLines(output_files[3]), readLines(html_file))

  # The R file should have been converted
  r_output_content <- readLines(output_files[1])
  expect_false(any(grepl("bill_length_mm", r_output_content)))
  expect_true(any(grepl("bill_len", r_output_content)))
})

test_that("convert_files validates that output is same length as input", {
  input_files <- c(
    test_path("fixtures", "example_dir", "penguins.R"),
    test_path("fixtures", "example_dir", "penguins.qmd")
  )
  output_files <- test_path("fixtures", "example_dir", "output.R")

  expect_error(
    convert_files(input_files, output_files),
    "`output` must be the same length as `input` \\(or NULL\\)"
  )
})

test_that("convert_files works with specified extension types", {
  # Setup test files
  r_file <- test_path("fixtures", "example_dir", "penguins.R")
  qmd_file <- test_path("fixtures", "example_dir", "penguins.qmd")

  # Create temporary output files
  temp_dir <- withr::local_tempdir()
  r_output <- file.path(temp_dir, "penguins.R")
  qmd_output <- file.path(temp_dir, "penguins.qmd")

  # Run conversion with only R extension
  result <- convert_files(
    c(r_file, qmd_file),
    c(r_output, qmd_output),
    extensions = "R"
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Check that both files exist
  expect_true(file.exists(r_output))
  expect_true(file.exists(qmd_output))

  # Check R file was converted
  r_content <- readLines(r_output)
  expect_false(any(grepl("palmerpenguins", r_content)))

  # Check qmd file was copied but not converted (since not in extensions)
  qmd_content <- readLines(qmd_output)
  expect_true(any(grepl("palmerpenguins", qmd_content)))
})

test_that("convert_files processes multiple files with mixed content correctly", {
  # Create a list of files in the fixture directory
  files_with_penguins <- c(
    test_path("fixtures", "example_dir", "penguins.R"),
    test_path("fixtures", "example_dir", "penguins.qmd"),
    test_path("fixtures", "example_dir", "nested", "penguins.rmd")
  )

  files_without_penguins <- c(
    test_path("fixtures", "example_dir", "no_penguins.R"),
    test_path("fixtures", "example_dir", "nested", "no_penguins.Rmd")
  )

  all_files <- c(files_with_penguins, files_without_penguins)

  # Create temporary output directory
  temp_dir <- withr::local_tempdir()
  output_files <- file.path(temp_dir, basename(all_files))

  # For the nested files, create the correct output paths
  nested_indices <- grep("nested", all_files)
  output_files[nested_indices] <- file.path(
    temp_dir,
    "nested",
    basename(all_files[nested_indices])
  )

  # Run conversion
  result <- convert_files(all_files, output_files)

  # Check that the result structure is correct
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Verify all output files were created
  for (file in output_files) {
    expect_true(file.exists(file))
  }

  # Check content of output files
  for (i in seq_along(all_files)) {
    output_content <- readLines(output_files[i])
    input_content <- readLines(all_files[i])
    file_ext <- tools::file_ext(all_files[i])

    # If this is a penguin file with valid extension, check that substitutions were made
    if (
      all_files[i] %in%
        files_with_penguins &&
        file_ext %in% c("R", "r", "qmd", "rmd", "Rmd")
    ) {
      # Check for specific patterns that should be replaced
      if (any(grepl("palmerpenguins", input_content))) {
        expect_false(any(grepl("palmerpenguins", output_content)))
      }
      if (any(grepl("bill_length_mm", input_content))) {
        expect_false(any(grepl("bill_length_mm", output_content)))
        expect_true(any(grepl("bill_len", output_content)))
      }
    } else {
      # For non-penguin files, content should be unchanged
      expect_equal(output_content, input_content)
    }
  }
})

test_that("convert_files handles paths with spaces correctly", {
  # Create a temporary directory with spaces in the name
  temp_dir <- withr::local_tempdir()
  space_dir <- file.path(temp_dir, "dir with spaces")
  dir.create(space_dir)

  # Create input file with spaces in path
  input_file <- file.path(space_dir, "input with spaces.R")
  file.copy(test_path("fixtures", "example_dir", "penguins.R"), input_file)

  # Create output path with spaces
  output_file <- file.path(space_dir, "output with spaces.R")

  # Run conversion
  result <- convert_files(input_file, output_file)

  # Check results
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Verify file was created and processed
  expect_true(file.exists(output_file))
  output_content <- readLines(output_file)
  expect_false(any(grepl("bill_length_mm", output_content)))
  expect_true(any(grepl("bill_len", output_content)))
})

test_that("convert_files handles empty input appropriately", {
  expect_error(
    convert_files(character(0), character(0)),
    "`input` must not be emply. Please supply at least one file to convert."
  )
})

test_that("convert_files doesn't generate message to check output if no changes", {
  no_penguin_file <- test_path("fixtures", "example_dir", "no_penguins.R")

  # Create output path
  output_dir <- withr::local_tempdir()
  output_file <- file.path(output_dir, "no_penguins.R")

  # Capture messages - there should be none about changed files
  messages <- capture_messages({
    result <- convert_files(no_penguin_file, output_file)
  })

  # Verify no message about checking changed files (since nothing changed)
  expect_false(any(grepl("Please check the changed output files", messages)))

  # Check result structure
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # The changed list should be empty
  expect_length(result$changed, 0)

  # The not_changed list should have our file
  expect_length(result$not_changed, 1)
  expect_true(no_penguin_file %in% names(result$not_changed))
  expect_equal(result$not_changed[[no_penguin_file]], output_file)
})

test_that("convert_files returns the correct structure and message with changed and not_changed files", {
  # This test specifically targets lines otherwise showing as uncovered
  # We need files that will produce both converted and not converted results

  # Create files for testing
  temp_dir <- withr::local_tempdir()

  # File with penguin references (will be converted)
  penguin_file <- file.path(temp_dir, "with_penguins.R")
  writeLines(
    c(
      "library(palmerpenguins)",
      "data <- penguins$bill_length_mm",
      "body_mass <- penguins$body_mass_g"
    ),
    penguin_file
  )

  # File without penguin references (won't be converted)
  no_penguin_file <- file.path(temp_dir, "no_penguins.R")
  writeLines("x <- 1", no_penguin_file)

  # Non-convertible file (will be copied but not processed)
  md_file <- file.path(temp_dir, "readme.md")
  writeLines("# Test file", md_file)

  input_files <- c(penguin_file, no_penguin_file, md_file)

  # Create output paths
  output_dir <- withr::local_tempdir()
  output_files <- file.path(output_dir, basename(input_files))

  # Capture messages to verify they're generated
  messages <- capture_messages({
    result <- convert_files(input_files, output_files)
  })

  # Check for expected message when files are changed
  expect_true(any(grepl("Please check the changed output files", messages)))

  # Check result structure
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Check specific structure of changed and not_changed
  expect_true(length(result$changed) > 0)
  expect_true(length(result$not_changed) > 0)

  # Verify the names in the result (these correspond to input paths)
  expect_true(penguin_file %in% names(result$changed))
  expect_true(no_penguin_file %in% names(result$not_changed))
  expect_true(md_file %in% names(result$not_changed))

  # Verify the values in the result (these correspond to output paths)
  expect_equal(result$changed[[penguin_file]], output_files[1])
  expect_equal(result$not_changed[[no_penguin_file]], output_files[2])
  expect_equal(result$not_changed[[md_file]], output_files[3])
})
