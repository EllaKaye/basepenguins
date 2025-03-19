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

# testing convert_dir() --------------------------------------------------

# EK checked
test_that("convert_dir correctly processes a directory to new directory", {
  # Input and output directories
  temp_dir <- withr::local_tempdir()
  example_dir <- test_path("fixtures", "example_dir")

  # Run the conversion
  result <- convert_dir(example_dir, temp_dir)

  # Check result structure
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Spot-check that a file with penguins references was modified
  # (not checking all files, one should be sufficient here)
  penguin_r_content <- readLines(file.path(temp_dir, "penguins.R"))
  expect_false(any(grepl("bill_length_mm", penguin_r_content)))
  expect_true(any(grepl("bill_len", penguin_r_content)))

  # Instead of directly checking paths, check that we have the expected number
  # of changed files and that their file name starts with "penguins"
  file_names <- basename(
    list.files(
      temp_dir,
      recursive = TRUE,
      pattern = "\\.(R|r|qmd|rmd|Rmd)$"
    )
  )
  penguin_file_count <- sum(grepl("^penguins", file_names))
  expect_equal(length(result$changed), penguin_file_count)
  changed_basenames <- basename(result$changed)
  expected_changed <- paste0("penguins", c(".rmd", ".qmd", ".R"))
  expect_setequal(changed_basenames, expected_changed)

  # Check that non-penguin files were not modified
  not_changed_basenames <- basename(result$not_changed)
  expect_true("no_penguins.R" %in% not_changed_basenames)
  expect_true("no_penguins.Rmd" %in% not_changed_basenames)
  expect_true("empty.R" %in% not_changed_basenames)

  # Verify output directory structure
  expect_true(dir.exists(file.path(temp_dir, "nested")))

  # Check some specific file content
  penguin_r_output <- file.path(temp_dir, "penguins.R")
  expect_true(file.exists(penguin_r_output))

  output_content <- readLines(penguin_r_output)
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    output_content
  )))
  expect_false(any(grepl("bill_length_mm", output_content)))
  expect_true(any(grepl("bill_len", output_content)))
})

# EK checked
test_that("convert_dir correctly processes files in-place when output is NULL", {
  # Create a temporary directory with copies of the files
  temp_dir <- withr::local_tempdir()
  example_dir <- test_path("fixtures", "example_dir")

  # Copy all files from example_dir to temp_dir
  file_names <- list.files(example_dir, recursive = TRUE)
  for (file_name in file_names) {
    src_path <- file.path(example_dir, file_name)
    dest_path <- file.path(temp_dir, file_name)

    # Create directory for file if it doesn't exist
    dest_dir <- dirname(dest_path)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    file.copy(src_path, dest_path)
  }

  # Verify files were copied correctly
  expect_true(file.exists(file.path(temp_dir, "penguins.R")))
  expect_true(file.exists(file.path(temp_dir, "nested", "penguins.rmd")))

  # Save original content to verify changes
  original_penguin_r_content <- readLines(file.path(temp_dir, "penguins.R"))

  # Run the conversion in-place (specifically targeting NULL branch of convert_dir)
  result <- convert_dir(temp_dir, NULL)

  # Check result structure
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Verify that files with penguins references were modified
  penguin_r_path <- file.path(temp_dir, "penguins.R")
  penguin_r_content <- readLines(penguin_r_path)

  # Verify content was actually changed
  expect_false(identical(penguin_r_content, original_penguin_r_content))

  # Verify specific replacements
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    penguin_r_content
  )))
  expect_false(any(grepl("bill_length_mm", penguin_r_content)))
  expect_true(any(grepl("bill_len", penguin_r_content)))

  # Instead of directly checking paths, check that we have the expected number
  # of changed files and that their names start with "penguins"
  file_names <- basename(
    list.files(
      temp_dir,
      recursive = TRUE,
      pattern = "\\.(R|r|qmd|rmd|Rmd)$"
    )
  )

  # files with penguins content all have names that start with "penguins"
  penguin_file_count <- sum(grepl("^penguins", file_names))
  expect_equal(length(result$changed), penguin_file_count)
  changed_basenames <- basename(result$changed)
  expect_true(all(grepl("^penguins", changed_basenames)))
})

# EK checked
test_that("convert_dir handles custom extensions", {
  # Create a temporary directory for output
  temp_dir <- withr::local_tempdir()
  example_dir <- test_path("fixtures", "example_dir")

  # Run with only .R extension
  result <- convert_dir(example_dir, temp_dir, extensions = "R")

  # Get the basenames of changed files
  changed_basenames <- basename(result$changed)

  # Check that only .R files were processed
  expect_true(all(tools::file_ext(changed_basenames) == "R"))

  # Verify .R file was converted
  penguin_r_output <- file.path(temp_dir, "penguins.R")
  expect_true(file.exists(penguin_r_output))

  r_content <- readLines(penguin_r_output)
  expect_false(any(grepl("library\\(['\"]?palmerpenguins['\"]?\\)", r_content)))

  # Verify .qmd file was copied but not converted
  penguin_qmd_output <- file.path(temp_dir, "penguins.qmd")
  expect_true(file.exists(penguin_qmd_output))

  qmd_content <- readLines(penguin_qmd_output)
  expect_true(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    qmd_content
  )))
})

# EK checked
test_that("convert_dir handles non-existent directory", {
  expect_error(
    convert_dir("non_existent_dir", NULL),
    "`input` must be a directory that exists"
  )
})

test_that("convert_dir handles empty directory", {
  empty_dir <- withr::local_tempdir()
  expect_error(
    convert_dir(empty_dir, NULL),
    "There are no files in `input` to convert"
  )
})

test_that("convert_dir handles no files with specified extensions)", {
  source_dir <- withr::local_tempdir()
  md_file <- file.path(source_dir, "readme.md")
  md_input_content <- c(
    "# This is a markdown file",
    "```",
    "library(palmerpenguins)",
    "```"
  )
  writeLines(md_input_content, md_file)

  expect_error(
    convert_dir(source_dir, NULL),
    "There are no files with specified extensions to convert"
  )
})

# EK checked
test_that("convert_dir creates all required subdirectories in output", {
  # Create a temporary directory with a deeply nested structure
  source_dir <- withr::local_tempdir()
  nested_dir <- file.path(source_dir, "level1", "level2", "level3")
  dir.create(nested_dir, recursive = TRUE)

  # Create a file with penguin references in the nested directory
  penguin_file <- file.path(nested_dir, "penguins.R")
  writeLines(
    c(
      "library(palmerpenguins)",
      "bill_len <- penguins$bill_length_mm",
      "body_mass <- penguins$body_mass_g"
    ),
    penguin_file
  )

  # Create output directory
  output_dir <- withr::local_tempdir()

  # Run conversion
  result <- convert_dir(source_dir, output_dir)

  # Check that nested directories were created
  expect_true(dir.exists(file.path(output_dir, "level1")))
  expect_true(dir.exists(file.path(output_dir, "level1", "level2")))
  expect_true(dir.exists(file.path(output_dir, "level1", "level2", "level3")))

  # Check that the file was converted
  output_file_path <- file.path(
    output_dir,
    "level1",
    "level2",
    "level3",
    "penguins.R"
  )
  expect_true(file.exists(output_file_path))

  output_content <- readLines(output_file_path)
  expect_false(any(grepl("palmerpenguins", output_content)))
  expect_false(any(grepl("bill_length_mm", output_content)))
  expect_true(any(grepl("bill_len", output_content)))
})

# EK checked
test_that("convert_dir handles a mixture of file types correctly", {
  # Create a temporary directory with various file types
  source_dir <- withr::local_tempdir()

  # Create an R file with penguin references
  penguin_r_file <- file.path(source_dir, "penguins.R")
  writeLines(
    c(
      "library(palmerpenguins)",
      "data <- penguins$bill_length_mm"
    ),
    penguin_r_file
  )

  # Create a markdown file
  md_file <- file.path(source_dir, "readme.md")
  md_input_content <- c(
    "# This is a markdown file",
    "```",
    "library(palmerpenguins)",
    "```"
  )
  writeLines(md_input_content, md_file)

  # Create an HTML file
  html_file <- file.path(source_dir, "page.html")
  html_input_content <- "<html><body>Test</body></html>"
  writeLines(html_input_content, html_file)

  # Create output directory
  output_dir <- withr::local_tempdir()

  # Run conversion
  result <- convert_dir(source_dir, output_dir)

  # Check result structure
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Verify all files were copied to output directory
  expect_true(file.exists(file.path(output_dir, "penguins.R")))
  expect_true(file.exists(file.path(output_dir, "readme.md")))
  expect_true(file.exists(file.path(output_dir, "page.html")))

  # Verify only the R file was converted
  r_content <- readLines(file.path(output_dir, "penguins.R"))
  expect_false(any(grepl("library\\(palmerpenguins\\)", r_content)))
  expect_false(any(grepl("bill_length_mm", r_content)))
  expect_true(any(grepl("bill_len", r_content)))

  # Verify non-R files were just copied
  md_output_content <- readLines(file.path(output_dir, "readme.md"))
  expect_equal(md_output_content, md_input_content)

  html_output_content <- readLines(file.path(output_dir, "page.html"))
  expect_equal(html_output_content, html_input_content)
})

# EK fixed
test_that("convert_dir reports correct statistics in result lists", {
  # Setup directories
  example_dir <- test_path("fixtures", "example_dir")
  output_dir <- withr::local_tempdir()

  # Run conversion
  result <- convert_dir(example_dir, output_dir)

  # Count files in example directory
  all_files <- list.files(example_dir, recursive = TRUE)
  convertible_files <- grep("\\.(R|r|rmd|Rmd|qmd)$", all_files, value = TRUE)
  # files with palmerpenguins all have filesnames beginning "penguins",
  # maybe in "nested" dir
  penguins_convertible_files <- grep("^(nested/)?penguins", convertible_files)
  non_penguins_convertible_files <- grep(
    "^(nested/)?penguins",
    convertible_files,
    invert = TRUE
  )
  non_convertible_files <- grep(
    "\\.(R|r|rmd|Rmd|qmd)$",
    all_files,
    invert = TRUE
  )

  # Check that all files are accounted for in the result
  expect_equal(
    length(result$changed) + length(result$not_changed),
    length(all_files)
  )

  # Convert the names in the result to basenames for easier comparison
  changed_basenames <- basename(result$changed)
  not_changed_basenames <- basename(result$not_changed)

  # Count the number of files that have 'penguins' in their name
  expect_equal(length(result$changed), length(penguins_convertible_files))

  # Check that all changed filenames start with 'penguins'
  expect_true(all(grepl("^penguins", changed_basenames)))

  # Check that no non-penguin files were changed
  for (idx in non_penguins_convertible_files) {
    file_name <- basename(convertible_files[idx])
    expect_false(
      file_name %in% changed_basenames,
      info = paste("Non-penguin file was incorrectly changed:", file_name)
    )
  }

  # Check that non-convertible files were not changed
  for (idx in non_convertible_files) {
    file_name <- basename(all_files[idx])
    expect_false(
      file_name %in% changed_basenames,
      info = paste("Non-convertible file was incorrectly changed:", file_name)
    )
  }
})

# EK checked
test_that("convert_dir handles paths with spaces", {
  # Create source directory with spaces in the name
  source_dir <- withr::local_tempdir()
  space_dir <- file.path(source_dir, "dir with spaces")
  dir.create(space_dir)

  # Create file with penguin references
  penguin_file <- file.path(space_dir, "file with spaces.R")
  writeLines(
    c(
      "library(palmerpenguins)",
      "bill_len <- penguins$bill_length_mm"
    ),
    penguin_file
  )

  # Create output directory with spaces
  output_dir <- withr::local_tempdir()
  output_space_dir <- file.path(output_dir, "output with spaces")

  # Run conversion
  result <- convert_dir(source_dir, output_space_dir)

  # Check that directories and files with spaces were processed correctly
  expect_true(dir.exists(file.path(output_space_dir, "dir with spaces")))

  output_file <- file.path(
    output_space_dir,
    "dir with spaces",
    "file with spaces.R"
  )
  expect_true(file.exists(output_file))

  # Verify content was converted
  output_content <- readLines(output_file)
  expect_false(any(grepl("palmerpenguins", output_content)))
  expect_true(any(grepl("bill_len", output_content)))
})

# Add specific test to exercise the exact NULL output code path
test_that("convert_dir with NULL output explicitly exercises that code path", {
  # Setup a directory with a penguin file for in-place conversion
  temp_dir <- withr::local_tempdir()

  # Create a penguin file to ensure we have something to convert
  penguin_file <- file.path(temp_dir, "penguins.R")
  writeLines(
    c(
      "library(palmerpenguins)",
      "bill_len <- penguins$bill_length_mm",
      "body_mass <- penguins$body_mass_g"
    ),
    penguin_file
  )

  # Run the function with NULL output explicitly
  # This specifically exercises the is.null(output) branch
  result <- convert_dir(temp_dir, NULL)

  # Verify the result structure
  expect_type(result, "list")
  expect_named(result, c("changed", "not_changed"))

  # Verify the file was changed
  expect_equal(length(result$changed), 1)
  expect_true(penguin_file %in% result$changed)

  # Verify the content was changed
  converted_content <- readLines(penguin_file)
  expect_false(any(grepl("library\\(palmerpenguins\\)", converted_content)))
  expect_false(any(grepl("bill_length_mm", converted_content)))
  expect_true(any(grepl("bill_len", converted_content)))
})
