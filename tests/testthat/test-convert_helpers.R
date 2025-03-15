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


# testing penguins_substitute() ------------------------------------------

test_that("penguins_substitute correctly identifies and substitutes all patterns", {
  # Test with all patterns present
  file_content <- c(
    "library(palmerpenguins)",
    "library('palmerpenguins')",
    'library("palmerpenguins")',
    "penguins$bill_length_mm",
    "penguins$bill_depth_mm",
    "penguins$flipper_length_mm",
    "penguins$body_mass_g",
    'penguins |> select(body_mass_g, ends_with("_mm"))'
  )

  output_short <- "test_output.R"
  result <- penguins_substitute(file_content, output_short)

  # Check if matches were found
  expect_true(result$matches)

  # Check if library calls were removed
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    result$content
  )))

  # Check if variable names were replaced
  expect_false(any(grepl("bill_length_mm", result$content)))
  expect_false(any(grepl("bill_depth_mm", result$content)))
  expect_false(any(grepl("flipper_length_mm", result$content)))
  expect_false(any(grepl("body_mass_g", result$content)))

  expect_true(any(grepl("bill_len", result$content)))
  expect_true(any(grepl("bill_dep", result$content)))
  expect_true(any(grepl("flipper_len", result$content)))
  expect_true(any(grepl("body_mass", result$content)))

  # Check if ends_with was replaced
  expect_false(any(grepl('ends_with\\(["\']_mm["\']\\)', result$content)))
  expect_true(any(grepl(
    'starts_with\\("flipper_"\\), starts_with\\("bill_"\\)',
    result$content
  )))
})

test_that("penguins_substitute returns no matches when no patterns are found", {
  # Test with no patterns present
  file_content <- c(
    "x <- 1",
    "y <- 2",
    "z <- x + y"
  )

  output_short <- "test_output.R"
  result <- penguins_substitute(file_content, output_short)

  # Check that no matches were found
  expect_false(result$matches)

  # Check that content is unchanged
  expect_equal(result$content, file_content)
})

test_that("penguins_substitute handles empty content", {
  # Test with empty content
  file_content <- character(0)

  output_short <- "test_output.R"
  result <- penguins_substitute(file_content, output_short)

  # Check that no matches were found
  expect_false(result$matches)

  # Check that content is unchanged
  expect_equal(result$content, file_content)
})

test_that("penguins_substitute correctly handles individual patterns", {
  # Test each pattern individually

  # 1. palmerpenguins library call
  library_content <- "library(palmerpenguins)"
  result <- penguins_substitute(library_content, "test_output.R")
  expect_true(result$matches)
  expect_equal(result$content, "")

  # 2. bill_length_mm
  bill_length_content <- "penguins$bill_length_mm"
  result <- penguins_substitute(bill_length_content, "test_output.R")
  expect_true(result$matches)
  expect_equal(result$content, "penguins$bill_len")

  # 3. bill_depth_mm
  bill_depth_content <- "penguins$bill_depth_mm"
  result <- penguins_substitute(bill_depth_content, "test_output.R")
  expect_true(result$matches)
  expect_equal(result$content, "penguins$bill_dep")

  # 4. flipper_length_mm
  flipper_content <- "penguins$flipper_length_mm"
  result <- penguins_substitute(flipper_content, "test_output.R")
  expect_true(result$matches)
  expect_equal(result$content, "penguins$flipper_len")

  # 5. body_mass_g
  body_mass_content <- "penguins$body_mass_g"
  result <- penguins_substitute(body_mass_content, "test_output.R")
  expect_true(result$matches)
  expect_equal(result$content, "penguins$body_mass")

  # 6. ends_with("_mm")
  ends_with_content <- 'select(ends_with("_mm"))'
  result <- penguins_substitute(ends_with_content, "test_output.R")
  expect_true(result$matches)
  expect_equal(
    result$content,
    'select(starts_with("flipper_"), starts_with("bill_"))'
  )
})

test_that("penguins_substitute processes fixture files correctly", {
  # Test with actual fixture files
  fixtures_dir <- test_path("fixtures", "example_dir")

  # Test with penguins.R file (has penguin references)
  r_file_path <- file.path(fixtures_dir, "penguins.R")
  r_file_content <- readLines(r_file_path)
  r_result <- penguins_substitute(r_file_content, "penguins.R")
  expect_true(r_result$matches)
  expect_false(any(grepl("body_mass_g", r_result$content)))
  expect_true(any(grepl("body_mass", r_result$content)))

  # Test with penguins.qmd file (has penguins references and ends_with)
  qmd_file_path <- file.path(fixtures_dir, "penguins.qmd")
  qmd_file_content <- readLines(qmd_file_path)
  qmd_result <- penguins_substitute(qmd_file_content, "penguins.qmd")
  expect_true(qmd_result$matches)
  expect_true(any(grepl(
    'starts_with\\("flipper_"\\), starts_with\\("bill_"\\)',
    qmd_result$content
  )))

  # Test with no_penguins.R file (has no penguin references)
  no_penguins_path <- file.path(fixtures_dir, "no_penguins.R")
  no_penguins_content <- readLines(no_penguins_path)
  no_penguins_result <- penguins_substitute(
    no_penguins_content,
    "no_penguins.R"
  )
  expect_false(no_penguins_result$matches)
  expect_equal(no_penguins_result$content, no_penguins_content)
})

test_that("penguins_substitute handles multiple ends_with patterns correctly", {
  # Test with multiple lines containing ends_with
  file_content <- c(
    'select(ends_with("_mm"))',
    'filter(col1, col2) |> select(ends_with("_mm"))',
    'group_by(species) |> summarize(across(ends_with("_mm"), mean))'
  )

  output_short <- "test_output.R"
  result <- penguins_substitute(file_content, output_short)

  # Check that all ends_with patterns were replaced
  expect_false(any(grepl('ends_with\\(["\']_mm["\']\\)', result$content)))
  expect_equal(
    sum(grepl(
      'starts_with\\("flipper_"\\), starts_with\\("bill_"\\)',
      result$content
    )),
    3
  )

  # There should be a message about multiple replacements
  expect_message(
    penguins_substitute(file_content, output_short),
    "lines 1, 2, 3"
  )
})

test_that("penguins_substitute handles different quote styles in library calls", {
  # Test with different quote styles in library calls
  file_content <- c(
    "library(palmerpenguins)",
    "library('palmerpenguins')",
    'library("palmerpenguins")'
  )

  output_short <- "test_output.R"
  result <- penguins_substitute(file_content, output_short)

  # Check that all library calls were removed
  expect_equal(result$content, c("", "", ""))
  expect_true(result$matches)
})

test_that("penguins_substitute handles complex mixed content", {
  # Test with complex mixed content
  file_content <- c(
    "# Loading libraries",
    "library(dplyr)",
    "library(palmerpenguins)",
    "",
    "# Analysis",
    "result <- penguins |>",
    "  filter(!is.na(bill_length_mm)) |>",
    "  group_by(species) |>",
    "  summarize(",
    "    mean_bill_length = mean(bill_length_mm),",
    "    mean_bill_depth = mean(bill_depth_mm),",
    "    mean_flipper_length = mean(flipper_length_mm),",
    "    mean_body_mass = mean(body_mass_g)",
    "  )",
    "",
    "# Visualization",
    "plot_data <- penguins |>",
    '  select(species, ends_with("_mm"), body_mass_g)'
  )

  output_short <- "test_output.R"
  result <- penguins_substitute(file_content, output_short)

  # Check that all patterns were replaced
  expect_true(result$matches)
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    result$content
  )))
  expect_false(any(grepl("bill_length_mm", result$content)))
  expect_false(any(grepl("bill_depth_mm", result$content)))
  expect_false(any(grepl("flipper_length_mm", result$content)))
  expect_false(any(grepl("body_mass_g", result$content)))
  expect_false(any(grepl('ends_with\\(["\']_mm["\']\\)', result$content)))

  # Check that other content was preserved
  expect_true(any(grepl("library\\(dplyr\\)", result$content)))
  expect_true(any(grepl("# Loading libraries", result$content)))
  expect_true(any(grepl("# Analysis", result$content)))
  expect_true(any(grepl("# Visualization", result$content)))
})


# testing penguins_convert() ---------------------------------------------
test_that("penguins_convert correctly converts file with penguins references", {
  # Create temporary files for testing
  input_file <- withr::local_tempfile(fileext = ".R")
  output_file <- withr::local_tempfile(fileext = ".R")

  # Write content with penguins references to input file
  writeLines(
    c(
      "library(palmerpenguins)",
      "data <- penguins",
      "bill_lengths <- penguins$bill_length_mm",
      "bill_depths <- penguins$bill_depth_mm",
      "flipper_lengths <- penguins$flipper_length_mm",
      "body_masses <- penguins$body_mass_g"
    ),
    input_file
  )

  # Run conversion
  result <- penguins_convert(input_file, output_file)

  # Check that the function reported changes
  expect_true(result)

  # Read output file content
  output_content <- readLines(output_file)

  # Check that library call was removed
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    output_content
  )))

  # Check that variable names were replaced
  expect_false(any(grepl("bill_length_mm", output_content)))
  expect_false(any(grepl("bill_depth_mm", output_content)))
  expect_false(any(grepl("flipper_length_mm", output_content)))
  expect_false(any(grepl("body_mass_g", output_content)))

  expect_true(any(grepl("bill_len", output_content)))
  expect_true(any(grepl("bill_dep", output_content)))
  expect_true(any(grepl("flipper_len", output_content)))
  expect_true(any(grepl("body_mass", output_content)))
})

test_that("penguins_convert returns FALSE for file without penguins references", {
  # Create temporary files for testing
  input_file <- withr::local_tempfile(fileext = ".R")
  output_file <- withr::local_tempfile(fileext = ".R")

  # Write content without penguins references
  writeLines(
    c(
      "x <- 1",
      "y <- 2",
      "z <- x + y",
      "print(z)"
    ),
    input_file
  )

  # Run conversion
  result <- penguins_convert(input_file, output_file)

  # Check that the function reported no changes
  expect_false(result)

  # Check that output content matches input content
  input_content <- readLines(input_file)
  output_content <- readLines(output_file)
  expect_equal(output_content, input_content)
})

test_that("penguins_convert works with in-place modification", {
  # Create temporary file for testing
  temp_file <- withr::local_tempfile(fileext = ".R")

  # Write content with penguins references
  writeLines(
    c(
      "library(palmerpenguins)",
      "data <- penguins$bill_length_mm"
    ),
    temp_file
  )

  # Run conversion in-place (output = NULL or same as input)
  result <- penguins_convert(temp_file, NULL)

  # Check that the function reported changes
  expect_true(result)

  # Read modified file content
  modified_content <- readLines(temp_file)

  # Check that modifications were made
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    modified_content
  )))
  expect_false(any(grepl("bill_length_mm", modified_content)))
  expect_true(any(grepl("bill_len", modified_content)))
})

test_that("penguins_convert handles Quarto documents correctly", {
  # Create temporary files for testing
  input_file <- withr::local_tempfile(fileext = ".qmd")
  output_file <- withr::local_tempfile(fileext = ".qmd")

  # Write Quarto document with penguins references
  writeLines(
    c(
      "---",
      "title: 'Penguin Analysis'",
      "---",
      "",
      "```{r}",
      "library('palmerpenguins')",
      "```",
      "",
      "```{r}",
      "penguins |>",
      "  select(body_mass_g, ends_with('_mm'))",
      "```"
    ),
    input_file
  )

  # Run conversion
  result <- penguins_convert(input_file, output_file)

  # Check that the function reported changes
  expect_true(result)

  # Read output file content
  output_content <- readLines(output_file)

  # Check front matter was preserved
  expect_equal(
    output_content[1:3],
    c("---", "title: 'Penguin Analysis'", "---")
  )

  # Check that library call was removed
  expect_false(any(grepl(
    "library\\(['\"]?palmerpenguins['\"]?\\)",
    output_content
  )))

  # Check that ends_with was replaced
  expect_false(any(grepl("ends_with\\(['\"]_mm['\"]\\)", output_content)))
  expect_true(any(grepl(
    "starts_with\\(\"flipper_\"\\), starts_with\\(\"bill_\"\\)",
    output_content
  )))

  # Check that body_mass_g was replaced
  expect_false(any(grepl("body_mass_g", output_content)))
  expect_true(any(grepl("body_mass", output_content)))
})

test_that("penguins_convert handles R Markdown documents correctly", {
  # Create temporary files for testing
  input_file <- withr::local_tempfile(fileext = ".Rmd")
  output_file <- withr::local_tempfile(fileext = ".Rmd")

  # Write R Markdown document with penguins references
  writeLines(
    c(
      "---",
      "title: 'Penguin Analysis'",
      "---",
      "",
      "```{r}",
      "library(\"palmerpenguins\")",
      "```",
      "",
      "```{r}",
      "head(penguins$bill_length_mm)",
      "head(penguins$bill_depth_mm)",
      "head(penguins$flipper_length_mm)",
      "head(penguins$body_mass_g)",
      "```"
    ),
    input_file
  )

  # Run conversion
  result <- penguins_convert(input_file, output_file)

  # Check that the function reported changes
  expect_true(result)

  # Read output file content
  output_content <- readLines(output_file)

  # Check that variable names were replaced
  expect_false(any(grepl("bill_length_mm", output_content)))
  expect_false(any(grepl("bill_depth_mm", output_content)))
  expect_false(any(grepl("flipper_length_mm", output_content)))
  expect_false(any(grepl("body_mass_g", output_content)))

  expect_true(any(grepl("bill_len", output_content)))
  expect_true(any(grepl("bill_dep", output_content)))
  expect_true(any(grepl("flipper_len", output_content)))
  expect_true(any(grepl("body_mass", output_content)))
})

test_that("penguins_convert works with fixture files", {
  # Use fixtures directory
  fixtures_dir <- test_path("fixtures", "example_dir")

  # Create temporary output files
  temp_output_r <- withr::local_tempfile(fileext = ".R")
  temp_output_qmd <- withr::local_tempfile(fileext = ".qmd")
  temp_output_rmd <- withr::local_tempfile(fileext = ".rmd")

  # Run conversion for penguins.R
  r_file <- file.path(fixtures_dir, "penguins.R")
  r_result <- penguins_convert(r_file, temp_output_r)
  expect_true(r_result)

  # Run conversion for penguins.qmd
  qmd_file <- file.path(fixtures_dir, "penguins.qmd")
  qmd_result <- penguins_convert(qmd_file, temp_output_qmd)
  expect_true(qmd_result)

  # Run conversion for penguins.rmd (in nested directory)
  rmd_file <- file.path(fixtures_dir, "nested", "penguins.rmd")
  rmd_result <- penguins_convert(rmd_file, temp_output_rmd)
  expect_true(rmd_result)

  # Also test a file without penguin references
  no_penguins_file <- file.path(fixtures_dir, "no_penguins.R")
  temp_output_no_penguins <- withr::local_tempfile(fileext = ".R")
  no_penguins_result <- penguins_convert(
    no_penguins_file,
    temp_output_no_penguins
  )
  expect_false(no_penguins_result)
})

test_that("penguins_convert handles empty files correctly", {
  # Create empty temporary files for testing
  input_file <- withr::local_tempfile(fileext = ".R")
  output_file <- withr::local_tempfile(fileext = ".R")

  # Create empty file
  file.create(input_file)

  # Run conversion
  result <- penguins_convert(input_file, output_file)

  # Check that the function reported no changes
  expect_false(result)

  # Check that output file was created but is empty
  expect_true(file.exists(output_file))
  expect_equal(readLines(output_file, warn = FALSE), character(0))
})
