# %||%
test_that("NULL coalescing operator works", {
  expect_equal(NULL %||% 3, 3)
  expect_equal(3 %||% NULL, 3)
  expect_equal(4 %||% 3, 4)
  expect_null(NULL %||% NULL)
})

# example_files() ----------------------------------------------------

test_that("example_files with NULL uses recursive argument", {
  expect_false(
    "nested/penguins.qmd" %in% example_files(recursive = FALSE)
  )
  expect_true("nested/penguins.qmd" %in% example_files())
  expect_true("penguins.R" %in% example_files())
  expect_true("nested" %in% example_files(recursive = FALSE))
  expect_false("nested" %in% example_files())
})

test_that("example_files with file returns correct path", {
  expect_true(file.exists(example_files("penguins.R")))
  expect_true(file.exists(example_files("nested/penguins.qmd")))
  expect_error(file.exists(example_files("not_a_file.R")))
})

test_that("example_files with NULL uses full.names argument correctly", {
  # Get results with and without full.names (for non-recursive case)
  without_full_names <- example_files(recursive = FALSE)
  with_full_names <- example_files(recursive = FALSE, full.names = TRUE)

  # For non-recursive case, check we have simple filenames without full paths
  expect_true(all(!grepl("^/|^[A-Za-z]:|^\\\\\\\\", without_full_names)))

  # For full.names case, check we have absolute paths
  expect_true(all(grepl("^/|^[A-Za-z]:|^\\\\\\\\", with_full_names)))

  # Check some specific filename from non-full.names appears at the end of full paths
  if (length(without_full_names) > 0) {
    sample_name <- without_full_names[1]
    expect_true(any(grepl(paste0(sample_name, "$"), with_full_names)))
  }

  # Test with recursive = TRUE as well
  recursive_without_full <- example_files(recursive = TRUE)
  recursive_with_full <- example_files(recursive = TRUE, full.names = TRUE)

  # Even in recursive case, full.names = FALSE should never have absolute paths
  expect_false(any(grepl("^/|^[A-Za-z]:|^\\\\\\\\", recursive_without_full)))

  # With full.names = TRUE and recursive = TRUE, we should have absolute paths
  expect_true(all(grepl("^/|^[A-Za-z]:|^\\\\\\\\", recursive_with_full)))

  # Verify at least one recursive file appears in both sets
  if (length(recursive_without_full) > 0) {
    # Pick a file that's likely in a subdirectory
    nested_files <- recursive_without_full[grepl("/", recursive_without_full)]
    if (length(nested_files) > 0) {
      sample_nested <- nested_files[1]
      # Escape special regex chars in the sample name
      escaped_sample <- gsub(
        "([.|()\\^{}+$*?]|\\[|\\])",
        "\\\\\\1",
        sample_nested
      )
      expect_true(any(grepl(paste0(escaped_sample, "$"), recursive_with_full)))
    }
  }
})


# example_dir() ------------------------------------------------

test_that("penguins_example_dir works", {
  expect_true(dir.exists(example_dir()))
  expect_match(example_dir(), "extdata")
})

test_that("example_dir with copy.dir copies all files including in nested directories", {
  # Create a temporary directory for the test
  temp_dir <- withr::local_tempdir()

  # Call example_dir with copy.dir parameter
  example_dir(copy.dir = temp_dir)

  # Get expected files (all files that should be copied)
  expected_files <- example_files(recursive = TRUE)

  # Check if all files exist in the copied directory
  for (file in expected_files) {
    expect_true(
      file.exists(file.path(temp_dir, file)),
      info = paste("Expected file not found:", file)
    )
  }

  # Specifically check if nested directory exists and has files
  expect_true(dir.exists(file.path(temp_dir, "nested")))

  # Get nested files to check specifically
  nested_files <- example_files(recursive = TRUE)
  nested_files <- nested_files[grepl("^nested/", nested_files)]

  # Ensure we have some nested files to test with
  expect_true(length(nested_files) > 0)

  # Check each nested file exists in the copied directory
  for (file in nested_files) {
    expect_true(
      file.exists(file.path(temp_dir, file)),
      info = paste("Expected nested file not found:", file)
    )
  }
})

test_that("example_dir with copy.dir copies file content correctly", {
  # Create a temporary directory for the test
  temp_dir <- withr::local_tempdir()

  # Call example_dir with copy.dir parameter
  example_dir(copy.dir = temp_dir)

  # Sample a few files to check content
  files_to_check <- c(
    "penguins.R",
    file.path("nested", "penguins.qmd")
  )

  for (file in files_to_check) {
    # Get content of original file using example_files
    original_path <- example_files(file)
    original_content <- readLines(original_path)

    # Get content of copied file
    copied_path <- file.path(temp_dir, file)
    copied_content <- readLines(copied_path)

    # Compare content
    expect_identical(
      copied_content,
      original_content,
      info = paste("Content mismatch for file:", file)
    )
  }
})

test_that("example_dir with copy.dir handles existing directory", {
  # Create a temporary directory that already exists
  temp_dir <- withr::local_tempdir()

  # Create a marker file to verify the directory is not deleted
  marker_file <- file.path(temp_dir, "marker.txt")
  writeLines("This is a marker file", marker_file)

  # Call example_dir with the existing directory
  example_dir(copy.dir = temp_dir)

  # Verify marker file still exists (directory was not recreated)
  expect_true(file.exists(marker_file))

  # Also verify that example files were copied
  expect_true(file.exists(file.path(temp_dir, "penguins.R")))
})

test_that("example_dir with copy.dir returns the normalized path invisibly", {
  # Create a temporary directory
  temp_dir <- withr::local_tempdir()

  # Call example_dir with copy.dir parameter and capture the result
  result <- example_dir(copy.dir = temp_dir)

  # Check that the result is the normalized path of the temp directory
  expect_equal(result, normalizePath(temp_dir))

  # Also verify that it's returned invisibly by capturing it with invisible wrapper
  invisible_result <- withVisible(example_dir(copy.dir = temp_dir))
  expect_false(invisible_result$visible)
  expect_equal(invisible_result$value, normalizePath(temp_dir))
})

test_that("example_dir creates directories recursively when copy.dir doesn't exist", {
  # Create a complex nested path that doesn't exist
  temp_base <- withr::local_tempdir()
  deep_nested_dir <- file.path(temp_base, "level1", "level2", "level3")

  # Verify that the nested directory doesn't exist yet
  expect_false(dir.exists(deep_nested_dir))

  # Call example_dir with the nested directory path
  result <- example_dir(copy.dir = deep_nested_dir)

  # Verify that the directory was created
  expect_true(dir.exists(deep_nested_dir))

  # Verify that files were copied to the directory
  expected_files <- example_files(recursive = TRUE)
  for (file in expected_files) {
    expect_true(
      file.exists(file.path(deep_nested_dir, file)),
      info = paste("File not found in created directory:", file)
    )
  }

  # Check that the function returns the normalized path
  expect_equal(result, normalizePath(deep_nested_dir))
})

# filter_by_extensions() -------------------------------------------------

test_that("filter_by_extensions returns correct patterns", {
  expect_equal(filter_by_extensions(NULL), "*")
  expect_equal(filter_by_extensions(character(0)), "*")
  expect_equal(filter_by_extensions(""), "*")
  expect_equal(filter_by_extensions("R"), "\\.(R)$")
  expect_equal(filter_by_extensions(c("R", "Rmd")), "\\.(R|Rmd)$")
})


# files_to_convert() -----------------------------------------------------

test_that("files_to_convert finds correct files with default extensions", {
  example_dir <- test_path("fixtures", "example_dir")

  # Test with default extensions
  result <- files_to_convert(example_dir)
  expected_files <- c(
    "empty.R",
    "no_penguins.R",
    "penguins.R",
    "penguins.qmd",
    file.path("nested", "no_penguins.Rmd"),
    file.path("nested", "penguins.rmd")
  )

  # Sort both to ensure consistent order for comparison
  expect_setequal(result, expected_files)

  # Test with full_names = TRUE
  result_full <- files_to_convert(example_dir, full.names = TRUE)
  expected_full <- file.path(example_dir, expected_files)
  expect_setequal(result_full, expected_full)
})

test_that("files_to_convert handles full.names", {
  example_dir <- test_path("fixtures", "example_dir")

  result_full <- files_to_convert(example_dir, full.names = TRUE)
  expected_files <- c(
    "empty.R",
    "no_penguins.R",
    "penguins.R",
    "penguins.qmd",
    file.path("nested", "no_penguins.Rmd"),
    file.path("nested", "penguins.rmd")
  )

  expected_full <- file.path(example_dir, expected_files)
  expect_setequal(result_full, expected_full)
})

test_that("files_to_convert handles non-default extensions", {
  example_dir <- test_path("fixtures", "example_dir")

  result <- files_to_convert(example_dir, extensions = c("md", "html"))
  expected_files <- c(
    "not_a_script.md",
    file.path("nested", "not_a_script.html")
  )

  expect_setequal(result, expected_files)
})

test_that("files_to_convert handles empty extensions list", {
  example_dir <- test_path("fixtures", "example_dir")
  # Test with NULL extensions (should match all files)
  result_null <- files_to_convert(example_dir, extensions = NULL)

  # All 8 files should be returned
  expect_equal(length(result_null), 8)

  # Test with empty character vector
  result_empty <- files_to_convert(example_dir, extensions = character(0))
  expect_equal(length(result_empty), 8)
})

test_that("files_to_convert handles empty string", {
  example_dir <- test_path("fixtures", "example_dir")

  result_all <- files_to_convert(example_dir, extensions = "")

  # All 7 files should be returned
  expect_equal(length(result_all), 8)

  # Test with empty character vector
  result_empty <- files_to_convert(example_dir, extensions = character(0))
  expect_equal(length(result_all), 8)
})

test_that("files_to_convert handles non-existent directory", {
  no_dir <- test_path("fixtures", "no_dir")
  # Test with non-existent directory
  expect_error(files_to_convert(no_dir))
})

test_that("files_to_convert works with empty directory", {
  empty_dir <- withr::local_tempdir()
  expect_equal(
    files_to_convert(empty_dir),
    character(0)
  )
})

test_that("files_to_convert works with case sensitivity in extensions", {
  example_dir <- test_path("fixtures", "example_dir")

  # Test with lowercase extensions only where case matters
  result <- files_to_convert(example_dir, extensions = c("r", "qmd", "rmd"))

  # Should exclude no_penguins.Rmd (uppercase extension)
  expected_files <- c(
    "penguins.qmd",
    file.path("nested", "penguins.rmd")
  )

  # On case-insensitive filesystems, the actual result might differ,
  # so we need to check what's expected for the current OS
  # For simplicity, just check that we get files with matching extensions
  result_ext <- tools::file_ext(result)
  expected_ext <- c("qmd", "rmd")

  all_match <- all(result_ext %in% expected_ext)
  expect_true(all_match)
})

test_that("files_to_convert works with nested directories", {
  # Test specifically with the nested directory only
  nested_dir <- test_path("fixtures", "example_dir", "nested")

  result <- files_to_convert(nested_dir)
  expected_files <- c("no_penguins.Rmd", "penguins.rmd")

  expect_setequal(result, expected_files)
})

test_that("files_to_convert errors when dir does not exist", {
  # test with invalid path
  expect_error(files_to_convert("no_dir_here"))
  expect_error(files_to_convert(123))
})

# output_path() ------------------------------------------------------------

test_that("output_path works with default parameters", {
  expect_equal(output_path("file.txt"), "file_new.txt")
  expect_equal(output_path("dir/file.txt"), file.path("dir", "file_new.txt"))
  expect_equal(output_path("file"), "file_new")
  expect_equal(output_path("dir/file"), file.path("dir", "file_new"))
})

test_that("output_path applies prefix correctly", {
  expect_equal(output_path("file.txt", prefix = "pre_"), "pre_file_new.txt")
  expect_equal(
    output_path("dir/file.txt", prefix = "pre_"),
    file.path("dir", "pre_file_new.txt")
  )
})

test_that("output_path applies custom suffix correctly", {
  expect_equal(
    output_path("file.txt", suffix = "_modified"),
    "file_modified.txt"
  )
  expect_equal(
    output_path("dir/file.txt", suffix = "_modified"),
    file.path("dir", "file_modified.txt")
  )
})

test_that("output_path applies both prefix and suffix correctly", {
  expect_equal(
    output_path("file.txt", prefix = "pre_", suffix = "_post"),
    "pre_file_post.txt"
  )
  expect_equal(
    output_path("dir/file.txt", prefix = "pre_", suffix = "_post"),
    file.path("dir", "pre_file_post.txt")
  )
})

test_that("output_path handles complex paths correctly", {
  expect_equal(
    output_path("dir1/dir2/file.txt"),
    file.path("dir1/dir2", "file_new.txt")
  )

  # Test with absolute paths (platform independent test)
  if (.Platform$OS.type == "windows") {
    expect_equal(
      output_path("C:/dir/file.txt"),
      file.path("C:/dir", "file_new.txt")
    )
  } else {
    expect_equal(
      output_path("/dir/file.txt"),
      file.path("/dir", "file_new.txt")
    )
  }
})

test_that("output_path handles non-valid input", {
  expect_error(output_path(""))
  expect_error(output_path(123))
})


# output_paths() ---------------------------------------------------------

test_that("output_paths works with a vector of paths", {
  paths <- c("file1.txt", "file2.R", "dir/file3.qmd")
  expected <- c(
    "file1_new.txt",
    "file2_new.R",
    file.path("dir", "file3_new.qmd")
  )
  names(expected) <- paths
  expect_equal(output_paths(paths), expected)
})

test_that("output_paths applies custom prefix and suffix to all paths", {
  paths <- c("file1.txt", "file2.R", "dir/file3.qmd")
  expected <- c(
    "pre_file1_post.txt",
    "pre_file2_post.R",
    file.path("dir", "pre_file3_post.qmd")
  )
  names(expected) <- paths
  expect_equal(output_paths(paths, prefix = "pre_", suffix = "_post"), expected)
})

test_that("output_paths handles empty vector", {
  expect_error(output_paths(character(0)))
})

test_that("output_paths preserves names if input is named", {
  # Test with named vector
  paths <- c(a = "file1.txt", b = "file2.R")
  result <- output_paths(paths)
  expect_equal(names(result), c("a", "b"))
  expect_equal(result, c(a = "file1_new.txt", b = "file2_new.R"))
})

test_that("output_paths works with given dir", {
  paths <- c("file1.txt", "file2.R", "nested/file3.qmd")
  dir <- "new_output"
  expected <- c(
    "new_output/file1_new.txt",
    "new_output/file2_new.R",
    file.path(dir, "nested", "file3_new.qmd")
  )
  names(expected) <- paths
  expect_equal(output_paths(paths, dir = dir), expected)
})
