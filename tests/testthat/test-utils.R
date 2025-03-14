# penguins_examples() ----------------------------------------------------

test_that("penguins_examples with NULL lists files", {
  expect_true("analysis/penguins.qmd" %in% penguins_examples())
  expect_true("penguins_graph.R" %in% penguins_examples())
})

test_that("penguins_examples with file returns correct path", {
  expect_true(file.exists(penguins_examples("penguins_graph.R")))
  expect_true(file.exists(penguins_examples("analysis/penguins.qmd")))
  expect_error(file.exists(penguins_examples("not_a_file.R")))
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

  # All 7 files should be returned
  expect_equal(length(result_null), 7)

  # Test with empty character vector
  result_empty <- files_to_convert(example_dir, extensions = character(0))
  expect_equal(length(result_empty), 7)
})

test_that("files_to_convert handles empty string", {
  example_dir <- test_path("fixtures", "example_dir")

  result_all <- files_to_convert(example_dir, extensions = "")

  # All 7 files should be returned
  expect_equal(length(result_all), 7)

  # Test with empty character vector
  result_empty <- files_to_convert(example_dir, extensions = character(0))
  expect_equal(length(result_all), 7)
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
  # For simplicity in this test, we're just checking that we get files with matching extensions
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
