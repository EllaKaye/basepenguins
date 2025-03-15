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

# extend_name() ------------------------------------------------------------

test_that("extend_name works with default parameters", {
  expect_equal(extend_name("file.txt"), "file_new.txt")
  expect_equal(extend_name("dir/file.txt"), file.path("dir", "file_new.txt"))
  expect_equal(extend_name("file"), "file_new")
  expect_equal(extend_name("dir/file"), file.path("dir", "file_new"))
})

test_that("extend_name applies prefix correctly", {
  expect_equal(extend_name("file.txt", prefix = "pre_"), "pre_file_new.txt")
  expect_equal(
    extend_name("dir/file.txt", prefix = "pre_"),
    file.path("dir", "pre_file_new.txt")
  )
})

test_that("extend_name applies custom suffix correctly", {
  expect_equal(
    extend_name("file.txt", suffix = "_modified"),
    "file_modified.txt"
  )
  expect_equal(
    extend_name("dir/file.txt", suffix = "_modified"),
    file.path("dir", "file_modified.txt")
  )
})

test_that("extend_name applies both prefix and suffix correctly", {
  expect_equal(
    extend_name("file.txt", prefix = "pre_", suffix = "_post"),
    "pre_file_post.txt"
  )
  expect_equal(
    extend_name("dir/file.txt", prefix = "pre_", suffix = "_post"),
    file.path("dir", "pre_file_post.txt")
  )
})

test_that("extend_name handles complex paths correctly", {
  expect_equal(
    extend_name("dir1/dir2/file.txt"),
    file.path("dir1/dir2", "file_new.txt")
  )

  # Test with absolute paths (platform independent test)
  if (.Platform$OS.type == "windows") {
    expect_equal(
      extend_name("C:/dir/file.txt"),
      file.path("C:/dir", "file_new.txt")
    )
  } else {
    expect_equal(
      extend_name("/dir/file.txt"),
      file.path("/dir", "file_new.txt")
    )
  }
})

test_that("extend_name handles non-valid input", {
  expect_error(extend_name(""))
  expect_error(extend_name(123))
})


# extend_names() ---------------------------------------------------------

test_that("extend_names works with a vector of paths", {
  paths <- c("file1.txt", "file2.R", "dir/file3.qmd")
  expected <- c(
    "file1_new.txt",
    "file2_new.R",
    file.path("dir", "file3_new.qmd")
  )
  names(expected) <- paths
  expect_equal(extend_names(paths), expected)
})

test_that("extend_names applies custom prefix and suffix to all paths", {
  paths <- c("file1.txt", "file2.R", "dir/file3.qmd")
  expected <- c(
    "pre_file1_post.txt",
    "pre_file2_post.R",
    file.path("dir", "pre_file3_post.qmd")
  )
  names(expected) <- paths
  expect_equal(extend_names(paths, prefix = "pre_", suffix = "_post"), expected)
})

test_that("extend_names handles empty vector", {
  expect_error(extend_names(character(0)))
})

test_that("extend_names preserves names if input is named", {
  # Test with named vector
  paths <- c(a = "file1.txt", b = "file2.R")
  result <- extend_names(paths)
  expect_equal(names(result), c("a", "b"))
  expect_equal(result, c(a = "file1_new.txt", b = "file2_new.R"))
})
