# TODO: document
# TODO: test
# TODO: check file tye
base_penguins <- function(input, output = NULL) {
  # check input is a file that exists
  if (!file.exists(input)) {
    stop(paste0("The path '", input, "' does not exist."))
  }

  # check file type (.R, .qmd, .rmd, .Rmd)
  ## Other? What happens if I try readLines on, say, a csv?
  ## Actually, csv is OK, e.g. readLines(palmerpenguins::path_to_file("penguins.csv"))readLines(palmerpenguins::path_to_file("penguins.csv"))

  # set output - if NULL, overwrite input
  output <- output %||% input

  # read in the file
  file <- readLines(input)

  # remove call(s) to palmerpenguins
  file <- gsub("library\\(palmerpenguins\\)", "", file)

  # shorter variable names
  file <- file |>
    gsub("bill_length_mm", "bill_len", x = _) |>
    gsub("bill_depth_mm", "bill_dep", x = _) |>
    gsub("flipper_length_mm", "flipper_len", x = _) |>
    gsub("body_mass_g", "body_mass", x = _)

  # deal with ends_with(),
  # as in https://allisonhorst.github.io/palmerpenguins/articles/intro.html
  # use starts_with for flipper so that code doesn't error if col not present
  file <- file |>
    gsub(
      'ends_with\\("_mm"\\)',
      'starts_with("flipper_"), starts_with("bill_")',
      x = _
    )

  # write output
  writeLines(file, output)
}

penguins_examples <- function(path = NULL) {
  if (is.null(path)) {
    system.file("extdata", package = "basepenguins")
  } else {
    system.file("extdata", path, package = "basepenguins", mustWork = TRUE)
  }
}

files_to_convert <- function(dir) {
  paths <- list.files(dir, recursive = TRUE)
  paths[tools::file_ext(paths) %in% c("R", "Rmd", "rmd", "qmd")]
}
