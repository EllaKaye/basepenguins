# TODO: document
# TODO: test
base_penguins <- function(input, output = NULL) {
  # check input is a file that exists
  if (!file.exists(input)) {
    stop(paste0("The path '", input, "' does not exist."))
  }

  # set output - if NULL, overwrite input
  output <- output %||% input

  # read in the file
  file <- readLines(input)

  # remove call to palmerpenguins
  file <- sub("library\\(palmerpenguins\\)", "", file)

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
