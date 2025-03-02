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
    gsub("gth_mm", "", x = _) |>
    gsub("th_mm", "", x = _) |>
    gsub("_g", "", x = _)

  # write output
  writeLines(file, output)
}
