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
  # shorter variable names
  # write output

  file
}
