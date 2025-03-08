# TODO: write this function
# TODO: document
# TODO: test
# output = NULL: overwrite
# output = "something": check length(output) == length(input)
# then loop over both lists with convert_file()
convert_files <- function(input, output = NULL) {
  NULL
}

# TODO: write this function
# TODO: document
# TODO: test
# output = NULL: overwrite
# output = "something": create new dir "something" and leave all filenames the same
convert_dir <- function(input, output = NULL) {
  NULL
}

# TODO: document
# TODO: test
# TODO: check file type
# TODO: non-exported function for ONE file
convert_file <- function(input, output = NULL) {
  input <- normalizePath(input, mustWork = TRUE) # this will check that path exists

  # check input is a file that exists
  # if (!file.exists(input)) {
  #   stop(paste0("The path '", input, "' does not exist."))
  # }

  # check file type (.R, .qmd, .rmd, .Rmd)
  ## Other? What happens if I try readLines on, say, a csv?
  ## Actually, csv is OK, e.g. readLines(palmerpenguins::path_to_file("penguins.csv"))

  # set output - if NULL, overwrite input
  output <- output %||% input
  # Need to create the output file first before normalizing the path?
  #output <- normalizePath(output)

  # read in the file
  file <- readLines(input)

  # remove call(s) to palmerpenguins
  file <- file |>
    #gsub("library\\(palmerpenguins\\)", "", x = _) #|>
    #gsub("library\\(\\'palmerpenguins\\'\\)", "", x = _) |>
    #gsub('library\\(\\"palmerpenguins\\"\\)', "", x = _)

    # can remove pipe if just using this line
    gsub("library\\(['\"]?palmerpenguins['\"]?\\)", "", x = _)

  # shorter variable names
  file <- file |>
    gsub("bill_length_mm", "bill_len", x = _, fixed = TRUE) |>
    gsub("bill_depth_mm", "bill_dep", x = _, fixed = TRUE) |>
    gsub("flipper_length_mm", "flipper_len", x = _, fixed = TRUE) |>
    gsub("body_mass_g", "body_mass", x = _, fixed = TRUE)

  # deal with ends_with(),
  # as in https://allisonhorst.github.io/palmerpenguins/articles/intro.html
  # use starts_with for flipper so that code doesn't error if col not present

  file <- file |>
    gsub(
      'ends_with(["\']_mm["\'])',
      'starts_with("flipper_"), starts_with("bill_")',
      x = _
    )

  # does `ends_with("_mm")` exist in script?
  # if so, message about the substitution
  # maybe with line number(s) - could use which() to find them
  # if in a wrapper, make sure we're saying which file!

  # write output
  writeLines(file, output)

  # need to track if any substitutions made,
  # i.e. any of the patterns found

  # if qmd or r/Rmd, message reminding to re-render or re-knit

  # not output, but changed files?
  # in wrapper
  # or list of two vectors, one of changed files and one not changed
  # so this should return an indicator of whether file changed or not
  invisible(output)
}
