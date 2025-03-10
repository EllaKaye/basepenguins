# TODO: write this function
# TODO: document
# TODO: test
# output = NULL: overwrite
# output = "something": check length(output) == length(input)
# then loop over both lists with convert_file()
# if qmd or r/Rmd, message reminding to re-render or re-knit
convert_files <- function(input, output = NULL) {
  NULL
}

# TODO: write this function
# TODO: document
# TODO: test
# output = NULL: overwrite
# output = "something": create new dir "something" and leave all filenames the same
# if qmd or r/Rmd, message reminding to re-render or re-knit
convert_dir <- function(input, output = NULL) {
  NULL
}

# TODO: document
# TODO: test
penguins_gsub <- function(input, output = NULL) {
  if (length(input) != 1) {
    stop("`input` must be a single character string (a path)")
  }

  if (!is.null(output) && length(output) != 1) {
    stop("`output` must be a single character string (a path)")
  }

  output_short <- output %||% input # keep non-normalised for display
  input <- normalizePath(input, mustWork = TRUE) # will check path exists

  # check file type (.R, .qmd, .rmd, .Rmd)
  if (!(tools::file_ext(input) %in% c("R", "qmd", "rmd", "Rmd"))) {
    stop("`input` must be a .R, .qmd, .rmd or .Rmd file")
  }

  # set output - if NULL, overwrite input
  output <- output %||% input
  # Need to create the output file first before normalizing the path
  if (!file.exists(output)) {
    file.create(output)
  }
  output <- normalizePath(output)

  # read in the file
  file <- readLines(input)

  # patterns to look for
  pp <- "library\\(['\"]?palmerpenguins['\"]?\\)" # may have "" or '' around it
  bl <- "bill_length_mm"
  bd <- "bill_depth_mm"
  fl <- "flipper_length_mm"
  bm <- "body_mass_g"
  ew <- 'ends_with\\(["\']_mm["\']\\)'

  patterns <- c(pp, bl, bd, fl, bm, ew)
  pattern <- paste0(patterns, collapse = "|")

  # Do any of the patterns appear in the file?
  # If not, return
  if (!any(grepl(pattern, file))) {
    writeLines(file, output)
    invisible(FALSE)
  }

  # remove call(s) to palmerpenguins
  # palmerpenguins
  file <- gsub(pp, "", file)

  # shorter variable names
  file <- file |>
    gsub(bl, "bill_len", x = _, fixed = TRUE) |>
    gsub(bd, "bill_dep", x = _, fixed = TRUE) |>
    gsub(fl, "flipper_len", x = _, fixed = TRUE) |>
    gsub(bm, "body_mass", x = _, fixed = TRUE)

  # does `ends_with("_mm")` exist in script?
  # if so, message about the substitution with line numbers
  ew_matches <- grep(ew, file)
  ew_matches_str <- paste0(ew_matches, collapse = ", ")
  cond <- length(ew_matches) == 1
  lns <- ifelse(cond, "line", "lines")
  subs <- ifelse(cond, "subsitution is", "substitutions are")

  if (length(ew_matches) > 0) {
    message(
      paste0(
        'In ',
        output_short,
        ', ends_with("_mm") replaced on ',
        lns,
        " ",
        ew_matches_str,
        " - ",
        "please check that the ",
        subs,
        " appropriate."
      )
    )
  }

  # deal with ends_with(),
  # as in https://allisonhorst.github.io/palmerpenguins/articles/intro.html
  # use starts_with for flipper so that code doesn't error if col not present
  file <- file |>
    gsub(
      ew,
      'starts_with("flipper_"), starts_with("bill_")',
      x = _
    )

  # write output
  writeLines(file, output)

  # indicate that file has changed
  invisible(TRUE)
}
