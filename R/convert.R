# TODO: document
# TODO: test
convert_files <- function(input, output = NULL) {
  output <- output %||% input

  if (length(output) != length(input)) {
    stop("`output` must be the same length as `input` (or NULL)")
  }

  # limit input and output to R/qmd/rmd/Rmd
  convertible <- tools::file_ext(input) %in% c("R", "qmd", "rmd", "Rmd")
  input <- input[convertible]
  output <- output[convertible]

  converted <- mapply(penguins_gsub, input, output)
  changed <- output[converted]
  not_changed <- output[!converted]

  if (length(changed) > 0) {
    message(
      "- Please check the changed output files and remember to re-knit or re-render any Rmarkdown or quarto documents."
    )
  }

  invisible(list(changed = changed, not_changed = not_changed))
}

# TODO: write this function
# TODO: document
# TODO: test
# Converts all R/qmd/rmd/Rmd files in a directory
#
# @param input Path to the input directory containing files to convert
# @param output Optional path to output directory. If NULL, files are modified in place
# @return Invisible list with changed and unchanged files
convert_dir <- function(input, output = NULL) {
  if (!dir.exists(input)) {
    stop("`input` must be a directory that exists")
  }

  # Find all convertible files in the input directory
  relative_files <- files_to_convert(input)
  if (length(relative_files) == 0) {
    stop("There are no .R, .qmd, .rmd or .Rmd files in `input` to convert")
  }
  input_files <- file.path(input, relative_files)

  if (is.null(output)) {
    # If no output directory specified, convert files in place
    result <- convert_files(input_files)
  } else {
    # Create output directory if it doesn't exist
    if (!dir.exists(output)) {
      dir.create(output, recursive = TRUE)
    }

    # Create output paths maintaining the same relative structure
    output_files <- file.path(output, relative_files)

    # Create necessary subdirectories in output
    output_dirs <- unique(dirname(output_files))
    for (dir in output_dirs) {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
    }

    # Convert files from input to output paths
    result <- convert_files(input_files, output_files)
  }

  return(result)
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

  matches <- any(grepl(pattern, file))

  # If any of the patterns appear in the file, make substitutions
  if (matches) {
    #writeLines(file, output)
    #invisible(FALSE)
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
          '- In ',
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
  }

  # write output
  writeLines(file, output)

  # return logical of whether file has changed
  matches
}
