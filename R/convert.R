# TODO: document
# TODO: test
convert_files <- function(
  input,
  output,
  extensions = c("R", "qmd", "rmd", "Rmd")
) {
  overwrite <- (is.null(output) || identical(input, output))

  output <- output %||% input

  if (length(output) != length(input)) {
    stop("`output` must be the same length as `input` (or NULL)")
  }

  # only want to touch scripts, but still need to keep track of other files
  convertible <- tools::file_ext(input) %in% extensions
  not_convertible <- output[!convertible]
  names(not_convertible) <- input[!convertible]

  # need a file for each output, so still copy not_convertible files,
  # but don't need to overwrite with something identical
  # Create output directories for non-convertible files
  if (!overwrite && length(not_convertible) > 0) {
    # Ensure output directories exist for non-convertible files
    for (file_path in not_convertible) {
      dir_path <- dirname(file_path)
      if (!dir.exists(dir_path) && dir_path != ".") {
        dir.create(dir_path, recursive = TRUE)
      }
    }

    # Copy non-convertible files
    file.copy(input[!convertible], not_convertible)
  }

  # substitutions on convertible files
  convertible_input <- input[convertible]
  convertible_output <- output[convertible]
  converted <- mapply(penguins_gsub, convertible_input, convertible_output)
  changed <- convertible_output[converted]
  not_changed <- c(convertible_output[!converted], not_convertible)

  if (length(changed) > 0) {
    message(
      "- Please check the changed output files and remember to re-knit or re-render any Rmarkdown or quarto documents."
    )
  }

  invisible(list(changed = changed, not_changed = not_changed))
}

# TODO: document
# TODO: test
#
# @param input Path to the input directory containing files to convert
# @param output Optional path to output directory. If NULL, files are modified in place
# @return Invisible list with changed and unchanged files
# TODO: return named vectors (names are input)
# TODO: argument to control which extensions are considered?
convert_dir <- function(
  input,
  output,
  extensions = c("R", "qmd", "rmd", "Rmd")
) {
  if (!dir.exists(input)) {
    stop("`input` must be a directory that exists")
  }

  # Get all files from input directory then determine which are convertible
  file_names <- list.files(input, recursive = TRUE)

  if (length(file_names) == 0) {
    stop("There are no files in `input` to convert")
  }

  input_files <- file.path(input, file_names)

  if (is.null(output)) {
    # If no output directory specified, convert files in place
    result <- convert_files(input_files, NULL, extensions)
  } else {
    # Create output directory if it doesn't exist
    if (!dir.exists(output)) {
      dir.create(output, recursive = TRUE)
    }

    # Create output paths maintaining the same relative structure
    output_files <- file.path(output, file_names)

    # Create necessary subdirectories in output
    output_dirs <- unique(dirname(output_files))
    for (dir in output_dirs) {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
    }

    # Convert files from input to output paths
    result <- convert_files(input_files, output_files, extensions)
  }

  invisible(result)
}

# TODO: document
# TODO: test
# TODO: put input validation in a separate fuction
# penguins_gsub <- function(
#   input,
#   output,
#   extensions = c("R", "qmd", "rmd", "Rmd")
# ) {
#   if (length(input) != 1) {
#     stop("`input` must be a single character string (a path)")
#   }

#   if (!is.null(output) && length(output) != 1) {
#     stop("`output` must be a single character string (a path)")
#   }

#   output_short <- output %||% input # keep non-normalised for display
#   input <- normalizePath(input, mustWork = TRUE) # will check path exists

#   # check file type
#   if (!(tools::file_ext(input) %in% extensions)) {
#     stop("`input` does not have a valid file extension")
#   }

#   # set output, full paths - if NULL, overwrite input
#   output <- output %||% input
#   # Need to create the output file first before normalizing the path
#   if (!file.exists(output)) {
#     file.create(output)
#   }
#   output <- normalizePath(output)

#   # read in the file
#   file <- readLines(input)

#   # patterns to look for
#   pp <- "library\\(['\"]?palmerpenguins['\"]?\\)" # may have "" or '' around it
#   bl <- "bill_length_mm"
#   bd <- "bill_depth_mm"
#   fl <- "flipper_length_mm"
#   bm <- "body_mass_g"
#   ew <- 'ends_with\\(["\']_mm["\']\\)'

#   patterns <- c(pp, bl, bd, fl, bm, ew)
#   pattern <- paste0(patterns, collapse = "|")

#   matches <- any(grepl(pattern, file))

#   # If any of the patterns appear in the file, make substitutions
#   # if (!matches) {
#   #writeLines(file, output)
#   #invisible(FALSE)
#   # } feels like this would be better for shorter `if` section
#   # TODO: try return(invisible(matches))
#   # but check because I tried this before and it didn't seem to work properly
#   if (matches) {
#     # remove call(s) to palmerpenguins
#     # palmerpenguins
#     file <- gsub(pp, "", file)

#     # shorter variable names
#     file <- file |>
#       gsub(bl, "bill_len", x = _, fixed = TRUE) |>
#       gsub(bd, "bill_dep", x = _, fixed = TRUE) |>
#       gsub(fl, "flipper_len", x = _, fixed = TRUE) |>
#       gsub(bm, "body_mass", x = _, fixed = TRUE)

#     # does `ends_with("_mm")` exist in script?
#     # if so, message about the substitution with line numbers
#     ew_matches <- grep(ew, file)
#     ew_matches_str <- paste0(ew_matches, collapse = ", ")
#     cond <- length(ew_matches) == 1
#     lns <- ifelse(cond, "line", "lines")
#     subs <- ifelse(cond, "subsitution is", "substitutions are")

#     if (length(ew_matches) > 0) {
#       message(
#         paste0(
#           '- In ',
#           output_short,
#           ', ends_with("_mm") replaced on ',
#           lns,
#           " ",
#           ew_matches_str,
#           " - ",
#           "please check that the ",
#           subs,
#           " appropriate."
#         )
#       )
#     }

#     # deal with ends_with(),
#     # as in https://allisonhorst.github.io/palmerpenguins/articles/intro.html
#     # use starts_with for flipper so that code doesn't error if col not present
#     file <- file |>
#       gsub(
#         ew,
#         'starts_with("flipper_"), starts_with("bill_")',
#         x = _
#       )
#   }

#   # write output
#   writeLines(file, output)

#   # return logical of whether file has changed
#   matches
# }

# Input validation function
validate_penguins_input <- function(
  input,
  output,
  extensions = c("R", "qmd", "rmd", "Rmd")
) {
  if (length(input) != 1) {
    stop("`input` must be a single character string (a path)")
  }

  if (!is.null(output) && length(output) != 1) {
    stop("`output` must be a single character string (a path)")
  }

  # Normalize input path and check if it exists
  input <- normalizePath(input, mustWork = TRUE)

  # Check file type (.R, .qmd, .rmd, .Rmd)
  if (!(tools::file_ext(input) %in% extensions)) {
    #     stop("`input` does not have a valid file extension")
  }

  # Set output path - if NULL, use input path
  output_short <- output %||% input # keep non-normalized for display
  output <- output %||% input

  # Create parent directory if it doesn't exist
  output_dir <- dirname(output)
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create output file if it doesn't exist
  if (!file.exists(output)) {
    file.create(output)
  }

  # Normalize output path
  output <- normalizePath(output)

  # Return validated paths
  list(
    input = input,
    output = output,
    output_short = output_short
  )
}

# Main substitution function
penguins_substitute <- function(file_content, output_short) {
  # Define patterns to look for
  pp <- "library\\(['\"]?palmerpenguins['\"]?\\)" # may have "" or '' around it
  bl <- "bill_length_mm"
  bd <- "bill_depth_mm"
  fl <- "flipper_length_mm"
  bm <- "body_mass_g"
  ew <- 'ends_with\\(["\']_mm["\']\\)'

  patterns <- c(pp, bl, bd, fl, bm, ew)
  pattern <- paste0(patterns, collapse = "|")

  # Check if any patterns match
  matches <- any(grepl(pattern, file_content))

  # If matches found, perform substitutions
  if (matches) {
    # Remove call(s) to palmerpenguins library
    file_content <- gsub(pp, "", file_content)

    # Replace variable names with shorter versions
    file_content <- file_content |>
      gsub(bl, "bill_len", x = _, fixed = TRUE) |>
      gsub(bd, "bill_dep", x = _, fixed = TRUE) |>
      gsub(fl, "flipper_len", x = _, fixed = TRUE) |>
      gsub(bm, "body_mass", x = _, fixed = TRUE)

    # Check for ends_with("_mm") pattern
    ew_matches <- grep(ew, file_content)

    if (length(ew_matches) > 0) {
      # Format message about substitutions
      ew_matches_str <- paste0(ew_matches, collapse = ", ")
      cond <- length(ew_matches) == 1
      lns <- ifelse(cond, "line", "lines")
      subs <- ifelse(cond, "subsitution is", "substitutions are")

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

    # Replace ends_with() pattern
    file_content <- file_content |>
      gsub(
        ew,
        'starts_with("flipper_"), starts_with("bill_")',
        x = _
      )
  }

  # Return modified content and match status
  list(
    content = file_content,
    matches = matches
  )
}

# Main function combining validation and substitution
penguins_gsub <- function(input, output) {
  # Validate inputs
  paths <- validate_penguins_input(input, output)

  # Read file content
  file_content <- readLines(paths$input)

  # Perform substitutions
  result <- penguins_substitute(file_content, paths$output_short)

  # Write output
  writeLines(result$content, paths$output)

  # Return logical indicating whether file was changed
  invisible(result$matches)
}
