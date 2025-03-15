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
  converted <- mapply(penguins_convert, convertible_input, convertible_output)

  # Create named vectors for changed and not_changed files
  changed <- convertible_output[converted]
  names(changed) <- convertible_input[converted]

  not_changed_convertible <- convertible_output[!converted]
  names(not_changed_convertible) <- convertible_input[!converted]

  not_changed <- c(not_changed_convertible, not_convertible)

  if (length(changed) > 0) {
    message(
      "- Please check the changed output files and remember to re-knit or re-render any Rmarkdown or quarto documents."
    )
  }

  invisible(list(changed = changed, not_changed = not_changed))
}

# TODO: document
# TODO: test
# TODO: return named vectors (names are input)
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
    stop("`input` does not have a valid file extension")
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
