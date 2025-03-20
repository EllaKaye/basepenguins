# TODO: document
convert_files <- function(
  input,
  output,
  extensions = c("R", "r", "qmd", "rmd", "Rmd")
) {
  if (length(input) == 0) {
    stop(
      "`input` must not be emply. Please supply at least one file to convert."
    )
  }

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
      "- Please check the changed output files."
    )
  }

  if (any(tolower(tools::file_ext(changed)) %in% c("qmd", "rmd"))) {
    message(
      "- Remember to re-knit or re-render and changed Rmarkdown or Quarto documents."
    )
  }

  invisible(list(changed = changed, not_changed = not_changed))
}

convert_files_inplace <- function(input) {
  convert_files(input, input)
}

# TODO: document
convert_dir <- function(
  input,
  output,
  extensions = c("R", "r", "qmd", "rmd", "Rmd")
) {
  if (!dir.exists(input)) {
    stop("`input` must be a directory that exists")
  }

  # Get all files from input directory then determine which are convertible
  file_names <- list.files(input, recursive = TRUE)

  # Check if there are any files
  if (length(file_names) == 0) {
    stop("There are no files in `input` to convert")
  }

  # Check that there are files with required extensions
  if (!any(tools::file_ext(file_names) %in% extensions)) {
    stop("There are no files with specified extensions to convert")
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

convert_dir_inplace <- function(input) {
  convert_dir(input, input)
}
