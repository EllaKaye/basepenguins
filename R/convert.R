#' Convert files to use datasets versions of penguins and penguins_raw
#'
#' @description
#' These functions convert files that use the
#' [palmerpenguins](https://CRAN.R-project.org/package=palmerpenguins) package
#' to use the versions of `penguins` and `penguins_raw` included in the datasets
#' package in R 4.5.0. They removes calls to `library(palmerpenguins)` and make
#' necessary changes to some variable names (see Details section below).
#'
#' @param input For `convert_files()` and `convert_files_inplace()`:
#'   A character vector of file paths to convert.
#'   For `convert_dir()` and `convert_dir_inplace()`:
#'   A string with a path to a directory of files to convert.
#' @param output For `convert_files()`:
#'   A character vector of output file paths, or NULL to modify files in place.
#'   If provided, must be the same length as `input`.
#'   For `convert_dir()`:
#'   A string with the output directory,
#'   or `NULL` to modify the files in the directory in place.
#' @param extensions A character vector of file extensions to process,
#'   defaults to R scripts and R Markdown and Quarto documents.
#'
#' @returns
#' An invisible list with two components:
#' \itemize{
#'   \item `changed`: A named character vector of output paths for files that
#'     were modified, with input paths as the names.
#'   \item `not_changed`: A named character vector of output paths for files
#'     that were not modified, with input paths as names. Files are not changed
#'     if they do not contain references to palmerpenguins (i.e. the patterns)
#'     listed in the Details section, or if they do not have one of the
#'     specified `extensions`.
#' }
#'
#' @details
#' Files are converted by:
#' \itemize{
#'   \item Replacing `library(palmerpenguins)` with `""`
#'   \item Replacing variable names:
#'   \itemize{
#'      \item `bill_length_mm` -> `bill_len`
#'      \item `bill_depth_mm` -> `bill_dep`
#'      \item `flipper_length_mm` -> `flipper_len`
#'      \item `body_mass_g` -> `body_mass`
#'   }
#'   \item Replacing `ends_with("_mm")` with `starts_with("flipper_"), starts_with(bill_)`
#' }
#'
#'
#' Non-convertible files (those without the specified extensions) are copied to
#' the output location if `output` is provided, but are not modified.
#'
#' If the `output` files or directory do not (yet) exist, they will be created
#' (recursively if necessary).
#'
#' Replacing `ends_with("_mm")` with `starts_with("flipper_"), starts_with(bill_)`
#' ensures that modified R code will always run. `starts_with("flipper_")` isn't
#' intuitively necessary, as there is only one variable starting with "flipper_",
#' in `penguins`, but this code will not error inside `dplyr::(select)`, even if
#' `flipper_len` isn't in the data frame (trying to select `flipper_len`
#' directly will cause an error if that column isn't in the data frame).
#' In an educational context, we suggest manually editing the converted files to
#' replace `starts_with("flipper_")` to `flipper_len` if appropriate.
#' To facilitate this, the functions documented here produce a message
#' indicating the files and line numbers where the `ends_with("_mm")` was made.
#'
#' @seealso [penguins_examples()], [penguins_examples_dir()]
#'
#' @examples
#' # Single file
#' penguin_file <- penguins_examples("penguins.R")
#' output_file <- withr::local_tempfile(fileext = ".R")
#' result <- convert_files(penguin_file, output_file)
#' cat(readLines(output_file), sep = "\n") # view changes
#'
#' # Convert multiple files to new locations
#' input_files <- c(
#'   penguins_examples("penguins.R"),
#'   penguins_examples("analysis/penguins.qmd")
#' )
#' output_files <- c(
#'   withr::local_tempfile(fileext = ".R"),
#'   withr::local_tempfile(fileext = ".qmd")
#' )
#' result <- convert_files(input_files, output_files)
#'
#' # Convert all files in a directory
#' penguins_dir <- penguins_examples_dir()
#' output_dir <- withr::local_tempdir()
#' result <- convert_dir(penguins_dir, output_dir)
#' result$changed # see which files have changed
#'
#' # Modify files in-place
#' input_file <- penguins_examples("penguins.R")
#'
#' # Copy file so don't overwrite example provided by package
#' copy_path <- withr::local_tempfile(fileext = ".R")
#' file.copy(input_file, copy_path)
#' convert_files_inplace(copy_path)
#'
#' \dontrun{ # don't overwrite the example files provided by the package
#'   # Get all example files
#'   input_files <- penguins_examples(recursive = TRUE, full.names = TRUE)
#'
#'   # Convert them in place
#'   convert_files_inplace(input_files)
#' }
#'
#' @export
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

#' @rdname convert_files
#' @export
convert_files_inplace <- function(
  input,
  extensions = c("R", "r", "qmd", "rmd", "Rmd")
) {
  convert_files(input, input, extensions)
}

#' @rdname convert_files
#' @export
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

#' @rdname convert_files
#' @export
convert_dir_inplace <- function(
  input,
  extensions = c("R", "r", "qmd", "rmd", "Rmd")
) {
  convert_dir(input, input, extensions)
}
