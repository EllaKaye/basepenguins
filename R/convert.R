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
#'   Can be relative or absolute paths.
#'   For `convert_dir()` and `convert_dir_inplace()`:
#'   A string with a path to a directory of files to convert.
#' @param output For `convert_files()`:
#'   A character vector of output file paths, or NULL to modify files in place.
#'   If provided, must be the same length as `input`.
#'   Can be absolute or relative paths.
#'   For `convert_dir()`:
#'   A string with the output directory,
#'   or `NULL` to modify the files in the directory in place.
#' @param extensions A character vector of file extensions to process,
#'   defaults to R scripts and R Markdown and Quarto documents.
#'
#' @returns
#' A list (returned invisibly) with two components:
#' - `changed`: A character vector of paths for files that were modified.
#' - `not_changed`: A character vector of paths for files that were not
#'    modified. Files are not changed if they do not load the palmerpenguins
#'    package via `library(palmerpenguins)`, `library('palmerpenguins')` or
#'    `library("palmerpenguins")`, or if they do not have one of the specified
#'    `extensions`.
#'
#' For both the `changed` and `not_changed` vectors, these will be subsets of
#' the `output` paths, if they were provided, with the corresponding `input`
#' paths as names. If `output` was not specified, then these vectors will be
#' subsets of the `input` paths, and the vectors will not be named.
#'
#' @details
#' Files are converted by:
#' - Replacing `library(palmerpenguins)` with `""`
#' - Replacing variable names:
#'   - `bill_length_mm` -> `bill_len`
#'   - `bill_depth_mm` -> `bill_dep`
#'   - `flipper_length_mm` -> `flipper_len`
#'   - `body_mass_g` -> `body_mass`
#' - Replacing `ends_with("_mm")` with `starts_with("flipper_"), starts_with(bill_)`
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
#' indicating the files and line numbers where the `ends_with("_mm")`
#' substitution was made.
#'
#' @seealso [example_dir()], [output_paths()]
#'
#' @examples
#' \dontshow{.old_wd <- setwd(tempdir())}
#' # Note that all examples below write output to a temporary directory
#' # and file paths are relative to that directory (unless otherwise stated).
#'
#' # For all examples below, use a copy of the examples provided by the package,
#' # copied to an "examples" directory in the working directory
#' example_dir("examples")
#'
#' # Single file - new output
#' result <- convert_files("examples/penguins.R", "penguins_new.R")
#' cat(readLines("penguins_new.R"), sep = "\n") # view changes
#'
#' # Single file - copy, then modify that in place
#' file.copy("examples/penguins.R", "penguins_copy.R")
#' convert_files_inplace("penguins_copy.R")
#'
#' # Multiple files - new output locations
#' input_files <- c("examples/penguins.R", "examples/nested/penguins.qmd")
#' output_files <- output_paths(input_files, dir = "new_dir")
#' convert_files(input_files, output_files)
#'
#' # Directory - new output location
#' result <- convert_dir("examples", "new_directory")
#' result # see `changed` and `not_changed` files
#'
#' # Overwrite the files in "examples"
#' result <- convert_dir_inplace("examples")
#' result # see `changed` and `not_changed` files
#'
#' \dontrun{
#' # Overwrite all convertible files in the working directory
#' convert_dir_inplace(".")
#' }
#'
#' \dontshow{setwd(.old_wd)}
#'
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
  not_changed_convertible <- convertible_output[!converted]
  not_changed <- c(not_changed_convertible, not_convertible)

  # use input as names, if output is different from input
  if (!overwrite) {
    names(changed) <- convertible_input[converted]
    names(not_changed) <- c(convertible_input[!converted], input[!convertible])
  }

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
