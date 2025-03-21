# Define here so don't require R 4.4, which causes R CMD check to fail on oldrel
`%||%` <- function(x, y) if (is.null(x)) y else x

#' List or Find Example Files from basepenguins Package
#'
#' @description
#' This function provides access to example files included with the basepenguins package.
#' When called with `path = NULL`, it lists available example files. When called with a
#' specific path, it returns the full path to that file.
#'
#' @param path Character string. If NULL (default), lists all available example files.
#'   If specified, returns the full path to the specified file or directory.
#' @param recursive Logical. If TRUE, lists files in subdirectories recursively.
#'   Only used when `path = NULL`. Default is FALSE.
#' @param full.names Logical. If TRUE, returns full file paths rather than relative paths.
#'   Only used when `path = NULL`. Default is FALSE.
#'
#' @return
#' If `path = NULL`, returns a character vector of available file/directory names.
#' If `path` is specified, returns the full file path to the requested resource.
#'
#' @examples
#' # List all files and directories in the root directory
#' penguins_examples()
#' penguins_examples(recursive = TRUE)
#'
#' # Get the full path to a specific example files
#' penguins_examples("penguins.R")
#' penguins_examples("analysis/penguins.qmd")
#'
#' @export
penguins_examples <- function(
  path = NULL,
  recursive = FALSE,
  full.names = FALSE
) {
  if (is.null(path)) {
    dir(
      system.file("extdata", package = "basepenguins"),
      recursive = recursive,
      full.names = full.names
    )
  } else {
    system.file(
      "extdata",
      path,
      package = "basepenguins",
      mustWork = TRUE
    )
  }
}

filter_by_extensions <- function(extensions) {
  # handles NULL and, e.g. character(0)
  if (length(extensions) == 0 || identical(extensions, "")) {
    return("*")
  }
  extensions_pattern <- paste0(extensions, collapse = "|")

  paste0("\\.(", extensions_pattern, ")$")
}

files_to_convert <- function(
  dir,
  full.names = FALSE,
  extensions = c("R", "r", "qmd", "rmd", "Rmd")
) {
  if (!dir.exists(dir)) {
    stop("`dir` directory does not exist")
  }

  pattern <- filter_by_extensions(extensions)

  list.files(dir, full.names = full.names, recursive = TRUE, pattern = pattern)
}

extend_name <- function(path, prefix = "", suffix = "_new") {
  if (!is.character(path)) {
    stop("`path` must be a string")
  }

  if (path == "") {
    stop("`path` must not be an empty string")
  }

  dir_part <- dirname(path) # directory part
  file_part <- basename(path) # filename without path
  ext <- tools::file_ext(file_part)
  filename <- tools::file_path_sans_ext(file_part)

  # Create new filename with prefix and suffix
  if (ext == "") {
    new_file <- paste0(prefix, filename, suffix)
  } else {
    new_file <- paste0(prefix, filename, suffix, ".", ext)
  }

  # Combine directory and new filename
  if (dir_part == ".") {
    return(new_file)
  } else {
    return(file.path(dir_part, new_file))
  }
}

extend_names <- function(paths, prefix = "", suffix = "_new") {
  if (length(paths) == 0) {
    stop("`paths` must have length at least 1 (not 0)")
  }

  sapply(paths, extend_name, prefix = prefix, suffix = suffix)
}
