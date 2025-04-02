# Define here so don't require R 4.4, which causes R CMD check to fail on oldrel
`%||%` <- function(x, y) if (is.null(x)) y else x

#' List or find example files from basepenguins package
#'
#' @description
#' These functions provides access to example files included with the basepenguins package.
#' When `example_files()` is called with `path = NULL`, it lists available example files.
#' When called with a specific path, it returns the full path to that file.
#' `example_dir()` provides the path to the directory containing the examples,
#' and also takes a `copy.dir` argument which, if specified, will copy all the
#' example files to a new directory. This is useful for testing the `convert_dir_inplace()`
#' and `convert_files_inplace()` functions without overwriting package files.
#'
#' @param path Character string. If `NULL` (default), lists all available example files.
#'   If specified, returns the full path to the specified file or directory.
#' @param full.names Logical. If `TRUE`, returns full file paths rather than relative paths.
#'   Only used when `path = NULL`. Default is FALSE.
#' @param recursive Logical. If `TRUE`, lists files in subdirectories recursively.
#'   Only used when `path = NULL`. Default is TRUE.
#' @param copy.dir Character string. A directory name or path to a directory
#'   into which the files in the example directory will be copied.
#'   If `NULL` (default) the files will not be copied.
#'
#' @return
#' `example_files`:
#'  - If `path = NULL`, a character vector of available file/directory names.
#'  - If `path` is specified, the absolute path to the requested file.
#'
#'` example_dir`:
#'  - If `copy.dir = NULL`, the absolute path to the directory containing all examples.
#'  - If `copy.dir` is specified, the specified directory is created if it
#'    doesn't already exist, and all example files are copied into it,
#'    preserving nesting structure. The absolute path to the directory is
#'    returned invisibly.
#'
#' @details
#' There are four example files in the example directory:
#' - penguins.R - an R script using the palmerpenguins package
#' - no_penguins.Rmd - an Rmarkdown file with `ends_with("_mm")`,
#'   but *not* in the context of the palmerpenguins package
#' - nested/penguins.qmd - a Quarto document that uses the palmerpenguins package
#' - nested/not_a_script.md - contains `library(palmerpenguins)`, but is not a script,
#'   i.e. not one of the default extensions for the convert functions.
#'
#' @examples
#' # List all files in the example directory provided by the package
#' example_files()
#'
#' # Get the full path to a specific example files
#' example_files("penguins.R") # path/to/basepenguins/extdata/penguins.R
#' example_files("nested/penguins.qmd")
#'
#' # Get the path to the directory containing the example files
#' example_dir() # path/to/basepenguins/extdata/
#' \dontshow{.old_wd <- setwd(tempdir())}
#' # Copy all files in the example directory
#' example_dir(".") # copy example files into working directory
#' example_dir("examples") # create subdirectory
#' \dontshow{setwd(.old_wd)}
#'
#' @export
example_files <- function(
  path = NULL,
  full.names = FALSE,
  recursive = TRUE
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

#' @rdname example_files
#' @export
example_dir <- function(copy.dir = NULL) {
  if (is.null(copy.dir)) {
    return(system.file("extdata", package = "basepenguins"))
  }

  # copy over all example files to new folder
  if (!dir.exists(copy.dir)) {
    dir.create(copy.dir, recursive = TRUE)
  }

  copy.path <- normalizePath(copy.dir)

  from <- example_files(full.names = TRUE, recursive = TRUE)
  rel_paths <- example_files(full.names = FALSE, recursive = TRUE)
  to <- file.path(copy.path, rel_paths)

  # Create all necessary directories
  for (file_path in to) {
    dir_path <- dirname(file_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
  }

  file.copy(from = from, to = to, overwrite = TRUE)

  invisible(copy.path)
}

filter_by_extensions <- function(extensions) {
  # handles NULL and, e.g. character(0)
  if (length(extensions) == 0 || identical(extensions, "")) {
    return("*")
  }
  extensions_pattern <- paste0(extensions, collapse = "|")

  paste0("\\.(", extensions_pattern, ")$")
}

#' List files to convert in a directory
#'
#' @description
#' This function lists all files in a directory (and subdirectories) that match
#' the specified file extensions. It can be used as a helper to find files paths
#' to pass to `convert_files()` and `convert_files_inplace()`,
#' or to preview which files those functions, as well as `convert_dir()` and
#' `convert_dir_inplace()` will look to convert. It can also be used as input to
#' `output_paths()` to help generate output paths for new files.
#'
#' @param dir A character string specifying the directory path to search
#' @param full.names Logical. If `TRUE`, returns full file paths rather than relative paths.
#'   Default is `FALSE`.
#' @param extensions A character vector of file extensions to filter by.
#'   Default is `c("R", "r", "qmd", "rmd", "Rmd")`. If `NULL` or empty, returns all files.
#'
#' @return A character vector of file paths that match the specified extensions.
#'
#' @seealso [convert_files()], [convert_files_inplace()], [convert_dir()], [convert_dir_inplace()],
#' [output_paths()].
#'
#' @examples
#' example_dir <- example_dir() # Get examples directory
#' files_to_convert(example_dir)
#' files_to_convert(example_dir, full.names = TRUE)
#' files_to_convert(example_dir, extensions = "R")
#' files_to_convert(example_dir, extensions = NULL) # all files
#'
#' @export
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

output_path <- function(path, prefix = "", suffix = "_new") {
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

#' Generate modified file paths by adding prefixes and/or suffixes
#'
#' @description
#' This function takes a vector of file paths and returns a vector of modified paths
#' with prefixes and/or suffixes added to the filenames. It's useful for generating
#' output paths for the `convert_files()` and `convert_dir()` functions.
#' `files_to_convert()` is useful for generating the input `paths`.
#'
#' @param paths A character vector of file paths to modify
#' @param prefix A character string to add at the beginning of each filename. Default is the empty sting `""`.
#' @param suffix A character string to add at the end of each filename, before the extension.
#'   Default is `"_new"`.
#' @param dir An optional character string specifying the output directory.
#'   If provided, the modified filenames will be placed in this directory.
#'   This is useful if `paths` are relative and a different output directory is required.
#'   Default is `NULL`.
#'
#' @return A character vector of modified file paths with the specified prefixes and suffixes.
#'   The original paths are preserved as names of the returned vector.
#'
#' @examples
#' # Get all convertible files from examples directory
#' input_files <- files_to_convert(example_dir())
#'
#' # Generate output paths with "_converted" suffix
#' output_paths(input_files, suffix = "_converted")
#'
#' # Add both prefix and suffix and place in a new directory
#' output_paths(
#'   input_files,
#'   prefix = "base_",
#'   suffix = "_converted",
#'   dir = "~/new_dir"
#' )
#'
#' @seealso [convert_files()], [files_to_convert()]
#'
#' @export
output_paths <- function(paths, prefix = "", suffix = "_new", dir = NULL) {
  if (length(paths) == 0) {
    stop("`paths` must have length at least 1 (not 0)")
  }

  out <- sapply(paths, output_path, prefix = prefix, suffix = suffix)

  if (!is.null(dir)) {
    out <- file.path(dir, out)
    names(out) <- paths
  }

  out
}
