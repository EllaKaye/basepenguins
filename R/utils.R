penguins_examples <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "basepenguins"))
  } else {
    system.file("extdata", path, package = "basepenguins", mustWork = TRUE)
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
