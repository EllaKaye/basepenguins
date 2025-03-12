penguins_examples <- function(path = NULL) {
  if (is.null(path)) {
    system.file("extdata", package = "basepenguins")
  } else {
    system.file("extdata", path, package = "basepenguins", mustWork = TRUE)
  }
}

# TODO: add full.names to give longer paths
# e.g. prepend `dir` to the files with file.path
# is this what full.names does in base R functions? - CHECK
# yes - this is what full.names does in list.files, so can pass that
# think about how this function gets used in other functions, and in examples
files_to_convert <- function(dir, full.names = FALSE) {
  # limit to .R, .Rmd, .rmd, .qmd files
  list.files(
    dir,
    full.names = full.names,
    recursive = TRUE,
    pattern = "\\.(R|[Rrq]md)$"
  )
}

extend_name <- function(path, prefix = "", suffix = "_new") {
  dir_part <- dirname(path) # directory part
  file_part <- basename(path) # filename without path
  ext <- tools::file_ext(file_part)
  filename <- tools::file_path_sans_ext(file_part)

  # MAYBE: can I use file.path here instead of paste0?
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
  sapply(paths, extend_name, prefix = prefix, suffix = suffix)
}
