penguins_examples <- function(path = NULL) {
  if (is.null(path)) {
    system.file("extdata", package = "basepenguins")
  } else {
    system.file("extdata", path, package = "basepenguins", mustWork = TRUE)
  }
}

files_to_convert <- function(dir) {
  # limit to .R, .Rmd, .rmd, .qmd files
  list.files(dir, recursive = TRUE, pattern = "\\.(R|[Rrq]md)$")
}

extend_name <- function(path, prefix = "", suffix = "_new") {
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
  sapply(paths, extend_name, prefix = prefix, suffix = suffix)
}
