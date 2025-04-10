% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{files_to_convert}
\alias{files_to_convert}
\title{List files to convert in a directory}
\usage{
files_to_convert(
  dir,
  full.names = FALSE,
  extensions = c("R", "r", "qmd", "rmd", "Rmd")
)
}
\arguments{
\item{dir}{A character string specifying the directory path to search}

\item{full.names}{Logical. If \code{TRUE}, returns full file paths rather than relative paths.
Default is \code{FALSE}.}

\item{extensions}{A character vector of file extensions to filter by.
Default is \code{c("R", "r", "qmd", "rmd", "Rmd")}. If \code{NULL} or empty, returns all files.}
}
\value{
A character vector of file paths that match the specified extensions.
}
\description{
This function lists all files in a directory (and subdirectories) that match
the specified file extensions. It can be used as a helper to find files paths
to pass to \code{convert_files()} and \code{convert_files_inplace()},
or to preview which files those functions, as well as \code{convert_dir()} and
\code{convert_dir_inplace()} will look to convert. It can also be used as input to
\code{output_paths()} to help generate output paths for new files.
}
\examples{
example_dir <- example_dir() # Get examples directory
files_to_convert(example_dir)
files_to_convert(example_dir, full.names = TRUE)
files_to_convert(example_dir, extensions = "R")
files_to_convert(example_dir, extensions = NULL) # all files

}
\seealso{
\code{\link[=convert_files]{convert_files()}}, \code{\link[=convert_files_inplace]{convert_files_inplace()}}, \code{\link[=convert_dir]{convert_dir()}}, \code{\link[=convert_dir_inplace]{convert_dir_inplace()}},
\code{\link[=output_paths]{output_paths()}}.
}
