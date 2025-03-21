% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{convert_files}
\alias{convert_files}
\alias{convert_files_inplace}
\alias{convert_dir}
\alias{convert_dir_inplace}
\title{Convert files to use datasets versions of penguins and penguins_raw}
\usage{
convert_files(input, output, extensions = c("R", "r", "qmd", "rmd", "Rmd"))

convert_files_inplace(input, extensions = c("R", "r", "qmd", "rmd", "Rmd"))

convert_dir(input, output, extensions = c("R", "r", "qmd", "rmd", "Rmd"))

convert_dir_inplace(input, extensions = c("R", "r", "qmd", "rmd", "Rmd"))
}
\arguments{
\item{input}{For \code{convert_files()} and \code{convert_files_inplace()}:
A character vector of file paths to convert.
For \code{convert_dir()} and \code{convert_dir_inplace()}:
A string with a path to a directory of files to convert.}

\item{output}{For \code{convert_files()}:
A character vector of output file paths, or NULL to modify files in place.
If provided, must be the same length as \code{input}.
For \code{convert_dir()}:
A string with the output directory,
or \code{NULL} to modify the files in the directory in place.}

\item{extensions}{A character vector of file extensions to process,
defaults to R scripts and R Markdown and Quarto documents.}
}
\value{
An invisible list with two components:
\itemize{
\item \code{changed}: A named character vector of output paths for files that
were modified, with input paths as the names.
\item \code{not_changed}: A named character vector of output paths for files
that were not modified, with input paths as names. Files are not changed
if they do not contain references to palmerpenguins (i.e. the patterns)
listed in the Details section, or if they do not have one of the
specified \code{extensions}.
}
}
\description{
These functions convert files that use the
\href{https://CRAN.R-project.org/package=palmerpenguins}{palmerpenguins} package
to use the versions of \code{penguins} and \code{penguins_raw} included in the datasets
package in R 4.5.0. They removes calls to \code{library(palmerpenguins)} and make
necessary changes to some variable names (see Details section below).
}
\details{
Files are converted by:
\itemize{
\item Replacing \code{library(palmerpenguins)} with \code{""}
\item Replacing variable names:
\itemize{
\item \code{bill_length_mm} -> \code{bill_len}
\item \code{bill_depth_mm} -> \code{bill_dep}
\item \code{flipper_length_mm} -> \code{flipper_len}
\item \code{body_mass_g} -> \code{body_mass}
}
\item Replacing \code{ends_with("_mm")} with \verb{starts_with("flipper_"), starts_with(bill_)}
}

Non-convertible files (those without the specified extensions) are copied to
the output location if \code{output} is provided, but are not modified.

If the \code{output} files or directory do not (yet) exist, they will be created
(recursively if necessary).

Replacing \code{ends_with("_mm")} with \verb{starts_with("flipper_"), starts_with(bill_)}
ensures that modified R code will always run. \code{starts_with("flipper_")} isn't
intuitively necessary, as there is only one variable starting with "flipper_",
in \code{penguins}, but this code will not error inside \verb{dplyr::(select)}, even if
\code{flipper_len} isn't in the data frame (trying to select \code{flipper_len}
directly will cause an error if that column isn't in the data frame).
In an educational context, we suggest manually editing the converted files to
replace \code{starts_with("flipper_")} to \code{flipper_len} if appropriate.
To facilitate this, the functions documented here produce a message
indicating the files and line numbers where the \code{ends_with("_mm")}
substitution was made.
}
\examples{
# Single file
penguin_file <- penguins_examples("penguins.R")
output_file <- withr::local_tempfile(fileext = ".R")
result <- convert_files(penguin_file, output_file)
cat(readLines(output_file), sep = "\n") # view changes

# Convert multiple files to new locations
input_files <- c(
  penguins_examples("penguins.R"),
  penguins_examples("analysis/penguins.qmd")
)
output_files <- c(
  withr::local_tempfile(fileext = ".R"),
  withr::local_tempfile(fileext = ".qmd")
)
result <- convert_files(input_files, output_files)

# Convert all files in a directory
penguins_dir <- penguins_examples_dir()
output_dir <- withr::local_tempdir()
result <- convert_dir(penguins_dir, output_dir)
result$changed # see which files have changed

# Modify files in-place
input_file <- penguins_examples("penguins.R")

# Copy file so don't overwrite example provided by package
copy_path <- withr::local_tempfile(fileext = ".R")
file.copy(input_file, copy_path)
convert_files_inplace(copy_path)

\dontrun{ # don't overwrite the example files provided by the package
  # Get all example files
  input_files <- penguins_examples(recursive = TRUE, full.names = TRUE)

  # Convert them in place
  convert_files_inplace(input_files)
}

}
\seealso{
\code{\link[=penguins_examples]{penguins_examples()}}, \code{\link[=penguins_examples_dir]{penguins_examples_dir()}}
}
