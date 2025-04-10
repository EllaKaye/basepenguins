
<!-- README.md is generated from README.Rmd. Please edit that file -->

# basepenguins <a href="https://ellakaye.github.io/basepenguins/"><img src="man/figures/logo.png" align="right" height="138" alt="basepenguins website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/EllaKaye/basepenguins/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EllaKaye/basepenguins/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/EllaKaye/basepenguins/graph/badge.svg)](https://app.codecov.io/gh/EllaKaye/basepenguins)
[![CRAN
status](https://www.r-pkg.org/badges/version/basepenguins)](https://CRAN.R-project.org/package=basepenguins)
<!-- badges: end -->

Convert scripts that use the
[**palmerpenguins**](https://allisonhorst.github.io/palmerpenguins/index.html)
package to use the versions of the `penguins` and `penguins_raw`
datasets that are available in R ≥ 4.5.0.

The Palmer Penguins datasets have become [very
popular](https://apreshill.github.io/palmerpenguins-useR-2022/) in the R
community, especially in educational contexts, and as an [alternative to
the `iris`
dataset](https://journal.r-project.org/articles/RJ-2022-020/). Now that
`penguins` and `penguins_raw` are in **datasets** (R ≥ 4.5.0), they are
more widely available and easier to get started with, especially for new
R users and for use in teaching.

The version of `penguins` in **datasets** (R ≥ 4.5.0) has some shorter
variable names than the **palmerpenguins** equivalent (e.g. `bill_len`
instead of `bill_length_mm`), for more compact code and data display. It
does mean, however, that for those wanting to use R’s version of
`penguins`, it isn’t simply a case of removing the call to
`library(palmerpenguins)` or replacing `palmerpenguins` with `datasets`
in `data("penguins", package = "palmerpenguins")` and the script still
running. The **basepenguins** package takes care of converting files by
removing the call to **palmerpenguins** and making the necessary
conversions to variable names, ensuring that the resulting scripts still
run using the **datasets** versions of `penguins` and `penguins_raw`.

## Installation

**basepenguins** is available on CRAN:

``` r
install.packages("basepenguins")
```

Or get the development version from R-universe:

``` r
install.packages("basepenguins", repos = "https://ellakaye.r-universe.dev")
```

## Converting a file

If a file is ‘convertible’, i.e. it contains `library(palmerpenguins)`
or `data("penguins", package = "palmerpenguins")` (with any style of
quotes), and has one of a specified set of extensions (by default `"R"`,
`"r"`, `"qmd"`, `"rmd"`, `"Rmd"`), then converting it will do the
following:

- Replace `library(palmerpenguins)` (or same with `palmerpenguins` in
  quotes) with the empty string`""`
- Replace `data("penguins", package = "palmerpenguins")` (with any style
  of quotes) with `data("penguins", package = "datasets")`
- Replace variable names:
  - `bill_length_mm` -\> `bill_len`
  - `bill_depth_mm` -\> `bill_dep`
  - `flipper_length_mm` -\> `flipper_len`
  - `body_mass_g` -\> `body_mass`
- Replace `ends_with("_mm")` with
  `starts_with("flipper_"), starts_with("bill_")`

Here, we simply show the ‘before-and-after’ when converting a single
file. For a more extensive guide to using the package, see the [Get
Started](https://ellakaye.github.io/basepenguins/articles/basepenguins.html)
vignette, `vignette("basepenguins")`.

``` r
library(basepenguins)
```

Get and see an example file provided by the package (adapted from a
section of the **palmerpenguins** [Get
Started](https://allisonhorst.github.io/palmerpenguins/articles/intro.html)
vignette):

``` r
penguins_file <- example_files("penguins.R")
cat(readLines(penguins_file), sep = "\n")
#> library(palmerpenguins)
#> library(ggplot2)
#> library(dplyr)
#> 
#> # exploring scatterplots
#> penguins |>
#>   select(body_mass_g, ends_with("_mm")) |>
#>   ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
#>   geom_point(aes(color = species, shape = species), size = 2) +
#>   scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))
```

Then, convert the file (saving it to a new file) and see the new script:

``` r
convert_files(penguins_file, "penguins_converted.R")
#> - ends_with("_mm") replaced on line 7 in penguins_converted.R
#> - Please check the changed output files.
```

``` r
cat(readLines("penguins_converted.R"), sep = "\n")
#> 
#> library(ggplot2)
#> library(dplyr)
#> 
#> # exploring scatterplots
#> penguins |>
#>   select(body_mass, starts_with("flipper_"), starts_with("bill_")) |>
#>   ggplot(aes(x = flipper_len, y = body_mass)) +
#>   geom_point(aes(color = species, shape = species), size = 2) +
#>   scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))
```

## Converting multiple files or a directory

There are four functions in **basepenguins** to convert mulitple files.
In each case, the default `extensions` (i.e. file types to convert) are
`"R"`, `"r"`, `"qmd"`, `"rmd"`, `"Rmd"`. If `input` contains
non-convertible files (i.e. without the specified extensions or without
reference to `palmerpenguins`), they will be copied unmodified to their
new `output` location in `convert_files()` and `convert_dir()`, or left
untouched by `convert_files_inplace()` and `convert_dir_inplace()`.

|  |  |
|----|----|
| `convert_files(input, output, extensions)` | convert an `input` vector of files to new `output` locations |
| `convert_files_inplace(input, extensions)` | convert a vector of files by overwriting them |
| `convert_dir(input, output, extensions)` | convert all files in `input` directory into a new `output` directory (preserving nesting structure) |
| `convert_dir_inplace(input, extensions)` | convert all files in a directory by overwriting them |
