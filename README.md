
<!-- README.md is generated from README.Rmd. Please edit that file -->

# basepenguins

A work-in-progress package. The main functionality is now in place, as
is a fully-comprehensive test suite. **Still to do: documentation.**

Convert scripts that use the
**[palmerpenguins](https://allisonhorst.github.io/palmerpenguins/index.html)**
library to use the versions of the `penguins` and `penguins_raw`
datasets that are now in R-devel, so coming in R 4.5.

The Palmer Penguins data has become [very
popular](https://apreshill.github.io/palmerpenguins-useR-2022/) in the R
community, especially in educational contexts, and as an [alternative to
the `iris`
dataset](https://journal.r-project.org/articles/RJ-2022-020/). Including
the datasets in R makes them more widely available, and easier to get
started with, especially for new R users, and for use in teaching.

The version of `penguins` in R’s **datasets** package has some shorter
variable names than the **palmerpenguins** equivalent. The differences
are:

- `bill_dep` instead of `bill_depth_mm`
- `bill_len` instead of `bill_length_mm`
- `flipper_len` instead of `flipper_length_mm`
- `body_mass` instead of `body_mass_g`

These changes were made for more compact code and data display.
(e.g. printing `penguins` now shows all columns in under 80 characters).

It does mean, however, that for those wanting to use R’s version of
`penguins`, it isn’t simply a case of removing the call to
`library(palmerpenguins)` and the script still running.

This package provides functions that will take a vector of files
(`convert_files()`) or a directory (`convert_dir()`), and for all files
with specified extensions (by default .R/.r/.qmd/.rmd/.Rmd ), remove any
calls to `library(palmerpenguins)` and replace long palmerpenguins
variable names with the shorter R equivalents. It also deals with
`ends_with("_mm")` (a selector used in the [palmerpenguins Get Started
vignette](https://allisonhorst.github.io/palmerpenguins/articles/intro.html)).
**DETAILS OF THIS SUBSTITUTION WILL BE GIVEN IN THE GET STARTED
VIGNETTE, WHEN WRITTEN.**

The versions of `penguins_raw` in **palmerpenguins** and **datasets**
are identical, expect that in the former it’ll have class `tbl_df` if
the [tibble](https://tibble.tidyverse.org) package is installed. No
specific changes are made to `penguins_raw` in **basepenguins**, but by
removing the call to `library(palmerpenguins)`, the **datasets** version
will be used in any scripts, which is always a `data.frame` (never a
`tbl_df`).

Note that the **palmerpenguins** package provides features that are not
in R, such as vignettes and articles on the [package
website](https://allisonhorst.github.io/palmerpenguins/index.html). The
package also contains the data in two csv files and provides a
[function](https://allisonhorst.github.io/palmerpenguins/reference/path_to_file.html)
to access them. And, of course, Allison Horst’s wonderful [penguins
artwork](https://allisonhorst.github.io/palmerpenguins/articles/art.html)!

<!-- badges: start -->

[![R-CMD-check](https://github.com/EllaKaye/basepenguins/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EllaKaye/basepenguins/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/EllaKaye/basepenguins/graph/badge.svg)](https://app.codecov.io/gh/EllaKaye/basepenguins)
<!-- badges: end -->

<!-- 
The goal of basepenguins is to ...
&#10;## Installation
&#10;You can install the development version of basepenguins from [GitHub](https://github.com/) with:
&#10;``` r
# install.packages("pak")
pak::pak("EllaKaye/basepenguins")
```
&#10;-->
