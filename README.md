
<!-- README.md is generated from README.Rmd. Please edit that file -->

# basepenguins

A work-in-progress package in very early stages of development.

Convert .R/.qmd/.rmd/.Rmd files that use the
**[palmerpenguins](https://allisonhorst.github.io/palmerpenguins/index.html)**
library to use the version of the `penguins` and `penguins_raw` datasets
that are now in R-devel, so coming in R 4.5.

The Palmer Penguins data has become [very
popular](https://apreshill.github.io/palmerpenguins-useR-2022/) in the R
community, especially in educational contexts, and as an [alternative to
the `iris`
dataset](https://journal.r-project.org/articles/RJ-2022-020/). Including
the datasets in R makes them more widely available, and easier to get
started with, especially for new R users, and for use in teaching.

The version of `penguins` in R’s **datasets** package use some shorter
column names than the **palmerpenguins** equivalent. The differences
are:

- `bill_dep` instead of `bill_depth_mm`
- `bill_len` instead of `bill_length_mm`
- `flipper_len` instead of `flipper_length_mm`
- `body_mass` instead of `body_mass_g`

These changes were made for more compact code and data display.
(e.g. printing `penguins` now shows all columns in under 80 characters).

It does mean, however, that for those wanting to use R’s version of the
datasets, it isn’t simply a case of removing the call to
`library(palmerpenguins)` and the script still running.

This package will provide a function that will take a file or directory,
remove any calls to `library(palmerpenguins)` and replace long
palmerpenguins variable names with the shorter R equivalents.

Note that the **palmerpenguins** package provides features that are not
in R, such as vignettes and articles on the [package
website](https://allisonhorst.github.io/palmerpenguins/index.html). The
package also contains the data in two csv files and provides a
[function](https://allisonhorst.github.io/palmerpenguins/reference/path_to_file.html)
to access them. And, of course, Allison Horst’s wonderful [penguins
artwork](https://allisonhorst.github.io/palmerpenguins/articles/art.html)!

<!-- badges: start -->

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
