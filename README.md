
<!-- README.md is generated from README.Rmd. Please edit that file -->

# basepenguins

<!-- badges: start -->

[![R-CMD-check](https://github.com/EllaKaye/basepenguins/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EllaKaye/basepenguins/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/EllaKaye/basepenguins/graph/badge.svg)](https://app.codecov.io/gh/EllaKaye/basepenguins)
<!-- badges: end -->

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
`library(palmerpenguins)` and the script still running. The
**basepenguins** package takes care of converting files to remove the
call to `library(palmerpenguins)` and makes the necessary conversions to
variable names, ensuring that the resulting scripts still run.

## Installation

You can install the development version of basepenguins from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("EllaKaye/basepenguins")
```

## Converting a file

If a file contains `library(palmerpenguins)`, and is ‘convertible’,
i.e.  has one of a specified set of extensions (by default .R, .r, .qmd,
.rmd, .Rmd), then converting it will do the following:

- Replace `library(palmerpenguins)` with `""`
- Replace variable names:
  - `bill_length_mm` -\> `bill_len`
  - `bill_depth_mm` -\> `bill_dep`
  - `flipper_length_mm` -\> `flipper_len`
  - `body_mass_g` -\> `body_mass`
- Replace `ends_with("_mm")` with
  `starts_with("flipper_"), starts_with(bill_)`

Here, we simply show the ‘before-and-after’ when converting a single
file.

For a more extensive guide to using the pacakge, see the [Get
Started](https://ellakaye.github.io/basepenguins/articles/basepenguins.html)
vignette, `vignette("basepenguins")`.

``` r
library(basepenguins)
```

Get and see an example file provided by the package:

``` r
penguin_file <- example_files("penguins.R")
cat(readLines(penguin_file), sep = "\n")
#> library(palmerpenguins)
#> library(ggplot2)
#> library(dplyr)
#> 
#> # exploring scatterplots
#> penguins |>
#>   select(body_mass_g, ends_with("_mm")) |>
#>   glimpse()
#> 
#> # Scatterplot example: penguin flipper length versus body mass
#> ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
#>   geom_point(aes(color = species, shape = species), size = 2) +
#>   scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))
```

Then, convert the file (saving it to a new file) and see the new script:

``` r
convert_files(penguin_file, "penguins_converted.R")
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
#>   glimpse()
#> 
#> # Scatterplot example: penguin flipper length versus body mass
#> ggplot(data = penguins, aes(x = flipper_len, y = body_mass)) +
#>   geom_point(aes(color = species, shape = species), size = 2) +
#>   scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))
```
