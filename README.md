
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

## Usage

This is a quick tour. For a more extensive guide, see the [Get
Started](https://ellakaye.github.io/basepenguins/articles/basepenguins.html)
vignette, `vignette("basepenguins")`.

``` r
library(basepenguins)
```

## Converting files

``` r
# get absolute paths of example files
input <- example_files(full.names = TRUE)

# See one of the input files
cat(readLines(input[2]), sep = "\n") 
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

``` r
# generate output file paths (by default prefix "_new" to input filenames)
output <- extend_names(input) 
```

``` r
# convert the files
result <- convert_files(input, output)
#> - In /private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/penguins_new.R, ends_with("_mm") replaced on line 7 - please check that the subsitution is appropriate.
#> - Please check the changed output files.
#> - Remember to re-knit or re-render and changed Rmarkdown or Quarto documents.
```

``` r
# See which files have changed
result
#> $changed
#>       /private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/analysis/penguins.qmd 
#> "/private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/analysis/penguins_new.qmd" 
#>                  /private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/penguins.R 
#>            "/private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/penguins_new.R" 
#> 
#> $not_changed
#> character(0)
```

``` r
# See the changes to the input file we saw above
cat(readLines(output[2]), sep = "\n") 
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

Conversions can also be run in-place, which will overwrite files. Here’s
how do to that, limited to R scripts, though we don’t execute the chunk
below:

``` r
convert_files_inplace(input, extensions = "R")
```

## Coverting a directory

Whilst `convert_files()` allows the conversion of specified files,
`convert_dir()` will convert all the files (with the stated extensions)
in a given directory (and its subdirectories).

``` r
example_dir <- example_dir()
output_dir <- tempdir()
result <- convert_dir(example_dir, output_dir)
#> - In /var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T//RtmpMwC006/penguins.R, ends_with("_mm") replaced on line 7 - please check that the subsitution is appropriate.
#> - Please check the changed output files.
#> - Remember to re-knit or re-render and changed Rmarkdown or Quarto documents.
```

``` r

result
#> $changed
#> /private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/analysis/penguins.qmd 
#>                                                     "/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T//RtmpMwC006/analysis/penguins.qmd" 
#>            /private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/penguins.R 
#>                                                                "/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T//RtmpMwC006/penguins.R" 
#> 
#> $not_changed
#> /private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/analysis/penguins_new.qmd 
#>                                                     "/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T//RtmpMwC006/analysis/penguins_new.qmd" 
#>            /private/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T/RtmpjX8utG/temp_libpath1164130dc63f2/basepenguins/extdata/penguins_new.R 
#>                                                                "/var/folders/zd/v1_3x7fs7h9bjxmv6thqx30h0000gq/T//RtmpMwC006/penguins_new.R"
```

There is also a version to modify files in place (not executed here):

``` r
convert_dir_inplace(example_dir)
```
