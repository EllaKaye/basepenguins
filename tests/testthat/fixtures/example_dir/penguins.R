# fmt: skip file
# All library call variations
library(palmerpenguins)
library('palmerpenguins')
library("palmerpenguins")
data("penguins", package = "palmerpenguins")
data(penguins, package='palmerpenguins')

# All variable patterns
bill_len_data <- penguins$bill_length_mm
bill_dep_data <- penguins$bill_depth_mm
flipper_data <- penguins$flipper_length_mm
body_mass_data <- penguins$body_mass_g

# Multiple ends_with examples (and repeat bill_length_mm)
penguins |> dplyr::select(ends_with("_mm")) |> dplyr::select(bill_length_mm)
penguins |> dplyr::filter(species == "Adelie") |> dplyr::select(ends_with("_mm"))
