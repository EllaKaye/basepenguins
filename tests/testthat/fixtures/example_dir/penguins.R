# All library call variations
library(palmerpenguins)
library('palmerpenguins')
library("palmerpenguins")
library(dplyr)

# All variable patterns
bill_len_data <- penguins$bill_length_mm
bill_dep_data <- penguins$bill_depth_mm
flipper_data <- penguins$flipper_length_mm
body_mass_data <- penguins$body_mass_g

# Multiple ends_with examples (and repeat bill_length_mm)
penguins |> select(ends_with("_mm")) |> select(bill_length_mm)
penguins |> filter(species == "Adelie") |> select(ends_with("_mm"))
