# All library call variations
library(palmerpenguins)
library('palmerpenguins')
library("palmerpenguins")
library(dplyr)

# All variable patterns
bill_len_data <- penguins$bill_length_mm
bill_dep_data <- penguins$bill_depth_mm
flipper_data <- penguins$flipper_length_mm
mass_data <- penguins$body_mass_g

# Multiple ends_with examples
penguins |> select(, ends_with("_mm"))
penguins |> summarize(penguins, across(ends_with("_mm"), mean))
penguins |> filter(species == "Adelie") |> select(ends_with("_mm"))
