library(palmerpenguins)
library(ggplot2)
library(dplyr)

# exploring scatterplots
penguins |>
  select(body_mass_g, ends_with("_mm")) |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species), size = 2) +
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))
