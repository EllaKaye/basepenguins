library(palmerpenguins)
library(ggplot2)
library(dplyr)

# exploring scatterplots
penguins |>
  select(body_mass_g, ends_with("_mm")) |>
  glimpse()

# Scatterplot example: penguin flipper length versus body mass
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species), size = 2) +
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))
