library(palmerpenguins)
library(ggplot2)
library(dplyr)

# exploring scatterplots
penguins |>
  select(body_mass_g, ends_with("_mm")) |>
  glimpse()

# Scatterplot example 1: penguin flipper length versus body mass
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species), size = 2) +
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))

# Scatterplot example 2: penguin bill length versus bill depth
ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species, shape = species), size = 2) +
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))

# With facets
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = sex)) +
  scale_color_manual(values = c("darkorange", "cyan4"), na.translate = FALSE) +
  facet_wrap(~species)
