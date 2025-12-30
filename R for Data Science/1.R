library(tidyverse)
library(palmerpenguins)
library(ggthemes)
library(plotly)

glimpse(penguins)

avg_bill_length <- mean(penguins$bill_length_mm, na.rm=TRUE)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point(mapping = aes(color = species, shape = species)) + 
  geom_smooth(method = "lm") + 
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body m",
    color = "Sp", shape = "Sp"
  ) +
  scale_color_colorblind()


ggplotly(
  p = ggplot2::last_plot()
) 

plot <- ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

ggplotly(plot)
 