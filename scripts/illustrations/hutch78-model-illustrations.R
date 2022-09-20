library(tidyverse)
library(ggpubr)
library(hrep)

theme_set(theme_pubr())

dissonance_curve <- function(bandwidth_distance) {
  (4 * bandwidth_distance * exp(1 - 4 * bandwidth_distance)) ^ 2
}

tibble(
  cbw_dist = seq(from = 0, to = 2, by = 0.01),
  diss = dissonance_curve(cbw_dist)
) %>% 
  ggplot(aes(cbw_dist, diss)) + 
  geom_vline(xintercept = 0.25, colour = "grey50", linetype = "dotted") +
  geom_line() + 
  scale_x_continuous("Distance between partials (critical bandwidths)") + 
  scale_y_continuous("Dissonance factor")

R.utils::mkdirs("output/illustrations")
ggsave("output/illustrations/hutchinson-knopoff-dissonance-curve.pdf", width = 5, height = 2)

tibble(
  cbw_dist = seq(from = 0, to = 2, by = 0.01),
  diss = revised_dissonance_factor(cbw_dist)
) %>% 
  ggplot(aes(cbw_dist, diss)) + 
  geom_vline(xintercept = 0.25, colour = "grey50", linetype = "dotted") +
  geom_line() + 
  scale_x_continuous("Distance between partials (critical bandwidths)") + 
  scale_y_continuous("Dissonance factor")
