library(tidyverse)
library(ggpubr)
library(hrep)

source("src/custom_hutchinson_knopoff_model.R")

theme_set(theme_pubr())

dissonance_curve <- function(bandwidth_distance) {
  (4 * bandwidth_distance * exp(1 - 4 * bandwidth_distance)) ^ 2
}

tibble(
  cbw_dist = seq(from = 0, to = 2, by = 0.001),
  "Original model" = dissonance_curve(cbw_dist),
  "Revised model" = revised_dissonance_factor(cbw_dist)
) %>% 
  pivot_longer(cols = ends_with("model")) %>%
  ggplot(aes(cbw_dist, value)) + 
  geom_vline(xintercept = 0.25, colour = "grey50", linetype = "dotted") +
  geom_line() + 
  scale_x_continuous("Distance between partials (critical bandwidths)") + 
  scale_y_continuous("Dissonance factor", breaks = -1:1) + 
  facet_wrap(~ name, ncol = 1, scales = "free_y") + 
  theme(
    strip.background = element_blank(),
    panel.spacing.y = unit(20, "pt")
  )

R.utils::mkdirs("output/illustrations")
ggsave("output/illustrations/hutchinson-knopoff-dissonance-curve.pdf", width = 5, height = 4)
 