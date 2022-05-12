library(tidyverse)
library(ggpubr)

theme_set(theme_pubr())

raw <- 
  read_csv(
    "input/common-scales.csv",
    col_types = cols(
      Pythagorean = col_double(),
      `Just intonation` = col_double(),
      `Quarter-comma meantone` = col_double(),
      `Equal temperament` = col_double()
    )
)

scales <- names(raw)

raw %>% 
  pivot_longer(cols = everything(), names_to = "scale", values_to = "cents") %>% 
  mutate(
    scale = factor(scale, levels = scales)
  ) %>% 
  na.omit() %>% 
  ggplot() + 
  scale_x_continuous("Interval (cents)", breaks = seq(from = 0, to = 1200, by = 100)) +
  geom_vline(aes(xintercept = cents, color = scale)) + 
  scale_color_brewer(palette = "Dark2") + 
  # scale_color_viridis_d(option = "A", begin = 0.8, end = 0.2) + 
  facet_grid(scale ~ ., switch = "y") + 
  theme(
    # panel.grid.major = element_line(color = "black"),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    axis.text = element_text(size = 8),
    axis.line.y = element_blank(),
    legend.position = "none", 
  )

R.utils::mkdirs("output/illustrations")
ggsave("output/illustrations/common-scales.pdf", width = 6, height = 2)
