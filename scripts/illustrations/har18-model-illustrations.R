library(tidyverse)
library(ggpubr)
library(hrep)

theme_set(theme_pubr())

x_axis_pitch_class <- function(x_axis_label = "Pitch class") {
  scale_x_continuous(
    x_axis_label, breaks = seq(from = 0, to = 12, by = 2),
    sec.axis = sec_axis(
      trans = identity,
      breaks = seq(from = 0, to = 12, by = 1),
      labels = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B", "C")
    )
  )
}

plot_templates <-
  list(
    "C pitch template" = 0,
    "D pitch template" = 2,
    "E pitch template" = 4,
    "F pitch template" = 5
  ) %>% 
  imap_dfr(function(root, template_name) {
    hrep::milne_pc_spectrum(
      hrep::pc_set(root),
      array_dim = 1200,
      num_harmonics = 12,
      rho = 0.75,
      sigma = 6.83
    ) %>% 
      as_tibble() %>% 
      mutate(template_name = template_name)
  }) %>% 
  ggplot(aes(x, y)) + 
    geom_line() + 
    x_axis_pitch_class() + 
    scale_y_continuous("Weight") +
    geom_line() + 
    facet_wrap(~ template_name)
  
plot_har_18_chords <- function(x) {
  chord_spectra <- 
    x %>% 
    map(pi_chord) %>% 
    map(milne_pc_spectrum)
  
  f0_profiles <- 
    chord_spectra %>% 
    map(~ har18::sweep_harmonic_template(
      .,
      num_harmonics = 12,
      rho = 0.75,
      sigma = 6.83
    ))
  
  bind_rows(
    chord_spectra %>% imap_dfr(~ as_tibble(.x) %>% mutate(chord = .y, type = "Chord spectrum")),
    f0_profiles %>% imap_dfr(~ as_tibble(.x) %>% mutate(chord = .y, type = "Virtual pitches"))
  ) %>% 
    mutate(
      type = factor(type, levels = unique(type))
    ) %>% 
    ggplot(aes(x, y)) +
    x_axis_pitch_class() +
    scale_y_continuous("Strength") +
    geom_line() + 
    facet_grid(type ~ chord, scales = "free")
}
  
cowplot::plot_grid(
  plot_templates + 
    theme(
      axis.text.x.top = element_text(size = 8),
      plot.margin = unit(c(6, 6, 20, 6), "pt"),
      strip.background = element_rect(colour = NA)
    ),
  plot_har_18_chords(list(
    "D G# (tritone)" = c(62, 68),
    "D A (perfect fifth)" = c(62, 69)
  )) + theme(
    axis.text.x.top = element_text(size = 8),
    plot.margin = unit(c(20, 6, 6, 6), "pt"),
    strip.background = element_rect(colour = NA)
  ),
  ncol = 1,
  # rel_heights = c(1.5, 2),
  labels = "AUTO"
)

R.utils::mkdirs("output/illustrations")
ggsave("output/illustrations/har18-explanation.pdf", width = 14, height = 14, scale = 0.6)
