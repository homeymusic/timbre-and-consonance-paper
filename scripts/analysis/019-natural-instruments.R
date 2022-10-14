source("scripts/analysis/015-figure-setup.R")

HARMONIC_DYADS <- EXPERIMENTS$`Harmonic dyads (3 dB roll-off)`

NATURAL_INSTRUMENTS <- 
  PlotDyadModelsExperiments$new(
    experiments = list(
      "Flute" = EXPERIMENTS$`Flute dyads`,
      "Guitar" = EXPERIMENTS$`Guitar dyads`,
      "Piano" = EXPERIMENTS$`Piano dyads`
    ),
    models = MODELS,
    label_spectrum = FALSE,
    label_dyad = FALSE,
    label_experiment = c("spectrum", "consonance"),
    dyad_measure_labels = "left",
    rel_widths = c(4, 4),
    return_plot_list = TRUE,
    debug = FALSE,
    profile_row_heights = rep(c(3, 1, 1, 3), times = 3),
    spectrum_plot_direction = "horizontal",
    plot_bootstrap_peaks = TRUE,
    interval_breaks = 0:15
  )

INSTRUMENT_NAMES <- NATURAL_INSTRUMENTS$profile_plot_data$timbre %>% unique()

########################################################################
# Main plot (natural instrument dyads) ####
########################################################################

cowplot::plot_grid(
  NATURAL_INSTRUMENTS$plot$spectra + 
    scale_x_continuous("Interval (semitones)", breaks = c(0, 20, 40)) + 
    theme(
      plot.margin = unit(c(6, 20, 6, 20), "pt")
    ),
  NATURAL_INSTRUMENTS$plot$profiles + 
    # reverse_interference_scales() +
    ggh4x::facetted_pos_scales(
      y = list(
        measure == "Interference model" ~ scale_y_continuous(breaks = c(-0.1, -0.4), labels = function(x) -x),
        measure == "Harmonicity model" ~ scale_y_continuous(breaks = scales::extended_breaks(n = 3))
      )
    ) +
    theme(
      panel.grid.major.x = element_line(colour = "grey95"),
    ),
  ncol = 1,
  rel_heights = c(1, 4.5),
  labels = c("A", "B"),
  scale = 0.975
)

ggsave("output/plots/019-natural-instruments-with-models.pdf", width = 14, height = 14, scale = 0.6) 

########################################################################
# Similar plot but without models ####
########################################################################

NATURAL_INSTRUMENTS$profile_plot_data %>% 
  filter(measure == "Participants") %>% 
  ggplot(aes(interval, value, ymin = ymin, ymax = ymax)) + 
  geom_line(
      mapping = aes(interval, rating),
      data = HARMONIC_DYADS$behaviour$summary, 
      inherit.aes = FALSE,
      linetype = "dotted",
  ) +
  geom_ribbon(colour = NA, fill = "black", alpha = 0.15) +
  geom_line() + 
  geom_rect(
    data = NATURAL_INSTRUMENTS$bootstrapped_peaks,
    aes(
      xmin = avg_interval - 1.96 * se_interval,
      xmax = avg_interval + 1.96 * se_interval,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.1,
    colour = "pink"
  ) +
  geom_point(
    aes(actual_interval, actual_value),
    data = NATURAL_INSTRUMENTS$bootstrapped_peaks,
    inherit.aes = FALSE,
    colour = "red"
  ) +
  scale_x_continuous("Interval (semitones)", breaks = 0:15) + 
  scale_y_continuous("Pleasantness") +
  facet_wrap(~ timbre, ncol = 1) +
  theme(
    strip.background = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    panel.grid.major.x = element_line(colour = "grey95")
  )

ggsave(
  "output/plots/019-natural-instruments-no-models.pdf",
  width = 14,
  height = 10,
  scale = 0.6
)

########################################################################
# How well do the natural instruments correlate with the baseline tone? ####
########################################################################

cor_methods <- list(
  pearson = "pearson",
  spearman = "spearman"
)

map(cor_methods, function(method) {
  INSTRUMENT_NAMES %>% map_dbl(function(instrument) {
    NATURAL_INSTRUMENTS$profile_plot_data %>% 
      filter(timbre == instrument, measure == "Participants") %>% 
      pull(value) %>%
      cor(HARMONIC_DYADS$behaviour$summary$rating, method = method)
  }) %>% 
    mean()
})


########################################################################
# Supplementary/presentation plots ####
########################################################################

R.utils::mkdirs("output/plots/presentation/instruments")

for (timbre in unique(NATURAL_INSTRUMENTS$profile_plot_data$timbre)) {
  NATURAL_INSTRUMENTS$profile_plot_data %>%
    filter(timbre == !!timbre) %>%
    ggplot(aes(interval, value, ymin = ymin, ymax = ymax, colour = colour)) +
    geom_ribbon(colour = NA, fill = "lightgrey") +
    geom_line() +  
    scale_x_continuous("Interval (semitones)", breaks = 0:15) + 
    scale_y_continuous("Pleasantness") +
    scale_colour_identity(NULL) +
    facet_wrap(
      ~ measure,
      ncol = 1
    ) + 
    theme(legend.direction = "vertical",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          panel.grid.major.x = element_line(),
          strip.text.y.left = element_text(angle = 0),
          strip.text.y.right = element_text(angle = 0)) + 
    ggh4x::force_panelsizes(
      rows = c(3, 1, 1)
    )
  ggsave(
    paste0(timbre, " - dyads.png"), 
    dpi = 250, path = "output/plots/presentation/instruments",
    width = 6, 
    height = 6
  )
}

ggsave(
  "output/plots/presentation/instruments/natural-instruments-spectra.pdf",
  width = 7, 
  height = 3,
  plot = NATURAL_INSTRUMENTS$plot$spectra
)
