source("scripts/analysis/015-figure-setup.R")

TRIAD_EXPERIMENTS <- list(
  "Stretched" = EXPERIMENTS$`Stretched triads (3 dB roll-off)`,
  "Harmonic" = EXPERIMENTS$`Harmonic triads (3 dB roll-off)`,
  "Compressed" = EXPERIMENTS$`Compressed triads (3 dB roll-off)`
)

TRIAD_PLOTTING <- PlotTriadModelsExperiments$new(
  experiments = TRIAD_EXPERIMENTS,
  models = MODELS,
  plot_spectra = FALSE,
  label_experiment = "consonance",
  consonance_label = "Pleasantness    ",
  ablines = "stretching",
  profile_legend = TRUE,
  legend_position = "right", 
  return_plot_list = TRUE,
  interval_breaks = 1:8,
  reference_spectrum = 2
)

cowplot::plot_grid(
  cowplot::plot_grid(
    NULL,
    cowplot::plot_grid(
      plot_viridis_legend("viridis"),
      plot_viridis_legend("inferno", reverse = TRUE),
      plot_viridis_legend("mako"),
      nrow = 1
    ),
    NULL,
    nrow = 1,
    rel_widths = c(0.85, 4, 0.45)
  ),
  TRIAD_PLOTTING$plot[[1]] +
    theme(
      aspect.ratio = 1,
      strip.placement = "bottom",
      legend.position = "none"
    ),
  ncol = 1,
  rel_heights = c(1, 7.5)
)

ggsave("062-stretched-triads.pdf", path = PLOT_DIR, width = 14, height = 14, scale = 0.6)

for (timbre in c("Stretched", "Harmonic", "Compressed")) {
  .triad_experiments <- 
    EXPERIMENTS[paste0(timbre, " triads (3 dB roll-off)")] %>% 
    set_names(timbre)
  
  .triad_plots <- PlotTriadModelsExperiments$new(
    experiments = .triad_experiments,
    models = MODELS,
    plot_spectra = FALSE,
    label_experiment = "none",
    consonance_label = "Pleasantness",
    ablines = "stretching",
    profile_legend = TRUE
  )
  
  ggsave(
    paste0(timbre, " triads (3 dB roll-off).png"),
    plot = .triad_plots$plot,
    path = "output/plots/presentation/stretching",
    dpi = 250,
    width = 11, 
    height = 4,
    scale = 0.8
  )
  
  measures <- c("Participants", names(MODELS))
  
  for (m in measures) {
    .triad_plots$plot_profiles(
      .triad_plots$profile_plot_data,
      select_measures = m
    )
    ggsave(
      paste0(timbre, " triads (3 dB roll-off) -- ", m, ".png"),
      path = "output/plots/presentation/stretching",
      dpi = 250,
      width = 6, 
      height = 5
    )
  }
  
  TRIAD_PLOTTING$plot_timbres(
    experiments = TRIAD_EXPERIMENTS, 
    select_timbre = timbre
  ) +
    theme(strip.text.x = element_blank())
  
  ggsave(
    paste0(timbre, " tones (3 dB roll-off) -- spectrum.png"),
    path = "output/plots/presentation/stretching",
    dpi = 250,
    width = 5, 
    height = 4,
    scale = 0.8
  )
}
