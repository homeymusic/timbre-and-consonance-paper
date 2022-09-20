if (FALSE) {
  inputs <- c(
    "output/batches/Bonang dyads/models/Hutchinson & Knopoff (1978) (revised).rds",
    "output/batches/Bonang dyads/models/Hutchinson & Knopoff (1978) (revised).csv"
  )
  
  for (i in inputs) suppressWarnings(file.remove(i))
  
  source("~/git/timbre-and-consonance-paper/scripts/analysis/010-run-model-batches.R")
}

source("scripts/analysis/015-figure-setup.R")

########################################################################
# Plots
########################################################################

BONANG <- 
  PlotDyadModelsExperiments$new(
    experiments = list(
      "Bonang" = EXPERIMENTS$`Bonang dyads`
    ),
    models = MODELS,
    label_spectrum = FALSE,
    label_dyad = FALSE,
    label_experiment = c("spectrum", "consonance"),
    dyad_measure_labels = "left",
    interval_breaks = c(0, 4, 8, 12),
    rel_widths = c(4, 4),
    return_plot_list = TRUE,
    debug = FALSE,
    profile_row_heights = c(3, 1, 1),
    spectrum_plot_direction = "horizontal",
    plot_bootstrap_peaks = TRUE
  )

BONANG$plot$spectra <- 
  BONANG$plot$spectra + 
  theme(
    strip.text = element_blank(), 
    strip.background = element_blank(), 
    plot.margin = unit(c(6, 150, 6, 150), units = "pt")
  )

BONANG$plot$profiles <- 
  BONANG$plot$profiles +
  facet_grid(measure ~ "", switch = "y", scales = "free_y") +
  ggh4x::force_panelsizes(rows = c(3, 1.5, 1.5)) +
  scale_x_continuous("Interval (semitones)", breaks = seq(from = 0, to = 15, by = 1), minor_breaks = 0:15) +
  geom_vline(xintercept = 2.4, alpha = 0.6, linetype = "dotted") +
  geom_vline(xintercept = 2.4 * 2, alpha = 0.6, linetype = "dotted") +
  geom_vline(xintercept = 2.4 * 3, alpha = 0.6, linetype = "dotted") +
  geom_vline(xintercept = 2.4 * 4, alpha = 0.6, linetype = "dotted") +
  geom_vline(xintercept = 2.4 * 5, alpha = 0.6, linetype = "dotted") +
  geom_vline(xintercept = 2.4 * 6, alpha = 0.6, linetype = "dotted") +
  reverse_interference_scales() +
  theme(
    strip.placement = "outside"
  )

cowplot::plot_grid(
  BONANG$plot$spectra,
  BONANG$plot$profiles,
  ncol = 1,
  labels = "AUTO",
  # rel_widths = c(2, 5)
  rel_heights = c(2, 5)
)

ggsave("025-bonang-dyads.pdf", path = PLOT_DIR, width = 14, height = 10, scale = 0.6)

########################################################################
# Video
########################################################################

sweep_v_line_over_plot(
  plot = BONANG$plot$profiles,
  x_start = 0, 
  x_end = 15, 
  duration = 75, 
  path = "output/videos/bonang-dyads.mp4", 
  fps = 30,
  dpi = 300,
  audio_components = c(
    static_basic_harmonic_complex_tone(
      midi = 60, 
      amplitude = 1, 
      n_harmonics = 5L, 
      decay_dB_per_octave = 0,
      octave_definition = 2
    ),
    linear_freq_sweep_bonang_tone(
      start_midi_root = 60, 
      end_midi_root = 75, 
      duration = 75, 
      amplitude = 1
    )
  )
)