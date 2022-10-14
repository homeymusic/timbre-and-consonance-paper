if (FALSE) {
  inputs <- c(
    "output/batches/Stretched dyads (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).csv",
    "output/batches/Stretched dyads (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).rds",
    "output/batches/Compressed dyads (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).csv",
    "output/batches/Compressed dyads (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).rds",
    "output/batches/Harmonic dyads (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).csv",
    "output/batches/Harmonic dyads (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).rds"
  )
  
  for (i in inputs) suppressWarnings(file.remove(i))
  
  source("~/git/timbre-and-consonance-paper/scripts/analysis/010-run-model-batches.R")
}

source("scripts/analysis/015-figure-setup.R")

########################################################################
# Plots for the paper
########################################################################

DYAD_EXPERIMENTS <- list(
  "Stretched" = EXPERIMENTS$`Stretched dyads (3 dB roll-off)`,
  "Harmonic" = EXPERIMENTS$`Harmonic dyads (3 dB roll-off)`,
  "Compressed" = EXPERIMENTS$`Compressed dyads (3 dB roll-off)`
)

DYAD_PLOTS <- PlotDyadModelsExperiments$new(
  experiments = DYAD_EXPERIMENTS,
  models = MODELS,
  ablines = "stretching",
  label_dyad = FALSE,
  label_spectrum = FALSE,
  label_experiment = c("spectrum", "consonance"),
  dyad_measure_labels = "left",
  consonance_label = "Pleasantness",
  reference_spectrum = 2,
  profile_row_heights = rep(c(2.5, 1, 1, 2.5), times = 3),
  return_plot_list = TRUE,
  spectrum_plot_direction = "horizontal",
  plot_bootstrap_peaks = TRUE,
  interval_breaks = 0:15
)$plot

cowplot::plot_grid(
  DYAD_PLOTS$spectra +
    theme(plot.margin = unit(c(6, 20, 6, 20), "pt")),
  DYAD_PLOTS$profiles +
    ggh4x::facetted_pos_scales(
      y = list(
        measure == "Interference model" ~ scale_y_continuous(breaks = c(-0.1, -0.4), labels = function(x) -x),
        measure == "Harmonicity model" ~ scale_y_continuous(breaks = scales::extended_breaks(n = 3))
      )
    ) +
    theme(
      panel.grid.major.x = element_line(colour = "grey95")
      # axis.text.y = element_text(size = 5)
    ),
  labels = "AUTO",
  ncol = 1,
  rel_heights = c(1, 5)
  # scale = 0.975
)

ggsave("output/plots/020-stretching-dyads.pdf", width = 14, height = 14, scale = 0.6) 


########################################################################
# Plots for presentations, supplementary etc.
########################################################################

R.utils::mkdirs("output/plots/presentation/stretching")

DYAD_PLOTS$spectra
ggsave(
  "stretched spectra.png", 
  path = "output/plots/presentation/stretching",
  dpi = 250,
  width = 7,
  height = 2.5,
  scale = 1.1
)

for (timbre in c("Stretched", "Harmonic", "Compressed")) {
  .dyad_experiments <- 
    EXPERIMENTS[paste0(timbre, " dyads (3 dB roll-off)")] %>% 
    set_names(timbre)
  
  .dyad_plots <- PlotDyadModelsExperiments$new(
    experiments = .dyad_experiments,
    models = MODELS,
    ablines = "stretching",
    label_dyad = FALSE,
    label_spectrum = FALSE,
    label_experiment = c("spectrum", "consonance"),
    dyad_measure_labels = "left",
    consonance_label = "Pleasantness",
    reference_spectrum = 1,
    profile_row_heights = c(3, 1, 1),
    return_plot_list = TRUE,
    spectrum_plot_direction = "horizontal",
    plot_bootstrap_peaks = TRUE,
    interval_breaks = 0:15
  )
  .dyad_plots$plot$profiles
  
  .dyad_profile <- .dyad_plots$plot$profiles + 
    ggh4x::facetted_pos_scales(
      y = list(
        NULL,
        # This bit negates the interference scale (the middle row) so that it plots 
        # interference rather than pleasantness.
        # Not sure why reverse_interference_scales() fails here
        # but this does the same conceptually.
        # Be careful with this code, as it relies on the order of the three 
        # panels (top = participants, middle = interference, bottom = harmonicity)
        scale_y_continuous(breaks = scales::extended_breaks(n = 4), labels = function(x) -x),
        NULL
      )
    ) +
    scale_x_continuous("Interval (semitones)", breaks = 0:15) 

  ggsave(
    plot = .dyad_profile,
    filename = paste0(timbre, " dyads (3 dB roll-off).png"),
    path = "output/plots/presentation/stretching",
    dpi = 250,
    width = 6, 
    height = 6
  )
  
  R.utils::mkdirs("output/videos")
  .dyad_timbre <- .dyad_experiments[[1]]$timbre$full
  .dyad_video_path <- glue::glue("output/videos/{tolower(timbre)}-dyads.mp4")

  sweep_v_line_over_plot(
    .dyad_profile,
    x_start = 0,
    x_end = 15,
    duration = 75, 
    path = .dyad_video_path,
    fps = 30,
    dpi = 300,
    width = 7,
    height = 4,
    audio_components = c(
      static_basic_harmonic_complex_tone(
        midi = 60,
        amplitude = 1,
        n_harmonics = .dyad_timbre$n_harmonics,
        decay_dB_per_octave = .dyad_timbre$decay_dB_per_octave,
        octave_definition = .dyad_timbre$octave_definition
      ),
      linear_freq_sweep_basic_harmonic_complex_tone(
        start_midi_root = 60,
        end_midi_root = 75,
        duration = 75,
        amplitude = 1,
        n_harmonics = .dyad_timbre$n_harmonics,
        decay_dB_per_octave = .dyad_timbre$decay_dB_per_octave,
        octave_definition = .dyad_timbre$octave_definition
      )
    )
  )
}

########################################################################
# Audio examples
########################################################################

R.utils::mkdirs("output/audio-examples")

for (i in seq_along(DYAD_EXPERIMENTS)) {
  exp_label <- names(DYAD_EXPERIMENTS)[i]
  exp <- DYAD_EXPERIMENTS[[i]]
  timbre <- exp$timbre$full
  timbre$tone_sparse_fr_spectrum(60) %>% 
    wave() %>% 
    hrep::filter_adsr(attack = 0.05, decay = 0.05, sustain = 0.9, hold = 0.6, release = 0.25) %>% 
    hrep::save_wav(
      glue::glue("output/audio-examples/{exp_label}.wav")
    )
}
