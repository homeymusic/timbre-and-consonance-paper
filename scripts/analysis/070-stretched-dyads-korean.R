source("scripts/analysis/015-figure-setup.R")

US_DYAD_EXPERIMENTS <- list(
  "Stretched" = EXPERIMENTS$`Stretched dyads (3 dB roll-off)`,
  "Harmonic" = EXPERIMENTS$`Harmonic dyads (3 dB roll-off)`,
  "Compressed" = EXPERIMENTS$`Compressed dyads (3 dB roll-off)`
)

US_DYAD_PLOTS <- PlotDyadModelsExperiments$new(
  experiments = US_DYAD_EXPERIMENTS,
  models = MODELS,
  ablines = "stretching",
  label_dyad = FALSE,
  label_spectrum = FALSE,
  label_experiment = c("spectrum", "consonance"),
  dyad_measure_labels = "left",
  consonance_label = "Pleasantness",
  reference_spectrum = 2,
  profile_row_heights = c(2.5, 1, 1, 2.5, 1, 1, 2.5, 1, 1),
  return_plot_list = TRUE,
  spectrum_plot_direction = "horizontal",
  plot_bootstrap_peaks = TRUE,
  interval_breaks = 0:15
)

KOREAN_DYAD_EXPERIMENTS <- list(
  "Stretched" = EXPERIMENTS$`Stretched dyads (3 dB roll-off) (Korean)`,
  "Harmonic" = EXPERIMENTS$`Harmonic dyads (3 dB roll-off) (Korean)`,
  "Compressed" = EXPERIMENTS$`Compressed dyads (3 dB roll-off) (Korean)`
)

# Number of participants
map_int(KOREAN_DYAD_EXPERIMENTS, ~ .$behaviour$full$num_participants) %>%
  print() %>% 
  sum()

# Slopes
slopes <- 
  list(
    Korean = KOREAN_DYAD_EXPERIMENTS,
    US = US_DYAD_EXPERIMENTS
  ) %>% map(function(dataset) {
    c("Stretched", "Harmonic", "Compressed") %>% 
      set_names(., .) %>% 
      map(function(timbre) {
        dataset[[timbre]]$behaviour$full$slope
      })
  })

slopes$Korean$Stretched

KOREAN_DYAD_EXPERIMENTS$Stretched$behaviour$full$slope

KOREAN_DYAD_PLOTS <- PlotDyadModelsExperiments$new(
  experiments = KOREAN_DYAD_EXPERIMENTS,
  models = MODELS,
  ablines = "stretching",
  label_dyad = FALSE,
  label_spectrum = FALSE,
  label_experiment = c("spectrum", "consonance"),
  dyad_measure_labels = "left",
  consonance_label = "Pleasantness",
  reference_spectrum = 2,
  profile_row_heights = c(2.5, 1, 1, 2.5, 1, 1, 2.5, 1, 1),
  return_plot_list = TRUE,
  spectrum_plot_direction = "horizontal",
  plot_bootstrap_peaks = TRUE,
  interval_breaks = 0:15
)

TIMBRE_LEVELS <- c(
  "Stretched",
  "Harmonic",
  "Compressed"
)

KOREAN_DYAD_PLOTS$bootstrapped_peaks$timbre <-
  factor(KOREAN_DYAD_PLOTS$bootstrapped_peaks$timbre, levels = TIMBRE_LEVELS)

US_DYAD_PLOTS$bootstrapped_peaks$timbre <-
  factor(US_DYAD_PLOTS$bootstrapped_peaks$timbre, levels = TIMBRE_LEVELS)

KOREAN_DYAD_PLOTS$profile_plot_data %>% 
  filter(measure == "Participants") %>% 
  mutate(timbre = factor(timbre, levels = TIMBRE_LEVELS)) %>% 
  ggplot(aes(interval, value, ymin = ymin, ymax = ymax)) + 
  geom_line(
    mapping = aes(interval, value),
    data = US_DYAD_PLOTS$profile_plot_data %>% 
      filter(measure == "Participants") %>% 
      mutate(timbre = factor(timbre, levels = TIMBRE_LEVELS)),
    inherit.aes = TRUE,
    linetype = "dotted",
  ) +
  geom_point(
    aes(actual_interval, actual_value),
    data = US_DYAD_PLOTS$bootstrapped_peaks,
    inherit.aes = FALSE,
    colour = "black",
    fill = "white",
    shape = 21
  ) +
  geom_ribbon(colour = NA, fill = "black", alpha = 0.15) +
  geom_line() + 
  geom_rect(
    data = KOREAN_DYAD_PLOTS$bootstrapped_peaks,
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
    data = KOREAN_DYAD_PLOTS$bootstrapped_peaks,
    inherit.aes = FALSE,
    colour = "red"
  ) +
  scale_x_continuous("Interval (semitones)", breaks = 0:15) + 
  scale_y_continuous("Pleasantness") +
  facet_wrap(~ timbre, ncol = 1) +
  theme(
    strip.background = element_blank(),
    panel.spacing = unit(1.5, "lines")
    # panel.grid.major.x = element_line(colour = "grey95")
  )

ggsave("070-stretched-dyads-korean.pdf", path = PLOT_DIR, width = 14, height = 10, scale = 0.6)

for (timbre in c("Stretched", "Harmonic", "Compressed")) {
  .dyad_experiments <- 
    KOREAN_DYAD_EXPERIMENTS[timbre] %>% 
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
  
  
  R.utils::mkdirs("output/videos")
  .dyad_timbre <- .dyad_experiments[[1]]$timbre$full
  .dyad_video_path <- glue::glue("output/videos/korean-{tolower(timbre)}-dyads.mp4")
  
  sweep_v_line_over_plot(
    .dyad_profile,
    x_start = 0,
    x_end = 15,
    duration = 75, 
    path = .dyad_video_path,
    fps = 30,
    dpi = 300,
    width = 8,
    height = 5,
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
