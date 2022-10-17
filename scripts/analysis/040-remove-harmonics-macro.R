library(bootstrap)

if (FALSE) {
  inputs <- c(
    "output/batches/Harmonic dyads (5 equal harmonics)/models/Harrison & Pearce (2018) (revised).rds",
    "output/batches/Harmonic dyads (5 equal harmonics)/models/Harrison & Pearce (2018) (revised).csv",
    "output/batches/Harmonic dyads (no 3rd harmonic)/models/Harrison & Pearce (2018) (revised).rds",
    "output/batches/Harmonic dyads (no 3rd harmonic)/models/Harrison & Pearce (2018) (revised).csv",
    "output/batches/Pure dyads/models/Harrison & Pearce (2018) (revised).rds",
    "output/batches/Pure dyads/models/Harrison & Pearce (2018) (revised).csv"
  )
  
  for (i in inputs) suppressWarnings(file.remove(i))
  
  source("~/git/timbre-and-consonance-paper/scripts/analysis/010-run-model-batches.R")
}

source("scripts/analysis/015-figure-setup.R")
source("src/Timbre.R")

REMOVAL_PLOTS <- list()

########################################################################
# Main plots
########################################################################

REMOVAL_EXPERIMENTS <- list(
  "Pure\ntones" = EXPERIMENTS$`Pure dyads`,
  "No 3rd\nharmonic" = EXPERIMENTS$`Harmonic dyads (no 3rd harmonic)`,
  "5 equal\nharmonics" = EXPERIMENTS$`Harmonic dyads (5 equal harmonics)`
)

REMOVAL_PLOTS$Dyads <- PlotDyadModelsExperiments$new(
  experiments = REMOVAL_EXPERIMENTS,
  models = MODELS,
  dyad_measure_labels = "left",
  label_spectrum = FALSE,
  label_experiment = c("spectrum", "consonance"),
  label_dyad = FALSE,
  return_plot_list = TRUE,
  profile_row_heights = rep(c(2.25, 1, 1, 2.25), times = 3),
  spectrum_plot_direction = "horizontal",
  plot_bootstrap_peaks = TRUE,
  interval_breaks = 0:15
) 

REMOVAL_PLOTS$Dyads$plot$profiles

hack_scales <- function(f) {
  function(x) {
    orig <- f(x)
    new <- c(orig[1], orig[length(orig)])
    # Dirty hack to fix scales that don't display properly
    if (abs(new[1] - 1.6) < 0.05) new[1] <- 1.7
    new
  }
}

cowplot::plot_grid(
  REMOVAL_PLOTS$Dyads$plot$spectra +
    theme(plot.margin = unit(c(6, 20, 6, 20), "pt")),
  REMOVAL_PLOTS$Dyads$plot$profiles +
    ggh4x::facetted_pos_scales(
      y = list(
        measure == "Interference model" ~ scale_y_continuous(breaks = c(-0.1, -0.4), labels = function(x) -x),
        measure == "Harmonicity model" ~ scale_y_continuous(breaks = hack_scales(scales::extended_breaks(n = 2)))
      )
    ) +
    theme(
      # plot.margin = unit(c(6, 50, 6, 50), "pt"),
      panel.grid.major.x = element_line(colour = "grey95")
    ),
  labels = "AUTO",
  ncol = 1,
  rel_heights = c(1, 5.2),
  scale = 0.975
)

ggsave("output/plots/040-remove-harmonics-macro.pdf", width = 14, height = 15, scale = 0.6) 


########################################################################
# Supplementary plots
########################################################################

.experiments <- list(
  "Pure\ntones" = EXPERIMENTS$`Pure dyads`,
  "No 3rd\nharmonic" = EXPERIMENTS$`Harmonic dyads (no 3rd harmonic)`,
  "Harmonic\ntones" = EXPERIMENTS$`Harmonic dyads (5 equal harmonics)`
)

for (i in seq_along(.experiments)) {
  plots <- PlotDyadModelsExperiments$new(
    experiments = .experiments[i],
    models = MODELS,
    dyad_measure_labels = "left",
    label_spectrum = FALSE,
    label_experiment = c("spectrum", "consonance"),
    label_dyad = FALSE,
    return_plot_list = TRUE,
    profile_row_heights = c(2.25, 1, 1),
    spectrum_plot_direction = "horizontal",
    plot_bootstrap_peaks = TRUE
  )$plot
  
  plots$profiles <- 
    plots$profiles +
    scale_x_continuous("Interval (semitones)", breaks = 0:15) +
    reverse_interference_scales()
  
  plots$profiles
  
  R.utils::mkdirs("output/plots/presentation/harmonics")
  
  ggsave(
    paste0(
      "output/plots/presentation/harmonics/Remove dyad harmonics -- ", 
      .experiments[[i]]$label, 
      " -- profile",
      ".pdf"
    ),
    dpi = 250,
    width = 6,
    height = 4,
    plot = plots$profiles,
    scale = 1.2
  )
  
  ggsave(
    paste0(
      "output/plots/presentation/harmonics/Remove dyad harmonics -- ", 
      .experiments[[i]]$label, 
      " -- spectrum",
      ".pdf"
    ),
    dpi = 250,
    width = 4,
    height = 3,
    plot = plots$spectra + 
      scale_x_continuous("Interval (semitones)", limits = c(NA, 30)) +
      theme(strip.text = element_blank()),
    scale = 1.2
  )
  
  .video_path <- paste0(
    "output/videos/remove_dyad_harmonics__", 
    .experiments[[i]]$label, 
    ".mp4"
  ) %>% 
    tolower() %>% 
    gsub(" ", "_", .)
  
  .dyad_timbre <- .experiments[[i]]$timbre$full
  if (.dyad_timbre$label == "Pure") {
    .spectrum_frequency_ratios <- 1
    .spectrum_amplitudes <- 1
  } else {
    .n_harmonics <- .dyad_timbre$n_harmonics
    .spectrum_frequency_ratios <- 1:.n_harmonics
    .spectrum_amplitudes <- .dyad_timbre$harmonic_amplitudes
    
  }
  
  sweep_v_line_over_plot(
    plots$profiles,
    x_start = 0,
    x_end = 15,
    duration = 75,
    path = .video_path,
    fps = 30,
    dpi = 300,
    width = 7,
    height = 4,
    audio_components = c(
      static_complex_tone(
        midi_root = 60,
        spectrum_frequency_ratios = .spectrum_frequency_ratios,
        spectrum_amplitudes = .spectrum_amplitudes
      ),
      linear_freq_sweep_complex_tone(
        start_midi_root = 60,
        end_midi_root = 60 + 15,
        duration = 75,
        spectrum_frequency_ratios = .spectrum_frequency_ratios,
        spectrum_amplitudes = .spectrum_amplitudes
      )
    ),
    edit_plot = function(plot, interval, plot_data, pars, ...) {
      points <-
        plot_data %>%
        group_by(measure) %>%
        summarise(
          experiment = unique(experiment),
          value = approx(
            x = interval,
            y = value,
            xout = !!interval
          )$y,
          interval = !!interval
        )
      plot +
        geom_point(
          aes(interval, value),
          alpha = 0.4,
          data = points,
          inherit.aes = FALSE,
          size = 4,
          colour = "blue"
        ) +
        coord_cartesian(clip = 'off')
    }
  )
}

ggsave(
  "output/plots/presentation/harmonics/remove-harmonics-spectra.pdf",
  plot = REMOVAL_PLOTS$Dyads$plot$spectra + facet_wrap(~ timbre_label, ncol = 1, ),
  width = 5, 
  height = 3
)

########################################################################
# Audio
########################################################################

for (i in seq_along(REMOVAL_EXPERIMENTS)) {
  exp_label <- names(REMOVAL_EXPERIMENTS)[i] %>% gsub("\n", " ", .)
  exp <- REMOVAL_EXPERIMENTS[[i]]
  timbre <- exp$timbre$full
  timbre$sparse_fr_spectrum(60, coherent = TRUE) %>% 
    wave() %>% 
    hrep::filter_adsr(attack = 0.05, decay = 0.05, sustain = 0.9, hold = 0.6, release = 0.25) %>% 
    hrep::save_wav(
      glue::glue("output/audio-examples/{exp_label}.wav")
    )
}


