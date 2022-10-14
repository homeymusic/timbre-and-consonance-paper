source("scripts/analysis/015-figure-setup.R")

ROLL_OFF_EXPERIMENTS <- list(
  "12 dB/octave roll-off" = EXPERIMENTS$`12 dB roll-off (harmonic dyads)`,
  "7 dB/octave roll-off" = EXPERIMENTS$`7 dB roll-off (harmonic dyads)`,
  "2 dB/octave roll-off" = EXPERIMENTS$`2 dB roll-off (harmonic dyads)`
)

########################################################################
# Plots
########################################################################
  
ROLL_OFF <- 
  PlotDyadModelsExperimentsRollOff$new(
    experiments = ROLL_OFF_EXPERIMENTS,
    models = MODELS %>% set_names(., gsub("\n", " ", names(.))),
    label_spectrum = FALSE,
    label_dyad = FALSE,
    label_experiment = "spectrum",
    profile_legend = FALSE,
    rel_widths = c(3.5, 6), #, 1.75),
    debug = FALSE,
    return_plot_list = TRUE,
    timbre_alphas = seq(from = 0.4, to = 0.8, length.out = 3),
    spectrum_plot_direction = "horizontal",
    interval_breaks = 0:15,
    plot_bootstrap_peaks = TRUE,
    profile_row_heights = c(2.25, 1, 1),
  )

ROLL_OFF$plot$spectra <-
  ROLL_OFF$plot$spectra +
  scale_y_continuous("Amplitude")

ggsave(
  "output/plots/presentation/roll-off-spectra.pdf", 
  plot = ROLL_OFF$plot$spectra,
  width = 7, height = 3
)

ROLL_OFF$plot$profiles <- 
  ROLL_OFF$plot$profiles + 
  reverse_interference_scales(breaks = waiver()) +
  theme(legend.position = "right") +
  guides(alpha = guide_legend(override.aes = list(color = NA))) +
  scale_alpha_manual(
    "Roll-off", 
    values = ROLL_OFF$timbre_alphas, 
    labels = c("12 dB/octave", "7 dB/octave", "2 dB/octave")
  )

cowplot::plot_grid(
  ROLL_OFF$plot[[1]] +
    theme(
      panel.spacing = unit(2, "lines"),
      plot.margin = unit(c(2, 2, 12, 2), "pt")
    ),
  ROLL_OFF$plot[[2]] +
    # reverse_interference_scales(breaks = waiver()) + 
    ggh4x::facetted_pos_scales(
      y = list(
        measure == "Interference model" ~ scale_y_continuous(breaks = c(-0.1, -0.3, -0.5), labels = function(x) -x)
        # measure == "Harmonicity model" ~ scale_y_continuous(breaks = scales::extended_breaks(n = 3))
      )
    ) +
    theme(plot.margin = unit(c(2, 2, 12, 2), "pt")),
  ncol = 1,
  rel_heights = c(2, 7),
  labels = c("A", "B")
  # scale = 0.9
)

ggsave("030-roll-off-profiles.pdf", path = PLOT_DIR, width = 14, height = 12, scale = 0.55)


########################################################################
# Statistics for behavioral data
########################################################################

dyads_2db <- EXPERIMENTS$`2 dB roll-off (harmonic dyads)`
dyads_7db <- EXPERIMENTS$`7 dB roll-off (harmonic dyads)`
dyads_12db <- EXPERIMENTS$`12 dB roll-off (harmonic dyads)`

data_2db <- dyads_2db$behaviour$full$profile %>% mutate(roll_off_bin = "2db")
data_7db <- dyads_7db$behaviour$full$profile %>% mutate(roll_off_bin = "7db")
data_12db <- dyads_12db$behaviour$full$profile %>% mutate(roll_off_bin = "12db")

# We perform a GAM analysis to quantify how much variance 
# can be explained by main effects of roll-off and of interval.
# This also gives us a standardised regression coefficient for roll-off.

combined_data <- rbind(data_2db, data_7db, data_12db) %>%
  mutate(roll_off_bin = as.factor(roll_off_bin))

gam_mod_1 <- gam(
  scale(rating) ~ scale(roll_off_bin) + s(interval, k=200),
  data = combined_data |> 
    mutate(
      roll_off_bin = gsub("db", "", roll_off_bin) |> as.numeric()
    ),
  method = "GCV.Cp"
)
summary(gam_mod_1)
plot(gam_mod_1, all.terms = TRUE, pages = 1)

########################################################################
# Statistics for computational models
########################################################################

# We perform a series of GAM analyses to derive standardized regression
# coefficients for spectral roll-off for different computational models.
# This provides a quantification of how sensitive each model is 
# to spectral roll-off.

roll_off_models <- 
  MODELS_ALL |> 
  map(function(model) {
    df_2db <- dyads_2db$models[[model]]$summary |> mutate(roll_off_bin = "2db")
    df_7db <- dyads_7db$models[[model]]$summary |> mutate(roll_off_bin = "7db")
    df_12db <- dyads_12db$models[[model]]$summary |> mutate(roll_off_bin = "12db")
    
    df <- 
      bind_rows(df_2db, df_7db, df_12db) |> 
      mutate(roll_off_bin = as.factor(roll_off_bin))
    
    mod <- 
      gam(
        scale(output) ~ 
          scale(roll_off_bin) +
          s(interval, k=200), 
        data = df |> 
          mutate(
            roll_off_bin = gsub("db", "", roll_off_bin) |> as.numeric()
          ),
        method = "GCV.Cp"
      )
    list(
      mod = mod,
      summary = summary(mod),
      roll_off_beta = coef(mod)["scale(roll_off_bin)"]
    )
  })

map_dbl(roll_off_models, "roll_off_beta")

########################################################################
# Videos
########################################################################

base_plot <- ROLL_OFF$plot$profiles
spectrum_plot <- ROLL_OFF$plot[[1]]

for (.exp in ROLL_OFF_EXPERIMENTS) {
  R.utils::mkdirs("output/videos")
  .dyad_timbre <- .exp$timbre$full
  .roll_off <- .dyad_timbre$decay_dB_per_octave
  
  .video_path <- glue::glue("output/videos/{.roll_off}-dB-roll-off-dyads.mp4")
  
  sweep_v_line_over_plot(
    base_plot, 
    x_start = 0, 
    x_end = 15, 
    duration = 75, 
    path = .video_path, 
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
    ), 
    edit_plot = function(plot, interval, plot_data, pars, ...) {
      points <- 
        plot_data %>% 
        filter(
          experiment == glue::glue("{pars$roll_off} dB/octave roll-off")
        ) %>% 
        group_by(measure) %>% 
        summarise(
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
    },
    pars = list(
      roll_off = .roll_off,
      spectrum_plot = spectrum_plot
    )
  )
}

########################################################################
# Generate audio examples
########################################################################

for (roll_off in c(2, 7, 12)) {
  exp_label <- glue::glue("{roll_off} dB/octave roll-off")
  timbre <- ROLL_OFF_EXPERIMENTS[[exp_label]]$timbre$full
  timbre$tone_sparse_fr_spectrum(60) %>% 
    wave() %>% 
    hrep::filter_adsr(attack = 0.05, decay = 0.05, sustain = 0.9, hold = 0.6, release = 0.25) %>% 
    hrep::save_wav(
      glue::glue("output/audio-examples/{roll_off} dB per octave.wav")
    )
}
