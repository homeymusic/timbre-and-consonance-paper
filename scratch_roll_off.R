inputs <- c(
  "output/batches/2 dB roll-off (harmonic dyads)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/2 dB roll-off (harmonic dyads)/models/Hutchinson & Knopoff (1978) (revised).rds",
  "output/batches/7 dB roll-off (harmonic dyads)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/7 dB roll-off (harmonic dyads)/models/Hutchinson & Knopoff (1978) (revised).rds",
  "output/batches/12 dB roll-off (harmonic dyads)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/12 dB roll-off (harmonic dyads)/models/Hutchinson & Knopoff (1978) (revised).rds"
)

for (i in inputs) suppressWarnings(file.remove(i))

source("~/git/timbre-and-consonance-paper/scripts/analysis/010-run-model-batches.R")

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

ROLL_OFF$plot$profiles + 
  reverse_interference_scales(breaks = waiver()) +
  theme(legend.position = "right") +
  guides(alpha = guide_legend(override.aes = list(color = NA))) +
  scale_alpha_manual(
    "Roll-off", 
    values = ROLL_OFF$timbre_alphas, 
    labels = c("12 dB/octave", "7 dB/octave", "2 dB/octave")
  )


print(ROLL_OFF$plot$profiles)
