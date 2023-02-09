library(tidyverse)
library(ggpubr)
library(magrittr)
library(furrr)

theme_set(
  theme_pubr() + theme(
    strip.background = element_blank(),
  )
)

source("src/import_experiment.R")
source("src/plots.R")
source("src/utils.R")
source("src/TriadGSP.R")
source("src/parameters.R")
source("src/Timbre.R")
source("src/sine_sweep.R")

EXPERIMENTS <- 
  list_experiments() %>% 
  map(import_experiment)

PLOT_DIR = "explorations/results/plots"
R.utils::mkdirs(PLOT_DIR)

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession)
  multisession_launched <- TRUE
}

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
      panel.grid.major.x = element_line(colour = "grey95"),
      axis.text.y = element_text(size = 7)
    ),
  labels = "AUTO",
  ncol = 1,
  rel_heights = c(1, 5)
  # scale = 0.975
)

ggsave(paste0(PLOT_DIR,"/stretching-dyads.pdf"), width = 14, height = 15, scale = 0.6)


########################################################################
# Plots for presentations, supplementary etc.
########################################################################

R.utils::mkdirs(paste0(PLOT_DIR,"/presentation/stretching"))

DYAD_PLOTS$spectra
ggsave(
  "stretched spectra.png", 
  path = paste0(PLOT_DIR,"/presentation/stretching"),
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
    profile_row_heights = c(3, 1, 1, 3),
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
        NULL,
        NULL
      )
    ) +
    scale_x_continuous("Interval (semitones)", breaks = 0:15) 
  
  ggsave(
    plot = .dyad_profile,
    filename = paste0(timbre, " dyads (3 dB roll-off).png"),
    path = paste0(PLOT_DIR,"/presentation/stretching"),
    dpi = 250,
    width = 8, 
    height = 6
  )
}

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
  # reverse_interference_scales() +
  ggh4x::facetted_pos_scales(
    y = list(
      measure == "Interference model" ~ scale_y_continuous(breaks = c(-0.1, -0.2, -0.3, -0.4), labels = function(x) -x),
      measure == "Harmonicity model" ~ scale_y_continuous(breaks = scales::extended_breaks(n = 3))
    )
  ) +
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
