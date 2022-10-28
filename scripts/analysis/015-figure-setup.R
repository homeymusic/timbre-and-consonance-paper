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

ROLL_OFF_EXPS  <- c(
  "12 dB roll-off (harmonic dyads)",
  "7 dB roll-off (harmonic dyads)",
  "2 dB roll-off (harmonic dyads)"
)

# Median of composite model output over all the roll-off experiments
ROLL_OFF_COMBINED_MEDIAN <-
  EXPERIMENTS[ROLL_OFF_EXPS] %>%
  map_dfr(function(exp) {
    exp$models$Combined$summary %>% select(output)
  }) %>%
  pull(output) %>%
  median()

# Median-normalise all the roll-off experiments to the same baseline
for (label in ROLL_OFF_EXPS) {
  EXPERIMENTS[[label]]$models$Combined$summary$output <- 
    EXPERIMENTS[[label]]$models$Combined$summary$output - ROLL_OFF_COMBINED_MEDIAN
}

MODELS <- c(
  "Interference model" = "Hutchinson & Knopoff (1978)",
  "Harmonicity model" = "Harrison & Pearce (2018)",
  # "Harmonicity" = "Harrison & Pearce (2018) (revised)",
  "Composite model" = "Combined"
  # "Interference (revised)" = "Hutchinson & Knopoff (1978) (revised)"
)

MODELS_ALL <- c(
  "Hutchinson & Knopoff (1978)",
  "Interference (revised)" = "Hutchinson & Knopoff (1978) (revised)",
  "Sethares (1993)",
  "Vassilakis (2001)",
  "Harrison & Pearce (2018)",
  "Milne (2013)",
  "Praat (F0)",
  "Composite model" = "Combined"
) %>% set_names(., .)

PLOT_DIR = "output/plots"
R.utils::mkdirs(PLOT_DIR)

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession)
  multisession_launched <- TRUE
}
