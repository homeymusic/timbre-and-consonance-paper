library(tidyverse)
library(ggpubr)
library(magrittr)
library(furrr)

theme_set(
  theme_pubr() + theme(
    strip.background = element_blank()
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

MODELS <- c(
  "Interference" = "Hutchinson & Knopoff (1978)",
  "Harmonicity" = "Harrison & Pearce (2018)",
  "Final model" = "Combined"
  # "Interference (revised)" = "Hutchinson & Knopoff (1978) (revised)"
)

MODELS_ALL <- c(
  "Hutchinson & Knopoff (1978)",
  "Sethares (1993)",
  "Vassilakis (2001)",
  "Harrison & Pearce (2018)",
  "Milne (2013)",
  "Praat (F0)"
) %>% set_names(., .)

PLOT_DIR = "output/plots"
R.utils::mkdirs(PLOT_DIR)

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession)
  multisession_launched <- TRUE
}
