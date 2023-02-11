library(tidyverse)
library(furrr)
library(ggpubr)
library(lazyeval)
library(parallelly)

theme_set(theme_pubr())

source("src/ConsonanceModel.R")
source("src/DyadModelProfile.R")
source("src/DyadRating.R")
source("src/DyadRatingByRollOffSlice.R")
source("src/TriadGSP.R")
source("src/Timbre.R")
source("src/TriadModelProfile.R")
source("src/utils.R")
source("src/instrument-timbres.R")
source("src/parameters.R")

OUTPUT_DIR <- "explorations/data"

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession, workers=parallelly::availableCores())
  multisession_launched <- TRUE
}

if (FALSE) {
  # Delete output directory, forcing all results to regenerate
  unlink(OUTPUT_DIR, recursive = TRUE)
}

Experiment <- function(
    label, 
    timbre, 
    domain, 
    data, 
    smooth_bandwidth,
    timbre_bass = NULL,
    rescale_combined_model_within_experiment = TRUE
) {
  x <- as.list(environment())
  class(x) <- c("Experiment", class(x))
  x
}

Domain <- function(
    label, 
    n_dimensions, 
    int_range = NULL, 
    int_1_range = NULL, 
    int_2_range = NULL
) {
  x <- as.list(environment())
  class(x) <- c("Domain", class(x))
  x
}
