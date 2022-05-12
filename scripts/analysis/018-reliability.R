########################################################################
# Setup ####
########################################################################

source("scripts/analysis/015-figure-setup.R")
source("src/DyadRating.R")
source("src/utils.R")
source("src/plots.R")
source("src/sine_sweep.R")
Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")
library(magick)

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession)
  multisession_launched <- TRUE
}

HARMONIC_DYADS <- EXPERIMENTS$`Harmonic dyads (3 dB roll-off)`
HARMONIC_TRIADS <- EXPERIMENTS$`Harmonic triads (3 dB roll-off)`

split_half_reliability <- function(x, resolution, resample_over) {
  Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")
  source("src/DyadRating.R")
  source("src/TriadGSP.R")
  
  rebuild_profile <- function(x, newdata, resolution) {
    UseMethod("rebuild_profile")
  }
  
  rebuild_profile.DyadRating <- function(x, new_data, resolution) {
    x$get_profile(new_data, resolution = resolution)
  }
  
  rebuild_profile.TriadGSP <- function(x, new_data, resolution) {
    old_data <- x$data
    old_profile <- x$profile
    
    x$data <- new_data
    x$build_profile(x$int_1_range, x$int_2_range, resolution)
    new_profile <- x$profile
    
    x$data <- old_data
    x$profile <- old_profile
    
    new_profile
  }
  
  compare_profiles <- function(experiment, profile_1, profile_2) {
    UseMethod("compare_profiles")
  }
  
  compare_profiles.DyadRating <- function(experiment, profile_1, profile_2) {
    stopifnot(all.equal(profile_1$interval, profile_2$interval))
    
    tibble(
      pearson = cor(profile_1$rating, profile_2$rating, method = "pearson"),
      spearman = cor(profile_1$rating, profile_2$rating, method = "spearman")
    )
  }
  
  compare_profiles.TriadGSP <- function(experiment, profile_1, profile_2) {
    stopifnot(
      all.equal(profile_1$interval_1, profile_2$interval_1),
      all.equal(profile_1$interval_2, profile_2$interval_2)
    )
    
    tibble(
      pearson = cor(profile_1$relative_density, profile_2$relative_density, method = "pearson"),
      spearman = cor(profile_1$relative_density, profile_2$relative_density, method = "spearman")
    )
  }
  
  experiment <- x$behaviour$full
  
  ids <- unique(experiment$data[[resample_over]])
  stopifnot(length(ids) > 1)
  
  groups <- list()
  groups[[1]] <- sample(ids, size = floor(length(ids) / 2), replace = FALSE) %>% sort()
  groups[[2]] <- setdiff(ids, groups[[1]]) %>% sort()
  
  datasets <- map(groups, function(ids) {
    experiment$data[experiment$data[[resample_over]] %in% ids, ]
  })
  
  profiles <- map(datasets, function(dataset) {
    rebuild_profile(experiment, dataset, resolution = resolution)
  })
  
  compare_profiles(experiment, profiles[[1]], profiles[[2]])
}

reliability_estimates <- list()

########################################################################
# Reliability estimates for harmonic dyads ####
########################################################################

message("Computing reliability estimates for harmonic dyads...")
reliability_estimates$harmonic_dyads <-
  future_map_dfr(
    1:1000,
    ~ split_half_reliability(HARMONIC_DYADS, resolution = 100, resample_over = "participant_id"),
    .options = future_options(seed = 1L),
    .progress = TRUE
  )

# Monte Carlo mean
print(mean(reliability_estimates$harmonic_dyads$pearson))

# 95% confidence interval
# The standard +/- 1.96 * SE method can give strange results for correlations
# so we use quantile methods instead.
# c(- 1, 1) * 1.96 * sd(reliability_estimates$harmonic_dyads$pearson) + 
#   mean(reliability_estimates$harmonic_dyads$pearson)
quantile(reliability_estimates$harmonic_dyads$pearson, c(0.025, 0.975))

########################################################################
# Reliability estimates for the major 3rd ####
########################################################################

message("Computing reliability estimates for major 3rd (3 dB roll-off...")
reliability_estimates$harmonic_major_3rd <- 
  future_map_dfr(
    1:1000,
    ~ split_half_reliability(EXPERIMENTS$`Major 3rd (3 dB roll-off)`, resolution = 100, resample_over = "participant_id"),
    .options = future_options(seed = 1L),
    .progress = TRUE
  )

# Monte Carlo mean
print(mean(reliability_estimates$harmonic_major_3rd$pearson))

# 95% confidence interval
quantile(reliability_estimates$harmonic_major_3rd$pearson, c(0.025, 0.975))

########################################################################
# Reliability estimates for harmonic triads ####
########################################################################

message("Computing reliability estimates for harmonic triads...")
reliability_estimates$harmonic_triads <- 
  future_map_dfr(
    1:1000, 
    ~ split_half_reliability(HARMONIC_TRIADS, resolution = 100, resample_over = "network_id"),
    .options = future_options(seed = 1L),
    .progress = TRUE
  )

# Monte Carlo mean
print(mean(reliability_estimates$harmonic_triads$spearman))

# 95% confidence interval
quantile(reliability_estimates$harmonic_triads$pearson, c(0.025, 0.975))

R.utils::mkdirs("output/validation")
saveRDS(reliability_estimates, "output/validation/harmonic-profiles-reliability-estimates.rds")
