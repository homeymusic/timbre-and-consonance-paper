source("scripts/analysis/015-figure-setup.R")
source("src/DyadRating.R")
source("src/utils.R")
source("src/plots.R")
source("src/sine_sweep.R")
Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession)
  multisession_launched <- TRUE
}

HARMONIC_TRIADS <- EXPERIMENTS$`Harmonic triads (3 dB roll-off)`

# This analysis quantifies the location of the 'octave diagonal' in the 
# triadic consonance profiles.
# The implementation is inefficient and takes a long time to run, sorry.
# You can speed it up by reducing NBOOT to a smaller number
# (note that the confidence intervals will become less reliable as a result).
compute_diagonal_marginal <- function(kde_data, interval_from, interval_to, resolution, sleeve_margin) {
  marginal_list <- c()
  interval_list <- c()
  L1 = max(kde_data$interval_1) - min(kde_data$interval_1)
  L2 = max(kde_data$interval_2) - min(kde_data$interval_2)
  baseline = 1 / (L1 * L2)
  delta = (interval_to - interval_from) / resolution
  for (i in 1:resolution) {
    d = interval_from + delta * (i-1)
    current_slice <- kde_data %>% filter(interval_1 + interval_2 >= d - sleeve_margin,
                                         interval_1 + interval_2 <= d + sleeve_margin)
    marginal_mass <- sum(current_slice$relative_density)
    baseline_mass <- baseline * nrow(current_slice)
    marginal_list <- c(marginal_list, marginal_mass / baseline_mass)
    interval_list <- c(interval_list, d)
  }
  
  diagonal_marginal <- data.frame(interval = interval_list,
                                  density = marginal_list / sum(marginal_list))
}

resolution = 1000
interval_from = 9.7
interval_to = 13.7
sleeve_margin = 0.1

STRETCHED_TRIADS <- EXPERIMENTS$`Stretched triads (3 dB roll-off)`
COMPRESSED_TRIADS <- EXPERIMENTS$`Compressed triads (3 dB roll-off)`

objects_to_boot_over <- list(HARMONIC_TRIADS,STRETCHED_TRIADS,COMPRESSED_TRIADS)
obj_names <- c("harmonic", "stretched", "compressed")

# Increase this to 1000 if you want a more accurate answer
# Set it to lower values to speed the computation
NBOOT <- 1000
set.seed(1)
peak_df <- furrr::future_map_dfr(1:NBOOT, function(i) {
  imap_dfr(objects_to_boot_over, function(obj, obj_name) {
    Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")
    source("src/TriadGSP.R")
    
    old_raw_data <- obj$behaviour$full$data
    data_sample <- old_raw_data %>% group_by(network_id) %>% group_split() 
    data_sample <- sample(data_sample, length(data_sample), replace=TRUE)
    data_sample <- bind_rows(data_sample)
    obj$behaviour$full$data <- data_sample # hack
    obj$behaviour$full$build_profile(int_1_range = c(0.5, 8.5),
                                     int_2_range = c(0.5, 8.5),
                                     resolution = 500)
    
    kde_profile <- obj$behaviour$full$profile
    marginal <- compute_diagonal_marginal(kde_profile, interval_from, interval_to,
                                          resolution, sleeve_margin)
    peaks <- 
      get_peaks(marginal$interval, marginal$density) %>%
      map_dfr(as_tibble) %>%
      mutate(type = obj_name)
    
    peaks <- peaks[which.max(peaks$value), ]
    obj$behaviour$full$data <- old_raw_data
    peaks
  })
}, .progress = TRUE, .options = furrr_options(seed = TRUE))


diagonal_summary <- peak_df %>% 
  group_by(type) %>%
  summarise(peak = mean(interval),
            se = sd(interval),
            lb = peak - 1.96 * se,
            ub = peak + 1.96 * se,
            prob = n() / NBOOT)

write_csv(diagonal_summary, "output/plots/065-stretched-triads-diagonal-summary.csv")
