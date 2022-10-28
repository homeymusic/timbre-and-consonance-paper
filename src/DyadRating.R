library(tidyverse)
library(mgcv)
library(R6)

DyadBootstrappable <- R6Class(
  "DyadBootstrappable",
  
  public = list(
    get_bootstrapped_profiles = function(bootstrap_iter) {
      data_by_participant <- self$data %>% group_by(participant_id) %>% group_split()
      n_participant <- length(data_by_participant)
      
      get_bootstrapped_dataset <- function() {
        data_by_participant[sample(n_participant, size = n_participant, replace = TRUE)] %>% bind_rows()
      }
      
      message("... getting bootstrapped profiles...")
      
      bootstrapped_profiles <- future_map(
        seq_len(bootstrap_iter), function(i) {
          Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")
          self$get_profile(get_bootstrapped_dataset())
        },
        .progress = TRUE, 
        .options = future_options(seed = TRUE)
      )
      bootstrapped_profiles
    },
    
    add_bootstrapped_error = function(profile, bootstrapped_profiles) {
      # Each row is a different interval
      # Each column is a different profile
      bootstrapped_matrix <- do.call(cbind, map(bootstrapped_profiles, "rating"))
      
      bootstrapped_means <- 
        if (ncol(bootstrapped_matrix) == 0) NA_real_ else rowMeans(bootstrapped_matrix)
      
      bootstrapped_ses <- 
        if (ncol(bootstrapped_matrix) == 0) NA_real_ else apply(bootstrapped_matrix, 1, sd)
      
      profile %>% 
        mutate(
          rating_boot_mean = bootstrapped_means,
          rating_boot_se = bootstrapped_ses
        )
    },
    
    eval_peaks = function(bootstrapped_profiles) {
      res <- list()
      
      res$peaks <- get_peaks(self$profile$interval, self$profile$rating)
      res$num_peaks <- length(res$peaks)
      
      res$bootstrap <- list()
      
      message("... bootstrapping peak locations...")
      res$bootstrap$peaks <- furrr::future_map(
        bootstrapped_profiles, 
        ~ get_peaks(.$interval, .$rating),
        .progress = TRUE
      )
      res$bootstrap$num_peaks <- map_int(res$bootstrap$peaks, length)
      
      res
    },
    
    eval_slope = function(bootstrapped_profiles) {
      get_slope <- function(profile) {
        mod <- lm(rating ~ interval, data = profile)
        12 * coef(mod)[["interval"]]
      }
      
      estimate <- get_slope(self$profile)
      samples <- map_dbl(bootstrapped_profiles, get_slope)
      boot_se <- sd(samples)
      boot_95_ci <- c(estimate - 1.96 * boot_se,
                      estimate + 1.96 * boot_se)
      list(
        estimate = estimate,
        bootstrap = list(
          samples = samples,
          boot_se = boot_se,
          boot_95_ci = boot_95_ci
        )
      )
    }
  )
)

DyadRating <- R6Class(
  "DyadRating",
  inherit = DyadBootstrappable,
  
  public = list(
    
    file = NA_character_,
    int_range = NULL,
    resolution = NULL,
    data = NULL,
    model = NULL,
    num_participants = NA_integer_,
    profile = NULL,
    smooth_bandwidth = NA_real_,
    # gamma = NA_real_,
    peaks = NULL,
    slope = NULL,
    
    initialize = function(
      file, 
      int_range, 
      resolution, 
      smooth_bandwidth,
      bootstrap_iter = 0
    ) {
      stopifnot(
        !is.null(smooth_bandwidth)
      )
      
      self$file <- file
      self$int_range <- int_range
      self$resolution <- resolution
      self$smooth_bandwidth <- smooth_bandwidth
      
      self$load_data()
      self$profile <- self$get_profile(data = self$data)
      
      bootstrapped_profiles <- self$get_bootstrapped_profiles(bootstrap_iter)
      self$profile <- self$add_bootstrapped_error(self$profile, bootstrapped_profiles)
      self$peaks <- self$eval_peaks(bootstrapped_profiles)
      self$slope <- self$eval_slope(bootstrapped_profiles)
    },
    
    load_data = function() {
      raw <- if (is.data.frame(self$file)) {
        self$file %>% as_tibble()
      } else {
        read_csv(self$file, col_types = cols())
      }
      raw <- raw %>% rename(interval = v1) 
      self$num_participants <- raw$participant_id %>% unique() %>% length()
      self$data <- 
        raw %>% 
        group_by(participant_id) %>% 
        mutate(rating = as.numeric(scale(rating))) %>% 
        ungroup() %>% 
        na.omit()
    },

    get_profile = function(data, int_range = self$int_range, resolution = self$resolution) {
      if (int_range[1] < min(data$interval) - 0.255) 
        warning("beware of extrapolation, the lower bound is ", int_range[1], 
                " whereas the minimum tested interval was ", min(data$interval))
      
      if (int_range[2] > max(data$interval) + 0.25) 
        warning("beware of extrapolation, the upper bound is ", int_range[2], 
                " whereas the maximum tested interval was ", max(data$interval))
      
      stopifnot(
        !is.null(data$interval),
        !is.null(data$rating)
      )
    
      tibble(
        interval = seq(
          from = int_range[1],
          to = int_range[2],
          length.out = resolution
        ),
        rating = smooth_2d_gaussian(
          data_x = data$interval,
          data_y = rep(0, times = nrow(data)),
          data_val = data$rating,
          probe_x = interval, 
          probe_y = rep(0, times = length(interval)),
          sigma_x = self$smooth_bandwidth,
          sigma_y = self$smooth_bandwidth
        )
      )
    }
  )
)
