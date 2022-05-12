library(tidyverse)
library(mgcv)
library(R6)

source("src/DyadRating.R")

DyadRatingByRollOffSlice <- R6Class(
  "DyadRatingByRollOffSlice",
  inherit = DyadBootstrappable,
  public = list(
    
    file = NA_character_,
    int_range = NULL,
    roll_off = NULL,
    roll_off_sigma = NULL,
    interval_sigma = NULL,
    resolution = NULL,
    data = NULL,
    model = NULL,
    num_participants = NA_integer_,
    profile = NULL,
    peaks = NULL,
    
    initialize = function(
      file, 
      roll_off,
      roll_off_sigma,
      interval_sigma,
      int_range = c(0, 15), 
      resolution = 1000,
      bootstrap_iter = 0
    ) {
      self$file <- file
      self$roll_off <- roll_off
      self$roll_off_sigma <- roll_off_sigma
      self$interval_sigma <- interval_sigma
      self$int_range <- int_range
      self$resolution <- resolution
      self$load_data()
      # self$fit_model()
      self$profile <- self$get_profile(self$data)
      
      bootstrapped_profiles <- self$get_bootstrapped_profiles(bootstrap_iter)
      self$profile <- self$add_bootstrapped_error(self$profile, bootstrapped_profiles)
      self$peaks <- self$eval_peaks(bootstrapped_profiles)
    },
    
    load_data = function() {
      raw <- if (is.data.frame(self$file)) {
        self$file %>% as_tibble()
      } else {
        read_csv(self$file, col_types = cols())
      }
      self$num_participants <- raw$participant_id %>% unique() %>% length()
      self$data <- 
        raw %>% 
        group_by(participant_id) %>% 
        mutate(rating = as.numeric(scale(rating))) %>% 
        ungroup() %>% 
        na.omit()
    },
    
    # fit_model = function() {
    #   self$model <- gam(rating ~ s(interval, roll_off, k = 100), data = self$data)
    # },
    
    plot = function() {
      self$profile %>% 
        ggplot(aes(interval, 
                   rating)) +
                   # ymin = rating - 1.96 * rating_se,
                   # ymax = rating + 1.96 * rating_se)) + 
        # geom_ribbon(alpha = 0.25, fill = "blue") + 
        geom_line() + 
        scale_x_continuous("Interval (semitones)") + 
        scale_y_continuous("Rating")
  
    },
    
    get_profile = function(data, int_range = self$int_range, resolution = self$resolution) {
      if (int_range[1] < min(self$data$interval) - 0.255) 
        warning("beware of extrapolation, the lower bound is ", int_range[1], 
                " whereas the minimum tested interval was ", min(df$interval))
      
      if (int_range[2] > max(self$data$interval) + 0.25) 
        warning("beware of extrapolation, the upper bound is ", int_range[2], 
                " whereas the maximum tested interval was ", max(df$interval))
      
      tibble(
        interval = seq(from = int_range[1],
                       to = int_range[2],
                       length.out = resolution),
        roll_off = self$roll_off, 
        rating = smooth_2d_gaussian(
          data_x = data$interval, 
          data_y = data$roll_off, 
          data_val = data$rating, 
          probe_x = interval, 
          probe_y = roll_off,
          sigma_x = self$interval_sigma, 
          sigma_y = self$roll_off_sigma
        ),
        rating_se = 0
      )

      # probe <-
      #   tibble(interval = seq(from = int_range[1],
      #                         to = int_range[2],
      #                         length.out = resolution),
      #          roll_off = self$roll_off)
      # 
      # predict(self$model, newdata = input, se.fit = TRUE) %>% 
      #   as_tibble() %>% 
      #   rename(rating = fit, rating_se = se.fit) %>% 
      #   bind_cols(input, .)
    }
    
    # get_curvature_from_dataset = function(df) {
    #   profile <- self$get_profile(df)
    #   get_curvature(profile$interval, profile$rating)
    # }
  )
)
