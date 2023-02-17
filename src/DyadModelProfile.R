library(R6)
source("src/ConsonanceModel.R")

DyadModelProfile <- R6Class(
  "DyadModelProfile",
  public = list(
    model = NULL,
    timbre_1 = NULL,
    timbre_2 = NULL,
    int_range = NULL,
    raw_resolution = NULL,
    smooth = NULL,
    smooth_resolution = NULL,
    smooth_sigma = NULL,
    raw_profile = NULL,
    profile = NULL,
    
    initialize = function(
      model,
      timbre_1, 
      timbre_2, 
      int_range = c(0, 15),
      raw_resolution = 1000, 
      smooth = FALSE,
      smooth_resolution = 1000,
      smooth_sigma = 0.25
    ) {
      self$model <- model
      self$timbre_1 <- timbre_1
      self$timbre_2 <- timbre_2
      self$int_range <- int_range
      self$raw_resolution <- raw_resolution
      self$smooth <- smooth
      self$smooth_resolution <- smooth_resolution
      self$smooth_sigma <- smooth_sigma
      
      self$raw_profile <- self$compute_raw_profile()
      
      self$profile <- if (self$smooth) {
        self$get_smoothed_profile(self$raw_profile, self$smooth_sigma, self$smooth_resolution)
      } else {
        self$raw_profile
      }
      
    },
    
    compute_raw_profile = function() {
      profile <-
        tibble(interval = seq(from = self$int_range[1],
                              to = self$int_range[2],
                              length.out = self$raw_resolution),
               midi = map(interval, ~ c(60, 60 + .)))
      
      profile$output <- if (self$model$vectorised) {
        self$model$get_consonance_list_batched(
          profile$midi, 
          timbre = list(self$timbre_1,
                        self$timbre_2)
        )
      } else if (self$model$allow_parallel) {
        furrr::future_map_dbl(profile$midi, 
                              self$model$get_consonance,
                              timbre = list(self$timbre_1,
                                            self$timbre_2),
                              .progress = TRUE,
                              .options = furrr::furrr_options(seed = TRUE))
      } else {
        plyr::laply(profile$midi, 
                    self$model$get_consonance, 
                    timbre = list(self$timbre_1,
                                  self$timbre_2),
                    .progress = "text")
      }
      
      profile$major_minor <- if (self$model$vectorised) {
        self$model$get_major_minor_list_batched(
          profile$midi, 
          timbre = list(self$timbre_1,
                        self$timbre_2)
        )
      } else if (self$model$allow_parallel) {
        furrr::future_map_dbl(profile$midi, 
                              self$model$get_major_minor,
                              timbre = list(self$timbre_1,
                                            self$timbre_2),
                              .progress = TRUE,
                              .options = furrr::furrr_options(seed = TRUE))
      } else {
        plyr::laply(profile$midi, 
                    self$model$get_major_minor, 
                    timbre = list(self$timbre_1,
                                  self$timbre_2),
                    .progress = "text")
      }
      
      profile %>% select(interval, output, major_minor)
    },
    
    get_smoothed_profile = function(profile, sigma, resolution) {
      message("... smoothing profile...")
      
      smoothed <- tibble(
        interval = seq(from = self$int_range[1],
                       to = self$int_range[2],
                       length.out = resolution),
        output = smooth_2d_gaussian(
          data_x = profile$interval,
          data_y = rep(0.0, times = length(profile$interval)),
          data_val = profile$output,
          probe_x = interval,
          probe_y = rep(0.0, times = length(interval)),
          sigma_x = sigma,
          sigma_y = sigma),
        major_minor = smooth_2d_gaussian(
          data_x = profile$interval,
          data_y = rep(0.0, times = length(profile$interval)),
          data_val = profile$major_minor,
          probe_x = interval,
          probe_y = rep(0.0, times = length(interval)),
          sigma_x = sigma,
          sigma_y = sigma)
      )
      
      smoothed
    },
    
    plot = function() {
      ggplot(self$profile, aes(interval, output)) + 
        geom_line() + 
        scale_x_continuous("Interval") + 
        scale_y_continuous("Model output")
    }
  )
)
