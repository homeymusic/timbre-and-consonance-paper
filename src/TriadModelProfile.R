library(R6)
source("src/ConsonanceModel.R")
Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")

TriadModelProfile <- R6Class(
  "TriadModelProfile",
  public = list(
    model = NULL,
    timbre = NULL,
    int_1_range = NULL,
    int_2_range = NULL,
    int_1_values = NULL,
    int_2_values = NULL,
    raw_resolution = NULL,
    smooth = NULL,
    smooth_resolution = NULL,
    smooth_sigma = NULL,
    raw_profile = NULL,
    profile = NULL,
    
    initialize = function(
      model, 
      timbre, 
      int_1_range = c(0.5, 7.5), 
      int_2_range = c(0.5, 7.5), 
      raw_resolution = 125,
      smooth = FALSE,
      smooth_resolution = 125,
      smooth_sigma = 2
    ) {
      self$model <- model
      self$timbre <- timbre
      self$raw_resolution <- raw_resolution
      self$smooth <- smooth
      self$smooth_resolution <- smooth_resolution
      self$smooth_sigma <- smooth_sigma
      
      self$int_1_range <- int_1_range
      self$int_2_range <- int_2_range
      
      self$raw_profile <- self$compute_raw_profile()
      
      self$profile <- if (self$smooth) {
        self$get_smoothed_profile(self$raw_profile, self$smooth_sigma, self$smooth_resolution)
      } else {
        self$raw_profile
      }
    },
    
    compute_raw_profile = function() {
      int_1_values <- seq(from = self$int_1_range[1],
                          to = self$int_1_range[2],
                          length.out = self$raw_resolution)
      int_2_values <- seq(from = self$int_2_range[1],
                          to = self$int_2_range[2],
                          length.out = self$raw_resolution)
      
      df <-
        expand_grid(
          interval_1 = int_1_values,
          interval_2 = int_2_values
        ) %>% 
        mutate(
          midi = map2(interval_1, interval_2,
                      ~ c(60, 60 + .x, 60 + .x + .y))
        )
      
      timbre_list <- map(1:3, ~ self$timbre)
      
      df$output <- if (self$model$vectorised) {
        self$model$get_consonance_list_batched(df$midi, timbre_list)
      } else if (self$model$allow_parallel) {
        furrr::future_map_dbl(df$midi, 
                              self$model$get_consonance,
                              timbre = timbre_list,
                              .progress = TRUE, 
                              .options = furrr::furrr_options(seed = TRUE))
      } else {
        plyr::laply(df$midi, 
                    self$model$get_consonance, 
                    timbre = timbre_list,
                    .progress = "text")
      }
      
      df %>% 
        select(interval_1, interval_2, output)
    },
    
    get_smoothed_profile = function(profile, sigma, resolution) {
      message("... smoothing profile...")
      
      smoothed <- expand_grid(
        interval_1 = seq(from = self$int_1_range[1],
                         to = self$int_1_range[2],
                         length.out = resolution),
        interval_2 = seq(from = self$int_2_range[1],
                         to = self$int_2_range[2],
                         length.out = resolution)
      )
      
      message("... number of points to evaluate: ", nrow(smoothed))
      
      smoothed$output <- furrr::future_pmap_dbl(
        list(
          probe_x = smoothed$interval_1,
          probe_y = smoothed$interval_2
        ),
        function(probe_x, probe_y) {
          Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")
          smooth_2d_gaussian(
            profile$interval_1,
            profile$interval_2,
            profile$output,
            probe_x,
            probe_y,
            sigma_x = sigma,
            sigma_y = sigma
          )
        },
        .progress = TRUE,
        .options = furrr_options(seed = TRUE)
      )

      # smoothed$output <- smooth_2d_gaussian(
      #   profile$interval_1,
      #   profile$interval_2,
      #   profile$output,
      #   smoothed$interval_1,
      #   smoothed$interval_2,
      #   sigma_x = sigma,
      #   sigma_y = sigma
      # )
      
      smoothed
    },
    
    smooth_2d_gaussian_legacy = function(data_x, data_y, data_val, probe_x, probe_y, sigma) {
      smoothed <- rep(NA_real_, times = length(probe_x))

      for (i in seq_along(smoothed)) {
        int_1 <- probe_x[i]
        int_2 <- probe_y[i]
        dists <- sqrt((data_x - int_1) ^ 2 +
                        (data_y - int_2) ^ 2)
        weights <- dnorm(dists, sd = sigma)
        smoothed[i] <- sum(data_val * weights) * (1 / sum(weights))
      }
      
      smoothed
    },
    
    # smooth_profile = function(profile, smooth_parameter = self$smooth_parameter) {
    #   if (is.na(smooth_parameter)) {
    #     return(profile)
    #   }
    #   mat <- self$profile_to_matrix(profile)
    #   smoothed <- smoothie::kernel2dsmooth(
    #     mat, 
    #     "gauss", 
    #     sigma = 0.1,
    #     nx = self$resolution,
    #     ny = self$resolution 
    #   )
    #   browser()
    #   
    #   profile
    # },
    
    # profile_to_matrix = function(profile) {
    #   mat <- matrix(nrow = self$resolution, ncol = self$resolution)
    #   for (r in seq_len(nrow(profile))) {
    #     i <- profile$i[r]
    #     j <- profile$j[r]
    #     val <- profile$output[r]
    #     mat[i, j] <- val
    #   }
    #   mat
    # },
    
    plot = function(reverse = FALSE, fill_label = "Model output") {
      ggplot(self$profile, aes(interval_1, interval_2, fill = scale(output))) + 
        scale_x_continuous("Interval 1") + 
        scale_y_continuous("Interval 2") +
        geom_raster() +
        scale_fill_viridis_c(fill_label) + 
        theme(aspect.ratio = 1, 
              legend.key.width = unit(0.5, "inches"))
    }
  )
)
