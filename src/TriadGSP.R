library(tidyverse)
library(mgcv)
library(R6)

TriadGSP <- R6Class(
  "TriadGSP",
  public = list(
    
    file = NA_character_,
    bandwidth = NA_real_,
    resolution = NA_integer_,
    int_1_range = NA_real_,
    int_2_range = NA_real_,
    data = NULL,
    model = NULL,
    profile = NULL,
    num_participants = NA_integer_,
    
    initialize = function(file, bandwidth, int_1_range = c(0.5, 7.5), int_2_range = c(0.5, 7.5), resolution = 125) {
      self$file <- file
      self$bandwidth <- bandwidth
      self$resolution <- resolution
      self$int_1_range <- int_1_range
      self$int_2_range <- int_2_range
      self$load_data()
      # self$fit_model()
      self$build_profile(int_1_range, int_2_range, resolution)
    },
    
    load_data = function() {
      self$data <- if (is_tibble(self$file)) {
        self$file
      } else {
        read_csv(self$file, col_types = cols()) %>% 
          rename(interval_1 = v1,
                 interval_2 = v2)
      }
    },
    
    # fit_model = function() {
    #   self$model <- mod <- gam(rating ~ s(interval_1, interval_2, k = 100), data = self$data)
    # },
    
    plot = function(x_label = "Lower interval (semitones)", y_label = "Upper interval (semitones)") {
      self$profile %>% 
        ggplot(aes(interval_1, interval_2, fill = relative_density)) + 
        geom_raster() + 
        scale_fill_viridis_c("Pleasantness") + 
        scale_x_continuous(x_label, breaks = seq(from = floor(self$int_1_range[1]),
                                                      to = ceiling(self$int_1_range[2]))) + 
        scale_y_continuous(y_label, breaks = seq(from = floor(self$int_2_range[1]),
                                                      to = ceiling(self$int_2_range[2]))) +
        theme(aspect.ratio = 1, 
              legend.key.width = unit(0.25, "inches"),
              legend.position = "right")
              # legend.key.height)
    },
    
    build_profile = function(int_1_range, int_2_range, resolution) {
      # if (int_1_range[1] < min(self$data$interval_1) - 0.25) 
      #   warning("beware of extrapolation, the lower bound for int_1_range is ", int_1_range[1], 
      #           " whereas the minimum tested interval was ", min(self$data$interval_1))
      # 
      # if (int_1_range[2] > max(self$data$interval_1) + 0.25) 
      #   warning("beware of extrapolation, the upper bound for int_1_range is ", int_1_range[2], 
      #           " whereas the maximum tested interval was ", max(self$data$interval_1))
      # 
      # if (int_2_range[1] < min(self$data$interval_2) - 0.25) 
      #   warning("beware of extrapolation, the lower bound for int_2_range is ", int_2_range[1], 
      #           " whereas the minimum tested interval was ", min(self$data$interval_2))
      # 
      # if (int_2_range[2] > max(self$data$interval_2) + 0.25) 
      #   warning("beware of extrapolation, the upper bound for int_2_range is ", int_2_range[2], 
      #           " whereas the maximum tested interval was ", max(self$data$interval_2))
      
      kde <- MASS::kde2d(
        x = self$data$interval_1,
        y = self$data$interval_2, 
        h = self$bandwidth,
        n = self$resolution,
        lims = c(self$int_1_range, self$int_2_range)
      )

      uniform_density <- 1 / (diff(self$int_1_range) * diff(self$int_2_range))
      
      self$profile <- 
        expand_grid(i = seq_len(self$resolution),
                    j = seq_len(self$resolution)) %>% 
        mutate(
          interval_1 = kde$x[i],
          interval_2 = kde$x[j],
          density = map2_dbl(i, j, ~ kde$z[.x, .y]),
          relative_density = density / uniform_density
        ) %>% 
        select(- i, -j)
    }
  )
)
