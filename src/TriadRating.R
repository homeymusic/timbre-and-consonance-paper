library(tidyverse)
library(mgcv)
library(R6)

TriadRating <- R6Class(
  "TriadRating",
  public = list(
    
    file = NA_character_,
    data = NULL,
    model = NULL,
    profile = NULL,
    
    initialize = function(file, int_1_range = c(0, 7.5), int_2_range = c(0, 7.5), resolution = 125) {
      self$file <- file
      self$load_data()
      self$fit_model()
      self$build_profile(int_1_range, int_2_range, resolution)
    },
    
    load_data = function() {
      self$data <-
        read_csv(self$file, col_types = cols()) %>% 
        rename(interval_1 = v1,
               interval_2 = v2) %>% 
        group_by(participant_id) %>% 
        mutate(rating = as.numeric(scale(rating))) %>% 
        ungroup()
    },
    
    fit_model = function() {
      self$model <- mod <- gam(rating ~ s(interval_1, interval_2, k = 100), data = self$data)
    },
    
    plot = function() {
      self$profile %>% 
        ggplot(aes(interval_1, interval_2, fill = rating)) + 
        geom_raster() + 
        scale_fill_viridis_c("Rating") + 
        scale_x_continuous("Interval 1") + 
        scale_y_continuous("Interval 2") +
        theme(aspect.ratio = 1, 
              legend.key.width = unit(0.5, "inches"))
    },
    
    build_profile = function(int_1_range, int_2_range, resolution) {
      if (int_1_range[1] < min(self$data$interval_1) - 0.25) 
        warning("beware of extrapolation, the lower bound for int_1_range is ", int_1_range[1], 
                " whereas the minimum tested interval was ", min(self$data$interval_1))
      
      if (int_1_range[2] > max(self$data$interval_1) + 0.25) 
        warning("beware of extrapolation, the upper bound for int_1_range is ", int_1_range[2], 
                " whereas the maximum tested interval was ", max(self$data$interval_1))
      
      if (int_2_range[1] < min(self$data$interval_2) - 0.25) 
        warning("beware of extrapolation, the lower bound for int_2_range is ", int_2_range[1], 
                " whereas the minimum tested interval was ", min(self$data$interval_2))
      
      if (int_2_range[2] > max(self$data$interval_2) + 0.25) 
        warning("beware of extrapolation, the upper bound for int_2_range is ", int_2_range[2], 
                " whereas the maximum tested interval was ", max(self$data$interval_2))
      
      self$profile <- 
        expand_grid(interval_1 = seq(from = int_1_range[1], to = int_1_range[2], length.out = resolution),
                    interval_2 = seq(from = int_2_range[1], to = int_2_range[2], length.out = resolution)) %>% 
        mutate(rating = predict(self$model, newdata = .))
    }
  )
)
