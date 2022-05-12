library(tidyverse)
library(dycon)
library(hrep)
library(har18)
# 
# relabel <- function(x) {
#   plyr::revalue(x, c(
#     Hutch78 = "Interference (absence)",
#     PraatF0 = "Periodicity/harmonicity",
#     Rating = "Pleasantness"
#   ), warn_missing = FALSE)
# }

stretch_interval <- function(stretched_semitones, octave_ratio = 2.1) {
  stretched_octaves <- stretched_semitones / 12
  freq_ratio <- octave_ratio ^ stretched_octaves
  normal_octaves <- log2(freq_ratio)
  normal_semitones <- normal_octaves * 12
  normal_semitones
}

get_num_peaks <- function(interval, value) {
  peaks <- get_peaks(interval, value)
  length(peaks)
}

get_peaks = function(
  interval, 
  value,
  # Initial peak finding step
  # (finds changing points where the second derivative is sufficiently large)
  min_sharpness = 0.05, # expressed relative to value range; should be positive; originally we set this to 0.1. Doesn't seem to be a limiting factor
  # Subsequent peak merging step 
  # (if either condition is satisfied, the peaks are merged)
  merge_peaks_closer_than = 0.0, # used to be interval range / 30,
  merge_peaks_with_troughs_smaller_than = 0.01,  # expressed relative to value range; originally we set this to 1/15 ~= 0.067
  # Final peak filtering step
  # (if the peak does not extrude enough, it is discarded)
  min_extrusion = 0.01, # expressed relative to value range; originally we set this to 0.05
  extrusion_neighborhood = 0.5
) {
  # mod <- gam(value ~ s(interval, k = 50))
  spline <- smooth.spline(interval, value, df = 100)
  
  first_derivative <- predict(spline, x = interval, deriv = 1)$y
  second_derivative <- predict(spline, x = interval, deriv = 2)$y
  
  second_derivative_threshold <- min_sharpness * diff(range(value))
  
  peaks <- list()
  troughs <- list()
  
  store_peak <- function(interval, value) {
    peaks[[length(peaks) + 1]] <<- as.list(environment())
  }
  
  store_trough <- function(interval, value) {
    troughs[[length(troughs) + 1]] <<- as.list(environment())
  }
  
  is_valid_peak <- function(first_derivative, next_first_derivative, second_derivative) {
    first_derivative > 0 && 
      next_first_derivative < 0 &&
      second_derivative < - second_derivative_threshold
  }
  
  is_valid_trough <- function(first_derivative, next_first_derivative, second_derivative) {
    first_derivative < 0 && 
      next_first_derivative > 0 &&
      second_derivative > second_derivative_threshold
  }
  
  should_merge_peaks <- function(peak_1, peak_2, troughs, value_range, interval_range) {
    stopifnot(peak_1$interval < peak_2$interval)
    intermediate_troughs <- Filter(
      function(x) x$interval > peak_1$interval && x$interval < peak_2$interval,
      troughs
    )
    if (length(intermediate_troughs) == 0) {
      should_merge <- TRUE
    } else {
      lowest_trough <- intermediate_troughs[[which.min(map_dbl(intermediate_troughs, "value"))]]
      
      interval_distance <- peak_2$interval - peak_1$interval
      peak_to_trough_distances <- map_dbl(list(peak_1, peak_2), ~ abs(.$value - lowest_trough$value))
      
      should_merge <- 
        interval_distance < merge_peaks_closer_than || 
        any(peak_to_trough_distances < value_range * merge_peaks_with_troughs_smaller_than)
    }
    should_merge
  }
  
  merge_peaks <- function(peak_1, peak_2) {
    peaks <- list(peak_1, peak_2)
    values <- map_dbl(peaks, "value")
    peaks[[which.max(values)]]
  }
  
  merge_adjacent_peaks <- function(peaks, troughs, value_range, interval_range) {
    # Merge peaks that aren't separated by deep enough troughs,
    # keeping the tallest peak.
    new_peak_list <- list()
    
    if (length(peaks) %in% c(0, 1)) {
      return (peaks)
    }
    
    current_peak <- peaks[[1]]

    for (i in seq(from = 2, to = length(peaks))) {
      next_peak <- peaks[[i]]
      if (should_merge_peaks(current_peak, next_peak, troughs, value_range, interval_range)) {
        current_peak <- merge_peaks(current_peak, next_peak)
      } else {
        # Peaks should not be merged, so we should save the previous one.
        new_peak_list[[length(new_peak_list) + 1]] <- current_peak
        current_peak <- next_peak
      }
    }
    new_peak_list[[length(new_peak_list) + 1]] <- current_peak
    new_peak_list
  }
  
  for (i in seq(length.out = length(interval) - 1)) {
    if (is_valid_peak(
      first_derivative = first_derivative[i],
      next_first_derivative = first_derivative[i + 1],
      second_derivative = second_derivative[i]
    )) store_peak(
      interval = mean(c(interval[i], interval[i + 1])),
      value = mean(c(value[i], value[i + 1]))
    )
    
    if (is_valid_trough(
      first_derivative = first_derivative[i],
      next_first_derivative = first_derivative[i + 1],
      second_derivative = second_derivative[i]
    )) store_trough(
      interval = mean(c(interval[i], interval[i + 1])),
      value = mean(c(value[i], value[i + 1]))
    )
  }
  
  is_sharp_enough <- function(peak, value_range) {
    # What's the range of values neighboring the peak?
    neighbourhood <- 
      tibble(interval, value) %>% 
      filter(abs(interval - peak$interval) <= extrusion_neighborhood) %>% 
      mutate(region = if_else(interval > peak$interval, "above", "below"))
    dropoff <- 
      neighbourhood %>% 
      group_by(region) %>% 
      summarise(dropoff = max(peak$value - value) / value_range)
    all(dropoff$dropoff >= min_extrusion)
  }
  
  interval_range <- diff(range(interval))
  value_range <- diff(range(value))
  
  peaks_merged <- merge_adjacent_peaks(peaks, troughs, value_range, interval_range)
  peaks_final <- Filter(function(x) is_sharp_enough(x, value_range), peaks_merged)
  peaks_final
  
  # predict(
  #   spline, 
  #   x = interval, 
  #   deriv = 2
  # )$y %>% {. ^ 2} %>% mean() %>% sqrt()
}

ratio_to_semitones <- function(ratio) {
  (ratio * midi_to_freq(0)) %>% freq_to_midi()
}

tone <- function(pitch, 
                 n_harmonics = 10L, 
                 decay_dB_per_octave = 12,
                 octave_definition = 2) {
  f0 <- midi_to_freq(pitch)
  tibble(
    i = seq_len(n_harmonics),
    frequency = f0 * octave_definition ^ log2(i),
    dB = - decay_dB_per_octave * log2(i),
    amplitude = 1 * 10 ^ (dB / 20) 
  ) %>% 
    select(frequency, amplitude) %>% 
    as.list() %>% 
    sparse_fr_spectrum()
}

spectrum <- function(midi, ...) {
  midi %>% 
    map(tone, ...) %>% 
    do.call(combine_sparse_spectra, .)
}

midi_from_lower_and_upper_ints <- function(lower_int, upper_int, bass_pitch) {
  bass_pitch + c(0, lower_int, lower_int + upper_int)
}

harrison_pc_harmonicity <- function(x, method = "kl_div") {
  spec <- smooth_pc_spectrum(x)
  if (method == "kl_div") {
    har18::kl_div_from_uniform(spec)
  } else if (method == "max") {
    max(spec)
  } else {
    stop("unrecognised method: ", method)
  }
}

shepard_tone <- function(pitch, 
                         envelope_mean = 65.5,
                         envelope_sd = 8.2) {
  
}

# Original BCA version:
# bootstrap_mean <- function(x, n) {
#   raw <- bootstrap::bcanon(x, n, mean, alpha = c(0.025, 0.975))
#   list(
#     mean = mean(x),
#     lower_95 = raw$confpoints[1, 2] %>% as.numeric(),
#     upper_95 = raw$confpoints[2, 2] %>% as.numeric()
#   )
# }

bootstrap_mean <- function(x, n) {
  samples <- map_dbl(seq_len(n), ~ mean(sample(x, size = length(x), replace = TRUE)))
  res <- list(
    mean = mean(x),
    boot_se = sd(samples)
  )
    # For consistency with the rest of the paper, we estimate confidence intervals 
    # using the Gaussian approximation, which is more robust given the relatively
    # small number of bootstrap replications that are computationally practical
    # in the general case
  res$lower_95 <- res$mean - 1.96 * res$boot_se
  res$upper_95 <- res$mean + 1.96 * res$boot_se
  res
}

summarise_bootstrap_mean <- function(data, column, n = 1e5) {
  data %>% 
    summarise(
      bootstrapped = list(bootstrap_mean(.data[[!!column]], n = n))
    ) %>%
    mutate(
      mean = map_dbl(bootstrapped, "mean"),
      boot_se = map_dbl(bootstrapped, "boot_se"),
      lower_95 = map_dbl(bootstrapped, "lower_95"),
      upper_95 = map_dbl(bootstrapped, "upper_95")
    ) %>% 
    select(- bootstrapped)
    # mutate(
      # lower_95 = if_else(is.na(lower_95), mean, lower_95),
      # upper_95 = if_else(is.na(upper_95), mean, upper_95)
    # )
}

harmonic_number_to_interval <- function(harmonic_number) {
  map_dbl(harmonic_number, function(i) pi_chord(0) %>% sparse_pi_spectrum() %>% slice(i) %>% pull(x))
}

ratio_to_semitones <- function(ratio) {
  (ratio * midi_to_freq(0)) %>% freq_to_midi()
}


# harrison_pc_harmonicity(chord(c(60, 64, 67)))
# harrison_pc_harmonicity(chord(c(60, 61, 62)))

# sparse_fr_spectrum(c(60, 64, 67), roll_off = 2, num_harmonics = 10) %>% plot


# dycon::roughness_hutch()
