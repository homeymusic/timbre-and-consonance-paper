source("src/utils.R")
library(furrr)

wav_spec <- function(
  granularity = 125L,
  n_harmonics = 10L, 
  decay_dB_per_octave = 12,
  octave_definition = 2,
  lower_int_range = c(0, 8),
  upper_int_range = c(0, 8),
  bass_pitch = 60
) {
  x <- as.list(environment())
  x$hash <- digest::digest(x)
  x
}

synth_grid <- function(spec) {
  dir <- file.path("output/wav-grid", spec$hash)
  R.utils::mkdirs(dir)
  
  if (file.exists(file.path(dir, "info.json"))) {
    return(FALSE)
  } else {
  grid <- 
    expand_grid(lower_int = seq(from = spec$lower_int_range[1], 
                                to = spec$lower_int_range[2] ,
                                length.out = spec$granularity),
                upper_int = seq(from = spec$upper_int_range[1], 
                                to = spec$upper_int_range[2], 
                                length.out = spec$granularity)) %>% 
    mutate(midi = map2(lower_int, upper_int, midi_from_lower_and_upper_ints, 
                       bass_pitch = spec$bass_pitch),
           file = sprintf("%i.wav", seq_along(midi)))
  
  grid %>% 
    future_pmap(function(midi, file, ...) {
      spectrum(midi,
               n_harmonics = spec$n_harmonics, 
               decay_dB_per_octave = spec$decay_dB_per_octave,
               octave_definition = spec$octave_definition) %>% 
        save_wav(file.path(dir, file))
    }, .progress = TRUE)
  
  info <- list(spec = spec, grid = grid) 
  saveRDS(info, file.path(dir, "info.rds"))
  jsonlite::toJSON(info, digits = 12) %>% write(file.path(dir, "info.json"))
  
  return (TRUE)
  }
}

wav_grids <- list(
  harmonic = wav_spec(),
  stretched = wav_spec(octave_definition = 2.1),
  compressed = wav_spec(octave_definition = 1.9),
  pure = wav_spec(n_harmonics = 1)
)
