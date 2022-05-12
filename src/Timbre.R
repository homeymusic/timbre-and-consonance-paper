library(R6)
library(hrep)
library(tidyverse)

Timbre <- R6Class(
  "Timbre",
  public = list(
    
    label = NULL,
    
    initialize = function(label) {
      self$label <- label
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      stop("needs to be implemented")
    }
    
  )
)

# .cached_sparse_fr_spectrum <- memoise::memoise(hrep::sparse_fr_spectrum)

PureTone <- R6Class(
  "PureTone",
  inherit = Timbre,
  public = list(
    
    initialise = function(label = "Pure") {
      super$initialize(label = label)
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      hrep::sparse_fr_spectrum(as.numeric(midi),
                               num_harmonics = 1,
                               coherent = coherent)
    }
    
  )
)

HarmonicTone <- R6Class(
  "HarmonicTone",
  inherit = Timbre,
  public = list(
    
    n_harmonics = NULL,
    harmonic_amplitudes = NULL, 
    octave_definition = NULL,
    
    initialize = function(harmonic_amplitudes,  
                          octave_definition = 2,
                          label = "Harmonic") {
      super$initialize(label = label)
      
      self$n_harmonics <- length(harmonic_amplitudes)
      self$harmonic_amplitudes <- harmonic_amplitudes
      self$octave_definition <- octave_definition
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      args <- midi %>% map(self$tone_sparse_fr_spectrum)
      args$coherent <- coherent
      do.call(hrep::combine_sparse_spectra, args)
    },
    
    tone_sparse_fr_spectrum = function(pitch) {
      f0 <- hrep::midi_to_freq(pitch)
      i <- seq_len(self$n_harmonics)
      frequency <- f0 * self$octave_definition ^ log2(i)
      amplitude <- self$harmonic_amplitudes
      hrep::sparse_fr_spectrum(
        list(frequency, amplitude)
      )
    }
  )
)

BasicHarmonicTone <- R6Class(
  "BasicHarmonicTone",
  inherit = HarmonicTone,
  public = list(
    
    decay_dB_per_octave = NULL,
    
    initialize = function(n_harmonics = 10L, 
                          decay_dB_per_octave = 12, 
                          octave_definition = 2,
                          label = sprintf("Harmonic (%i harmonics, %s dB/octave decay, octave ratio = %s",
                                          n_harmonics,
                                          decay_dB_per_octave,
                                          octave_definition)) {
      self$decay_dB_per_octave <- decay_dB_per_octave
      df <- tibble::tibble(
        i = seq_len(n_harmonics),
        dB = - decay_dB_per_octave * log2(i),
        amplitude = 1 * 10 ^ (dB / 20) 
      )
      super$initialize(harmonic_amplitudes = df$amplitude,
                       octave_definition = octave_definition,
                       label = label)
    }
  )
)

GamelanTone <- R6Class(
  "GamelanTone",
  inherit = Timbre,
  public = list(
    
    initialize = function(label = "Bonang") {
      super$initialize(label = label)
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      args <- midi %>% map(self$tone_sparse_fr_spectrum)
      args$coherent <- coherent
      do.call(hrep::combine_sparse_spectra, args)
    },
    
    tone_sparse_fr_spectrum = function(pitch) {
      f0 <- hrep::midi_to_freq(pitch)
      df <- tibble::tibble(
        frequency = f0 * c(1, 1.52, 3.46, 3.92),
        amplitude = 1
      )
      spec <- df %>% as.list() %>% hrep::sparse_fr_spectrum()
      spec
    }
  )
)
