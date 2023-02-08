library(R6)
library(reticulate)
library(tidyverse)
library(mami.codi.R)

source("src/custom_hutchinson_knopoff_model.R")

ConsonanceModel <- R6Class(
  "ConsonanceModel",
  public = list(
    
    vectorised = FALSE,
    allow_parallel = NULL,
    theory = NULL,
    label = NULL,
    plot_colour = NULL,
    options = NULL,
    
    initialize = function(
    theory, 
    label, 
    plot_colour = "black", 
    options = list()
    ) {
      self$theory <- theory
      self$label <- label
      self$plot_colour = plot_colour
      self$options = options
    },
    
    get_consonance = function(midi, timbre) {
      stop(paste("get_consonance not implemented for",self$name))
    },
    
    get_consonance_list = function(midi_list, timbre) {
      stop("get_consonance_list not implemented for",self$name)
    },
    
    get_consonance_list_batched = function(midi_list, timbre, batch_size = 500L) {
      batch_ids <- ceiling(seq_along(midi_list) / batch_size)
      batches <- split(midi_list, batch_ids)
      results_by_batch <- plyr::llply(batches, 
                                      self$get_consonance_list, 
                                      timbre = timbre, 
                                      .progress = "time")
      do.call(c, unname(results_by_batch))
    },
    
    get_sparse_fr_spectrum = function(midi, timbre, coherent) {
      if (is(timbre, "Timbre")) {
        timbre$sparse_fr_spectrum(midi, coherent = coherent)
      } else if (is.list(timbre)) {
        stopifnot(length(timbre) == length(midi)) 
        spectra <- map2(timbre, midi, ~ .x$sparse_fr_spectrum(.y, coherent = coherent))
        args <- spectra
        args$coherent <- coherent
        do.call(hrep::combine_sparse_spectra, args)
      }
    }
  )
)

DyadInterferenceModel <- R6Class(
  "DyadInterferenceModel",
  inherit = ConsonanceModel,
  public = list(
    
    allow_parallel = TRUE,
    
    initialize = function(label, theory = "interference", plot_colour = "#B50000", ...) {
      super$initialize(
        label = label,
        theory = theory,
        plot_colour = plot_colour,
        ...
      )
    },
    
    get_consonance = function(midi, timbre) {
      spectrum <- self$get_sparse_fr_spectrum(midi, timbre, coherent = COHERENT_WAVES)
      - self$get_roughness_from_sparse_fr_spectrum(spectrum)
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      stop("not implemented")
    }
  )
)


Hutch78 <- R6Class(
  "Hutch78",
  inherit = DyadInterferenceModel,
  public = list(
    initialize = function(
    label = "Hutchinson & Knopoff (1978)",
    ...
    ) {
      super$initialize(
        label = label,
        ...
      )
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      dycon::roughness_hutch(spectrum)
    }
    
  )
)

RevisedHutch78 <- R6Class(
  "RevisedHutch78",
  inherit = DyadInterferenceModel,
  public = list(
    initialize = function(
    label = "Hutchinson & Knopoff (1978) (revised)",
    ...
    ) {
      super$initialize(
        label = label,
        ...
      )
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      source("src/custom_hutchinson_knopoff_model.R")
      roughness_hutch_revised(spectrum)
    }
  )
)


Seth93 <- R6Class(
  "Seth93",
  inherit = DyadInterferenceModel,
  public = list(
    
    initialize = function(label = "Sethares (1993)", ...) {
      super$initialize(
        label = label, 
        ...
      )
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      dycon::roughness_seth(spectrum)
    }
    
  )
)

Vass01 <- R6Class(
  "Vass01",
  inherit = DyadInterferenceModel,
  public = list(
    
    initialize = function(label = "Vassilakis (2001)", ...) {
      super$initialize(
        label = label, ...
      )
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      dycon::roughness_vass(spectrum)
    }
    
  )
)

PCTemplateModel <- R6Class(
  "PCTemplateModel",
  inherit = ConsonanceModel,
  public = list(
    
    allow_parallel = TRUE,
    template_sigma = 6.83,
    
    initialize = function(label, theory = "harmonicity", plot_colour = "#11A3FF", ...) {
      super$initialize(
        label = label,
        theory = theory,
        plot_colour = plot_colour,
        ...
      )
    },
    
    get_consonance = function(midi, timbre) {
      pc_spectrum <- 
        self$get_sparse_fr_spectrum(midi, timbre, coherent = COHERENT_WAVES) %>% 
        hrep::smooth_pc_spectrum(coherent = COHERENT_WAVES) # <--- 5 ms
      pitch_profile <- self$get_pitch_profile(pc_spectrum) # <--- 12 ms
      self$eval_profile(pitch_profile) # <--- v fast
    },
    
    get_pitch_profile = function(x) {
      stopifnot(is(x, "smooth_pc_spectrum"))
      x <- as.numeric(x)
      array_dim <- length(x)
      template <- hrep::smooth_pc_spectrum( # <--- 4 ms
        hrep::pi_chord(60),
        array_dim = array_dim,
        coherent = COHERENT_WAVES, 
        sigma = self$template_sigma
      )
      res <- har18::sweep_template(x, template) # <--- 8 ms
      hrep::.smooth_pc_spectrum(res)
    },
    
    eval_profile = function(profile) {
      stop("not implemented")
    }
  )
)


Milne13 <- R6Class(
  "Milne13",
  inherit = PCTemplateModel,
  public = list(
    
    initialize = function(label = "Milne (2013)", ...) {
      super$initialize(label = label, ...)
    },
    
    eval_profile = function(profile) {
      max(as.numeric(profile))
    }
    
  )
)

Har18 <- R6Class(
  "Har18",
  inherit = PCTemplateModel,
  public = list(
    
    initialize = function(label = "Harrison & Pearce (2018)", ...) {
      super$initialize(label = label, ...)
    },
    
    eval_profile = function(profile) {
      self$kl_div_from_uniform(as.numeric(profile))
    },
    
    kl_div_from_uniform = function(x) {
      probs <- x / sum(x)
      n <- length(probs)
      uniform_probs <- 1 / n
      non_zero_probs <- probs[probs > 0]
      sum(
        non_zero_probs * log(non_zero_probs / uniform_probs, base = 2)
      )
    }
  )
)

RevisedHar18 <- R6Class(
  "RevisedHar18",
  inherit = Har18,
  public = list(
    template_sigma = 15,
    
    initialize = function(label = "Harrison & Pearce (2018) (revised)", ...) {
      super$initialize(label = label, ...)
    }
  ),
)

PraatF0 <- R6Class(
  "PraatF0",
  inherit = ConsonanceModel,
  public = list(
    loaded = FALSE,
    
    vectorised = TRUE,
    allow_parallel = FALSE,
    
    initialize = function(label = "Praat (F0)", theory = "harmonicity", ...) {
      super$initialize(label = label, theory = theory, ...)
      self$plot_colour <- "#11A3FF"
    },
    
    load = function() {
      # These shouldn't both be necessary but it seems they are 
      Sys.setenv("RETICULATE_PYTHON" = "~/.virtualenvs/parselmouth/bin/python3.9")
      use_virtualenv("~/.virtualenvs/parselmouth", required = TRUE)
      py_run_string("import parselmouth")
      self$loaded = TRUE
    },
    
    get_consonance_list = function(midi_list, timbre) {
      # message("... generating audio files...")
      
      spec <- furrr::future_map(midi_list, function(midi) {
        spectrum <- self$get_sparse_fr_spectrum(midi, timbre, coherent = COHERENT_WAVES)
        stopifnot(is(spectrum, "sparse_fr_spectrum"))
        tmp_file <- tempfile(fileext = ".wav")
        hrep::save_wav(spectrum, tmp_file, amplitude = 0.05)
        f0_ceiling <- hrep::midi_to_freq(hrep::get_bass_pi(midi) - 4)
        list(
          file = jsonlite::unbox(tmp_file),
          f0_ceiling = jsonlite::unbox(f0_ceiling)
        )
      }, .progress = FALSE)
      
      json_input <- tempfile(fileext = ".json")
      json_output <- tempfile(fileext = ".json")
      
      jsonlite::write_json(spec, json_input)
      
      if (!self$loaded) {
        self$load()
      }
      
      reticulate::py_run_file("src/praat_consonance.py")
      
      cmd <- sprintf("analyse_chords('%s', '%s', 16)",
                     json_input, json_output)
      
      reticulate::py_run_string(cmd)
      
      map_chr(spec, "file") %>% file.remove() # deleting audio files
      
      raw <- jsonlite::read_json(json_output)
      map_dbl(raw, "f0")
    },
    
    get_consonance = function(midi, timbre) {
      self$get_consonance_list(list(midi), timbre)
    }
  )
)

praat_analyze <- function(midi, spectrum) {
  stopifnot(is(spectrum, "sparse_fr_spectrum"))
  tmp_file <- tempfile(fileext = ".wav")
  hrep::save_wav(spectrum, tmp_file, amplitude = 0.05)
  
  f0_ceiling <- hrep::midi_to_freq(hrep::get_bass_pi(midi) - 4)
  
  py_run_string(sprintf("sound = parselmouth.Sound('%s')", tmp_file))
  
  # Harmonicity
  py_run_string(sprintf("harmonicity = sound.to_harmonicity(time_step=0.1)"))
  harmonicity <- as.numeric(py$harmonicity$values)
  harmonicity <- harmonicity[harmonicity > -199]
  harmonicity <- median(harmonicity)
  
  # f0
  py_run_string(sprintf("pitch = sound.to_pitch(time_step=0.1, pitch_floor=10, pitch_ceiling=%.2f)", f0_ceiling))
  f0 <- py$pitch$to_matrix()$values %>% as.numeric() %>% median()
  
  list(harmonicity = harmonicity,
       f0 = f0)
}

praat_cache <- "~/Downloads/praat-cache"
R.utils::mkdirs(praat_cache)
praat_analyze <- memoise::memoise(praat_analyze, cache = memoise::cache_filesystem(praat_cache))


# CombinedConsonanceModel <- R6Class(
#   "CombinedConsonanceModel",
#   inherit = ConsonanceModel,
#   public = list(
#     allow_parallel = TRUE,
#     
#     models = list(
#       interference = RevisedHutch78$new(),
#       harmonicity = Har18$new()
#     ),
#     
#     coef = list(
#       # From https://github.com/pmcharrison/incon/blob/master/R/har-2019.R
#       -1.62001025973261,
#       1.77992362857478
#     ),
#     
#     initialize = function(label, theory = "combnied", plot_colour = "purple", ...) {
#       super$initialize(
#         label = label,
#         theory = theory,
#         plot_colour = plot_colour,
#         ...
#       )
#     },
#     
#     get_consonance = function(midi, timbre) {
#       outputs = map_dbl(self$models, ~ .$get_consonance(midi, timbre))
#       sum(outputs * self$coef)
#     },
#   )
# )

MaMi.CoDi <- R6Class(
  'MaMi.CoDi',
  inherit = ConsonanceModel,
  public = list(
    
    allow_parallel = TRUE,
    
    initialize = function(label='mami.codi', 
                          theory = 'periodicity',
                          plot_colour = '#664433', ...) {
      super$initialize(
        label = label,
        theory = theory,
        plot_colour = plot_colour,
        ...
      )
    },
    
    get_consonance = function(midi, timbre) {
      chord.timbre.hertz = self$get_sparse_fr_spectrum(midi,
                                                       timbre,
                                                       COHERENT_WAVES)$x
      
      # measure consonance with each pitch as the tonic
      # whichever tonic results in the most major chord is the winner
      mami.codi = midi %>% purrr::imap(function(tonic.midi, index) {
        tonic.hertz = hrep::midi_to_freq(tonic.midi)
        tonic.timbre.hertz = self$get_sparse_fr_spectrum(tonic.midi,
                                                         timbre[index],
                                                         COHERENT_WAVES)$x
        
        mami.codi.R::mami.codi.hertz(tonic.hertz,
                               tonic.timbre.hertz,
                               chord.timbre.hertz)
      }) %>% bind_rows %>% arrange(desc(major_minor))
      mami.codi$consonance_dissonance[[1]]
    }
  )
)
