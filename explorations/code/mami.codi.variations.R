source("explorations/code/setup.R")

models <- list(
  MaMi.CoDi$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  ))
)

EXPERIMENTS <- list(
  Experiment(
    "Harmonic dyads (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "Harmonic",
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/rating_dyh3dd.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),  
  Experiment(
    "Stretched dyads (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 2.1,
      label = "Stretched"
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15),
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/rating_dys3dd.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ), 
  Experiment(
    "Compressed dyads (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "Compressed",
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 1.9
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/rating_dyc3dd.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),
  Experiment(
    "Bonang dyads",
    timbre = GamelanTone$new(),
    timbre_bass = BasicHarmonicTone$new(
      n_harmonics = 4,
      decay_dB_per_octave = 0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/Rating/gamelan_dyad_gamdyrt.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      bootstrap_iter = BOOTSTRAP_REPS,
      smooth_bandwidth = experiment$smooth_bandwidth
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  )
)
