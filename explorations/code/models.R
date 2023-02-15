models <- list()

# winner so far: m1 t1 h2 l-1 r100
for (m in 1) {
  for (r in 80:89) {
    for (t in 1) {
      for (h in 2) {
        for (l in -1) {
          model <- MaMi.CoDi$new(
            options = list(
              raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
              raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
            ),
            metric         = m,
            resolution     = r,
            high_register  = h,
            low_register   = l,
            tonic_selector = t
          )
          models <- append(models, model)
        }
      }
    } 
  }
}

EXPERIMENTS <- list(
  Experiment(
    "stretched",
    timbre = BasicHarmonicTone$new(
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 2.1,
      label = "stretched"
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
    "compressed",
    timbre = BasicHarmonicTone$new(
      label = "compressed",
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
    "bonang",
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
  ),
  Experiment(
    "harmonic",
    timbre = BasicHarmonicTone$new(
      label = "harmonic",
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
  )
)
