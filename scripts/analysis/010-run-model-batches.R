library(tidyverse)
library(furrr)
library(ggpubr)
library(lazyeval)

theme_set(theme_pubr())

source("src/ConsonanceModel.R")
source("src/DyadModelProfile.R")
source("src/DyadRating.R")
source("src/DyadRatingByRollOffSlice.R")
source("src/TriadGSP.R")
source("src/Timbre.R")
source("src/TriadModelProfile.R")
source("src/utils.R")
source("src/instrument-timbres.R")
source("src/parameters.R")

OUTPUT_DIR <- "output/batches"

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession)
  multisession_launched <- TRUE
}

if (FALSE) {
  # Delete output directory, forcing all results to regenerate
  unlink(OUTPUT_DIR, recursive = TRUE)
}

# Note: it's helpful to keep the resolution consistent for the 
# behavioural and the model analyses so that the rasterized 
# plotting works well.

models <- list(
  Hutch78$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
    # smooth = FALSE
  )),
  RevisedHutch78$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
    # smooth = TRUE,
    # smooth_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    # smooth_resolution_2d = DEFAULT_RESOLUTION_MODEL_1D,
    # smooth_sigma_macro = BEHAVIOURAL_SMOOTH_BROAD,
    # smooth_sigma_micro = BEHAVIOURAL_SMOOTH_NARROW
  )),
  Seth93$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
    # smooth = FALSE
  )),
  Vass01$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
    # smooth = FALSE
  )),
  PraatF0$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = 100
    # smooth = FALSE
  )),
  Milne13$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
    # smooth = TRUE,
    # smooth_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    # smooth_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D,
    # smooth_sigma = 0.03 # motivated by the fact that the model discretizes into cents
  )),
  Har18$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
    # smooth = TRUE,
    # smooth_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    # smooth_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D,
    # smooth_sigma = 0.03 # motivated by the fact that the model discretizes into cents
  )),
  # RevisedHar18$new(options = list(
  #   raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
  #   raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  # ))
  # MaMi.CoDi.Lowest.As.Tonic$new(options = list(
  #   raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
  #   raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  # )),
  # MaMi.CoDi.Highest.As.Tonic$new(options = list(
  #   raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
  #   raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  # )),
  # MaMi.CoDi.All.As.Tonic$new(options = list(
  #   raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
  #   raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  # )),
  # MaMi.CoDi.Max.As.Tonic$new(options = list(
  #   raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
  #   raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  # )),
  MaMi.CoDi.Major.As.Tonic$new(options = list(
    raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
    raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  ))
  # MaMi.CoDi.Minor.As.Tonic$new(options = list(
  #   raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
  #   raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  # )),
  # MaMi.CoDi.Major.D.0.01.As.Tonic$new(options = list(
  #   raw_resolution_1d = DEFAULT_RESOLUTION_MODEL_1D,
  #   raw_resolution_2d = DEFAULT_RESOLUTION_MODEL_2D
  # ))
)

Experiment <- function(
    label, 
    timbre, 
    domain, 
    data, 
    smooth_bandwidth,
    timbre_bass = NULL,
    rescale_combined_model_within_experiment = TRUE
) {
  x <- as.list(environment())
  class(x) <- c("Experiment", class(x))
  x
}

Domain <- function(
    label, 
    n_dimensions, 
    int_range = NULL, 
    int_1_range = NULL, 
    int_2_range = NULL
) {
  x <- as.list(environment())
  class(x) <- c("Domain", class(x))
  x
}

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
    "Harmonic dyads (3 dB roll-off) (Korean)",
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
      "input/data-csv/rating/korean_dyad_harm.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),
  Experiment(
    "Harmonic triads (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "Harmonic",
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Triads",
      n_dimensions = 2,
      int_1_range = c(0.5, 8.5),
      int_2_range = c(0.5, 8.5)
    ),
    data = ~ TriadGSP$new(
      file = "input/data-csv/GSP/harmonic_GSP_3db_trdh3d.csv",
      smooth_bandwidth = experiment$smooth_bandwidth,
      resolution = RESOLUTION_BEHAVIOURAL_2D,
      int_1_range = c(0.5, 8.5),
      int_2_range = c(0.5, 8.5)
    ),
    smooth_bandwidth = BANDWIDTH_GSP_2D,
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
    "Stretched dyads (3 dB roll-off) (Korean)",
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
      "input/data-csv/rating/korean_dyad_str.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD,
  ),
  Experiment(
    "Stretched triads (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "Stretched",
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 2.1
    ),
    domain = Domain(
      label = "Triads",
      n_dimensions = 2,
      int_1_range = c(0.5, 8.5),
      int_2_range = c(0.5, 8.5)
    ),
    data = ~ TriadGSP$new(
      file = "input/data-csv/GSP/stretched_GSP_3db_trds3d.csv",
      smooth_bandwidth = experiment$smooth_bandwidth,
      resolution = RESOLUTION_BEHAVIOURAL_2D,
      int_1_range = c(0.5, 8.5),
      int_2_range = c(0.5, 8.5)
    ),
    smooth_bandwidth = BANDWIDTH_GSP_2D,
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
    "Compressed dyads (3 dB roll-off) (Korean)",
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
      "input/data-csv/rating/korean_dyad_comp.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),
  Experiment(
    "Compressed triads (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "Compressed",
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 1.9
    ),
    domain = Domain(
      label = "Triads",
      n_dimensions = 2,
      int_1_range = c(0.5, 8.5),
      int_2_range = c(0.5, 8.5)
    ),
    data = ~ TriadGSP$new(
      file = "input/data-csv/GSP/compressed_GSP_3db_trdc3d.csv",
      smooth_bandwidth = experiment$smooth_bandwidth,
      resolution = RESOLUTION_BEHAVIOURAL_2D,
      int_1_range = c(0.5, 8.5),
      int_2_range = c(0.5, 8.5)
    ),
    smooth_bandwidth = BANDWIDTH_GSP_2D
  ),
  Experiment(
    "Harmonic dyads (5 equal harmonics)",
    timbre = HarmonicTone$new(
      label = "5 equal harmonics",
      harmonic_amplitudes = c(1, 1, 1, 1, 1),
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/rating_w3rdd.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      bootstrap_iter = BOOTSTRAP_REPS,
      smooth_bandwidth = experiment$smooth_bandwidth
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),
  Experiment(
    "Harmonic dyads (no 3rd harmonic)",
    timbre = HarmonicTone$new(
      label = "No 3rd harmonic",
      harmonic_amplitudes = c(1, 1, 0, 1, 1),
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/rating_wo3rdd.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      bootstrap_iter = BOOTSTRAP_REPS,
      smooth_bandwidth = experiment$smooth_bandwidth
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),
  Experiment(
    "Pure dyads",
    timbre = PureTone$new(
      label = "Pure"
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/pure_dyad_purdyrt.csv",
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      bootstrap_iter = BOOTSTRAP_REPS,
      smooth_bandwidth = experiment$smooth_bandwidth
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),
  Experiment(
    "Flute dyads",
    timbre = HarmonicTone$new(
      label = "Flute",
      harmonic_amplitudes = get_instrument_amplitudes("flute", num_harmonics = 10),
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      read_csv("input/data-csv/Rating/rating_flute_harmonic_harflt.csv", col_types = cols()) %>%
        filter(synth == "flute"),
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      bootstrap_iter = BOOTSTRAP_REPS,
      smooth_bandwidth = experiment$smooth_bandwidth
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),
  Experiment(
    "Guitar dyads",
    timbre = HarmonicTone$new(
      harmonic_amplitudes = get_instrument_amplitudes("guitar", num_harmonics = 10),
      octave_definition = 2.0,
      label = "Guitar"
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      read_csv("input/data-csv/Rating/rating_guitar_harmonic_hargtr.csv", col_types = cols()) %>%
        filter(synth == "guitar"),
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      bootstrap_iter = BOOTSTRAP_REPS,
      smooth_bandwidth = experiment$smooth_bandwidth
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD
  ),
  Experiment(
    "Piano dyads",
    timbre = HarmonicTone$new(
      harmonic_amplitudes = get_instrument_amplitudes("piano", num_harmonics = 10),
      octave_definition = 2.0,
      label = "Piano"
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = ~ DyadRating$new(
      read_csv("input/data-csv/Rating/rating_piano_harmonic_harpno.csv", col_types = cols()) %>%
        filter(synth == "piano"),
      int_range = c(0, 15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      bootstrap_iter = BOOTSTRAP_REPS,
      smooth_bandwidth = experiment$smooth_bandwidth
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
  ),
  Experiment(
    "Major 3rd (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "3 dB roll-off",
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(3.65, 4.15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_4_tun3p9.csv",
      int_range = c(3.65, 4.15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # gamma = TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW,
  ),
  Experiment(
    "Major 3rd (12 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "12 dB roll-off",
      n_harmonics = 10,
      decay_dB_per_octave = 12,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(3.65, 4.15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_4_t12d3p9_12db.csv",
      int_range = c(3.65, 4.15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # gamma = TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW,
  ),
  Experiment(
    "Major 3rd (pure tones)",
    timbre = BasicHarmonicTone$new(
      label = "Pure tones",
      n_harmonics = 1,
      decay_dB_per_octave = 12,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(3.65, 4.15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_4_tunp39.csv",
      int_range = c(3.65, 4.15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # gamma = TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW
  ),
  Experiment(
    "Major 6th (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "3 dB roll-off",
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(8.65, 9.15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_9_tun8p9.csv",
      int_range = c(8.65, 9.15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # gamma = TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW,
  ),
  Experiment(
    "Major 6th (12 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "12 dB roll-off",
      n_harmonics = 10,
      decay_dB_per_octave = 12,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(8.65, 9.15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_9_t128p9_12db.csv",
      int_range = c(8.65, 9.15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # gamma = TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW,
  ),
  Experiment(
    "Major 6th (pure tones)",
    timbre = BasicHarmonicTone$new(
      label = "Pure tones",
      n_harmonics = 1,
      decay_dB_per_octave = 12,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(8.65, 9.15)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_9_tunp89.csv",
      int_range = c(8.65, 9.15),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW,
  ),
  Experiment(
    "Octave (3 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "3 dB roll-off",
      n_harmonics = 10,
      decay_dB_per_octave = 3,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(11.75, 12.25)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_12_tunoch.csv",
      int_range = c(11.75, 12.25),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW,
  ),
  Experiment(
    "Octave (12 dB roll-off)",
    timbre = BasicHarmonicTone$new(
      label = "12 dB roll-off",
      n_harmonics = 10,
      decay_dB_per_octave = 12,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(11.75, 12.25)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_12_t12och_12db.csv",
      int_range = c(11.75, 12.25),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW,
  ),
  Experiment(
    "Octave (pure tones)",
    timbre = BasicHarmonicTone$new(
      label = "Pure tones",
      n_harmonics = 1,
      decay_dB_per_octave = 12,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(11.75, 12.25)
    ),
    data = ~ DyadRating$new(
      "input/data-csv/rating/tuning_12_tunocp.csv",
      int_range = c(11.75, 12.25),
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      smooth_bandwidth = experiment$smooth_bandwidth,
      bootstrap_iter = BOOTSTRAP_REPS
      # TUNING_GAM_GAMMA
    ),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_NARROW,
  )
)

for (roll_off in c(2, 7, 12)) {
  .data <- function(roll_off) {
    force(roll_off)
    ~ DyadRatingByRollOffSlice$new(
      read_csv(
        "input/data-csv/Rating/rolloff_dyad_rodyrt.csv",
        col_types = cols()
      ) %>%
        rename(interval = intervals, roll_off = rolloff),
      int_range = c(0, 15),
      roll_off = roll_off,
      roll_off_sigma = ROLL_OFF_SMOOTH,
      interval_sigma = experiment$smooth_bandwidth,
      resolution = RESOLUTION_BEHAVIOURAL_1D,
      bootstrap_iter = BOOTSTRAP_REPS
    )
  }
  
  EXPERIMENTS[[1 + length(EXPERIMENTS)]] <- Experiment(
    "%s dB roll-off (harmonic dyads)" %>% sprintf(roll_off),
    timbre = BasicHarmonicTone$new(
      label = paste0(roll_off, " dB/octave\nroll-off"),
      n_harmonics = 10,
      decay_dB_per_octave = roll_off,
      octave_definition = 2.0
    ),
    domain = Domain(
      label = "Dyads",
      n_dimensions = 1,
      int_range = c(0, 15)
    ),
    data = .data(roll_off),
    smooth_bandwidth = BEHAVIOURAL_SMOOTH_BROAD,
    rescale_combined_model_within_experiment = FALSE
  )
}

export_behavioural <- function(dir, experiment) {
  message(sprintf(". Exporting behavioural data for %s...", experiment$label))
  R.utils::mkdirs(dir)
  
  export_csv <- experiment$domain$n_dimensions == 1
  
  paths <- list(csv = "csv", rds = "rds") %>% map(~ file.path(dir, paste0("profile", ".", .)))
  if (!export_csv) paths$csv <- NULL
  
  if (all(map_lgl(paths, file.exists))) {
    message("... skipped because output files exist already.")
    return()
  }
  
  behaviour <- f_eval(experiment$data, data = list(experiment = experiment))
  
  # behaviour <- if (is_formula(experiment$data)) {
  #   f_eval(experiment$data, data = list(experiment = experiment))
  # } else {
  #   experiment$data
  # }
  
  saveRDS(behaviour, paths$rds)
  
  if (export_csv) {
    write_csv(behaviour$profile, paths$csv)
  }
}

export_params <- function(output_dir, experiment) {
  experiment["rescale_combined_model_within_experiment"] %>%
    yaml::write_yaml(file.path(output_dir, "params.yml"))
}

get_model_profile <- function(model, experiment) {
  if (experiment$domain$n_dimensions == 1) {
    get_model_profile_1d(model, experiment)
  } else if (experiment$domain$n_dimensions == 2) {
    get_model_profile_2d(model, experiment)
  } else {
    stop(sprintf("Invalid n_dimensions: %s"), model$domain$n_dimensions)
  }
}

get_model_profile_1d <- function(model, experiment) {
  timbre_1 <- if (is.null(experiment$timbre_bass)) experiment$timbre else experiment$timbre_bass
  timbre_2 <- experiment$timbre
  
  # if (diff(experiment$domain$int_range) < 3) {
  #   scale <- "micro"
  #   if ("smooth_sigma_micro" %in% names(model$options)) {
  #     smooth_sigma <- model$options$smooth_sigma_micro
  #   } else {
  #     smooth_sigma <- model$options$smooth_sigma
  #   }
  # } else {
  #   scale <- "macro"
  #   if ("smooth_sigma_macro" %in% names(model$options)) {
  #     smooth_sigma <- model$options$smooth_sigma_macro
  #   } else {
  #     smooth_sigma <- model$options$smooth_sigma
  #   }
  # }
  
  DyadModelProfile$new(
    model = model,
    timbre_1 = timbre_1,
    timbre_2 = timbre_2,
    int_range = experiment$domain$int_range,
    raw_resolution = model$options$raw_resolution_1d,
    smooth = TRUE, #model$options$smooth,
    smooth_resolution = DEFAULT_RESOLUTION_MODEL_1D, #model$options$smooth_resolution_1d,
    smooth_sigma = experiment$smooth_bandwidth
  ) 
}

get_model_profile_2d <- function(model, experiment) {
  TriadModelProfile$new(
    model = model, 
    timbre = experiment$timbre,
    raw_resolution = model$options$raw_resolution_2d,
    int_1_range = experiment$domain$int_1_range, 
    int_2_range = experiment$domain$int_2_range,
    smooth = TRUE, #model$options$smooth,
    smooth_resolution = DEFAULT_RESOLUTION_MODEL_2D, #model$options$smooth_resolution_2d,
    smooth_sigma = BANDWIDTH_MODEL_2D # experiment$smooth_bandwidth * TRIAD_MODEL_SMOOTH_SIGMA_FACTOR #model$options$smooth_sigma
  )
}

export_timbre <- function(dir, experiment) {
  R.utils::mkdirs(dir)
  timbre <- experiment$timbre
  
  df <- 
    timbre$sparse_fr_spectrum(midi = freq_to_midi(1), coherent = COHERENT_WAVES) %>% 
    as_tibble() %>% 
    mutate(across(.fns = round, digits = 8)) %>% 
    rename(frequency = x, amplitude = y)
  
  filename <- gsub("/", " per ", timbre$label, fixed = TRUE)
  filename <- gsub("\n", " ", filename, fixed = TRUE)
  
  saveRDS(timbre, file.path(dir, paste0(filename, ".rds")))
  write_csv(df, file.path(dir, paste0(filename, ".csv")))
}

export_model <- function(dir, model, experiment) {
  message(sprintf(". Evaluating %s for '%s'...", model$label, experiment$label))
  R.utils::mkdirs(dir)
  
  export_csv <- experiment$domain$n_dimensions == 1
  
  paths <- 
    list(csv = "csv", rds = "rds") %>% 
    map(~ file.path(dir, paste0(model$label, ".", .)))
  
  if (!export_csv) paths$csv <- NULL
  
  if (all(map_lgl(paths, file.exists))) {
    message("... skipped because output files exist already.")
    return()
  }
  
  full <- get_model_profile(model, experiment)
  summary <- summarise_model_profile(full, experiment)
  
  saveRDS(
    list(
      full = full,
      summary = summary
    ),
    paths$rds
  )
  
  if (experiment$domain$n_dimensions == 1) {
    write_csv(summary, paths$csv)
  }
}

summarise_model_profile <- function(profile, experiment) {
  profile$profile %>% 
    select(starts_with("interval"), output) %>% 
    mutate(
      model_id = class(model)[[1]],
      model_label = model$label,
      model_theory = model$theory,
      model_colour = model$plot_colour,
      timbre_label = map_chr(model_id, ~ experiment$timbre$label)
    )
}

for (experiment in EXPERIMENTS) {
  message(sprintf("\nProcessing experiment '%s'...", experiment$label)) 
  
  output_dir <- file.path(OUTPUT_DIR, experiment$label)
  
  export_timbre(
    dir = file.path(output_dir, "timbre"),
    experiment
  )
  
  for (model in models) {
    export_model(
      dir = file.path(output_dir, "models"),
      model = model,
      experiment = experiment
    )
  }
  
  
  export_behavioural(
    dir = file.path(output_dir, "behaviour"),
    experiment = experiment
  )
  
  export_params(output_dir, experiment)
}
