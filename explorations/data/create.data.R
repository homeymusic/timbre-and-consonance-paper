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

