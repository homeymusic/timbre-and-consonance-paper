list_experiments <- function() {
  list.dirs("output/batches", recursive = FALSE) %>% 
    set_names(., basename(.))  
}

import_experiment <- function(path) {
  list(
    label = basename(path),
    behaviour = import_behaviour(file.path(path, "behaviour")),
    models = import_models(file.path(path, "models")),
    timbre = import_timbre(file.path(path, "timbre"))
  )
}

import_behaviour <- function(path) {
  res <- list(full = readRDS(file.path(path, "profile.rds")))
  res$summary <- res$full$profile
  res
}

import_models <- function(path) {
  params <- yaml::read_yaml(file.path(path, "..", "params.yml"))
  list_models(path) %>% 
    set_names(., .) %>% 
    map(import_model, dir = path) %>% 
    add_combined_model(params)
}

rescale_to_range <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

add_combined_model <- function(models, params) {

  coef = c(
    # From https://github.com/pmcharrison/incon/blob/master/R/har-2019.R
    # -1.62001025973261,
    # 1.77992362857478
    # Actually, these original parameters aren't relevant any more due to 
    # the new adjustments to the Hutchinson-Knopoff model, which change
    # the scaling.
    -1, 0.75
  )
  models$Combined <- list()
  models$Combined$summary <- 
    models[c(
      # "Hutchinson & Knopoff (1978)",
      "Hutchinson & Knopoff (1978) (revised)",
      "Harrison & Pearce (2018)"
      # "Harrison & Pearce (2018) (revised)"
    )] %>%
    map(`$`, "summary") %>%
    bind_rows() %>%
    select(starts_with("interval"), output, model_id) %>%
    pivot_wider(names_from = "model_id", values_from = "output") %>%
    # mutate(Hutch78 = - Hutch78) %>% # Undo the reversal that we did in the preprocessing
    mutate(RevisedHutch78 = - RevisedHutch78) %>% # Undo the reversal that we did in the preprocessing
    mutate(
      # output = coef[1] * Hutch78 + coef[2] * Har18,
      output = coef[1] * RevisedHutch78 + coef[2] * Har18,
      output = if (params$rescale_combined_model_within_experiment) 
        output - median(output) else output,
        # rescale_to_range(output) else output,
      model_id = "Combined",
      model_label = "Combined",
      model_theory = "Composite",
      model_colour = "purple",
      timbre_label = unique(models[["Hutchinson & Knopoff (1978)"]]$summary$timbre_label)
    )
  models
}

import_model <- function(id, dir) {
  readRDS(sprintf("%s/%s.rds", dir, id))
}

list_models <- function(path) {
  list.files(path, pattern = "\\.rds$") %>% 
    gsub("\\.rds$", "", .)
}

import_timbre <- function(path) {
  rds_files <- list.files(path, pattern = "\\.rds", full.names = TRUE)
  csv_files <- list.files(path, pattern = "\\.csv", full.names = TRUE)
  stopifnot(length(rds_files) == 1,
            length(csv_files) == 1)
  full <- readRDS(list.files(path, pattern = "\\.rds", full.names = TRUE))
  summary <- read_csv(list.files(path, pattern = "\\.csv", full.names = TRUE), col_types = cols())
  label <- full$label
  list(
    label = label,
    full = full,
    summary = summary
  )
}