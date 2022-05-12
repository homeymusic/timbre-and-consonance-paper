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
  list_models(path) %>% 
    set_names(., .) %>% 
    map(import_model, dir = path)
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