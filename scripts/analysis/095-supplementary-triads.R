source("scripts/analysis/015-figure-setup.R")

TRIAD_EXPERIMENTS <- list(
  "Stretched triads" = EXPERIMENTS$`Stretched triads (3 dB roll-off)`,
  "Harmonic triads" = EXPERIMENTS$`Harmonic triads (3 dB roll-off)`,
  "Compressed triads" = EXPERIMENTS$`Compressed triads (3 dB roll-off)`
)

root_dir <- "output/plots/supplementary/triads"
R.utils::mkdirs(root_dir)

for (i in seq_along(TRIAD_EXPERIMENTS)) {
  exp <- TRIAD_EXPERIMENTS[[i]]
  exp_label <- names(TRIAD_EXPERIMENTS)[[i]]
  
  R.utils::mkdirs(file.path(root_dir, exp_label))
  
  exp$behaviour$full$profile %>% 
    ggplot(aes(interval_1, interval_2, fill = relative_density)) + 
    geom_raster() + 
    scale_fill_viridis_c("Pleasantness\n(GSP relative density)") + 
    scale_x_continuous("Lower interval (semitones)", breaks = 0:9) +
    scale_y_continuous("Upper interval (semitones)", breaks = 0:9) +
    theme(
      aspect.ratio = 1, 
      legend.position = "right",
      legend.key.width = unit(0.25, "inches"),
    )
  
  
  for (ext in c(".png", ".pdf", ".svg"))
    ggsave(
      file.path(root_dir, exp_label, paste0("participants", ext)),
      width = 7, 
      height = 5,
      dpi = 200
    )
  
  R.utils::mkdirs(file.path(root_dir, exp_label, "models"))
  
  for (model in exp$models) {
    model_type <- unique(model$summary$model_theory)
    model_label <- unique(model$summary$model_label)
    is_interference_model <- model_type == "interference"
    reverse <- is_interference_model
    
    model$summary %>% 
      ggplot(aes(interval_1, interval_2, fill = if (is_interference_model) - output else output)) + 
      scale_x_continuous("Interval 1", breaks = 0:9) + 
      scale_y_continuous("Interval 2", breaks = 0:9) +
      geom_raster(interpolate = TRUE) +
      scale_fill_viridis_c(
        Hmisc::capitalize(model_type),
        option = "inferno", 
        direction = if (is_interference_model) -1 else 1
      ) + 
      theme(
        aspect.ratio = 1, 
        legend.position = "right",
        legend.key.width = unit(0.25, "inches"),
      )
    
    R.utils::mkdirs(file.path(root_dir, exp_label, "models", model_type))
    
    for (ext in c(".png", ".pdf", ".svg"))
      ggsave(
        file.path(root_dir, exp_label, "models", model_type, paste0(model_label, ext)),
        width = 6, 
        height = 5,
        dpi = 200
      )
  }
}
