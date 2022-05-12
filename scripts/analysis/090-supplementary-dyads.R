library(furrr)

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession)
  multisession_launched <- TRUE
}

source("scripts/analysis/015-figure-setup.R")
source("src/DyadRating.R")
source("src/DyadRatingByRollOffSlice.R")
source("src/parameters.R")
Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")

SUPPLEMENTARY_EXPS <- c(
  "Harmonic dyads (3 dB roll-off)",
  "Stretched dyads (3 dB roll-off)",
  "Compressed dyads (3 dB roll-off)",
  "Bonang dyads",
  "Harmonic dyads (5 equal harmonics)",
  "Harmonic dyads (no 3rd harmonic)",
  "Pure dyads",
  "2 dB roll-off (harmonic dyads)",
  "7 dB roll-off (harmonic dyads)",
  "12 dB roll-off (harmonic dyads)",
  "Flute dyads",
  "Guitar dyads",
  "Piano dyads",
  "Major 3rd (3 dB roll-off)",
  "Major 3rd (pure tones)",
  "Major 6th (3 dB roll-off)",
  "Major 6th (pure tones)",
  "Octave (3 dB roll-off)",
  "Octave (pure tones)",
  "Harmonic dyads (3 dB roll-off) (Korean)",
  "Stretched dyads (3 dB roll-off) (Korean)",
  "Compressed dyads (3 dB roll-off) (Korean)"
)

for (i in seq_along(SUPPLEMENTARY_EXPS)) {
  exp_label <- SUPPLEMENTARY_EXPS[i]
  experiment <- EXPERIMENTS[[exp_label]]
  
  # Explore models ####
  plotting <- PlotDyadModelExploration$new(
    experiment = experiment,
    rescale_mode = "none",
    models = MODELS_ALL,
    consonance_label = "Pleasantness",
    return_plot_list = TRUE,
  )
  
  interval_range <- plotting$profile_plot_data$interval %>% range() %>% diff()
  
  plot <- plotting$plot$profiles + 
    scale_x_continuous(
      "Interval (semitones)",
      breaks = if (interval_range > 10) 0:15 else waiver()
    ) +
    scale_y_continuous("Pleasantness") +
    theme(
      axis.ticks.y = element_line(), strip.placement = "outside",
      axis.text.y = element_text(),
      panel.grid.major.x = element_line(colour = "grey95"), 
      panel.spacing.y = unit(20, "pt")
    ) + 
    ggh4x::facetted_pos_scales(
      y = list(
        measure_group == "Interference" ~ scale_y_continuous(
          breaks = scales::extended_breaks(n = 4), 
          labels = function(x) -x
        )
      )
    )
  
  output_dir <- file.path("output/plots/supplementary/by-model")
  R.utils::mkdirs(output_dir)
  
  for (ext in c(".pdf", ".png", ".svg")) {
    ggsave(
      plot = plot,
      filename = paste0(i, " - ", exp_label, ext),
      path = output_dir, 
      width = 14, height = 18, scale = 0.6, dpi = 150
    )
  }
  
  write_csv(
    plotting$profile_plot_data, 
    file.path(output_dir, paste0(i, " - ", exp_label, ".csv"))
  )
  
  # Explore musicianship ####
  if (!grepl("Korean", exp_label)) {
    # The Korean data doens't have musicianship information
    
    is_roll_off_experiment <- grepl("[0-9]+ dB roll-off \\(harmonic dyads\\)", experiment$label)
    
    raw_data <- if (is.character(experiment$behaviour$full$file)) {
      read_csv(experiment$behaviour$full$file, col_types = cols())
    } else if (is_roll_off_experiment) {
      read_csv("input/data-csv/Rating/rolloff_dyad_rodyrt.csv", col_types = cols())
    } else {
      experiment$behaviour$full$file
    }
    
    median_musical_exp <- median(raw_data$musical_exp, na.rm = TRUE)
    split_datasets <- list(
      # `All participants` = raw_data,
      `Nonmusicians` = raw_data %>% filter(musical_exp <= median_musical_exp),
      `Musicians` = raw_data %>% filter(musical_exp > median_musical_exp)
    ) %>%
      set_names(c(
        # "All participants",
        sprintf("'Nonmusicians' (≤ %.1f years musical experience)", median_musical_exp),
        sprintf("'Musicians' (> %.1f years musical experience)", median_musical_exp)
      ))
    
    df <- map2_dfr(split_datasets, names(split_datasets), function(data, participant_group) {
      (if (is_roll_off_experiment) {
        roll_off <- experiment$timbre$full$decay_dB_per_octave
        DyadRatingByRollOffSlice$new(
          data %>% rename(interval = intervals, roll_off = rolloff),
          int_range = c(0, 15),
          roll_off = roll_off,
          roll_off_sigma = experiment$behaviour$full$roll_off_sigma,
          interval_sigma = experiment$behaviour$full$interval_sigma,
          resolution = experiment$behaviour$full$resolution,
          bootstrap_iter = BOOTSTRAP_REPS
        )
      } else {
        DyadRating$new(
          data,
          int_range = experiment$behaviour$full$int_range,
          resolution = experiment$behaviour$full$resolution,
          bootstrap_iter = BOOTSTRAP_REPS,
          smooth_sigma = experiment$behaviour$full$smooth_sigma
        )
      })$profile %>%
        mutate(
          participant_group = participant_group
        )
      #   rating_min = min(rating, na.rm = TRUE),
      #   rating_max = max(rating, na.rm = TRUE),
      #   rating_range = rating_max - rating_min,
      #   rating_norm = if_else(
      #     rating_range == 0, 
      #     0.5, 
      #     (rating - rating_min) / rating_range
      #   )
      # )
    })
    
    df %>%
      ggplot(aes(
        interval, 
        rating, 
        # linetype = participant_group,
        ymin = rating - 1.96 * rating_boot_se,
        ymax = rating + 1.96 * rating_boot_se
      )) + 
      geom_line(
        mapping = aes(interval, rating),
        data = experiment$behaviour$summary, 
        inherit.aes = FALSE,
        linetype = "dotted",
      ) +
      geom_ribbon(colour = NA, fill = "black", alpha = 0.15) +
      geom_line() + 
      facet_wrap(~ participant_group, ncol = 1) + 
      scale_x_continuous(
        "Interval (semitones)", 
        breaks = if (interval_range > 10) 0:15 else waiver()
      ) +
      scale_y_continuous("Pleasantness") + #, breaks = c(0, 1)) + 
      theme(
        panel.grid.major.x = element_line(colour = "grey95"),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        panel.spacing = unit(1.5, "lines"),
        legend.position = "none",
      )
    # ggtitle(exp_label)
    
    output_dir <- file.path("output/plots/supplementary/by-musicianship")
    R.utils::mkdirs(output_dir)
    
    # If you get a Cairo error on a Mac, you may need to install XQuartz.
    # If you get an 'Unable to revert mtime' error on a Mac, you may need to run brew install libmagic.
    for (ext in c(".pdf", ".png", ".svg")) {
      ggsave(
        paste0(i, " - ", exp_label, ext),
        path = output_dir, 
        width = 14, height = 10, scale = 0.6, dpi = 150,
        device = if (ext == ".pdf") cairo_pdf
      )
    }
    
    write_csv(
      df,
      file.path(output_dir, paste0(i, " - ", exp_label, " - profiles", ".csv"))
    )
    
    writeLines(
      c(
        sprintf("Median musical experience = %s years", median_musical_exp),
        sprintf("Number of bootstrap repetitions = %i", BOOTSTRAP_REPS)
      ),
      file.path(output_dir, paste0(i, " - ", exp_label, " - musicianship", ".txt"))
    )
  }
}


by_musicianship_dir <- "output/plots/supplementary/by-musicianship"
by_musicianship_stats <- 
  list.files(by_musicianship_dir, pattern = "[0-9]+.*\\.csv$") %>%
  map(function(file) {
    read_csv(
      file.path(by_musicianship_dir, file), 
      col_types = cols()
    ) %>% 
      mutate(
        experiment = file
      )
  }) %>%
  map_dfr(function(df) {
    long_df <- 
      df %>%
      pivot_wider(
        id_cols = "interval",
        names_from = "participant_group",
        values_from = "rating"
      ) %>%
      rename_with(
        ~ gsub(" \\(.*\\)", "", .)
      ) %>% 
      rename_with(
        ~ gsub("'", "", .)
      )
    
    experiment <- unique(df$experiment)
    stopifnot(length(experiment) == 1)
    
    tibble(
      experiment = experiment,
      nonmusician_sd = sd(long_df$Nonmusicians),
      musician_sd = sd(long_df$Musicians),
      cor_spearman = cor(long_df$Nonmusicians, long_df$Musicians, method = "spearman")
    )
  })
write_csv(
  by_musicianship_stats, 
  file.path(by_musicianship_dir, "by_musicianship_stats.csv")
)


# These are high-level statistics about how musician and nonmusician
# judgments vary.

# Q: How consistent are their judgments?
# A: Rather consistent; the main exceptions are the tuning experiments with high roll off,
#    but these are special cases anyway because the lines are rather flat.
set.seed(1)
bootstrap_mean(
  by_musicianship_stats$cor_spearman,
  n = 1e5
)

# Q: How differentiated are the profiles? More differentiation
#    implies that the group has a stronger consonance-dissonance hierarchy.
bootstrap_mean(by_musicianship_stats$nonmusician_sd, n = 1e5)
bootstrap_mean(by_musicianship_stats$musician_sd, n = 1e5)

bootstrap_mean(
  by_musicianship_stats$musician_sd - by_musicianship_stats$nonmusician_sd, 
  n = 1e5
)
