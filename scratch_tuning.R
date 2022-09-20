inputs <- c(
  "output/batches/Major 3rd (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/Major 3rd (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).rds",
  "output/batches/Major 3rd (pure tones)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/Major 3rd (pure tones)/models/Hutchinson & Knopoff (1978) (revised).rds",
  "output/batches/Major 6th (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/Major 6th (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).rds",
  "output/batches/Major 6th (pure tones)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/Major 6th (pure tones)/models/Hutchinson & Knopoff (1978) (revised).rds",
  "output/batches/Octave (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/Octave (3 dB roll-off)/models/Hutchinson & Knopoff (1978) (revised).rds",
  "output/batches/Octave (pure tones)/models/Hutchinson & Knopoff (1978) (revised).csv",
  "output/batches/Octave (pure tones)/models/Hutchinson & Knopoff (1978) (revised).rds"
)

for (i in inputs) suppressWarnings(file.remove(i))

source("~/git/timbre-and-consonance-paper/scripts/analysis/010-run-model-batches.R")

source("scripts/analysis/015-figure-setup.R")

TUNING_EXPRERIMENTS <- list(
  list(
    experiments = list(
      "Major 3rd (pure tones)" = EXPERIMENTS$`Major 3rd (pure tones)`,
      "Major 3rd (12 dB roll-off)" = EXPERIMENTS$`Major 3rd (12 dB roll-off)`,
      "Major 3rd (3 dB roll-off)" = EXPERIMENTS$`Major 3rd (3 dB roll-off)`
    ),
    interval_breaks = c(3.8, 4),
    domain = "Major 3rd"
  ),
  list(
    experiments = list(
      "Major 6th (pure tones)" = EXPERIMENTS$`Major 6th (pure tones)`,
      "Major 6th (12 dB roll-off)" = EXPERIMENTS$`Major 6th (12 dB roll-off)`,
      "Major 6th (3 dB roll-off)" = EXPERIMENTS$`Major 6th (3 dB roll-off)`
    ),
    interval_breaks = c(8.8, 9),
    domain = "Major 6th"
  ),
  list(
    experiments = list(
      "Octave (pure tones)" = EXPERIMENTS$`Octave (pure tones)`,
      "Octave (12 dB roll-off)" = EXPERIMENTS$`Octave (12 dB roll-off)`,
      "Octave (3 dB roll-off)" = EXPERIMENTS$`Octave (3 dB roll-off)`
    ),
    interval_breaks = c(11.9, 12.1),
    domain = "Octave"
  )
)

plot_data <- TUNING_EXPRERIMENTS %>% 
  map2(1:3, function(x, i) {
    y <- PlotDyadModelsExperimentsRollOff$new(
      experiments = x$experiments,
      models = c(
        "Interference" = "Hutchinson & Knopoff (1978)",
        # "Interference (revised)" = "Hutchinson & Knopoff (1978) (revised)",
        # "Interference" = "Sethares (1993)",
        # "Interference" = "Vassilakis (2001)",
        "Harmonicity" = "Harrison & Pearce (2018)",
        # "Harmonicity" = "Milne (2013)"
        # "Harmonicity" = "Praat (F0)",
        "Combined" = "combined"
      ),
      label_spectrum = i == 1,
      label_measure = i == 1,
      label_dyad = FALSE,
      label_experiment = "spectrum",
      plot_spectra = FALSE,
      profile_legend = FALSE,
      interval_breaks = x$interval_breaks,
      separate_roll_off_rows = TRUE,
      rel_widths = c(4, 6)
    )
    timbres <- map(x$experiments, "timbre")
    timbre_labels <- map_chr(timbres, "label")
    profiles <- y$profile_plot_data %>% 
      # group_by(timbre, measure) %>%
      # group_by(measure) %>%
      # y$rescale_all() %>% 
      ungroup() %>% 
      mutate(
        domain = x$domain,
        timbre = factor(timbre, levels = timbre_labels),
        measure = recode_factor(
          measure,
          "Participants" = "Pleasantness",
          "Interference" = "Interference",
          "Harmonicity" = "Harmonicity",
          "Combined" = "Combined",
        ),
        size = if_else(measure == "Pleasantness", 0.85, 0.3)
      )
    
    list(
      profiles = profiles,
      bootstrapped_peaks = map2_dfr(
        .x = x$experiments,
        .y = names(x$experiments),
        .f = get_bootstrapped_peaks,
        margin_size = 0.05,
        prob_threshold = 0.0 # We keep a record of all the peaks for now, but will only plot a subset 
      ) %>%
        mutate(domain = x$domain, measure = "Pleasantness")
    )
  }) %>% 
  {
    list(
      profiles = map_dfr(., "profiles"),
      bootstrapped_peaks = map_dfr(., "bootstrapped_peaks")
    )
  }

vlines <-
  bind_rows(
    tibble(domain = "Major 3rd", ratio = 5/4),
    tibble(domain = "Major 6th", ratio = 5/3),
    tibble(domain = "Octave", ratio = 2),
  ) %>% 
  mutate(
    interval = ratio_to_semitones(ratio),
    type = "Just intonation"
  ) %>% 
  bind_rows(
    tibble(domain = "Major 3rd", ratio = NA, interval = 4, type = "Equal temperament"),
    tibble(domain = "Major 6th", ratio = NA, interval = 9, type = "Equal temperament"),
    tibble(domain = "Octave", ratio = NA, interval = 12, type = "Equal temperament")
  ) %>%
  mutate(
    type = stringr::str_pad(type, width = 25, side = "right")
  )

get_tuning_x_axis_ticks <- function(x) {
  lower <- ceiling(min(x) * 10) / 10
  upper <- floor(max(x) * 10) / 10
  # c(ceiling(min(x) * 10) / 10, floor(max(x) * 10) / 10)
  
  domain <- if (lower < 4) "3rd" else if (lower < 9) "6th" else "Octave"
  
  breaks <- c(lower, upper)
  labels <- c(lower, upper)
  
  if (domain == "3rd") {
    breaks <- append(breaks, c(ratio_to_semitones(5/4), 4))
    labels <- append(labels, c("JI", "ET"))
  } else if (domain == "6th") {
    breaks <- append(breaks, c(ratio_to_semitones(5/3), 9))
    labels <- append(labels, c("JI", "ET"))
  } else if (domain == "Octave") {
    breaks <- append(breaks, 12)
    labels <- append(labels, "JI/ET")
  } else stop()
  
  list(breaks = breaks, labels = labels)
}

filter_and_rename_timbre <- function(df) {
  df %>% 
    filter(timbre %in% c("Pure tones", "3 dB roll-off")) %>%
    mutate(
      timbre = recode_factor(
        timbre,
        "Pure tones" = "Pure tones",
        "3 dB roll-off" = "Complex tones"
      )
    )
}

p <- 
  plot_data$profiles %>% 
  filter_and_rename_timbre() %>%
  # mutate(timbre = factor(timbre, levels = c("Pure tones", "3 dB roll-off"))) %>%
  ggplot(aes(
    interval, 
    value, 
    ymin = ymin,
    ymax = ymax,
    alpha = timbre,
    linetype = timbre,
    colour = colour,
    size = size
  )) + 
  geom_rect(
    data = plot_data$bootstrapped_peaks %>% 
      filter(prob >= 0.95) %>% 
      filter_and_rename_timbre() %>%
      mutate(measure = factor(measure, levels = levels(plot_data$profiles$measure))),
    aes(
      xmin = avg_interval - 1.96 * se_interval,
      xmax = avg_interval + 1.96 * se_interval,
      ymin = -Inf,
      ymax = Inf,
      linetype = timbre
      # y = mean_curve_peaks$value
    ),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.1,
    color = "pink",
    show.legend = FALSE
  ) +
  geom_ribbon(colour = NA, fill = "lightgrey") +
  geom_vline(
    aes(xintercept = interval),
    data = vlines, colour = "grey85", linetype = "dotted") +
  geom_line(alpha = 1) + 
  geom_point(
    aes(actual_interval, actual_value),
    data = plot_data$bootstrapped_peaks %>%
      filter(prob >= 0.95) %>% 
      filter_and_rename_timbre() %>%
      mutate(measure = factor(measure, levels = levels(plot_data$profiles$measure))),
    inherit.aes = FALSE,
    # size = 10*0.21, # 10*0.21
    colour = "red"
  ) +
  scale_x_continuous(
    "Interval (semitones)", 
    breaks = function(x) get_tuning_x_axis_ticks(x)$breaks,
    labels = function(x) get_tuning_x_axis_ticks(x)$labels
  ) + 
  scale_y_continuous(NULL) + 
  scale_colour_identity(NULL) +
  scale_linetype_manual(NULL, values = c("solid", "dashed")) +
  scale_size_identity(NULL) + 
  scale_alpha_manual(
    "Roll off", 
    values = seq(
      from = 0.2, 
      to = 1, 
      length.out = length(levels(plot_data$profiles$timbre))
    )
  ) +
  # ggh4x::facet_nested(
  facet_grid(
    # rows = vars(measure, timbre),
    rows = vars(measure),
    cols = vars(domain),
    scales = "free",
    switch = "y",
    # nest_line = TRUE
  ) +
  reverse_interference_scales() +
  ggh4x::force_panelsizes(rows = c(2.5, 1, 1)) +
  # ggh4x::force_panelsizes(rows = c(3, 3, 3, 1, 1, 1, 1, 1, 1)) +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    axis.text.y = element_text(size = 8),
    # axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    # axis.line.y = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    axis.text.x = element_text(size = 10),
    panel.spacing.y = unit(1.5, "lines")
  ) + 
  guides(
    alpha = FALSE,
    linetype = guide_legend(
      reverse = FALSE, 
      override.aes = list(fill = NA)
    )
  )

print(p)
