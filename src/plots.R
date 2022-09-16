library(tidyverse)
library(ggpubr)
library(R6)
library(hrep)

theme_set(theme_pubr())

viridis_themes = c(
  "inferno",
  "viridis", 
  "mako"
) %>% set_names(., .)

viridis_palettes <- map(viridis_themes, ~ viridisLite::viridis(n = 1001, option = .))

plot_viridis_legend <- function(theme, reverse = FALSE) {
  p <- 
    tibble(color = 0:1) %>% 
    ggplot(aes(x = 0, y = 0, color = color)) + 
    geom_point() + 
    scale_color_viridis_c(
      NULL, 
      option = theme, 
      breaks = c(0, 1), 
      labels = if (reverse) 1:0 else 0:1,
      guide = guide_colorbar(
        ticks = FALSE
      )
    ) +
    theme(
      legend.position = "right",
      legend.key.height = unit(0.3, 'cm')
    )
  cowplot::plot_grid(get_legend(p))
}

get_viridis_colour <- function(value, palette) {
  stopifnot(all(value >= 0), all(value <= 1), length(palette) == 1)
  ind <- 1L + round(value * 1000)
  viridis_palettes[[palette]][ind]
}

zero_mean <- function(x) {
  x - mean(x, na.rm = TRUE)
}

zero_midpoint <- function(x, .min = NULL, .max = NULL) {
  ..min <- if (is.null(.min)) min(x, na.rm = TRUE) else .min
  ..max <- if (is.null(.max)) max(x, na.rm = TRUE) else .max
  midpoint <- (..max - ..min) / 2
  x - midpoint
}

PlotDyadModelsSingleExperiment <- function(
  experiment,
  models
) {
  res <- list()
  res$data <- 
    bind_rows(
      get_dyad_behaviour(experiment),
      get_dyad_models(experiment, models)
    )
  
  res$plot <-
    ggplot(res$data, aes(interval, value, colour = colour)) + 
    geom_line() + 
    scale_colour_identity() + 
    facet_wrap(~ measure, ncol = 1, scales = "free") + 
    scale_x_continuous("Interval (semitones)", breaks = seq(from = floor(min(res$data$interval)),
                                                            to = ceiling(max(res$data$interval)),
                                                            by = 1)) +
    scale_y_continuous(NULL)
  
  class(res) <- c("PlotDyadModelsSingleExperiment", class(res))
  res
}

get_plot_spectra <- function(timbre_labels, timbres) {
  map2_dfr(timbre_labels, timbres, function(timbre_label, timbre) {
    timbre$sparse_fr_spectrum(midi = 1, coherent = COHERENT_WAVES) %>% 
      sparse_pi_spectrum() %>% 
      smooth_pi_spectrum() %>% 
      as_tibble() %>% 
      rename(interval = x, amplitude = y) %>% 
      mutate(timbre_label = timbre_label,
             interval = interval - 1)
  }) %>%
    filter(
      interval <= max(interval[amplitude >= 0.001] + 1)
    ) %>% 
    mutate(
      timbre_label = factor(timbre_label, levels = timbre_labels),
      amplitude = amplitude / max(amplitude)
    )
}

PlotModelsExperiments <- R6Class(
  "PlotModelsExperiments",
  
  public = list(
    experiments = NULL,
    models = NULL,
    ablines = NULL,
    
    debug = NULL,
    
    profile_plot_data = NULL,
    plot = NULL,
    stats = NULL,
    
    colour_spectrum_by_timbre = FALSE,
    
    label_spectrum = NULL,
    label_dyad = NULL,
    label_experiment = character(),
    profile_legend = NULL,
    return_plot_list = FALSE,
    rel_widths = NULL,
    
    interval_breaks = NULL,
    
    dyad_measure_labels = NA_character_,
    consonance_label = NA_character_,
    
    realign_mode = "midpoint",
    rescale_mode = "range",
    
    reference_spectrum = NULL,
    plot_spectra = NA,
    
    timbre_labels = NULL,
    timbre_alphas = NULL,
    
    data_num_peaks = NULL,
    
    profile_row_heights = NULL,
    rescale_each_profile = TRUE,
    
    legend_position = NULL,
    spectrum_plot_direction = NULL,
    reverse_spectrum_plot_order = NULL,
    
    initialize = function(
      experiments,
      models,
      label_spectrum = TRUE,
      label_dyad = TRUE,
      ablines = "none",
      label_experiment = "consonance",
      dyad_measure_labels = "right",
      consonance_label = "Pleasantness",
      interval_breaks = waiver(),
      profile_legend = FALSE,
      return_plot_list = FALSE,
      rel_widths = if ("spectrum" %in% label_experiment) c(4, 6) else c(3, 6),
      debug = FALSE,
      reference_spectrum = NULL,
      plot_spectra = TRUE,
      timbre_alphas = rep(1, times = length(experiments)),
      profile_row_heights = NULL,
      legend_position = "right",
      spectrum_plot_direction = "vertical",
      reverse_spectrum_plot_order = FALSE
    ) {
      self$experiments <- experiments
      self$models <- models
      self$label_spectrum <- label_spectrum
      self$label_dyad <- label_dyad
      self$ablines <- ablines
      self$label_experiment <- label_experiment
      self$dyad_measure_labels <- dyad_measure_labels
      self$consonance_label <- consonance_label
      self$interval_breaks <- interval_breaks
      self$profile_legend <- profile_legend
      self$return_plot_list <- return_plot_list
      self$rel_widths <- rel_widths
      self$debug <- debug
      self$reference_spectrum <- reference_spectrum
      self$plot_spectra <- plot_spectra
      self$profile_row_heights <- profile_row_heights
      self$legend_position <- legend_position
      self$spectrum_plot_direction <- spectrum_plot_direction
      self$reverse_spectrum_plot_order <- reverse_spectrum_plot_order
      
      self$timbre_labels <- map_chr(experiments, ~ .$timbre$label) %>% unique()
      stopifnot(!anyDuplicated(self$timbre_labels))
      
      self$timbre_alphas <- timbre_alphas %>% set_names(self$timbre_labels)
      
      self$profile_plot_data <- self$prepare_profile_plot_data(experiments, models)
      self$plot <- self$make_plot(self$profile_plot_data)
    },
    
    realign_values = function(values, group_min, group_max, group_median) {
      if (self$realign_mode == "midpoint") {
        values - (group_max - group_min) / 2
      } else if (self$realign_mode == "median") {
        values - group_median
      } else if (self$realign_mode == "none") { 
        values
      } else {
        stop()
      }
    },
    
    rescale_values = function(
      x, 
      group_min, 
      group_max, 
      group_lower_quantile, 
      group_upper_quantile
    ) {
      if (self$rescale_mode == "range") {
        if_else(group_max == group_min, 0.5, (x - group_min) / (group_max - group_min))
      } else if (self$rescale_mode == "iqr") {
        (x - group_lower_quantile) / (group_upper_quantile - group_lower_quantile) 
        # pmax(- 2.5, pmin(2.5, res))
      } else if (self$rescale_mode == "none") {
        x
      } else {
        stop()
      }
    },
    
    get_behaviour = function(experiment) {
      stop("needs to be implemented")
    },
    
    get_models = function(experiment, models) {
      stop("needs to be implemented")
    },
    
    plot_profiles = function(profile_plot_data) {
      stop("needs to be implemented")
    },
    
    make_plot = function(profile_plot_data) {
      plots <- list()
      if (self$plot_spectra) {
        plots$spectra <- self$plot_timbres(self$experiments)
      }
      
      plots$profiles <- self$plot_profiles(profile_plot_data)
      if (!is.null(self$profile_row_heights))
        plots$profiles <- plots$profiles + 
        ggh4x::force_panelsizes(rows = self$profile_row_heights)
      
      if (self$return_plot_list) {
        return(plots)
      } else {
        return(cowplot::plot_grid(
          plotlist = plots,
          nrow = 1, 
          rel_widths = self$rel_widths
        ))
      }
    },
    
    prepare_profile_plot_data = function(experiments, models) {
      df <- 
        map(experiments, ~ bind_rows(
          self$get_behaviour(.),
          self$get_models(., models)
        )) %>% 
        bind_rows()
      
      if (!is.null(names(models))) {
        df$measure <- plyr::mapvalues(
          df$measure, 
          from = c(models),
          to = c(names(models)),
          warn_missing = FALSE
        )
      }
      
      measure_order <- c(
        "Participants",
        if (is.null(names(models))) models else names(models)
      )
      
      experiment_labels = map(experiments, "label")
      
      df <- 
        df %>% 
        mutate(
          measure = factor(measure, levels = unique(measure_order)),
          measure_group = factor(measure_group, levels = unique(measure_group)),
          experiment = plyr::mapvalues(
            experiment, 
            from = experiment_labels, 
            to = names(experiment_labels),
            warn_missing = FALSE
          ),
          experiment = factor(experiment, levels = names(experiment_labels)),
          ymin = if (!"ymin" %in% names(df)) NA_real_ else ymin,
          ymax = if (!"ymax" %in% names(df)) NA_real_ else ymax,
        )
      
      if (self$rescale_each_profile) {
        df %>% group_by(measure, timbre) %>% self$rescale_all() %>% ungroup()
      } else {
        df %>% 
          group_by(measure) %>% self$rescale_all() %>% ungroup() %>% 
          group_by(measure, timbre) %>% self$realign_all() %>% ungroup()
      }
    },
    
    rescale_all = function(df) {
      # df should have columns value, ymin, and ymax
      df %>% 
        mutate(
          group_min = min(value, na.rm = TRUE),
          group_max = max(value, na.rm = TRUE),
          group_lower_quantile = as.numeric(quantile(value, 0.25, na.rm = TRUE)),
          group_upper_quantile = as.numeric(quantile(value, 0.75, na.rm = TRUE)),
          value = self$rescale_values(value, group_min, group_max, group_lower_quantile, group_upper_quantile),
          ymin = self$rescale_values(ymin, group_min, group_max, group_lower_quantile, group_upper_quantile),
          ymax = self$rescale_values(ymax, group_min, group_max, group_lower_quantile, group_upper_quantile)
        ) %>% 
        select(- group_min, - group_max, - group_lower_quantile, - group_upper_quantile)
    },
    
    realign_all = function(df) {
      # df should have columns value, ymin, and ymax
      df %>% 
        mutate(
          group_min = min(value, na.rm = TRUE),
          group_max = max(value, na.rm = TRUE),
          group_median = median(value, na.rm = TRUE),
          value = self$realign_values(value, group_min, group_max, group_median),
          ymin = self$realign_values(ymin, group_min, group_max, group_median),
          ymax = self$realign_values(ymax, group_min, group_max, group_median)
        ) %>%
        select(- group_min, - group_max, - group_median)
    },
    
    plot_timbres = function(experiments, select_timbre = NULL) {
      
      
      df <- map_dfr(experiments, ~ tibble(
        timbre_label = .$timbre$label,
        timbre = list(.$timbre$full))
      ) %>% 
        {
          get_plot_spectra(
            timbre_labels = .$timbre_label,
            timbres = .$timbre
          )
        } %>% 
        group_by(timbre_label) %>% 
        group_split()
      
      bind_rows(df)
      
      df_main <- df %>% bind_rows() %>% mutate(type = "main")
      
      df_reference <- if (!is.null(self$reference_spectrum)) {
        self$timbre_labels %>% map_dfr(function(timbre_label) {
          df[[self$reference_spectrum]] %>% 
            mutate(
              timbre_label = !!timbre_label,
              type = "reference"
            )
        })
      }
      
      df_all <- bind_rows(
        df_main,
        df_reference
      )
      
      if (!is.null(select_timbre)) {
        df_all <- 
          df_all %>% 
          filter(timbre_label %in% select_timbre)
      }
      
      df_all %>% 
        mutate(
          plot_label = "Spectrum",
          type = factor(type, levels = c("main", "reference")),
          timbre_label = factor(
            timbre_label, 
            levels = if (self$reverse_spectrum_plot_order) rev(self$timbre_labels) else self$timbre_labels
          )
        ) %>% 
        ggplot(
          aes(
            x = interval, 
            y = amplitude,
            alpha = if (is.null(self$reference_spectrum)) timbre_label else type
          )
        ) +
        {
          if (self$colour_spectrum_by_timbre) 
            geom_line(aes(colour = timbre_label)) else 
              geom_line()
        } + 
        scale_x_continuous("Interval (semitones)") + 
        scale_y_continuous("Amplitude") + 
        {
          if (is.null(self$reference_spectrum)) {
            scale_alpha_manual(values = self$timbre_alphas)
          } else {
            scale_alpha_manual(values = c(main = 1, reference = 0.15)) 
          }
        } +
        {
          if (self$spectrum_plot_direction == "vertical") {
            facet_grid(timbre_label ~ plot_label, switch = "y") 
          } else if (self$spectrum_plot_direction == "horizontal") {
            facet_grid(plot_label ~ timbre_label)
          } else stop("Unrecognised spectrum_plot_direction: ", self$spectrum_plot_direction)
        } +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          legend.position = "none",
        ) +  {
          if (self$spectrum_plot_direction == "vertical") {
            theme(
              strip.text.x = element_text(colour = if (self$label_spectrum) "black" else "white"),
              strip.text.y.left = if ("spectrum" %in% self$label_experiment) element_text() else element_blank()
            )
          } else if (self$spectrum_plot_direction == "horizontal") {
            theme(
              strip.text.y = element_text(colour = if (self$label_spectrum) "black" else "white")
              # element_blank()
              # strip.text.x.left = if ("spectrum" %in% self$label_experiment) element_text() else element_blank()
            )
          } else stop("Unrecognised spectrum_plot_direction: ", self$spectrum_plot_direction)
        }
    },
    
    gam_cor = function(x, y) {
      df <- tibble(x, y)
      mod <- gam(y ~ s(x), data = df)
      cor(y, predict(mod, newdata = df %>% select(x)))
    },
    
    eval_accuracy = function(model_data, reference_data) {
      testthat::expect_equal(
        model_data %>% select(starts_with("interval")),
        reference_data %>% select(starts_with("interval"))
      )
      
      df <- bind_cols(
        model_data %>% select(starts_with("interval")),
        model_output = model_data$value,
        participants = reference_data$value
      )
      cors <- 
        c("pearson", "spearman") %>% 
        set_names(., paste("cor", ., sep = "_")) %>% 
        map(~ cor(df$model_output, df$participants, method = .))
      
      measure <- model_data %>% pull(measure) %>% unique()
      stopifnot(length(measure) == 1)
      bind_cols(
        measure = measure,
        cors %>% as_tibble(),
        cor_gam = self$gam_cor(df$model_output, df$participants)
      )
    }
  )
)

PlotDyadModelsExperiments <- R6Class(
  "PlotDyadModelsExperiments",
  inherit = PlotModelsExperiments,
  
  public = list(
    plot_bootstrap_peaks = NULL,
    bootstrapped_peaks = NULL,
    
    rescale_mode = "none",
    
    initialize = function(plot_bootstrap_peaks = FALSE, ...) {
      self$plot_bootstrap_peaks = plot_bootstrap_peaks
      super$initialize(...)
    },
    
    make_plot = function(...) {
      if (self$plot_bootstrap_peaks) {
        self$bootstrapped_peaks <- 
          map2_dfr(
            .x = self$experiments, 
            .y = names(self$experiments),
            .f = get_bootstrapped_peaks,
            margin_size = 0.5, 
            prob_threshold = 0.95
          ) %>% mutate(
            experiment = experiment %>% factor(levels(self$profile_plot_data$experiment)),
            measure = measure %>% factor(levels(self$profile_plot_data$measure))
          )
      }
      super$make_plot(...)
    },
    
    get_behaviour = function(experiment) {
      experiment$behaviour$summary %>% 
        transmute(
          experiment = !!experiment$label,
          interval = interval,
          value = rating,
          measure = "Participants",
          measure_group = "",
          colour = "black",
          timbre = !!experiment$timbre$label,
          ymin = value - 1.96 * rating_boot_se,
          ymax = value + 1.96 * rating_boot_se,
        )
    },
    
    get_models = function(experiment, models) {
      experiment$models[models] %>% 
        map("summary") %>% 
        bind_rows() %>% 
        transmute(
          experiment = !!experiment$label,
          interval = interval,
          value = output,
          measure = model_label,
          measure_group = Hmisc::capitalize(model_theory),
          colour = model_colour,
          timbre = timbre_label,
          ymin = value,
          ymax = value
        )
    },
    
    plot_profiles = function(profile_plot_data) {
      rows <- expand_grid(
        experiment = profile_plot_data$experiment %>% levels(),
        measure = profile_plot_data$measure %>% levels()
      ) %>% 
        mutate(row = seq_along(experiment))
      
      p <- profile_plot_data %>% 
        left_join(rows, by = c("experiment", "measure")) %>% 
        mutate(
          experiment = factor(experiment, levels = levels(profile_plot_data$experiment)),
          measure = factor(measure, levels = levels(profile_plot_data$measure)),
          col = (if (self$label_dyad) "Dyads" else ""),
          size = if_else(measure == "Participants", 0.85, 0.3)
        ) %>% 
        ggplot(aes(
          interval, 
          value, 
          ymin = ymin,
          ymax = ymax,
          colour = colour,
          alpha = timbre,
          size = size
        ))
      
      if (self$plot_bootstrap_peaks) {
        p <- p +
          geom_rect(
            data = self$bootstrapped_peaks,
            aes(
              xmin = avg_interval - 1.96 * se_interval,
              xmax = avg_interval + 1.96 * se_interval,
              ymin = -Inf,
              ymax = Inf
            ),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.1,
            colour = "pink"
          )
      }
      
      
      if (self$ablines == "stretching") {
        measures <- profile_plot_data$measure %>% unique()
        
        df <- expand_grid(
          experiment = names(self$experiments),
          measure = measures
        ) %>% 
          pmap_dfr(function(experiment, measure) {
            bind_rows(
              tibble(linetype = "dotted", interval = stretch_interval(c(12), octave_ratio = 1.9)),
              tibble(linetype = "dotted", interval = c(12)),
              tibble(linetype = "dotted", interval = stretch_interval(c(12), octave_ratio = 2.1))
            ) %>% 
              mutate(experiment = experiment,
                     measure = measure)
          }) %>% 
          left_join(rows, by = c("measure", "experiment")) %>% 
          mutate(
            measure = factor(measure, levels = measures),
            experiment = factor(experiment, levels = names(self$experiments))
          )
        
        p <- p + geom_vline(
          aes(xintercept = interval, linetype = linetype), 
          data = df,
          alpha = 0.6
        ) + scale_linetype_identity(NULL)
      }
      
      p <- p + 
        geom_ribbon(colour = NA, fill = "lightgrey") +
        geom_line()
      
      if (self$plot_bootstrap_peaks) {
        p <- p + geom_point(
          aes(actual_interval, actual_value),
          data = self$bootstrapped_peaks,
          inherit.aes = FALSE,
          colour = "red"
        )
      }
      
      p <- p + 
        scale_x_continuous("Interval (semitones)", breaks = self$interval_breaks) + 
        scale_y_continuous(self$consonance_label, breaks = scales::extended_breaks(n = 4)) + #, breaks = minimal_breaks) +
        scale_colour_identity(NULL) + 
        scale_size_identity(NULL) + 
        scale_alpha_manual(values = self$timbre_alphas)
      
      add_experiment_labels <- "consonance" %in% self$label_experiment
      
      if (add_experiment_labels) {
        p <- p + ggh4x::facet_nested(
          experiment + measure ~ (if (self$label_dyad) "Dyads" else ""),
          nest_line = TRUE, 
          switch = if (self$dyad_measure_labels == "left") "y", 
          scales = "free_y"
          # labeller = .labeller
          # switch = "y"
        )
      } else {
        labels <- rows$measure %>% set_names(rows$row)
        .labeller <- labeller(row = as_labeller(labels))
        
        p <- p +
          facet_grid(
            row ~ col, 
            switch = if (self$dyad_measure_labels == "left") "y",
            labeller = .labeller, 
            scales = "free_y"
          ) 
      }
      
      p <- p +
        theme(
          legend.direction = "vertical",
          legend.position = if (self$profile_legend) "right" else "none",
          strip.placement = "outside",
          # axis.text.y = element_blank(),
          # axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text.y.left = element_text(angle = 0),
          strip.text.y.right = element_text(angle = 0),
          axis.text.y = element_text(size = 8),
          panel.spacing.y = unit(11, "pt")
        )
      # if (!add_experiment_labels) {
      #   # p <- p + theme(plot.margin = unit(c(6, -55, 6, 6), "points"))
      # }
      p
    }
  )
)

PlotDyadModelExploration <- R6Class(
  "PlotDyadModelExploration",
  inherit = PlotDyadModelsExperiments,
  
  public = list(
    initialize = function(experiment, rescale_mode, ...) {
      self$rescale_mode <- rescale_mode
      
      super$initialize(
        experiments = list(Experiment = experiment),
        ...
      )
    },
    
    plot_profiles = function(profile_plot_data) {
      rows <- profile_plot_data %>%
        select(measure, measure_group) %>%
        unique() %>% 
        mutate(row = seq_along(measure))
      
      p <- profile_plot_data %>% 
        left_join(rows, by = c("measure", "measure_group")) %>% 
        mutate(
          measure = factor(measure, levels = levels(profile_plot_data$measure)),
          measure_group = factor(measure_group, levels = levels(profile_plot_data$measure_group)),
          col = (if (self$label_dyad) "Dyads" else ""),
          size = if_else(measure == "Participants", 0.85, 0.3)
        ) %>% 
        ggplot(aes(
          interval, 
          value, 
          ymin = ymin,
          ymax = ymax,
          colour = colour,
          alpha = timbre,
          size = size
        )) + 
        geom_ribbon(colour = NA, fill = "lightgrey") +
        geom_line() +  
        scale_x_continuous("Interval (semitones)", breaks = self$interval_breaks) + 
        scale_y_continuous(self$consonance_label) +
        scale_colour_identity(NULL) + 
        scale_size_identity(NULL) + 
        scale_alpha_manual(values = self$timbre_alphas) +
        ggh4x::facet_nested(
          measure_group + measure ~ "",
          nest_line = TRUE, 
          switch = "y", 
          scales = "free_y"
        ) +
        theme(
          legend.direction = "vertical",
          legend.position = if (self$profile_legend) "right" else "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text.y.left = element_text(angle = 0),
          strip.text.y.right = element_text(angle = 0)
        )
      p
    }
  )
)

PlotDyadModelsExperimentsRollOff <- R6Class(
  "PlotDyadModelsExperimentsRollOff",
  inherit = PlotDyadModelsExperiments,
  
  public = list(
    colour_spectrum_by_timbre = TRUE,
    separate_roll_off_rows = NA,
    label_measure = NA,
    
    rescale_mode = "none",
    realign_mode = "none",
    
    # We are going to plot multiple profiles on the same panel,
    # so rescaling them would be misleading
    rescale_each_profile = FALSE,
    
    initialize = function(..., 
                          separate_roll_off_rows = FALSE,
                          label_measure = TRUE) {
      self$separate_roll_off_rows <- separate_roll_off_rows
      self$label_measure <- label_measure
      super$initialize(...)
    },
    
    get_roll_off_levels = function() {
      self$experiments %>% 
        map("timbre") %>% 
        map("full") %>% 
        map_chr("label") %>% 
        as.character()
    },
    
    plot_profiles = function(profile_plot_data) {
      roll_off_levels <- self$get_roll_off_levels()
      
      df <- profile_plot_data %>% 
        mutate(
          timbre = factor(timbre, levels = roll_off_levels),
          size = if_else(measure == "Participants", 0.85, 0.3)
        ) 
      if (self$separate_roll_off_rows) {
        df <- df %>%
          group_by(timbre, measure) %>%
          self$rescale_all()
      }
      p <- 
        df %>% 
        ggplot(aes(
          interval, 
          value, 
          ymin = ymin,
          ymax = ymax,
          colour = colour,
          size = size,
          alpha = timbre
        )) + 
        geom_ribbon(colour = NA, fill = "lightgrey") +
        geom_line()
      
      if (self$plot_bootstrap_peaks) {
        p <- p + geom_point(
          aes(actual_interval, actual_value, alpha = timbre),
          data = self$bootstrapped_peaks,
          inherit.aes = FALSE,
          colour = "red"
        )
      }
      
      p + 
        scale_x_continuous("Interval (semitones)", breaks = self$interval_breaks) + 
        scale_y_continuous(self$consonance_label) +
        scale_colour_identity(NULL) +
        scale_size_identity(NULL) + 
        scale_alpha_manual(
          "Roll off", 
          values = seq(
            from = 0.4, 
            to = 1, 
            length.out = length(roll_off_levels)
          )
        ) + 
        {
          if (self$separate_roll_off_rows) {
            facet_grid(timbre ~ measure)
          } else {
            facet_wrap(~ measure, ncol = 1, scales = "free_y") 
          }
        } + 
        theme(legend.direction = "vertical",
              legend.position = if (self$profile_legend) "right" else "none",
              axis.text.y = element_text(size = 8),
              strip.background = element_blank(),
              strip.text.x = (if (self$label_measure) element_text() else element_text(colour = "white")),
              strip.text.y = element_blank()) +
        guides(alpha = guide_legend(
          override.aes = list(fill = "white")
        ))
    },
    
    plot_timbres = function(experiments) {
      n_timbres <- length(self$get_roll_off_levels())
      super$plot_timbres(experiments) + 
        scale_colour_manual(
          values = map_chr(seq(from = 0.2, to = 1, length.out = n_timbres), ~ gray(0, alpha = .))
        )
    }
  )
)

PlotTriadModelsExperiments <- R6Class(
  "PlotTriadModelsExperiments",
  inherit = PlotModelsExperiments,
  
  public = list(
    realign_mode = "none",
    
    get_behaviour = function(experiment) {
      experiment$behaviour$summary %>% 
        transmute(
          experiment = !!experiment$label,
          interval_1 = interval_1,
          interval_2 = interval_2,
          value = relative_density,
          measure = "Participants",
          measure_group = "",
          colour = "black",
          timbre = !!experiment$timbre$label
        )
    },
    
    get_models = function(experiment, models) {
      experiment$models[models] %>% 
        map("summary") %>% 
        bind_rows() %>% 
        transmute(
          experiment = !!experiment$label,
          interval_1 = interval_1,
          interval_2 = interval_2,
          value = output,
          measure = model_label,
          measure_group = Hmisc::capitalize(model_theory),
          colour = model_colour,
          timbre = timbre_label
        )
    },
    
    plot_profiles = function(profile_plot_data, select_measures = NULL) {
      df <- profile_plot_data %>% 
        group_by(measure) %>%
        mutate(
          colour_2 = get_viridis_colour(
            value,
            palette = list(
              Participants = "viridis",
              Interference = "inferno",
              Harmonicity = "mako"
            )[[unique(measure)]]
          )
        )
      
      
      if (is.null(select_measures)) {
        measures <- profile_plot_data$measure %>% unique() 
      } else {
        measures <- unique(select_measures)
        df <- df %>% 
          filter(measure %in% select_measures) %>% 
          mutate(measure = factor(measure, levels = measures))
      }
      
      p <- 
        df %>% 
        ggplot(aes(interval_1, interval_2, fill = colour_2, colour = value)) +
        scale_x_continuous(
          "Lower interval (semitones)", 
          expand = c(0, 0), 
          breaks = self$interval_breaks, 
          sec.axis = dup_axis(name = NULL)
        ) + 
        scale_y_continuous(
          "Upper interval (semitones)", 
          expand = c(0, 0), 
          breaks = self$interval_breaks, 
          sec.axis = dup_axis(name = NULL)
        ) +
        # Included as an invisible hack to get a single colour bar
        geom_point(
          aes(x, y, colour = colour),
          data = tribble(
            ~ x,    ~ y,    ~ colour,
            4,      4,      0,
            4,      4,      1
          ),
          fill = "white"
        ) +
        geom_raster() +
        scale_colour_viridis_c("Pleasantness") +
        scale_fill_identity() +
        facet_grid(cols = vars(measure),
                   rows = vars(experiment), switch = "y") +
        theme(
          aspect.ratio = 1,
          legend.key.width = unit(0.5, "inches"),
          legend.position = if (self$profile_legend) self$legend_position else "none",
          strip.background = element_blank()
        ) 
      p + theme(aspect.ratio = 1)
      if (!("consonance" %in% self$label_experiment)) {
        p <- p + theme(
          strip.text.y = element_blank()
        )
      }
      
      if (self$ablines == "stretching") {
        df <- expand_grid(
          experiment = names(self$experiments),
          measure = measures
        ) %>% 
          pmap_dfr(function(experiment, measure) {
            bind_rows(
              tibble(linetype = "dotted", octave = stretch_interval(c(12), octave_ratio = 1.9)),
              tibble(linetype = "dotted", octave = c(12)),
              tibble(linetype = "dotted", octave = stretch_interval(c(12), octave_ratio = 2.1))
            ) %>% 
              mutate(experiment = experiment,
                     measure = measure)
          }) %>% 
          mutate(
            measure = factor(measure, levels = measures),
            experiment = factor(experiment, levels = names(self$experiments))
          )
        p <- p + geom_abline(
          aes(slope = -1, intercept = octave, linetype = linetype), 
          data = df,
          alpha = 0.5,
          size = 0.75,
          colour = "grey95"
          # colour = "#ff52f6"
        ) + scale_linetype_identity(NULL)
      }
      
      p
    }
  )
)

monotonic_transform <- function(x, reference, lower = NA, upper = NA) {
  library(mgcv)
  k <- 5
  y <- reference
  dat <- data.frame(x = x, y = y)
  weights <- rep(1, times = length(x))
  init_gam <- mgcv::gam(y ~ s(x, k = k, bs = "cr"), weights = weights)
  # Create Design matrix, constraints etc. for monotonic spline....
  sm <- mgcv::smoothCon(s(x, k = k, bs = "cr"), dat, knots = NULL)[[1]]
  mc <- mgcv::mono.con(sm$xp, lower = lower, upper = upper) # monotonicity constraints
  M <- list(X = sm$X, y = reference, # design matrix, outcome
            C = matrix(0, 0, 0), # equality constraints (none)
            Ain = mc$A, bin = mc$b, # inequality constraints
            sp = init_gam$sp, p = sm$xp, # initial guesses for param estimates
            S = sm$S, # smoothness penalty matrix
            w = weights, # y * 0 + 1, # weights
            off = 0 # offset
  )
  # fit spine using penalized constrained least squares
  p <- mgcv::pcls(M) 
  
  as.numeric(mgcv::Predict.matrix(sm, data.frame(x = x)) %*% p)
}

process_peak_bootstrap <- function(data) {
  peak_bootstrap <- list()
  for (i in 1:length(data)) {
    current_boot <- data[[i]]
    if (length(current_boot) == 0) { next }
    for (j in 1:length(current_boot)) {
      df <- data.frame(bootstrap = i,
                       interval = current_boot[[j]]$interval,
                       value = current_boot[[j]]$value)
      peak_bootstrap[[length(peak_bootstrap) + 1]] <- df
    }
  }
  bind_rows(peak_bootstrap)
}

compute_peak_stats <- function(peak_bootstrap, peak_locations, margin_size) {
  n_boot <- length(unique(peak_bootstrap$bootstrap))
  map_dfr(peak_locations, function(peak_location) {
    # For each bootstrap iteration, choose the nearest peak 
    # within margin_size (if there is such a peak)
    peak_interval <- peak_location$interval
    peak_value <- peak_location$value
    peak_bootstrap %>%
      mutate(distance_from_target_peak = abs(interval - peak_interval)) %>%
      filter(distance_from_target_peak <= margin_size) %>%
      group_by(bootstrap) %>%
      summarise(
        bootstrap = bootstrap[1],
        interval = interval[which.min(distance_from_target_peak)],
        value = value[which.min(distance_from_target_peak)]
      ) %>%
      summarise(
        actual_interval = peak_interval,
        actual_value = peak_value,
        avg_interval = mean(interval),
        se_interval = sd(interval),
        lb_interval = avg_interval - 1.96 * sd(interval),
        ub_interval = avg_interval + 1.96 * sd(interval),
        avg_value = mean(value),
        lb_value = avg_value - 1.96 * sd(value),
        ub_value = avg_value + 1.96 * sd(value),
        prob = length(unique(bootstrap)) / n_boot
      )
  })
  
}

minimal_breaks <- function(x) {
  res <- scales::extended_breaks()(x) 
  res <- res[res > min(x)]
  res <- res[res < max(x)]
  c(res[1], res[length(res)])
}

reverse_interference_scales <- function(breaks = scales::extended_breaks(n = 4)) {
  ggh4x::facetted_pos_scales(
    y = list(
      measure == "Interference" ~ scale_y_continuous(breaks = breaks, labels = function(x) -x),
      measure == "Interference (revised)" ~ scale_y_continuous(breaks = breaks, labels = function(x) -x)
    )
  )
}

get_bootstrapped_peaks <- function(
  experiment, 
  experiment_name, 
  margin_size, 
  prob_threshold
) {
  candidate_peak_loc <- 
    experiment$behaviour$full$peaks$peaks
  
  experiment$behaviour$full$peaks$bootstrap$peaks %>%
    process_peak_bootstrap() %>%
    compute_peak_stats(
      peak_loc = candidate_peak_loc, 
      margin_size = margin_size
    ) %>%
    filter(prob >= !!prob_threshold) %>%
    mutate(
      experiment = experiment_name,
      measure = "Participants",
      timbre = !!experiment$timbre$label
    )
}