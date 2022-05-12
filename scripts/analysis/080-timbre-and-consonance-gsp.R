# GSP on timbre and interval for different adjectives

library(pdftools)

source("scripts/analysis/015-figure-setup.R")

DATA <- read_csv("input/data-csv/GSP/tmb_int_gsp_mixintm.csv", col_types = cols()) %>% 
  mutate(
    category = map(category, jsonlite::fromJSON) %>% map_chr("target")
  ) %>% 
  rename(
    target = category,
    interval_1 = v1,
    interval_2 = v2,
    harmonic_2 = v3,
    harmonic_3 = v4,
    harmonic_4 = v5,
    harmonic_5 = v6,
    volume = v7
  ) %>% 
  add_column(
    harmonic_1 = 1,
    .before = "harmonic_2"
  ) %>% 
  mutate(
    target = Hmisc::capitalize(target)
  )

TARGETS <- c("Pleasant", "Bright", "Dark")

DATA_INTERVALS <- map_dfr(TARGETS, function(target) {
  TriadGSP$new(
    file = DATA %>% filter(target == !!target),
    bandwidth = BANDWIDTH_GSP_2D,
    int_1_range = c(0.5, 7.5),
    int_2_range = c(0.5, 7.5),
    resolution = RESOLUTION_BEHAVIOURAL_2D
  )$profile %>% 
    mutate(target = factor(target, levels = TARGETS))
})

write_csv(DATA_INTERVALS, "output/plots/080-timbre-and-consonance-gsp-pitch-interval-profiles.csv")

DATA |> 
  ggplot(aes(volume)) + 
  geom_density() + 
  facet_wrap(~ target, ncol = 1)

JOINT_PLOTS <- list()

target_to_colour_map <- list(
  Pleasant = "viridis",
  Bright = "mako",
  Dark = "inferno"
)

get_representative_color <- function(palette) {
  viridis_palettes[[palette]][500]
}

JOINT_PLOTS$Spectra <- local({
  harmonics <-
    TARGETS %>% 
    set_names(., .) %>% 
    map_dfr(function(target) {
      DATA %>% 
        filter(target == !!target) %>% 
        pivot_longer(starts_with("harmonic_"), names_to = "harmonic_number", values_to = "amplitude") %>% 
        mutate(
          harmonic_number = as.integer(gsub("harmonic_", "", harmonic_number))
        ) %>% 
        group_by(network_id, harmonic_number) %>%
        summarise(amplitude = mean(amplitude)) %>%
        group_by(harmonic_number) %>% 
        summarise_bootstrap_mean("amplitude", n = 1000) %>% 
        mutate(
          target = factor(!!target, levels = TARGETS),
          colour_map = map_chr(target, ~ target_to_colour_map[[.]]),
          colour = map_chr(colour_map, get_representative_color)
        )
    })
  timbres <- TARGETS %>% set_names(., .) %>% 
    map(function(target) {
      harmonics %>% 
        filter(target == !!target) %>% 
        arrange(harmonic_number) %>% 
        pull(mean) %>% 
        {HarmonicTone$new(harmonic_amplitudes = .)}
    })
  continuous_plot_data <- get_plot_spectra(
    timbre_labels = TARGETS,
    timbres = timbres
  ) %>% 
    rename(target = timbre_label)
  
  continuous_plot_data %>% 
    ggplot(aes(x = interval, 
               y = amplitude)) +
    geom_line() + 
    geom_errorbar(
      aes(x = harmonic_number_to_interval(harmonic_number) - 0.75,
          y = mean,
          ymin = lower_95,
          ymax = upper_95,
          colour = colour),
      data = harmonics,
      width = 0.5
      # colour = "blue",
      # # alpha = 0.5
    ) +
    scale_colour_identity(NULL) +
    scale_x_continuous("Interval (semitones)") + 
    scale_y_continuous("Amplitude", breaks = c(0, 1)) + 
    facet_grid(target ~ "Spectra", switch = "y") + 
    # facet_wrap(timbre_label ~ plot_label, switch = "y") + 
    theme(
      # axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.position = "none",
      # strip.text.x = element_text(colour = if (self$label_spectrum) "black" else "white"),
      # strip.text.y.left = if ("spectrum" %in% self$label_experiment) element_text(angle = 0) else element_blank()
      # aspect.ratio = 1
    )
})

JOINT_PLOTS$intervals <- 
  DATA_INTERVALS %>% 
  
  group_by(target) %>% 
  mutate(
    norm_density = density / max(density),
    colour = get_viridis_colour(
      norm_density,
      palette = target_to_colour_map[[unique(target)]]
    )
  ) %>% 
  ungroup() %>% 
  
  ggplot(aes(
    interval_1, 
    interval_2, 
    # fill = norm_density
    fill = colour
  )) +
  scale_x_continuous("Lower interval (semitones)", expand = c(0, 0), breaks = 0:8) + #, breaks = self$interval_breaks) + 
  scale_y_continuous("Upper interval (semitones)", expand = c(0, 0), breaks = 0:8) + #, breaks = self$interval_breaks) +
  geom_raster() +
  scale_fill_identity("Density") + 
  # scale_fill_viridis_c("Semantic\nmatch", breaks = c(0.05, 0.95), labels = c("Low", "High")) + 
  facet_grid(target ~ "Intervals") + 
  theme(
    legend.key.width = unit(0.5, "inches"),
    legend.position = "right", # if (self$profile_legend) "right" else "none",
    strip.background = element_blank(),
    strip.text = element_blank()
    # aspect.ratio = 1
    # strip.text.y = element_blank()
  )
# guides(fill = guide_colourbar(barwidth = 0.5))

JOINT_PLOTS$legends <- map(TARGETS, function(target) {
  palette <- target_to_colour_map[[target]]
  p <- 
    tibble(x = 1:10, y = 1:10, z = seq(from = 0, to = 1, length.out = 10)) %>%
    ggplot(aes(x, y, colour = z)) + 
    geom_point() + 
    scale_colour_viridis_c(
      paste0(target, "ness"), #%>% stringr::str_pad(width = 15, side = "right"),
      option = palette, 
      breaks = c(0, 0.5, 1)
    ) +
    guides(colour = guide_colorbar(title.position = "top"))
  # theme(legend.position = "right")
  p
  get_legend(p)
  cowplot::plot_grid(get_legend(p))
}) %>%
  cowplot::plot_grid(
    plotlist = ., 
    ncol = 1
  ) + 
  theme(plot.margin = unit(c(6, 20, 6, 30), units = "pt"))

message("Note: You may see the following two errors; if so, don't worry, ",
        "they can be safely ignored.")
message("Ignorable error 1: xref num 42 not found but needed, try to reconstruct<0a>")
message("Ignorable error 2: Failed to parse XRef entry [42].")
        
cowplot::plot_grid(
  cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_image(magick::image_read_pdf("media/gsp-aggregation.pdf"))
    # labels = "A"
  ),
  cowplot::plot_grid(
    # cowplot::plot_grid(ggplot() + theme(axis.line = element_blank())),
    JOINT_PLOTS$Spectra + theme(plot.margin = unit(c(6, 12, 6, 6), units = "pt")),
    JOINT_PLOTS$intervals,
    JOINT_PLOTS$legends,
    nrow = 1,
    scale = 0.9,
    rel_widths = c(1.1, 1, 0.5),
    labels = c("", "C", "")
  ) + theme(plot.margin = unit(c(6, 80, 6, 40), units = "pt")),
  nrow = 2,
  labels = c("A", "B"),
  rel_heights = c(1.5, 3.5)
)

ggsave("output/plots/080-timbre-and-consonance-gsp.pdf", width = 14, height = 16, scale = 0.6)



get_plot_spectra("Example", list(HarmonicTone$new(harmonic_amplitudes = c(1, 0.3, 0.7, 0.2, 0.25)))) %>% 
  ggplot(aes(interval, amplitude)) + 
  geom_line() + 
  scale_x_continuous("Interval (semitones)") + 
  scale_y_continuous("Amplitude") + 
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )
ggsave("output/plots/080-timbre-gsp-example-spectrum.pdf", width = 5, height = 4, scale = 0.44)
