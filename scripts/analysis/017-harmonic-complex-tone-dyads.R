########################################################################
# Setup ####
########################################################################

source("scripts/analysis/015-figure-setup.R")
source("src/DyadRating.R")
source("src/utils.R")
source("src/plots.R")
source("src/sine_sweep.R")
Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")
library(magick)

if (!(exists("multisession_launched") && multisession_launched)) {
  # Enable parallel processing (only run this once per session)
  message("Setting up parallel compute...")
  plan(multisession)
  multisession_launched <- TRUE
}

########################################################################
# Main harmonic dyad plot ####
########################################################################

HARMONIC_DYADS <- EXPERIMENTS$`Harmonic dyads (3 dB roll-off)`

peak_stats <- get_bootstrapped_peaks(
  HARMONIC_DYADS, 
  "Harmonic dyads", 
  margin_size = 0.5,
  prob_threshold = 0.95
)

p1 <- HARMONIC_DYADS$behaviour$summary %>%
  ggplot(aes(
    interval, 
    rating, 
    ymin = rating - 1.96 * rating_boot_se, 
    ymax = rating + 1.96 * rating_boot_se
  )) + 
  geom_ribbon(colour = NA, fill = "lightgrey") +
  geom_point(
    aes(x = interval, y = rating),
    data = HARMONIC_DYADS$behaviour$full$data,
    color = 'skyblue',
    size = 3*0.21, # 7*0.21
    alpha = 0.24, # 0.28
    inherit.aes = FALSE) + 
  geom_line(data = HARMONIC_DYADS$behaviour$summary) +
  geom_rect(
    data = peak_stats,
    aes(
      xmin = lb_interval,
      xmax = ub_interval,
      ymin = -Inf,
      ymax = 0.8
    ),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.1,
    color = "pink"
  ) +
  geom_point(
    aes(actual_interval, actual_value),
    data = peak_stats,
    inherit.aes = FALSE,
    # size = 10*0.21, # 10*0.21
    colour = "red"
  ) +
  scale_x_continuous(breaks = 0:15) +
  coord_cartesian(ylim = c(-2, 3)) +
  grids(axis = "x", linetype = "dashed", size = 0.5) +
  xlab("Interval (semitones)") +
  ylab("Pleasantness rating (z-score)") 

ggsave("output/plots/017-harmonic-dyads-exposition.pdf", width = 8, height = 6, scale = 0.7)

R.utils::mkdirs("output/videos")
sweep_v_line_over_plot(
  p1, 
  x_start = 0, 
  x_end = 15, 
  duration = 75, 
  path = "output/videos/017-harmonic-dyads-exposition.mp4", 
  fps = 30,
  dpi = 300,
  audio_components = c(
    static_basic_harmonic_complex_tone(
      midi = 60, 
      amplitude = 1, 
      n_harmonics = 11, 
      decay_dB_per_octave = 3
    ),
    linear_freq_sweep_basic_harmonic_complex_tone(
      start_midi_root = 60, 
      end_midi_root = 75, 
      duration = 75, 
      amplitude = 1, 
      n_harmonics = 11, 
      decay_dB_per_octave = 3
    )
  )
)


########################################################################
# Quantifying the 'integerness' of the dyad profile ####
########################################################################

peak_raw <- HARMONIC_DYADS$behaviour$full$peaks$bootstrap$peaks %>% 
  process_peak_bootstrap()

peak_summary <- get_bootstrapped_peaks(
  HARMONIC_DYADS, 
  "Harmonic dyads", 
  margin_size = 0.5,
  prob_threshold = 0.95
)

compute_integerness <- function(intervals) {
  distance_from_nearest_integer <- abs(round(intervals) - intervals)
  mean(distance_from_nearest_integer)
}

integer_boot <- 
  peak_raw %>%
  group_by(bootstrap) %>%
  summarise(integerness = compute_integerness(interval)) %>%
  summarise(mean_integerness = mean(integerness),
            lb = mean_integerness - 1.96 * sd(integerness),
            ub = mean_integerness + 1.96 * sd(integerness)) %>%
  mutate(type = "Real")

# Null distribution is a set of uniform random peaks, an alternative
# would be to run peak picking algorithm on uniform samples of same data size
set.seed(1)
null_integer_boot <- 
  peak_raw %>% 
  mutate(null_interval = runif(nrow(peak_raw), min = 0, max = 15)) %>%
  group_by(bootstrap) %>% 
  summarise(integerness = compute_integerness(null_interval)) %>%
  summarise(mean_integerness = mean(integerness),
            lb = mean_integerness - 1.96 * sd(integerness),
            ub = mean_integerness + 1.96 * sd(integerness)) %>%
  mutate(type = "Null")

integerness_summary <- rbind(integer_boot, null_integer_boot) %>%
  mutate(type = as.factor(type))

ggplot(integerness_summary, aes(x=type, y=mean_integerness, fill=type)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,
                position=position_dodge(.9)) + 
  labs(x = "Type", y = "Distance from nearest integer (semitones)") +
  scale_fill_viridis_d() + theme_minimal() +
  theme(legend.position="none") 


########################################################################
# Relating the results to music-theoretic consonance categories  ####
########################################################################

NBOOT = 1000
data <- HARMONIC_DYADS$behaviour$full$data
get_profile <- HARMONIC_DYADS$behaviour$full$get_profile

get_apriori_consonance <- function(interval) {
  interval <- round(interval %% 12)
  if (interval %in% c(0, 3, 4, 5, 7, 8, 9)) {
    type <- "Consonant"
  } else {
    type <- "Dissonant"
  }
  type
}

set.seed(1)
consonance_minus_dissonance_rating <- 
  furrr::future_map_dbl(1:NBOOT, function(i) {
    Rcpp::sourceCpp("src/smooth_2d_gaussian.cpp")
    
    data_sample <- data %>% group_by(participant_id) %>% group_split() 
    data_sample <- sample(data_sample, length(data_sample), replace=TRUE)
    data_sample <- bind_rows(data_sample)
    data_sample <- get_profile(data_sample, c(0,15), 10000)
    apriori_vals <- c()
    for (j in 0:15){
      apriori_vals <- c(apriori_vals, which.min(abs(j - data_sample$interval)))
    }
    data_sample <- data_sample[apriori_vals,]
    data_sample <- data_sample %>%
      mutate(type = sapply(interval, get_apriori_consonance)) %>%
      group_by(type) %>%
      summarise(avg_rating = mean(rating))
    consonance_summary <- data_sample %>% filter(type == "Consonant")
    dissonance_summary <- data_sample %>% filter(type == "Dissonant")
    consonance_summary$avg_rating - dissonance_summary$avg_rating
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))

difference_summary <- 
  tibble(
    difference = mean(consonance_minus_dissonance_rating),
    se = sd(consonance_minus_dissonance_rating),
    lb = difference - 1.96 * se,
    ub = difference + 1.96 * se
  )

########################################################################
# Computing correlations with previous studies ####
########################################################################

NBOOT = 1000
data <- HARMONIC_DYADS$behaviour$full$data 
get_profile <- HARMONIC_DYADS$behaviour$full$get_profile

# Fig 4b from McPherson, Malinda J., et al. Nature communications 11.1 (2020): 1-14
mcpherson_data <- read.csv(paste0('./input','/mcpherson_rating.csv'), header = FALSE)
mcpherson_intervals <- 0:14

mcpherson_data_scaled <- t(apply(mcpherson_data, 1, scale)) # z-score within participants

baseline <- unname(as.vector(colMeans(mcpherson_data_scaled)))
baseline <- data.frame(interval = mcpherson_intervals, mean_rating = baseline)

lookup_interval <- function(int) {
  .profile <- HARMONIC_DYADS$behaviour$summary
  .match <- which.min(abs(int - .profile$interval))
  .rating <- .profile %>% 
    slice(.match) %>% 
    transmute(
      mean = rating,
      lower_95 = mean - 1.96 * rating_boot_se,
      upper_95 = mean + 1.96 * rating_boot_se,
    ) %>%
    as.list()
}

df_previous_studies <- 
  tibble(
    i = 1:15,
    interval = mcpherson_intervals,
    category = map_chr(interval, get_apriori_consonance)
  ) %>%
  mutate(
    mcpherson = map(i, ~ bootstrap_mean(mcpherson_data_scaled[, .], n = 1000)),
    mcpherson_mean = map_dbl(mcpherson, "mean"),
    mcpherson_boot_se = map_dbl(mcpherson, "boot_se"),
    mcpherson_lower_95 = map_dbl(mcpherson, "lower_95"),
    mcpherson_upper_95 = map_dbl(mcpherson, "upper_95"),
    
    ours = map(interval, lookup_interval),
    our_mean = map_dbl(ours, "mean"),
    our_lower_95 = map_dbl(ours, "lower_95"),
    our_upper_95 = map_dbl(ours, "upper_95")
  ) %>%
  left_join(
    inconData::sch03 %>% 
      transmute(
        interval = map_dbl(pi_chord_type, 2),
        sch03_dissonance = rating
      ),
    by = "interval"
  )

# McPherson et al. (2020)
df_previous_studies %>% 
  ggplot(aes(
    x = our_mean,
    xmin = our_lower_95,
    xmax = our_upper_95,
    y = mcpherson_mean,
    ymin = mcpherson_lower_95,
    ymax = mcpherson_upper_95,
    colour = category,
    label = interval
  )) + 
  geom_errorbar(alpha = 0.2) + 
  geom_errorbarh(alpha = 0.2) +
  geom_point() +
  geom_label(show.legend = FALSE) +
  scale_x_continuous("Pleasantness rating (z-score)") +
  scale_y_continuous("McPherson et al. (2020) rating (z-score)") +
  scale_colour_manual(
    "Music-theoretic\ncategory",
    values = c("#5071B2", "#B5482A")
  ) +
  theme(
    aspect.ratio = 1,
    legend.position = c(0.8, 0.2)
  )

ggsave("output/plots/017-harmonic-dyads-vs-mcpherson.pdf", width = 4, height = 4)

# Schwartz et al. (2003)
df_previous_studies %>% 
  ggplot(aes(
    x = our_mean,
    xmin = our_lower_95,
    xmax = our_upper_95,
    y = mcpherson_mean,
    label = interval
  )) +
  geom_errorbarh(alpha = 0.2) +
  geom_point() +
  geom_label(show.legend = FALSE) +
  scale_x_continuous("Pleasantness rating (z-score)") +
  scale_y_continuous("Ranking in Schwartz et al. (2020)") +
  theme(
    aspect.ratio = 1
  )

R.utils::mkdirs("output/validation")

cor_methods <- list(
  pearson = "pearson",
  spearman = "spearman"
)

cor_mcpher <-
  map(cor_methods, ~ cor.test(df_previous_studies$our_mean, df_previous_studies$mcpherson_mean, method = .))

print(cor_mcpher)
withr::with_output_sink("output/validation/correlation-with-mcpherson-2020.txt", print(cor_mcpher))

cor_sch03 <- 
  map(cor_methods, ~ suppressWarnings(cor.test(df_previous_studies$our_mean, df_previous_studies$sch03_dissonance, method = .)))

print(cor_sch03)
withr::with_output_sink("output/validation/correlation-with-schwartz-2003.txt", print(cor_sch03))


bowl18_comparison <-
  inconData::bowl18 %>% 
  filter(chord_size == 2) %>% 
  transmute(
    interval = map_dbl(pi_chord, diff),
    bowl18_attractiveness = rating,
    our_mean = map(interval, lookup_interval) %>% map_dbl("mean")
  ) 

cor_bowl18 <-
  map(cor_methods, ~ suppressWarnings(cor.test(bowl18_comparison$our_mean, bowl18_comparison$bowl18_attractiveness, method = .)))

print(cor_bowl18)
withr::with_output_sink("output/validation/correlation-with-bowling-2018-dyads.txt", print(cor_bowl18))
