source("scripts/analysis/015-figure-setup.R")

########################################################################
# Compares our triadic consonance maps to data from 
# Bowling et al. (2018)
########################################################################

triad_data <- EXPERIMENTS$`Harmonic triads (3 dB roll-off)`$behaviour$summary

lookup_chord <- function(pi_chord, tolerance = 0.05) {
  stopifnot(length(pi_chord) == 3)
  interval_1 <- pi_chord[2] - pi_chord[1]
  interval_2 <- pi_chord[3] - pi_chord[2]
  
  distances <- sqrt(
    (triad_data$interval_1 - interval_1) ^ 2 + 
      (triad_data$interval_2 - interval_2) ^ 2  
  )
  if (!any(distances <= tolerance)) {
    NA
  } else {
    triad_data$relative_density[which.min(distances)]
  }
}

lookup_chord(c(0, 4, 7))
lookup_chord(c(0, 1, 2))

bowl18_triad_comparison <- 
  inconData::bowl18 %>% 
  filter(chord_size == 3) %>% 
  transmute(
    bowl_18_rating = rating, 
    pi_chord = pi_chord,
    our_data = map_dbl(pi_chord, lookup_chord)
  ) %>% 
  na.omit()

cor_bowl18_triads <- cor.test(bowl18_triad_comparison$our_data,
                              bowl18_triad_comparison$bowl_18_rating, 
                              method = "pearson")
print(cor_bowl18_triads)

R.utils::mkdirs("output/validation")
withr::with_output_sink(
  "output/validation/correlation-with-bowling-2018-triads.txt", 
  print(cor_bowl18_triads)
)
