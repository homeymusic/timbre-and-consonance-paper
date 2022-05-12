source("scripts/analysis/015-figure-setup.R")

chord <- c(60, 63, 66)

timbre_1 <- BasicHarmonicTone$new(
  n_harmonics = 10, 
  decay_dB_per_octave = 3, 
  octave_definition = 2.0, 
  label = "Harmonic"
)

timbre_2 <- BasicHarmonicTone$new(
  n_harmonics = 10, 
  decay_dB_per_octave = 6, 
  octave_definition = 2.0, 
  label = "Harmonic"
)

timbre_1$sparse_fr_spectrum(chord, coherent = TRUE) %>%
  plot(gg = TRUE)

ggsave(
  "Diminished triad - 3 dB roll-off.png",
  path = "output/plots/presentation/extra",
  width = 6,
  height = 4,
  dpi = 250
)

timbre_2$sparse_fr_spectrum(chord, coherent = TRUE) %>%
  plot(gg = TRUE)

ggsave(
  "Diminished triad - 6 dB roll-off.png",
  path = "output/plots/presentation/extra",
  width = 6,
  height = 4,
  dpi = 250
)

