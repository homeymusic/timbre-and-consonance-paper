# This code builds on and modifies code from Peter Harrison's dyon 
# repository: https://github.com/pmcharrison/dycon

#' Spectral roughness (Hutchinson & Knopoff)
#'
#' Gets the roughness of a sonority according to the model of
#' \insertCite{Hutchinson1978;textual}{dycon}.
#' @param x Object to analyse, which is coerced to the class
#' \code{\link[hrep]{sparse_fr_spectrum}}.
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' and expanded into their implied harmonics.
#' * Two-element lists will be treated as finalised spectra,
#' with the first element being a numeric vector of frequencies,
#' and the second element being a numeric vector of amplitudes.
#'
#' @param a Parameter passed to \code{\link{hutch_g}()}.
#'
#' @param b Parameter passed to \code{\link{hutch_g}()}.
#'
#' @param cbw_cut_off Parameter passed to \code{\link{hutch_g}()}.
#'
#' @param dissonance_function
#' Function for computing dissonance contribution as a function of
#' critical bandwidth distance, defaulting to \code{\link{hutch_dissonance_function}}.
#' Custom functions may be specified here as long as they use the same parameter list
#' as the original function.
#'
#' @return Numeric scalar, identifying the roughness of the spectrum.
#'
#' @references
#' \insertAllCited{}
#'
#' @rdname roughness_hutch_revised
#'
#' @md
#'
#' @export
roughness_hutch_revised <- function(
    x,
    cbw_cut_off = 1.2,
    a = 0.25,
    b = 2,
    dissonance_function = hutch_dissonance_function,
    ...) {
  UseMethod("roughness_hutch_revised")
}

#' @param ... Further arguments to pass to \code{\link[hrep]{sparse_fr_spectrum}}.
#' @rdname roughness_hutch_revised
#' @export
roughness_hutch_revised.default <- function(
    x,
    cbw_cut_off = 1.2,
    a = 0.25,
    b = 2,
    dissonance_function = hutch_dissonance_function,
    ...
) {
  x <- hrep::sparse_fr_spectrum(x, ...)
  roughness_hutch_revised(x, cbw_cut_off = cbw_cut_off, a = a, b = b, dissonance_function = dissonance_function)
}

#' @rdname roughness_hutch_revised
#' @export
roughness_hutch_revised.sparse_fr_spectrum <- function(
    x,
    cbw_cut_off = 1.2,
    a = 0.25,
    b = 2,
    dissonance_function = hutch_dissonance_function,
    ...
) {
  frequency <- hrep::freq(x)
  amplitude <- hrep::amp(x)
  n <- length(frequency)
  if (n < 2) 0 else {
    # Compute denominator
    # denominator <- (sum(amplitude ^ 2))  # to do - update this?
    denominator <- sum(amplitude)
    # Compute numerator
    df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
      (function(df) {
        df[df$i < df$j, ]
      })
    df$amplitude_factor <- amplitude_factor_new(
      amplitude[df$i], 
      amplitude[df$j]
    )
    # df$a_i_a_j <- amplitude[df$i] * amplitude[df$j]
    df$dissonance_factor <- revised_dissonance_function(
    # df$dissonance_factor <- dissonance_function(
      f1 = frequency[df$i],
      f2 = frequency[df$j],
      cbw_cut_off = cbw_cut_off,
      a = a,
      b = b
    )
    numerator <- sum(df$amplitude_factor * df$dissonance_factor)
    numerator / denominator
  }
}

amplitude_factor_original <- function(x, y) {
  x * y
}

amplitude_factor_new <- function(x, y) {
  # amplitude_factor_original(x, y)
  
  # a <- 0.001
  a <- 0.01
  b <- 0.1
  m <- 1 + b / a
  
  orig <- amplitude_factor_original(x, y)
  # ifelse(
  #   orig <= a,
  #   m * orig,
  #   b + orig
  # )
  sqrt(orig)
  
  # 0.1 + amplitude_factor_original(x, y)
  # log(amplitude_factor_original(x, y))
}

#' Get dissonance contribution
#'
#' Computes the dissonance contribution of a pair of pure tones.
#'
#' @inheritParams roughness_hutch_revised
#' @inheritParams hutch_cbw
#'
#' @return Numeric vector of dissonance contributions.
#'
#' @export
hutch_dissonance_function <- function(f1, f2, cbw_cut_off = 1.2, a = 0.25, b = 2) {
  hutch_g(
    y = hutch_y(f1 = f1, f2 = f2),
    cbw_cut_off = cbw_cut_off,
    a = a,
    b = b
  )
}

revised_dissonance_function = function(f1, f2, ...) {
  cbw <- dycon::hutch_y(f1, f2)
  revised_dissonance_factor(cbw)
}

revised_dissonance_factor = function(cbw) {
  pi <- 3.14159
  classic_diss <- dycon::hutch_g(cbw)
  pleasantness_boundary <- 0.1
  # pleasantness_strength <- 0
  pleasantness_strength <- 1.5
  
  ifelse(
    cbw > pleasantness_boundary, 
    classic_diss, 
    (cbw / pleasantness_boundary) * classic_diss +
      (1 - cbw / pleasantness_boundary) * 
      - pleasantness_strength * (1 + sin(- pi / 2 + cbw * 2 * pi / pleasantness_boundary))
    # classic_diss + 500 * (cbw * (cbw - 0.05))
  )
}


#' Critical bandwidth (Hutchison & Knopoff)
#'
#' Calculates the critical bandwidth given pairs of frequencies
#' \code{f1} and \code{f2},
#' according to the model of \insertCite{Hutchinson1978;textual}{dycon}.
#' @param f1 (Numeric vector) Frequency 1, Hz
#' @param f2 (Numeric vector) Frequency 2, Hz
#' @return (Numeric vector) Critical bandwidths.
#' @references
#'   \insertAllCited{}
#' @export
hutch_cbw <- function(f1, f2) {
  mean_f <- (f1 + f2) / 2
  1.72 * (mean_f ^ 0.65)
}

#' Critical bandwidth distance (Hutchison & Knopoff)
#'
#' Calculates the distance between pairs of frequencies in units of
#' critical bandwidths, according to the model of
#' \insertCite{Hutchinson1978;textual}{dycon}.
#' @param f1 (Numeric vector) Frequency 1, Hz
#' @param f2 (Numeric vector) Frequency 2, Hz
#' @return (Numeric vector) Unsigned distances in frequency bandwidths.
#' @references
#'   \insertAllCited{}
#' @export
hutch_y <- function(f1, f2) {
  abs_freq_diff <- abs(f1 - f2)
  critical_bandwidth <- hutch_cbw(f1, f2)
  abs_freq_diff / critical_bandwidth
}

#' Dissonance factor (Hutchinson & Knopoff)
#'
#' Computes dissonance factors given frequency distances in units of
#' critical bandwidths, after Mashinter's implementation of
#' Hutchinson & Knopoff's model
#' \insertCite{Mashinter2006,Hutchinson1978}{dycon}.
#' This function corresponds to an approximation of the
#' look-up table in \insertCite{Plomp1965;textual}{dycon}.
#' @param y (Numeric vector) Frequency distance in units of critical bandwidths.
#' @param cbw_cut_off (Numeric scalar)
#' If not \code{NULL}, then should be a number
#' corresponding to the variable CBWcutoff in Mashinter's own implementation.
#' If \code{y >= cbw_cut_off}, then the dissonance factor will be approximated as 0.
#' Setting \code{cbw_cut_off} to 1.2 is necessary for replicating Mashinter's results.
#' A cut-off of 1.2 was also used by \insertCite{Bigand1996;textual}{dycon}.
#' @param a (Numeric scalar, default = 0.25)
#' Parameter from \insertCite{Mashinter2006;textual}{dycon}.
#' @param b (Numeric scalar, default = 2)
#' Parameter from \insertCite{Mashinter2006;textual}{dycon}.
#' @return (Numeric vector) Dissonance factors.
#' @references
#'   \insertAllCited{}
#' @export
hutch_g <- function(y, cbw_cut_off = 1.2, a = 0.25, b = 2) {
  assertthat::assert_that(
    is.numeric(y), all(y >= 0),
    is.null(cbw_cut_off) || (assertthat::is.scalar(cbw_cut_off) && is.numeric(cbw_cut_off)),
    is.numeric(a), is.numeric(b),
    assertthat::is.scalar(a), assertthat::is.scalar(b)
  )
  res <- ((y / a) * exp(1 - (y / a))) ^ b
  if (!is.null(cbw_cut_off)) {
    res[y > cbw_cut_off] <- 0
  }
  res
}

hutch_visualise_data <- function(x, cbw_cut_off, a, b, min_freq, max_freq, ...) {
  frequency <- c(min_freq, hrep::freq(x), max_freq)
  amplitude <- c(0, hrep::amp(x), 0)
  n <- length(frequency)
  df <- expand.grid(j = seq_len(n), i = seq_len(n)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      a_i_a_j = amplitude[.data$i] * amplitude[.data$j],
      g_ij = hutch_g(
        y = hutch_y(f1 = frequency[.data$i],
                    f2 = frequency[.data$j]),
        cbw_cut_off = cbw_cut_off,
        a = a,
        b = b
      )) %>%
    dplyr::group_by(.data$i) %>%
    dplyr::summarise(dissonance = sum(.data$g_ij))
  df2 <- tibble::tibble(i = seq_len(n), frequency, amplitude) %>%
    dplyr::left_join(df, by = "i") %>%
    dplyr::mutate(pitch = hrep::freq_to_midi(frequency))
  df3 <- data.frame(pitch = numeric(n * 5),
                    amplitude = numeric(n * 5),
                    dissonance = numeric(n * 5))
  for (i in seq_len(n)) {
    I <- (i - 1L) * 5L
    df3[I + 1:5, "pitch"] <- df2[i, "pitch"]
    df3[I + 2:4, "dissonance"] <- df2[i, "dissonance"]
    df3$amplitude[I + 3L] <- df2$amplitude[i]
  }
  df3
}

#' ggplot theme
#'
#' Defines a default theme for visualising computations
#' for Hutchinson & Knopoff's (1978) model
#' (see \code{\link{hutch_visualise}}).
#' @export
hutch_visualise_theme <- ggplot2::theme_classic() +
  ggplot2::theme(
    panel.spacing = ggplot2::unit(1.9, "lines"),
    strip.background = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(colour = "black"),
    axis.text.y = ggplot2::element_text(colour = "black"),
    axis.ticks = ggplot2::element_line(colour = "black")
  )

#' Visualise
#'
#' Creates a plot visualising computations for Hutchinson & Knopoff's model.
#'
#' @param x Passed to \code{\link{roughness_hutch_revised}}.
#' @param cbw_cut_off Passed to \code{\link{roughness_hutch_revised}}.
#' @param a Passed to \code{\link{roughness_hutch_revised}}.
#' @param b Passed to \code{\link{roughness_hutch_revised}}.
#' @param label (Character scalar) x-axis label.
#' @param amplitude_breaks Numeric vector of tick locations for the y-axis.
#' @param colour_limits Defines the limits of the roughness scale.
#' @param colour_low Colour to use for the lowest roughness.
#' @param colour_high Colour to use for the highest roughness.
#' @param theme \code{\link[ggplot2]{ggplot}} theme to use.
#' @param ... Passed to \code{\link[hrep]{sparse_fr_spectrum}}.
#' @export
hutch_visualise <- function(x,
                            cbw_cut_off = 1.2,
                            a = 0.25,
                            b = 2,
                            label = "Roughness",
                            amplitude_breaks = c(0, 1),
                            colour_limits = c(0, 3),
                            colour_low = "darkblue",
                            colour_high = "red",
                            theme = hutch_visualise_theme,
                            ...) {
  stopifnot(is.list(x), !is.null(names(x)), !anyDuplicated(names(x)))
  x <- purrr::map(x, hrep::sparse_fr_spectrum, ...)
  min_freq <- min(purrr::map_dbl(x, ~ min(hrep::freq(.))))
  max_freq <- max(purrr::map_dbl(x, ~ max(hrep::freq(.))))
  labels <- factor(names(x), levels = names(x))
  purrr::map(x, hutch_visualise_data, cbw_cut_off, a, b, min_freq, max_freq, ...) %>%
    purrr::map2(labels, ~ dplyr::mutate(.x, label = .y)) %>%
    dplyr::bind_rows() %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "pitch",
                                        y = "amplitude",
                                        colour = "dissonance")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Pitch (MIDI)") +
    ggplot2::scale_y_continuous("Amplitude", breaks = amplitude_breaks) +
    ggplot2::scale_colour_gradient(label,
                                   low = colour_low,
                                   high = colour_high,
                                   limits = colour_limits) +
    ggplot2::facet_wrap(~ label, ncol = 1) +
    theme
}
