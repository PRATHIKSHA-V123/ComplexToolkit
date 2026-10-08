# fourier.R
# Discrete Fourier Transform implemented manually using complex exponentials,
# validated against R's built-in fft().

#' Generate a synthetic signal as the sum of sine waves
#'
#' @param freqs Numeric vector of frequencies (Hz) to include
#' @param amplitudes Numeric vector of amplitudes, same length as freqs
#' @param sampling_rate Samples per second
#' @param duration Signal duration in seconds
#' @return A list with time vector `t` and signal vector `x`
generate_signal <- function(freqs, amplitudes, sampling_rate = 500, duration = 1) {
  if (length(freqs) != length(amplitudes)) {
    stop("freqs and amplitudes must be the same length")
  }
  t <- seq(0, duration - 1 / sampling_rate, by = 1 / sampling_rate)
  x <- rep(0, length(t))
  for (i in seq_along(freqs)) {
    x <- x + amplitudes[i] * sin(2 * pi * freqs[i] * t)
  }
  list(t = t, x = x)
}

#' Manual Discrete Fourier Transform
#'
#' Implements X_k = sum_{n=0}^{N-1} x_n * exp(-2i*pi*k*n/N) directly, using
#' R's native complex numbers rather than relying on fft().
#'
#' @param x Numeric (real-valued) signal vector
#' @return A complex vector, the DFT of x
manual_dft <- function(x) {
  N <- length(x)
  X <- complex(length.out = N)
  n <- 0:(N - 1)
  for (k in 0:(N - 1)) {
    X[k + 1] <- sum(x * exp(-2i * pi * k * n / N))
  }
  X
}

#' Compare the manual DFT against R's built-in fft() for validation
#'
#' @param x Numeric signal vector
#' @return A list with both results and the maximum absolute difference
compare_dft <- function(x) {
  manual <- manual_dft(x)
  builtin <- fft(x)
  list(
    manual = manual,
    builtin = builtin,
    max_abs_diff = max(Mod(manual - builtin))
  )
}

#' Build a data.frame of the frequency-domain magnitude spectrum
#'
#' @param X Complex DFT output (from manual_dft or fft)
#' @param sampling_rate Samples per second, used to compute the frequency axis
#' @return A data.frame with columns freq and magnitude, positive frequencies only
spectrum_df <- function(X, sampling_rate) {
  N <- length(X)
  freqs <- (0:(N - 1)) * sampling_rate / N
  half <- floor(N / 2)
  data.frame(
    freq = freqs[1:half],
    magnitude = Mod(X)[1:half] / N * 2
  )
}
