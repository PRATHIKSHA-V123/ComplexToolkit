# complex_utils.R
# Core wrapper / helper functions built on top of R's native complex type.
# These exist to give the project a clean, documented API layer rather
# than scattering raw Re()/Im()/Mod()/Arg() calls throughout the app.

#' Convert a complex number to polar form
#'
#' @param z A complex number (or vector of complex numbers)
#' @return A list/data.frame with modulus (r) and argument in radians (theta)
to_polar <- function(z) {
  data.frame(
    r = Mod(z),
    theta = Arg(z)
  )
}

#' Convert polar coordinates back to a complex number
#'
#' @param r Modulus
#' @param theta Argument in radians
#' @return A complex number
to_rect <- function(r, theta) {
  complex(modulus = r, argument = theta)
}

#' Compute the n-th roots of a complex number
#'
#' @param z A single complex number
#' @param n Integer, the root to compute (e.g. 2 for square roots)
#' @return A complex vector of length n containing all n-th roots
nth_roots <- function(z, n) {
  if (n <= 0 || n %% 1 != 0) stop("n must be a positive integer")
  r <- Mod(z)^(1 / n)
  theta <- Arg(z)
  k <- 0:(n - 1)
  complex(modulus = r, argument = (theta + 2 * pi * k) / n)
}

#' Compute the n-th roots of unity
#'
#' @param n Integer, number of roots
#' @return A complex vector of the n-th roots of unity
roots_of_unity <- function(n) {
  nth_roots(1 + 0i, n)
}

#' Nicely format a complex number as a string, e.g. "3 + 4i" or "2 - 5i"
#'
#' @param z A complex number
#' @param digits Number of decimal places to round to
#' @return A formatted character string
format_complex <- function(z, digits = 3) {
  re <- round(Re(z), digits)
  im <- round(Im(z), digits)
  sign <- ifelse(im >= 0, "+", "-")
  sprintf("%s %s %si", re, sign, abs(im))
}

#' Safe complex division with a clear error for division by zero
#'
#' @param z1 Numerator (complex)
#' @param z2 Denominator (complex)
#' @return z1 / z2
safe_complex_div <- function(z1, z2) {
  if (Mod(z2) == 0) {
    stop("Division by zero complex number is undefined.")
  }
  z1 / z2
}

#' Vector of complex numbers -> data.frame ready for ggplot2 (Argand plane)
#'
#' @param z A complex vector
#' @param label Optional character vector of labels, recycled if length 1
#' @return A data.frame with columns re, im, label
complex_to_df <- function(z, label = "z") {
  data.frame(
    re = Re(z),
    im = Im(z),
    label = if (length(label) == 1) paste0(label, seq_along(z)) else label,
    stringsAsFactors = FALSE
  )
}
