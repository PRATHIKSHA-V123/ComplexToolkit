# roots.R
# Polynomial root finding + visualization on the Argand plane.

library(ggplot2)

#' Find roots of a polynomial given its coefficients
#'
#' Coefficients are given from the CONSTANT term up, matching R's
#' polyroot() convention: for a0 + a1*x + a2*x^2 + ... + an*x^n,
#' pass c(a0, a1, a2, ..., an).
#'
#' @param coeffs Numeric vector of coefficients, constant term first
#' @return A complex vector of roots
find_roots <- function(coeffs) {
  coeffs <- coeffs[!is.na(coeffs)]
  if (length(coeffs) < 2) stop("Need at least a linear polynomial (2 coefficients).")
  if (tail(coeffs, 1) == 0) stop("Leading coefficient cannot be zero.")
  polyroot(coeffs)
}

#' Build a ggplot2 Argand-plane visualization of a set of roots
#'
#' @param roots A complex vector of roots (from find_roots)
#' @return A ggplot object
plot_roots <- function(roots) {
  df <- data.frame(
    re = Re(roots),
    im = Im(roots),
    label = paste0("Root ", seq_along(roots), ": ", format_complex(roots))
  )

  max_range <- max(abs(c(df$re, df$im)), 1) * 1.4

  ggplot(df, aes(x = re, y = im)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 0, color = "grey60") +
    geom_point(size = 4, color = "#2C3E93") +
    geom_text(aes(label = label), vjust = -1.2, size = 3.5) +
    coord_equal(xlim = c(-max_range, max_range), ylim = c(-max_range, max_range)) +
    labs(
      title = "Polynomial Roots on the Argand Plane",
      x = "Real axis",
      y = "Imaginary axis"
    ) +
    theme_minimal(base_size = 13)
}

#' Human-readable polynomial string from coefficients (constant term first)
#'
#' @param coeffs Numeric vector of coefficients
#' @return A character string, e.g. "1 + 0x + 1x^2"
poly_to_string <- function(coeffs) {
  terms <- vapply(seq_along(coeffs), function(i) {
    power <- i - 1
    if (power == 0) as.character(coeffs[i])
    else if (power == 1) paste0(coeffs[i], "x")
    else paste0(coeffs[i], "x^", power)
  }, character(1))
  paste(terms, collapse = " + ")
}
