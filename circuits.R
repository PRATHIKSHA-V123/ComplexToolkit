# circuits.R
# AC circuit analysis using complex impedance: Z = R + i(X_L - X_C)

library(ggplot2)
library(grid)  # for unit() and arrow() used in the vector diagram

#' Compute impedance and derived quantities for a series RLC circuit
#'
#' @param R Resistance (ohms)
#' @param L Inductance (henries)
#' @param C Capacitance (farads), use a very large number / Inf to disable
#' @param freq Frequency in Hz
#' @param voltage Source voltage amplitude (volts), assumed at 0 phase
#' @param topology "series" or "parallel"
#' @return A list with impedance (complex), magnitude, phase (degrees),
#'   current (complex), and the individual reactances
analyze_circuit <- function(R, L, C, freq, voltage = 1, topology = c("series", "parallel")) {
  topology <- match.arg(topology)
  omega <- 2 * pi * freq

  X_L <- omega * L
  X_C <- if (C > 0) 1 / (omega * C) else 0

  Z_R <- complex(real = R, imaginary = 0)
  Z_L <- complex(real = 0, imaginary = X_L)
  Z_C <- complex(real = 0, imaginary = -X_C)

  if (topology == "series") {
    Z_total <- Z_R + Z_L + Z_C
  } else {
    # Parallel combination via admittances (1/Z), skipping any zero-impedance branch
    admittances <- c(
      if (Mod(Z_R) > 0) 1 / Z_R else 0,
      if (Mod(Z_L) > 0) 1 / Z_L else 0,
      if (Mod(Z_C) > 0) 1 / Z_C else 0
    )
    Y_total <- sum(admittances)
    Z_total <- if (Mod(Y_total) > 0) 1 / Y_total else complex(real = Inf, imaginary = 0)
  }

  current <- if (Mod(Z_total) > 0) voltage / Z_total else complex(real = NA, imaginary = NA)

  list(
    Z_R = Z_R, Z_L = Z_L, Z_C = Z_C,
    Z_total = Z_total,
    magnitude = Mod(Z_total),
    phase_deg = Arg(Z_total) * 180 / pi,
    current = current,
    current_magnitude = Mod(current),
    X_L = X_L, X_C = X_C
  )
}

#' Plot the impedance components as vectors on the complex plane
#'
#' @param result The list returned by analyze_circuit()
#' @return A ggplot object
plot_impedance_vectors <- function(result) {
  vecs <- data.frame(
    component = c("R", "L", "C", "Total Z"),
    re_end = c(Re(result$Z_R), Re(result$Z_L), Re(result$Z_C), Re(result$Z_total)),
    im_end = c(Im(result$Z_R), Im(result$Z_L), Im(result$Z_C), Im(result$Z_total)),
    stringsAsFactors = FALSE
  )
  vecs$re_start <- 0
  vecs$im_start <- 0
  vecs$is_total <- vecs$component == "Total Z"

  max_range <- max(abs(c(vecs$re_end, vecs$im_end)), 1) * 1.3

  ggplot(vecs) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = 0, color = "grey60") +
    geom_segment(
      aes(x = re_start, y = im_start, xend = re_end, yend = im_end, color = component,
          linewidth = is_total),
      arrow = arrow(length = unit(0.25, "cm")), lineend = "round"
    ) +
    scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.9), guide = "none") +
    coord_equal(xlim = c(-max_range, max_range), ylim = c(-max_range, max_range)) +
    labs(
      title = "Impedance Vectors on the Complex Plane",
      x = "Resistance (Ω, Real axis)",
      y = "Reactance (Ω, Imaginary axis)",
      color = "Component"
    ) +
    theme_minimal(base_size = 13)
}
