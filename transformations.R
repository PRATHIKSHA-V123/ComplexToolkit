# transformations.R
# 2D rotation/scaling of shapes via complex multiplication.
# A point (x, y) is represented as a complex number x + iy. Multiplying by
# r * e^{i*theta} scales by r and rotates by theta.

library(ggplot2)

#' Generate a simple polygon shape as complex points (default: a square)
#'
#' @param n_sides Number of sides of the regular polygon
#' @param radius Radius of the circumscribed circle
#' @param center Complex number, center of the shape
#' @return A complex vector of vertex coordinates (closed, first point repeated at end)
make_shape <- function(n_sides = 4, radius = 1, center = 0 + 0i) {
  angles <- seq(0, 2 * pi, length.out = n_sides + 1)
  center + radius * complex(modulus = 1, argument = angles)
}

#' Apply a rotation + scaling transformation via complex multiplication
#'
#' @param shape Complex vector of points
#' @param scale Scale factor (r)
#' @param angle_deg Rotation angle in degrees
#' @return Transformed complex vector of points
transform_shape <- function(shape, scale = 1, angle_deg = 0) {
  factor <- complex(modulus = scale, argument = angle_deg * pi / 180)
  shape * factor
}

#' Plot the original and transformed shapes together for comparison
#'
#' @param original Complex vector of original shape points
#' @param transformed Complex vector of transformed shape points
#' @return A ggplot object
plot_transformation <- function(original, transformed) {
  df_orig <- data.frame(re = Re(original), im = Im(original), type = "Original")
  df_trans <- data.frame(re = Re(transformed), im = Im(transformed), type = "Transformed")
  df <- rbind(df_orig, df_trans)

  max_range <- max(abs(c(df$re, df$im)), 1) * 1.3

  ggplot(df, aes(x = re, y = im, color = type, group = type)) +
    geom_path(linewidth = 1) +
    geom_point(size = 2) +
    coord_equal(xlim = c(-max_range, max_range), ylim = c(-max_range, max_range)) +
    labs(
      title = "2D Transformation via Complex Multiplication",
      x = "Real axis", y = "Imaginary axis", color = ""
    ) +
    scale_color_manual(values = c("Original" = "grey50", "Transformed" = "#E4572E")) +
    theme_minimal(base_size = 13)
}
