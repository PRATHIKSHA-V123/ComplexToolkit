# mandelbrot.R
# Mandelbrot and Julia set generation via complex iteration: z = z^2 + c
# Vectorized across the whole grid for reasonable performance in R.

#' Generate an escape-time matrix for the Mandelbrot set
#'
#' @param xmin,xmax Real-axis bounds
#' @param ymin,ymax Imaginary-axis bounds
#' @param resolution Number of pixels per axis (resolution x resolution grid)
#' @param max_iter Maximum number of iterations before assuming "in the set"
#' @return A matrix of escape-iteration counts (integer), suitable for image()/ggplot2
generate_mandelbrot <- function(xmin = -2, xmax = 0.5, ymin = -1.25, ymax = 1.25,
                                 resolution = 400, max_iter = 100) {
  x <- seq(xmin, xmax, length.out = resolution)
  y <- seq(ymin, ymax, length.out = resolution)

  c_grid <- outer(x, y, function(re, im) complex(real = re, imaginary = im))
  z <- matrix(0 + 0i, nrow = resolution, ncol = resolution)
  count <- matrix(0L, nrow = resolution, ncol = resolution)

  for (iter in seq_len(max_iter)) {
    active <- Mod(z) <= 2
    if (!any(active)) break
    z[active] <- z[active]^2 + c_grid[active]
    count[active] <- count[active] + 1L
  }

  count
}

#' Generate an escape-time matrix for a Julia set with a fixed parameter c
#'
#' @param c A fixed complex constant that defines the Julia set
#' @param xmin,xmax,ymin,ymax Plotting bounds (typically -2 to 2)
#' @param resolution Grid resolution per axis
#' @param max_iter Maximum iterations
#' @return A matrix of escape-iteration counts
generate_julia <- function(c, xmin = -2, xmax = 2, ymin = -2, ymax = 2,
                            resolution = 400, max_iter = 100) {
  x <- seq(xmin, xmax, length.out = resolution)
  y <- seq(ymin, ymax, length.out = resolution)

  z <- outer(x, y, function(re, im) complex(real = re, imaginary = im))
  count <- matrix(0L, nrow = resolution, ncol = resolution)

  for (iter in seq_len(max_iter)) {
    active <- Mod(z) <= 2
    if (!any(active)) break
    z[active] <- z[active]^2 + c
    count[active] <- count[active] + 1L
  }

  count
}

#' Plot an escape-time matrix as a fractal image using base graphics
#'
#' @param count_matrix Matrix returned by generate_mandelbrot() or generate_julia()
#' @param palette_colors Number of colors in the gradient palette
#' @param title Plot title
plot_fractal <- function(count_matrix, palette_colors = 256, title = "Fractal") {
  palette <- grDevices::colorRampPalette(
    c("#000428", "#004e92", "#00c6ff", "#f6f6f6", "#ffcc00", "#ff5e00")
  )(palette_colors)

  image(
    count_matrix,
    col = palette,
    axes = FALSE,
    main = title,
    useRaster = TRUE
  )
}
