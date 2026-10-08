library(testthat)

source("../../R/complex_utils.R")
source("../../R/roots.R")
source("../../R/circuits.R")
source("../../R/fourier.R")
source("../../R/transformations.R")

test_that("nth_roots returns correct count and each root satisfies z^n = original", {
  z <- complex(real = 1, imaginary = 1)
  roots <- nth_roots(z, 4)
  expect_length(roots, 4)
  for (r in roots) {
    expect_equal(Mod(r^4 - z), 0, tolerance = 1e-8)
  }
})

test_that("roots_of_unity sums to (approximately) zero for n > 1", {
  ru <- roots_of_unity(5)
  expect_equal(Mod(sum(ru)), 0, tolerance = 1e-8)
})

test_that("safe_complex_div throws on division by zero", {
  expect_error(safe_complex_div(1 + 1i, 0 + 0i), "Division by zero")
})

test_that("find_roots solves a known quadratic (x^2 - 1 = 0)", {
  roots <- find_roots(c(-1, 0, 1))
  expect_equal(sort(Re(roots)), c(-1, 1), tolerance = 1e-8)
})

test_that("find_roots correctly returns complex conjugate pair (x^2 + 1 = 0)", {
  roots <- find_roots(c(1, 0, 1))
  expect_equal(sort(Mod(roots)), c(1, 1), tolerance = 1e-8)
  expect_true(all(abs(Re(roots)) < 1e-8))
})

test_that("analyze_circuit computes correct series impedance", {
  # At resonance-ish check: pure resistor only (L=0, C effectively infinite via 0)
  res <- analyze_circuit(R = 100, L = 0, C = 0, freq = 60, voltage = 100, topology = "series")
  expect_equal(res$Z_total, complex(real = 100, imaginary = 0))
  expect_equal(res$current, complex(real = 1, imaginary = 0))
})

test_that("manual_dft matches R's built-in fft() within tolerance", {
  set.seed(42)
  x <- sin(2 * pi * 5 * seq(0, 1, length.out = 64))
  result <- compare_dft(x)
  expect_lt(result$max_abs_diff, 1e-8)
})

test_that("transform_shape correctly scales and rotates a point", {
  shape <- complex(real = 1, imaginary = 0)  # single point at (1, 0)
  transformed <- transform_shape(shape, scale = 2, angle_deg = 90)
  expect_equal(Re(transformed), 0, tolerance = 1e-8)
  expect_equal(Im(transformed), 2, tolerance = 1e-8)
})

test_that("make_shape generates the correct number of vertices (closed polygon)", {
  sq <- make_shape(n_sides = 4)
  expect_length(sq, 5)  # first vertex repeated to close the shape
})
