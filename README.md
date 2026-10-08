# ComplexToolkit

# ComplexToolkit

An interactive R Shiny app that explores complex numbers through polynomial roots, AC circuit impedance, Mandelbrot/Julia fractals, a from-scratch Fourier transform, and 2D geometric transformations.

   🔗 **Live demo:** [https://prathiksha-v123.shinyapps.io/ComplexToolkit/](https://prathiksha-v123.shinyapps.io/ComplexToolkit/)

## Features

The app has five tabs, each showing a real-world use of complex arithmetic.

| Tab | What it does |
|-----|--------------|
| **Polynomial Roots** | Enter coefficients (e.g. `1, 0, 1`) and plot all roots on the complex plane. Also includes nth roots and roots of unity. |
| **AC Circuits** | Compute the impedance of series and parallel RLC circuits from R, L, C, frequency and source voltage, and draw the impedance vectors as phasors. |
| **Fractals** | Generate the Mandelbrot set or a Julia set for any constant `c`, with adjustable resolution and iteration count. |
| **Fourier Transform** | Build a signal from chosen frequencies and amplitudes, then analyse it with a hand-written Discrete Fourier Transform, checked against R's built-in `fft()`. |
| **Transformations** | Create a regular polygon and scale or rotate it using complex multiplication. |

## Getting started

### 1. Install R packages

```r
install.packages(c("shiny", "bslib", "ggplot2"))

# Optional: loading spinners (the app works without it)
install.packages("shinycssloaders")
```

### 2. Run the app

Clone the repo, open the folder in R or RStudio, then run:

```r
shiny::runApp()
```

Or from the terminal:

```bash
git clone https://github.com/<your-username>/ComplexToolkit.git
cd ComplexToolkit
Rscript -e "shiny::runApp()"
```

## Project structure

```
ComplexToolkit/
├── app.R                  # Shiny UI and server
├── DESCRIPTION            # Project metadata and dependencies
├── R/
│   ├── complex_utils.R    # Polar/rectangular conversion, nth roots, helpers
│   ├── roots.R            # Polynomial root finding and plotting
│   ├── circuits.R         # RLC impedance analysis and phasor plot
│   ├── mandelbrot.R       # Mandelbrot and Julia set generation
│   ├── fourier.R          # Manual DFT, signal generation, spectrum
│   └── transformations.R  # Polygon creation and complex transformations
└── tests/
    ├── testthat.R
    └── testthat/
        └── test-modules.R
```

## Running the tests

```r
install.packages("testthat")
testthat::test_dir("tests/testthat")
```

## How it works

- **Roots:** coefficients are passed to `polyroot()` and the results are plotted as points on the complex plane.
- **Circuits:** resistor, inductor and capacitor impedances (`R`, `jωL`, `1/(jωC)`) are combined as complex numbers, so magnitude and phase angle come straight from `Mod()` and `Arg()`.
- **Fractals:** each point `z` is iterated with `z² + c`, and the number of steps before it escapes sets its colour.
- **Fourier:** the DFT is implemented directly from its definition and compared with `fft()` to confirm it is correct.
- **Transformations:** multiplying by `r·e^(iθ)` scales a shape by `r` and rotates it by `θ`.

## Built with

- [R](https://www.r-project.org/)
- [Shiny](https://shiny.posit.co/)
- [bslib](https://rstudio.github.io/bslib/)
- [ggplot2](https://ggplot2.tidyverse.org/)

## Author

**PRATHIKSHA V**
