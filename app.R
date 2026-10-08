# app.R
# ComplexR: an interactive Shiny web app exploring complex number arithmetic
# through polynomial roots, AC circuits, fractals, Fourier transforms, and
# 2D geometric transformations.

library(shiny)
library(bslib)
library(ggplot2)

# Optional spinner package: app still works fine without it.
has_spinners <- requireNamespace("shinycssloaders", quietly = TRUE)
with_spinner <- function(ui) {
  if (has_spinners) shinycssloaders::withSpinner(ui, type = 6, color = "#2C3E93") else ui
}

source("R/complex_utils.R")
source("R/roots.R")
source("R/circuits.R")
source("R/mandelbrot.R")
source("R/fourier.R")
source("R/transformations.R")

# ---- UI ---------------------------------------------------------------

ui <- page_navbar(
  title = "ComplexR",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E93"
  ),

  nav_panel(
    "Roots",
    layout_sidebar(
      sidebar = sidebar(
        h5("Polynomial coefficients"),
        helpText("Constant term first, e.g. 1, 0, 1 means 1 + x\u00b2"),
        textInput("poly_coeffs", "Coefficients (comma-separated)", value = "1, 0, 1"),
        actionButton("find_roots_btn", "Find Roots", class = "btn-primary"),
        hr(),
        verbatimTextOutput("poly_string")
      ),
      card(
        card_header("Roots on the Argand Plane"),
        with_spinner(plotOutput("roots_plot", height = "450px"))
      ),
      card(
        card_header("Root values"),
        tableOutput("roots_table")
      )
    )
  ),

  nav_panel(
    "AC Circuits",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("topology", "Topology", choices = c("series", "parallel"), inline = TRUE),
        numericInput("R_val", "Resistance R (\u03a9)", value = 100, min = 0),
        numericInput("L_val", "Inductance L (H)", value = 0.05, min = 0, step = 0.01),
        numericInput("C_val", "Capacitance C (F)", value = 0.00002, min = 0, step = 0.00001),
        numericInput("freq_val", "Frequency (Hz)", value = 60, min = 0.1),
        numericInput("v_val", "Source Voltage (V)", value = 120, min = 0)
      ),
      card(
        card_header("Impedance Vector Diagram"),
        with_spinner(plotOutput("circuit_plot", height = "450px"))
      ),
      card(
        card_header("Results"),
        verbatimTextOutput("circuit_summary")
      )
    )
  ),

  nav_panel(
    "Fractals",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("fractal_type", "Fractal", choices = c("Mandelbrot", "Julia")),
        conditionalPanel(
          "input.fractal_type == 'Julia'",
          numericInput("julia_re", "c (real part)", value = -0.7, step = 0.01),
          numericInput("julia_im", "c (imaginary part)", value = 0.27015, step = 0.01)
        ),
        sliderInput("resolution", "Resolution", min = 100, max = 600, value = 300, step = 50),
        sliderInput("max_iter", "Max Iterations", min = 20, max = 300, value = 100, step = 10),
        actionButton("render_fractal_btn", "Render", class = "btn-primary")
      ),
      card(
        card_header("Fractal Render"),
        with_spinner(plotOutput("fractal_plot", height = "550px"))
      )
    )
  ),

  nav_panel(
    "Fourier",
    layout_sidebar(
      sidebar = sidebar(
        textInput("freqs", "Frequencies (Hz, comma-separated)", value = "5, 20, 50"),
        textInput("amps", "Amplitudes (comma-separated)", value = "1, 0.5, 0.25"),
        numericInput("sampling_rate", "Sampling Rate (Hz)", value = 500, min = 50),
        numericInput("duration", "Duration (s)", value = 1, min = 0.1, step = 0.1),
        actionButton("run_dft_btn", "Run DFT", class = "btn-primary"),
        hr(),
        verbatimTextOutput("dft_validation")
      ),
      card(
        card_header("Time-Domain Signal"),
        with_spinner(plotOutput("signal_plot", height = "250px"))
      ),
      card(
        card_header("Frequency Spectrum (Manual DFT)"),
        with_spinner(plotOutput("spectrum_plot", height = "250px"))
      )
    )
  ),

  nav_panel(
    "Transformations",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("n_sides", "Polygon Sides", min = 3, max = 12, value = 4),
        sliderInput("scale_factor", "Scale Factor (r)", min = 0.1, max = 3, value = 1.5, step = 0.1),
        sliderInput("angle_deg", "Rotation Angle (degrees)", min = 0, max = 360, value = 45)
      ),
      card(
        card_header("Rotation & Scaling via Complex Multiplication"),
        with_spinner(plotOutput("transform_plot", height = "500px"))
      )
    )
  ),

  nav_spacer(),
  nav_item(tags$a("Source on GitHub", href = "#", target = "_blank"))
)

# ---- Server -------------------------------------------------------------

server <- function(input, output, session) {

  # --- Roots tab ---
  parsed_coeffs <- reactive({
    as.numeric(trimws(strsplit(input$poly_coeffs, ",")[[1]]))
  })

  computed_roots <- eventReactive(input$find_roots_btn, {
    tryCatch(find_roots(parsed_coeffs()), error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })
  }, ignoreNULL = FALSE)

  output$poly_string <- renderText({
    tryCatch(paste0("p(x) = ", poly_to_string(parsed_coeffs())), error = function(e) "")
  })

  output$roots_plot <- renderPlot({
    req(computed_roots())
    plot_roots(computed_roots())
  })

  output$roots_table <- renderTable({
    req(computed_roots())
    r <- computed_roots()
    data.frame(
      Root = paste0("r", seq_along(r)),
      Value = format_complex(r),
      Modulus = round(Mod(r), 4),
      `Argument (rad)` = round(Arg(r), 4),
      check.names = FALSE
    )
  })

  # --- Circuits tab ---
  circuit_result <- reactive({
    analyze_circuit(
      R = input$R_val, L = input$L_val, C = input$C_val,
      freq = input$freq_val, voltage = input$v_val,
      topology = input$topology
    )
  })

  output$circuit_plot <- renderPlot({
    plot_impedance_vectors(circuit_result())
  })

  output$circuit_summary <- renderText({
    res <- circuit_result()
    paste0(
      "Total impedance Z = ", format_complex(res$Z_total), " \u03a9\n",
      "Magnitude |Z|      = ", round(res$magnitude, 3), " \u03a9\n",
      "Phase angle        = ", round(res$phase_deg, 2), "\u00b0\n",
      "X_L                = ", round(res$X_L, 3), " \u03a9\n",
      "X_C                = ", round(res$X_C, 3), " \u03a9\n",
      "Current I          = ", format_complex(res$current), " A\n",
      "|I|                = ", round(res$current_magnitude, 4), " A"
    )
  })

  # --- Fractals tab ---
  fractal_matrix <- eventReactive(input$render_fractal_btn, {
    if (input$fractal_type == "Mandelbrot") {
      generate_mandelbrot(resolution = input$resolution, max_iter = input$max_iter)
    } else {
      c_val <- complex(real = input$julia_re, imaginary = input$julia_im)
      generate_julia(c_val, resolution = input$resolution, max_iter = input$max_iter)
    }
  }, ignoreNULL = FALSE)

  output$fractal_plot <- renderPlot({
    plot_fractal(fractal_matrix(), title = input$fractal_type)
  })

  # --- Fourier tab ---
  dft_data <- eventReactive(input$run_dft_btn, {
    freqs <- as.numeric(trimws(strsplit(input$freqs, ",")[[1]]))
    amps <- as.numeric(trimws(strsplit(input$amps, ",")[[1]]))
    sig <- generate_signal(freqs, amps, input$sampling_rate, input$duration)
    comparison <- compare_dft(sig$x)
    spec <- spectrum_df(comparison$manual, input$sampling_rate)
    list(signal = sig, comparison = comparison, spectrum = spec)
  }, ignoreNULL = FALSE)

  output$signal_plot <- renderPlot({
    d <- dft_data()$signal
    plot(d$t, d$x, type = "l", col = "#2C3E93", lwd = 1.5,
         xlab = "Time (s)", ylab = "Amplitude", main = "Signal x(t)")
  })

  output$spectrum_plot <- renderPlot({
    spec <- dft_data()$spectrum
    plot(spec$freq, spec$magnitude, type = "h", lwd = 3, col = "#E4572E",
         xlab = "Frequency (Hz)", ylab = "Magnitude", main = "Magnitude Spectrum")
  })

  output$dft_validation <- renderText({
    diff <- dft_data()$comparison$max_abs_diff
    paste0("Max abs difference vs built-in fft(): ", format(diff, scientific = TRUE))
  })

  # --- Transformations tab ---
  output$transform_plot <- renderPlot({
    shape <- make_shape(n_sides = input$n_sides)
    transformed <- transform_shape(shape, scale = input$scale_factor, angle_deg = input$angle_deg)
    plot_transformation(shape, transformed)
  })
}

shinyApp(ui, server)
