#' @import bslib
#' @import shiny
#' @import rlang .data

# species map to key together shown species choices and inputID
species_map <- c("NEA Cod" = "nea_cod",
                 "NEA Haddock" = "nea_haddock")


ui <- bslib::page_sidebar(
  title = "Stock Recruitment Models",
  sidebar = bslib::sidebar(
  selectInput(inputId = "species", label = "Species",
              choices = species_map),
  selectInput(inputId = "model", label = "Model",
              choices = c("Beverton & Holt","Ricker","Hockey Stick")),
  sliderInput(inputId = "year_range", label = "Year range",
              min = 1950, max = 2025, value = c(1990,2025)),
    withMathJax(), # mathjax for proper formatting of equations
         uiOutput("model_equation"),
         uiOutput("model_description")),
  plotOutput("stockrecruitment_plot")
)
