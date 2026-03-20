ui <- page_sidebar(
  title = "Stock Recruitment Models",
  selectInput(inputId = "species", label = "Species",
              choices = c("NEA Cod","NEA Haddock")),
  selectInput(inputId = "model", label = "Model",
              choices = c("Beverton & Holt","Ricker")),
  sliderInput(inputId = "year_range", label = "Year range",
              min = 1, max = 100, value = 10)
  #include slider for a and b
  card(plotOutput("stockrecruitment_plot"))
)
