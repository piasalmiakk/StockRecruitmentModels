#' @import shiny
#' @import ggplot2
#' @importFrom utils globalVariables data
#' @importFrom rlang .data


globalVariables(c(
  "data","combined_dataset","Recruitment", "species","Year","Recruitment_pred"
))


server <- function(input, output) {

  data("combined_dataset")

  # -----  FITTING MODEL ON PLOT ----- #

  model_fit <- reactive({ # putting the inputs in a reactive so it can be used in several outputs

  req(input$species, input$model) #require input to avoid NULL
    chosen_species <- combined_dataset |>
      dplyr::filter(species == input$species, # choosing species
             Year >= input$year_range[1], # lower year range
             Year <= input$year_range[2]) # upper year range

    # setting a and b starting values to NULL so the model will compute this each time
    a <- NULL
    b <- NULL

    # model fit using if else based on user input to choose models
    # using tryCatch in case something goes wrong
    fit <- tryCatch({
      if (input$model == "Beverton & Holt") {

        chosen_model <- bh_model(chosen_species,a,b)

        pred <- predict_bh(chosen_model, chosen_species)

      } else if (input$model == "Ricker") {

        chosen_model <- ricker_model(chosen_species,a,b)

        pred <- predict_ricker(chosen_model, chosen_species)

      } else if (input$model == "Hockey Stick") {

        chosen_model <- hockey_model(chosen_species, a, b)

        pred <- predict_hockey(chosen_model, chosen_species)
      }

      list(chosen_model = chosen_model,
           pred = pred,
           chosen_species = chosen_species)

    }, error = function(e) NULL) #error if something goes wrong

    fit
  })

    # making the plot
  output$stockrecruitment_plot <- renderPlot({

    fit <- model_fit() # fetching the fit from earlier

    # putting these in ifs to make sure they only run if fit isn't NULL
    if (is.null(fit)) return (NULL)
    plot_stockrecruit <- ggplot(fit$chosen_species, aes(
      x = SSB,
      y = Recruitment,
      colour = species)) +
      geom_point() +
      labs(title = "Stock-Recruitment relationship",
           colour = "Species"
           )


        if (!is.null(fit)) {
          plot_stockrecruit +
            geom_line(
            data = fit$pred,
            aes(SSB, Recruitment_pred),
            colour = "blue",
            linewidth = 1
          )

        } else {plot_stockrecruit}

  })

  # -----  MODEL EQUATIONS ----- #

  output$model_equation <- renderUI({

    fit <- model_fit()

    # using switch based on which model user chose
  equation <- switch(input$model,
    "Beverton & Holt" = "$$R = \\frac{a S}{1 + S/b}$$",
    "Ricker" = "$$R = a S e^{-b S}$$",
    "Hockey Stick" = "$$R = \\begin{cases} aS & S < b \\\\ ab & S \\ge b \\end{cases}$$"
  )

  # -----  MODEL EQUATIONS WITH FITTED COEFFICIENTS ----- #

  fitted <- NULL #setting fitted to NULL so inputs from UI can be selected

  if (!is.null(fit) && !is.null(fit$chosen_model)) {
    params <- coef(fit$chosen_model)
    a_hat <- round(params["a"], 3) # fetching a and b parameters
    b_hat <- round(params["b"], 3)

    # using sprintf to return both text and variables as string
    fitted <- switch(input$model,
      "Beverton & Holt" = sprintf("$$R = \\frac{%.3f S}{1 + S/%.3f}$$",
                                  a_hat, b_hat),
      "Ricker" = sprintf("$$R = %.3f S e^{-%.3f S}$$",
                         a_hat, b_hat),
      "Hockey Stick" = sprintf(
        "$$R = \\begin{cases} %.3f S & S < %.3f \\\\ %.3f & S \\ge %.3f \\end{cases}$$",
        a_hat, b_hat, a_hat * b_hat, b_hat
      )
    )
  }

  # mathjax formats equations nicely, hence them looking weird above
  withMathJax(HTML(paste0(
    "<b>Model:</b><br>", equation,
    "<br><br><b>Fitted:</b><br>", fitted
  )))
})

  # -----  MODEL EQUATION PARAMETERS ----- #

  # changing parameter text based on which model is chosen
  output$model_description <- renderUI({
    text <- switch(input$model,

                   "Beverton & Holt" = "
      <b>a:</b> maximum recruits per unit biomass<br>
      <b>b:</b> biomass at half-saturation
    ",

                   "Ricker" = "
      <b>a:</b> productivity at low stock size<br>
      <b>b:</b> density dependence strength
    ",

                   "Hockey Stick" = "
      <b>a:</b> slope at low stock size<br>
      <b>b:</b> breakpoint (threshold biomass)
    "
    )

    HTML(text)
  })
}


