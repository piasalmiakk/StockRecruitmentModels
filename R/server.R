server <- function(input, output) {

  output$stockrecruitment_plot <- renderPlot({

    chosen_species <- all_data_combined |>
      filter(species == input$species, # choosing species
             Year >= input$year_range[1], # lower year range
             Year <= input$year_range[2]) # upper year range

    # user input for starting values a and b for the nls
    a <- input$a
    b <- input$b

    # model fit using if else based on user input to choose models
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

      list(chosen_model = chosen_model, pred = pred)

      }, error = function(e) NULL) #error if something goes wrong


    ggplot(chosen_species, aes(
      x = SSB,
      y = Recruitment,
      colour = species)) +
      geom_point() +
      geom_line(data = pred, aes(
        SSB,
        Recruitment_pred),
        colour = "blue")
  })
}

shinyApp(ui = ui, server = server)
