server <- function(input, output) {
  output$stockrecruitment_plot <- renderPlot({
    chosen_species <- all_data_combined |>
      filter(species == input$species,
             Year = input$year_range)




    chosen_model <- if (input$model == "Beverton & Holt") {
      bh_model(a,b,chosen_species)
    } else if (input$model == "Ricker") {
      ricker_model(a,)
    }

    ggplot(chosen_species, aes(
      x = SSB,
      y = Recruitment,
      colour = species)) +
      geom_point() +
      geom_abline(chosen_model)
  })
}

shinyApp(ui = ui, server = server)
