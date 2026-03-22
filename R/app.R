#' Run Stock-Recruitment Relationship Models
#' @description Runs the shiny app for Stock-Recruitment Relationship models
#' @export
run_everything <- function() {
  shiny::shinyApp(ui = ui, server = server)
}

