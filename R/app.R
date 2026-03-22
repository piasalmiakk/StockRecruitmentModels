#' Run Stock-Recruitment Relationship Models
#' @description Runs the shiny app for Stock-Recruitment Relationship models
#' @importFrom rlang .data
#' @export
run_StockRecruitmentModels <- function() {
  shiny::shinyApp(ui = ui, server = server)
}

