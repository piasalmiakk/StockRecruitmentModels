#' @export
run_everything <- function() {
  shiny::shinyApp(ui = ui, server = server)
}

