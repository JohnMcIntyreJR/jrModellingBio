#' Regression with high leverage example
#'
#' This function runs an example of a shiny application to
#' do with a regression example to examine the change in
#' linear model fit when removing points of high influence.
#'
#' @param includecode Logical. If TRUE the code
#' included in the ui.R and server.R source files will
#' be shown alongside the app.
#' @usage regression_example(includecode = FALSE)
#' @import ggplot2
#' @import shiny
#' @export
regression_example = function(includecode = FALSE) {

  app_dir = system.file("shinyExamples", "regression", package = "jrModelling")
  if (app_dir == "") {
    stop("Could not find example directory. Try reinstalling `jrModelling`.",
         call. = FALSE)
  }
  shiny::runApp(app_dir,
                display.mode = ifelse(includecode, "showcase", "normal"))
}
