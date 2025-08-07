#' Run the WuKong Shiny web application locally.
#' @export
WuKong_app <- function() {
  shiny::runApp(system.file('home', package='WuKong'),
                host=getOption("0.0.0.0"), port =getOption("8989"))
}
