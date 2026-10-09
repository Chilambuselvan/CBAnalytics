# Trackers (was "Circling Amount"): progress towards targets set in config.yml,
# e.g. money lent to a friend being paid back.

trackers_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "gauges"))
}

trackers_server <- function(id, tx, trackers, currency) {
  shiny::moduleServer(id, function(input, output, session) {
    output$gauges <- shiny::renderUI({
      if (length(trackers) == 0) {
        return(bslib::card(
          "No trackers yet. Add a 'trackers:' section to config.yml in your data folder."
        ))
      }
      cards <- lapply(trackers, function(t) {
        bslib::card(
          plot_gauge(tracker_progress(tx, t), as.numeric(t$target), t$name, currency),
          min_height = 300
        )
      })
      do.call(bslib::layout_column_wrap, c(list(width = 1 / 2), cards))
    })
  })
}
