# CB Analytics - personal bank account dashboard.
# Files in R/ are loaded automatically by Shiny. See README.md for setup.

library(shiny)
library(bslib)

app_data <- load_app_data()
tx <- app_data$transactions
config <- app_data$config

ui <- page_navbar(
  title = config$app_title,
  theme = bs_theme(version = 5, preset = "flatly"),
  header = if (app_data$is_sample) {
    div(class = "alert alert-info m-2 py-2",
        "Showing fake sample data. Set CB_DATA_DIR to your data folder to see your own.")
  },
  nav_panel("Overview", overview_ui("overview", tx)),
  nav_panel("Company Report", company_ui("company", tx)),
  nav_panel("Trackers", trackers_ui("trackers")),
  nav_panel("Totals by Company", company_totals_ui("totals")),
  nav_panel("Transactions", transactions_ui("transactions")),
  nav_panel("Data Quality", quality_ui("quality"))
)

server <- function(input, output, session) {
  overview_server("overview", tx, config$currency)
  company_server("company", tx, config$currency)
  trackers_server("trackers", tx, config$trackers, config$currency)
  company_totals_server("totals", tx)
  transactions_server("transactions", tx)
  quality_server("quality", tx)
}

shinyApp(ui, server)
