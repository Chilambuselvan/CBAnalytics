# Table views: all transactions, totals per company, and data quality.

TRANSACTION_COLUMNS <- c("TransactionDate", "Direction", "Amount", "Company", "Segment",
                         "BookingText", "TransactionType", "MatchedKeyword", "SourceFile")

data_table <- function(df) {
  DT::datatable(
    df, rownames = FALSE, filter = "top", class = "cell-border stripe",
    options = list(pageLength = 25, scrollX = TRUE)
  )
}

transactions_ui <- function(id) {
  bslib::card(full_screen = TRUE, DT::DTOutput(shiny::NS(id, "table")))
}

transactions_server <- function(id, tx) {
  shiny::moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT(data_table(tx[, intersect(TRANSACTION_COLUMNS, names(tx))]))
  })
}

company_totals_ui <- function(id) {
  bslib::card(full_screen = TRUE, DT::DTOutput(shiny::NS(id, "table")))
}

company_totals_server <- function(id, tx) {
  shiny::moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT(data_table(company_totals(tx)))
  })
}

quality_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(bslib::card_header("Missing values per column"), DT::DTOutput(ns("missing"))),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Transactions no keyword matched - add keywords for these"),
      DT::DTOutput(ns("uncategorised"))
    )
  )
}

quality_server <- function(id, tx) {
  shiny::moduleServer(id, function(input, output, session) {
    output$missing <- DT::renderDT(DT::datatable(missing_summary(tx), rownames = FALSE,
                                                 options = list(dom = "t")))
    output$uncategorised <- DT::renderDT({
      df <- dplyr::filter(tx, is.na(Company))
      data_table(df[, c("TransactionDate", "Direction", "Amount", "BookingText", "SourceFile")])
    })
  })
}
