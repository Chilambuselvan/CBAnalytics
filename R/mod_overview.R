# Monthly overview (was "Market Analysis"): totals, biggest transactions and
# where the non-fixed money went.

overview_ui <- function(id, tx) {
  ns <- shiny::NS(id)
  latest <- max(tx$TransactionDate)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::selectInput(ns("year"), "Year", sorted_choices(tx$Year, decreasing = TRUE),
                         selected = format(latest, "%Y")),
      shiny::selectInput(ns("month"), "Month", month.abb, selected = month.abb[as.integer(format(latest, "%m"))])
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box("Income", shiny::textOutput(ns("income")), theme = "success"),
      bslib::value_box("Expenses", shiny::textOutput(ns("expenses")), theme = "danger"),
      bslib::value_box("Highest income", shiny::textOutput(ns("top_income")),
                       shiny::textOutput(ns("top_income_who")), theme = "primary"),
      bslib::value_box("Highest non-fixed expense", shiny::textOutput(ns("top_expense")),
                       shiny::textOutput(ns("top_expense_who")), theme = "warning")
    ),
    bslib::layout_columns(
      bslib::card(bslib::card_header("Non-fixed expenses by company"),
                  plotly::plotlyOutput(ns("by_company"))),
      bslib::card(bslib::card_header("Non-fixed expenses by weekday (month)"),
                  plotly::plotlyOutput(ns("by_weekday_month")))
    ),
    bslib::layout_columns(
      bslib::card(bslib::card_header("Income vs expenses per month (year)"),
                  plotly::plotlyOutput(ns("by_month_year"))),
      bslib::card(bslib::card_header("Non-fixed expenses by weekday (year)"),
                  plotly::plotlyOutput(ns("by_weekday_year")))
    )
  )
}

overview_server <- function(id, tx, currency) {
  shiny::moduleServer(id, function(input, output, session) {
    # Only offer months that have data in the chosen year; default to the latest.
    shiny::observeEvent(input$year, {
      months <- month.abb[month.abb %in% filter_period(tx, input$year)$Month]
      selected <- if (input$month %in% months) input$month else utils::tail(months, 1)
      shiny::updateSelectInput(session, "month", choices = months, selected = selected)
    })

    year_tx <- shiny::reactive(filter_period(tx, input$year))
    month_tx <- shiny::reactive({
      shiny::req(input$month)
      filter_period(year_tx(), month = input$month)
    })
    money <- function(x) format_money(x, currency)

    output$income <- shiny::renderText(money(sum(month_tx()$Amount[month_tx()$Direction == "Income"])))
    output$expenses <- shiny::renderText(money(sum(month_tx()$Amount[month_tx()$Direction == "Expense"])))

    top_income <- shiny::reactive(top_transaction(dplyr::filter(month_tx(), Direction == "Income")))
    top_expense <- shiny::reactive(top_transaction(expenses(month_tx())))
    output$top_income <- shiny::renderText(if (is.null(top_income())) "-" else money(top_income()$Amount))
    who <- function(t) if (is.null(t)) "" else dplyr::coalesce(t$Company, "(uncategorised)")
    output$top_income_who <- shiny::renderText(who(top_income()))
    output$top_expense <- shiny::renderText(if (is.null(top_expense())) "-" else money(top_expense()$Amount))
    output$top_expense_who <- shiny::renderText(who(top_expense()))

    output$by_company <- plotly::renderPlotly({
      df <- expenses(month_tx()) |>
        dplyr::mutate(Company = dplyr::coalesce(Company, "(uncategorised)")) |>
        total_by("Company")
      if (nrow(df) == 0) return(plot_empty())
      plot_pie(df, "Company", title = paste("Total", money(sum(df$Amount))))
    })

    output$by_weekday_month <- plotly::renderPlotly({
      df <- total_by(expenses(month_tx()), "Weekday")
      plot_bars(df, "Weekday", title = paste("Total", money(sum(df$Amount))))
    })

    output$by_month_year <- plotly::renderPlotly({
      df <- total_by(year_tx(), c("Month", "Direction"))
      plotly::plot_ly(df, x = ~Month, y = ~Amount, color = ~Direction, type = "bar",
                      colors = c(Expense = "#dc3545", Income = "#198754")) |>
        plotly::layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = ""))
    })

    output$by_weekday_year <- plotly::renderPlotly({
      df <- total_by(expenses(year_tx()), "Weekday")
      plot_bars(df, "Weekday", title = paste("Total", money(sum(df$Amount))))
    })
  })
}
