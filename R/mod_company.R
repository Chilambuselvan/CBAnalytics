# Company report: month-by-month spend for one fixed-cost company, one segment
# and one other company.

company_ui <- function(id, tx) {
  ns <- shiny::NS(id)
  years <- sorted_choices(tx$Year, decreasing = TRUE)
  spent <- dplyr::filter(tx, Direction == "Expense")

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::selectInput(ns("year"), "Year", years, selected = years[1]),
      shiny::selectInput(ns("segment"), "Segment", sorted_choices(spent$Segment)),
      shiny::selectInput(ns("standard"), "Fixed-cost company",
                         sorted_choices(spent$Company[spent$IsStandard])),
      shiny::selectInput(ns("irregular"), "Other company",
                         sorted_choices(spent$Company[!spent$IsStandard]))
    ),
    bslib::layout_columns(
      bslib::card(bslib::card_header(shiny::textOutput(ns("standard_title"))),
                  plotly::plotlyOutput(ns("standard_monthly"))),
      bslib::card(bslib::card_header(shiny::textOutput(ns("segment_title"))),
                  plotly::plotlyOutput(ns("segment_monthly")))
    ),
    bslib::layout_columns(
      bslib::card(bslib::card_header(shiny::textOutput(ns("irregular_title"))),
                  plotly::plotlyOutput(ns("irregular_monthly"))),
      bslib::card(bslib::card_header(shiny::textOutput(ns("irregular_weekday_title"))),
                  plotly::plotlyOutput(ns("irregular_weekday")))
    )
  )
}

company_server <- function(id, tx, currency) {
  shiny::moduleServer(id, function(input, output, session) {
    year_expenses <- shiny::reactive(expenses(filter_period(tx, input$year), include_standard = TRUE))
    for_company <- function(company) dplyr::filter(year_expenses(), Company %in% company)
    with_total <- function(label, df) paste0(label, " ", input$year, " - ", format_money(sum(df$Amount), currency))

    standard <- shiny::reactive(total_by(for_company(input$standard), "Month"))
    segment <- shiny::reactive(total_by(dplyr::filter(year_expenses(), Segment %in% input$segment), "Month"))
    irregular <- shiny::reactive(total_by(for_company(input$irregular), "Month"))
    irregular_weekday <- shiny::reactive(total_by(for_company(input$irregular), "Weekday"))

    output$standard_title <- shiny::renderText(with_total(input$standard, standard()))
    output$segment_title <- shiny::renderText(with_total(paste("Segment", input$segment), segment()))
    output$irregular_title <- shiny::renderText(with_total(input$irregular, irregular()))
    output$irregular_weekday_title <- shiny::renderText(paste(input$irregular, "by weekday", input$year))

    output$standard_monthly <- plotly::renderPlotly(plot_bars(standard(), "Month"))
    output$segment_monthly <- plotly::renderPlotly(plot_bars(segment(), "Month"))
    output$irregular_monthly <- plotly::renderPlotly(plot_bars(irregular(), "Month"))
    output$irregular_weekday <- plotly::renderPlotly(plot_bars(irregular_weekday(), "Weekday"))
  })
}
