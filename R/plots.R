# Chart builders shared by the tabs.

plot_bars <- function(df, x, y = "Amount", title = NULL, name = NULL) {
  plotly::plot_ly(
    df, x = df[[x]], y = df[[y]], type = "bar", name = name,
    text = formatC(df[[y]], format = "f", digits = 2, big.mark = ","),
    textposition = "auto"
  ) |>
    plotly::layout(title = title, xaxis = list(title = ""), yaxis = list(title = ""))
}

plot_pie <- function(df, labels, values = "Amount", title = NULL) {
  plotly::plot_ly(df, labels = df[[labels]], values = df[[values]], type = "pie") |>
    plotly::layout(title = title)
}

plot_gauge <- function(value, target, title, currency) {
  plotly::plot_ly(
    type = "indicator", mode = "gauge+number", value = value,
    title = list(text = title),
    number = list(suffix = paste0(" ", currency), valueformat = ",.2f"),
    gauge = list(
      axis = list(range = list(0, target)),
      bar = list(color = "#2c3e50"),
      steps = list(
        list(range = c(0, target * 0.45), color = "#f8d7da"),
        list(range = c(target * 0.45, target * 0.8), color = "#fff3cd"),
        list(range = c(target * 0.8, target), color = "#d1e7dd")
      )
    )
  ) |>
    plotly::layout(margin = list(t = 60, b = 10))
}

# Placeholder shown instead of an empty chart.
plot_empty <- function(message = "No data for this selection") {
  plotly::plot_ly() |>
    plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(text = message, showarrow = FALSE, font = list(size = 16))
    )
}
