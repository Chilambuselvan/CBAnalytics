# Calculations behind the charts. Plain functions of the transactions table,
# so they can be tested without running the app.

# Expenses, optionally without fixed monthly costs.
expenses <- function(tx, include_standard = FALSE) {
  out <- dplyr::filter(tx, Direction == "Expense")
  if (!include_standard) out <- dplyr::filter(out, !IsStandard)
  out
}

filter_period <- function(tx, year = NULL, month = NULL) {
  if (!is.null(year) && nzchar(year)) tx <- dplyr::filter(tx, Year == as.integer(year))
  if (!is.null(month) && nzchar(month)) tx <- dplyr::filter(tx, Month == month)
  tx
}

# Largest single transaction, or NULL when there is none.
top_transaction <- function(tx) {
  if (nrow(tx) == 0) return(NULL)
  dplyr::slice_max(tx, Amount, n = 1, with_ties = FALSE)
}

# Sum of Amount per value of `by`. Factor columns (Month, Weekday) keep every
# level, so empty months and days show as 0.
total_by <- function(tx, by) {
  tx |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by)), .drop = FALSE) |>
    dplyr::summarise(Amount = round(sum(Amount), 2), .groups = "drop")
}

# Income, expenses and net per company over all time.
company_totals <- function(tx) {
  tx |>
    dplyr::mutate(Company = dplyr::coalesce(Company, "(uncategorised)")) |>
    dplyr::group_by(Company, Segment) |>
    dplyr::summarise(
      Income = round(sum(Amount[Direction == "Income"]), 2),
      Expenses = round(sum(Amount[Direction == "Expense"]), 2),
      Net = Income - Expenses,
      Transactions = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(Expenses))
}

# Missing values per column, as a share of all rows.
missing_summary <- function(tx) {
  counts <- vapply(tx, function(x) sum(is.na(x)), integer(1))
  data.frame(
    Column = names(counts),
    Missing = unname(counts),
    Percent = round(unname(counts) / max(nrow(tx), 1) * 100, 1)
  ) |>
    dplyr::filter(Missing > 0) |>
    dplyr::arrange(dplyr::desc(Missing))
}

# Amount paid back so far for a tracker from config.yml:
#   trackers:
#     - name: Laptop loan
#       pattern: "loan repayment"   # matched against booking text
#       segment: Friends            # optional
#       target: 600
tracker_progress <- function(tx, tracker) {
  hit <- grepl(tracker$pattern, tx$BookingText, ignore.case = TRUE)
  if (!is.null(tracker$segment)) hit <- hit & tx$Segment %in% tracker$segment
  sum(tx$Amount[hit])
}

sorted_choices <- function(x, decreasing = FALSE) {
  sort(unique(x[!is.na(x)]), decreasing = decreasing)
}

format_money <- function(x, currency = "EUR") {
  paste(formatC(x, format = "f", digits = 2, big.mark = ","), currency)
}
