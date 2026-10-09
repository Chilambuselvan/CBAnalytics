raw_statement <- function(date, text, amount, value_date = date) {
  data.frame(
    "Transaction date" = date, "Value date" = value_date,
    "Booking text" = text, "Amount" = amount,
    "IBAN of account of initiator" = "DE00", check.names = FALSE
  )
}

test_that("amounts in English and German format are parsed", {
  expect_equal(parse_amount(c("1234.56", "-1,234.56", "1234,56", "-1.234,56", "-5")),
               c(1234.56, -1234.56, 1234.56, -1234.56, -5))
})

test_that("dates in dd.mm.yyyy and ISO format are parsed", {
  expect_equal(parse_date(c("31.12.2025", "2025-12-31")), as.Date(c("2025-12-31", "2025-12-31")))
})

test_that("the same transaction exported twice with different special characters is kept once", {
  raw <- raw_statement(c("03.12.2025", "03.12.2025"), c("CAFE SONNE 123", "CAFÉ  SONNE 123"), c("-5.50", "-5.50"))
  expect_equal(nrow(clean_transactions(raw)), 1)
})

test_that("different purchases with the same amount on the same day are both kept", {
  raw <- raw_statement(c("03.12.2025", "03.12.2025"), c("CAFE SONNE 123", "CAFE SONNE 456"), c("-5.50", "-5.50"))
  expect_equal(nrow(clean_transactions(raw)), 2)
})

test_that("zero amounts are dropped, sign becomes Direction, account details are removed", {
  raw <- raw_statement(c("01.01.2025", "02.01.2025", "03.01.2025"), c("A", "B", "C"), c("-10", "0", "20"))
  tx <- clean_transactions(raw)
  expect_equal(nrow(tx), 2)
  expect_setequal(tx$Direction, c("Expense", "Income"))
  expect_true(all(tx$Amount > 0))
  expect_false("IBAN of account of initiator" %in% names(tx))
})

test_that("month and weekday are ordered factors independent of locale", {
  tx <- clean_transactions(raw_statement("05.01.2026", "A", "-1"))  # a Monday
  expect_equal(as.character(tx$Month), "Jan")
  expect_equal(as.character(tx$Weekday), "Mon")
  expect_equal(levels(tx$Weekday)[1], "Mon")
})

test_that("a clear error names missing columns", {
  expect_error(clean_transactions(data.frame(Amount = "1")), "TransactionDate")
})
