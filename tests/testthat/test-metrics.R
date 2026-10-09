app <- load_app_data(sample_dir)
tx <- app$transactions

test_that("sample data loads without double counting the overlapping month", {
  expect_true(app$is_sample)
  dec <- dplyr::filter(tx, Year == 2025, Month == "Dec")
  raw_2025 <- readr::read_csv(file.path(sample_dir, "statements_2025.csv"), show_col_types = FALSE)
  raw_dec <- sum(grepl("\\.12\\.2025$", raw_2025$`Transaction date`))
  expect_equal(nrow(dec), raw_dec)
  expect_equal(anyDuplicated(tx[, c("TransactionDate", "Amount", "BookingText")]), 0)
})

test_that("every sample transaction except the deliberately unknown ones is categorised", {
  unmatched <- unique(sub(" \\d+$", "", tx$BookingText[is.na(tx$Company)]))
  expect_setequal(unmatched, c("POS UNKNOWN SHOP", "ATM WITHDRAWAL"))
})

test_that("the override from config.yml is applied", {
  agency <- dplyr::filter(tx, grepl("TRAVEL AGENCY", BookingText))
  expect_equal(agency$Company, "SkyAir")
})

test_that("expenses() excludes fixed costs unless asked", {
  expect_false(any(expenses(tx)$IsStandard))
  expect_true(any(expenses(tx, include_standard = TRUE)$IsStandard))
  expect_true(all(expenses(tx)$Direction == "Expense"))
})

test_that("total_by() keeps empty months and weekdays", {
  one_month <- filter_period(tx, 2025, "Mar")
  expect_equal(nrow(total_by(one_month, "Month")), 12)
  expect_equal(nrow(total_by(one_month[0, ], "Weekday")), 7)
})

test_that("top_transaction() handles an empty month", {
  expect_null(top_transaction(tx[0, ]))
  expect_equal(top_transaction(tx)$Amount, max(tx$Amount))
})

test_that("missing_summary() reports a share of rows", {
  df <- data.frame(a = c(1, NA, NA, 4), b = 1:4)
  expect_equal(missing_summary(df), data.frame(Column = "a", Missing = 2L, Percent = 50))
})

test_that("tracker sums matching repayments", {
  tracker <- app$config$trackers[[1]]
  expect_equal(tracker_progress(tx, tracker), 500)
})
