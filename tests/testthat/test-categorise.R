mapping <- data.frame(
  SearchKeyword = c("klarna", "klarna ab wish", "rent co", "booking.com"),
  Company = c("Klarna", "Wish", "Rent Co", "Booking.com"),
  Segment = c("Shopping", "Shopping", "Housing", "Travel"),
  Priority = 0,
  IsStandard = c(FALSE, FALSE, TRUE, FALSE)
)
tx <- data.frame(
  BookingText = c("KLARNA AB WISH ORDER 1", "Klarna payment", "RENT CO MIETE", "Booking.com hotel", "unknown shop"),
  Amount = c(10, 20, 900, 300, 5)
)

test_that("each transaction keeps one row and the most specific keyword wins", {
  out <- categorise(tx, mapping)
  expect_equal(nrow(out), nrow(tx))
  expect_equal(out$Company, c("Wish", "Klarna", "Rent Co", "Booking.com", NA))
})

test_that("Priority beats keyword length", {
  m <- mapping
  m$Priority[m$SearchKeyword == "klarna"] <- 1
  expect_equal(categorise(tx, m)$Company[1], "Klarna")
})

test_that("IsStandard comes from the mapping, or from the company list when the column is absent", {
  expect_equal(categorise(tx, mapping)$IsStandard, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  m <- mapping
  m$IsStandard <- NA
  expect_equal(categorise(tx, m, "Klarna")$IsStandard, c(FALSE, TRUE, FALSE, FALSE, FALSE))
})

test_that("overrides fix company and segment by amount or pattern", {
  out <- categorise(tx, mapping) |>
    apply_overrides(list(
      list(amount = 5, company = "Corner Shop", segment = "Groceries"),
      list(pattern = "hotel", segment = "Holiday")
    ))
  expect_equal(out$Company[5], "Corner Shop")
  expect_equal(out$Segment[c(4, 5)], c("Holiday", "Groceries"))
})

test_that("an xlsx mapping without Priority/IsStandard columns can be read", {
  path <- tempfile()
  dir.create(path)
  openxlsx::write.xlsx(mapping[, 1:3], file.path(path, "KeywordMapping.xlsx"))
  m <- read_keyword_mapping(path)
  expect_equal(m$Priority, rep(0, 4))
  expect_true(all(is.na(m$IsStandard)))
})
