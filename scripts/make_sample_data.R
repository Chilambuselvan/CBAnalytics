# Generates the fake bank statements in data-sample/.
# Run from the project root:  Rscript scripts/make_sample_data.R
#
# The CSVs mimic the bank's English export (same columns, dd.mm.yyyy dates,
# negative amounts for expenses). All merchants, people and accounts are made
# up. The data deliberately includes the tricky cases the app must handle:
#   - two exports overlapping by one month, with special characters differing
#     in the overlap (must not be counted twice)
#   - booking texts matching two keywords ("klarna" and "klarna ab wish")
#   - transactions no keyword matches
#   - one transaction fixed by an override in config.yml

set.seed(2025)
out_dir <- "data-sample"
dates <- seq(as.Date("2025-01-01"), as.Date("2026-09-30"), by = "day")
months <- unique(format(dates, "%Y-%m"))

rows <- list()
add <- function(date, text, amount, type) {
  rows[[length(rows) + 1]] <<- data.frame(date = as.Date(date), text = text, amount = amount, type = type)
}
ref <- function() sprintf("%08d", sample.int(99999999, 1))
on_day <- function(month, day) as.Date(sprintf("%s-%02d", month, day))

# Fixed monthly costs and salary.
for (m in months) {
  add(on_day(m, 1), paste("SUNRISE HOUSING GMBH MIETE", m), -1250.00, "Standing order")
  add(on_day(m, 2), paste("METRO TRANSIT ABO", ref()), -86.00, "Direct debit")
  add(on_day(m, 3), paste("FIBERNET INTERNET RECHNUNG", ref()), -39.99, "Direct debit")
  add(on_day(m, 5), paste("CITY POWER STROM ABSCHLAG", ref()), -85.00, "Direct debit")
  add(on_day(m, 10), paste("LITTLE STARS KITA BEITRAG", m), -230.00, "Direct debit")
  add(on_day(m, 15), "TRANSFER TO SAVINGS ACCOUNT", -400.00, "Standing order")
  add(on_day(m, 27), paste("ACME GMBH GEHALT", m), round(runif(1, 3700, 3900), 2), "Credit")
}

# Day-to-day spending.
merchants <- list(
  list(text = "FRESHMART SAGT DANKE",       min = 15, max = 120, per_month = 6),
  list(text = "BIO CORNER MARKT",           min = 8,  max = 60,  per_month = 3),
  list(text = "CAFE SONNE",                 min = 3,  max = 18,  per_month = 4),
  list(text = "PIZZA NAPOLI",               min = 20, max = 65,  per_month = 2),
  list(text = "CITY PHARMACY",              min = 5,  max = 40,  per_month = 1),
  list(text = "KLARNA AB WISH ORDER",       min = 10, max = 80,  per_month = 1),
  list(text = "KLARNA PAYMENT ONLINESTORE", min = 20, max = 150, per_month = 1),
  list(text = "BOOKWORM BOOKS",             min = 9,  max = 45,  per_month = 1),
  list(text = "POS UNKNOWN SHOP",           min = 5,  max = 50,  per_month = 1),
  list(text = "ATM WITHDRAWAL",             min = 50, max = 200, per_month = 1)
)
for (m in months) {
  month_days <- dates[format(dates, "%Y-%m") == m]
  for (mer in merchants) {
    for (i in seq_len(rpois(1, mer$per_month))) {
      add(sample(month_days, 1), paste(mer$text, ref()), -round(runif(1, mer$min, mer$max), 2), "Card payment")
    }
  }
}

# Occasional bigger items.
add("2025-03-14", paste("SKYAIR TICKET", ref()), -412.30, "Card payment")
add("2025-07-02", paste("BOOKING.COM HOTEL", ref()), -688.00, "Card payment")
add("2025-07-20", paste("SKYAIR TICKET", ref()), -380.10, "Card payment")
add("2026-04-08", paste("TRAVEL AGENCY XYZ", ref()), -899.90, "Card payment")  # fixed by override
add("2026-05-30", paste("BOOKING.COM HOTEL", ref()), -540.00, "Card payment")
add("2025-06-12", "FINANZAMT STEUERERSTATTUNG 2024", 612.45, "Credit")
add("2025-09-01", paste("TECHSTORE LAPTOP", ref()), -600.00, "Card payment")
for (m in c("2025-10", "2025-11", "2025-12", "2026-01", "2026-02")) {
  add(on_day(m, 20), "LOAN REPAYMENT FROM ALEX LAPTOP", 100.00, "Credit")
}

tx <- do.call(rbind, rows)
tx <- tx[order(tx$date), ]

to_export <- function(df) {
  data.frame(
    "Transaction date" = format(df$date, "%d.%m.%Y"),
    "Value date" = format(df$date + sample(0:2, nrow(df), replace = TRUE, prob = c(.7, .2, .1)), "%d.%m.%Y"),
    "Transaction type" = df$type,
    "Booking text" = df$text,
    "Amount" = sprintf("%.2f", df$amount),
    "Currency" = "EUR",
    "Account of initiator" = "0000000000",
    "Bank code of account of initiator" = "00000000",
    "IBAN of account of initiator" = "DE00000000000000000000",
    check.names = FALSE
  )
}

export <- to_export(tx)
in_2025 <- tx$date <= as.Date("2025-12-31")
in_2026_file <- tx$date >= as.Date("2025-12-01")

# The second export repeats December 2025 with different special characters,
# like re-downloading a period from online banking.
second <- export[in_2026_file, ]
overlap <- tx$date[in_2026_file] <= as.Date("2025-12-31")
second$`Booking text`[overlap] <- sub("CAFE", "CAF\u00c9", sub("GEHALT", "GEHALT /", second$`Booking text`[overlap]))

readr::write_csv(export[in_2025, ], file.path(out_dir, "statements_2025.csv"))
readr::write_csv(second, file.path(out_dir, "statements_2026.csv"))

cat("Wrote", sum(in_2025), "+", nrow(second), "rows;", nrow(tx), "unique transactions\n")
