# Reading and cleaning bank statement exports.

# Bank export column -> name used in the app.
COLUMN_NAMES <- c(
  "Transaction date" = "TransactionDate",
  "Value date"       = "ValueDate",
  "Transaction type" = "TransactionType",
  "Booking text"     = "BookingText",
  "Amount"           = "Amount",
  "Currency"         = "Currency"
)

# Columns with account details of the other party; never shown in the app.
SENSITIVE_COLUMNS <- c(
  "Account of initiator",
  "IBAN of account of initiator",
  "Bank code of account of initiator"
)

WEEKDAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

# Every statement CSV in `dir`, stacked. All columns are read as text and
# parsed in clean_transactions(). Set `csv_encoding` in config.yml if the
# bank exports e.g. "windows-1252" instead of UTF-8.
read_statements <- function(dir, encoding = "UTF-8") {
  files <- list.files(dir, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
  files <- files[!grepl("^KeywordMapping", basename(files), ignore.case = TRUE)]
  if (length(files) == 0) stop("No statement CSV files found in '", dir, "'")

  dplyr::bind_rows(lapply(files, function(f) {
    df <- readr::read_csv(
      f,
      col_types = readr::cols(.default = readr::col_character()),
      na = c("", "NA"),
      locale = readr::locale(encoding = encoding),
      progress = FALSE
    )
    df$SourceFile <- basename(f)
    df
  }))
}

# Accepts "1234.56", "-1,234.56", "1234,56" and "-1.234,56".
parse_amount <- function(x) {
  x <- gsub("[[:space:]]", "", x)
  german <- grepl(",\\d{1,2}$", x)
  x[german] <- gsub(",", ".", gsub(".", "", x[german], fixed = TRUE), fixed = TRUE)
  x[!german] <- gsub(",", "", x[!german], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Accepts "31.12.2025" and "2025-12-31".
parse_date <- function(x) {
  d <- as.Date(x, format = "%d.%m.%Y")
  iso <- is.na(d) & !is.na(x)
  d[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  d
}

# Booking text reduced to lowercase letters and digits, so the same
# transaction exported twice with different special characters compares equal.
# Accented letters are mapped by hand because iconv's transliteration depends
# on the system locale.
ACCENTED <- "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f9\u00fa\u00fb\u00fc\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d9\u00da\u00db\u00dc"
UNACCENTED <- "aaaaaaceeeeiiiinooooouuuuAAAAAACEEEEIIIINOOOOOUUUU"

normalise_text <- function(x) {
  x <- enc2utf8(dplyr::coalesce(x, ""))
  x <- chartr(ACCENTED, UNACCENTED, gsub("\u00df", "ss", x))
  gsub("[^a-z0-9]", "", tolower(x))
}

clean_transactions <- function(raw) {
  present <- intersect(names(COLUMN_NAMES), names(raw))
  tx <- dplyr::rename_with(raw, ~ COLUMN_NAMES[.x], dplyr::all_of(present))
  tx <- tx[, setdiff(names(tx), SENSITIVE_COLUMNS), drop = FALSE]

  missing <- setdiff(c("TransactionDate", "BookingText", "Amount"), names(tx))
  if (length(missing) > 0) {
    stop("Statement files are missing column(s): ", paste(missing, collapse = ", "))
  }
  if (!"ValueDate" %in% names(tx)) tx$ValueDate <- tx$TransactionDate

  tx |>
    dplyr::mutate(
      TransactionDate = parse_date(TransactionDate),
      ValueDate = parse_date(ValueDate),
      Amount = parse_amount(Amount)
    ) |>
    dplyr::filter(!is.na(TransactionDate), !is.na(Amount), Amount != 0) |>
    # Overlapping exports contain the same transaction twice.
    dplyr::mutate(.text_key = normalise_text(BookingText)) |>
    dplyr::distinct(TransactionDate, ValueDate, Amount, .text_key, .keep_all = TRUE) |>
    dplyr::select(-.text_key) |>
    dplyr::mutate(
      Direction = factor(ifelse(Amount < 0, "Expense", "Income"), levels = c("Income", "Expense")),
      Amount = abs(Amount),
      Year = as.integer(format(TransactionDate, "%Y")),
      Month = factor(month.abb[as.integer(format(TransactionDate, "%m"))], levels = month.abb),
      Weekday = factor(WEEKDAYS[as.integer(format(TransactionDate, "%u"))], levels = WEEKDAYS)
    ) |>
    dplyr::arrange(dplyr::desc(TransactionDate))
}

# Everything the app needs, loaded once at startup.
load_app_data <- function(dir = data_dir()) {
  config <- load_config(dir)
  mapping <- read_keyword_mapping(dir)
  tx <- read_statements(dir, config$csv_encoding) |>
    clean_transactions() |>
    categorise(mapping, config$standard_expenses) |>
    apply_overrides(config$overrides)

  list(
    transactions = tx,
    config = config,
    is_sample = basename(normalizePath(dir, mustWork = FALSE)) == SAMPLE_DATA_DIR
  )
}
