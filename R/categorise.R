# Assigning a Company and Segment to each transaction from its booking text.

# Reads KeywordMapping.xlsx or KeywordMapping.csv from the data folder.
# Required columns: SearchKeyword, Company, Segment.
# Optional: Priority (higher wins when several keywords match, default 0)
#           IsStandard (TRUE for fixed monthly costs like rent).
read_keyword_mapping <- function(dir) {
  xlsx <- file.path(dir, "KeywordMapping.xlsx")
  csv <- file.path(dir, "KeywordMapping.csv")
  mapping <- if (file.exists(xlsx)) {
    openxlsx::read.xlsx(xlsx, sheet = 1)
  } else if (file.exists(csv)) {
    readr::read_csv(csv, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
  } else {
    stop("No KeywordMapping.xlsx or KeywordMapping.csv found in '", dir, "'")
  }
  mapping <- as.data.frame(mapping)

  missing <- setdiff(c("SearchKeyword", "Company", "Segment"), names(mapping))
  if (length(missing) > 0) {
    stop("Keyword mapping is missing column(s): ", paste(missing, collapse = ", "))
  }
  mapping$SearchKeyword <- trimws(as.character(mapping$SearchKeyword))
  mapping <- mapping[!is.na(mapping$SearchKeyword) & nzchar(mapping$SearchKeyword), ]
  mapping$Priority <- if ("Priority" %in% names(mapping)) {
    dplyr::coalesce(as.numeric(mapping$Priority), 0)
  } else {
    0
  }
  mapping$IsStandard <- if ("IsStandard" %in% names(mapping)) {
    toupper(as.character(mapping$IsStandard)) %in% c("TRUE", "YES", "Y", "1", "X")
  } else {
    NA
  }
  mapping
}

# Each transaction gets at most one match: the keyword with the highest
# Priority, then the longest one ("klarna ab wish" beats "klarna"). Keywords
# are case-insensitive regular expressions, as in the original app.
#
# Standard (fixed) costs come from the mapping's IsStandard column, or from
# `standard_companies` when the mapping has no such column.
categorise <- function(tx, mapping, standard_companies = character()) {
  text <- dplyr::coalesce(tx$BookingText, "")
  order_best_first <- order(-mapping$Priority, -nchar(mapping$SearchKeyword))

  match_idx <- rep(NA_integer_, nrow(tx))
  for (i in order_best_first) {
    open <- is.na(match_idx)
    if (!any(open)) break
    hit <- tryCatch(
      grepl(mapping$SearchKeyword[i], text[open], ignore.case = TRUE),
      error = function(e) grepl(tolower(mapping$SearchKeyword[i]), tolower(text[open]), fixed = TRUE)
    )
    match_idx[which(open)[hit]] <- i
  }

  tx$Company <- mapping$Company[match_idx]
  tx$Segment <- mapping$Segment[match_idx]
  tx$MatchedKeyword <- mapping$SearchKeyword[match_idx]
  tx$IsStandard <- if (all(is.na(mapping$IsStandard))) {
    !is.na(tx$Company) & tx$Company %in% standard_companies
  } else {
    dplyr::coalesce(mapping$IsStandard[match_idx], FALSE)
  }
  tx
}

# One-off corrections from config.yml, e.g.
#   overrides:
#     - amount: 1490.55
#       company: Lufthansa
#       segment: Travel
#     - pattern: "some booking text"
#       company: Someone
# Each rule needs `amount` and/or `pattern`; `company` and `segment` are set
# on every matching transaction.
apply_overrides <- function(tx, overrides) {
  for (rule in overrides) {
    hit <- rep(TRUE, nrow(tx))
    if (!is.null(rule$amount)) hit <- hit & abs(tx$Amount - as.numeric(rule$amount)) < 0.005
    if (!is.null(rule$pattern)) hit <- hit & grepl(rule$pattern, tx$BookingText, ignore.case = TRUE)
    if (is.null(rule$amount) && is.null(rule$pattern)) next
    if (!is.null(rule$company)) tx$Company[hit] <- rule$company
    if (!is.null(rule$segment)) tx$Segment[hit] <- rule$segment
    if (!is.null(rule$standard)) tx$IsStandard[hit] <- isTRUE(rule$standard)
  }
  tx
}
