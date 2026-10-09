# Where the app finds its data and personal settings.
#
# Everything private (bank statements, keyword mapping, config.yml) lives in
# one folder. Point CB_DATA_DIR at it, e.g. in ~/.Renviron:
#   CB_DATA_DIR=D:/STUDIES/01_DataAnalytics/CBAnalytics/Data
# Without it the app runs on the fake data in data-sample/.

SAMPLE_DATA_DIR <- "data-sample"

data_dir <- function() {
  dir <- Sys.getenv("CB_DATA_DIR", unset = "")
  if (nzchar(dir)) dir else SAMPLE_DATA_DIR
}

default_config <- function() {
  list(
    app_title = "CB Analytics",
    currency = "EUR",
    csv_encoding = "UTF-8",
    standard_expenses = character(),
    overrides = list(),
    trackers = list()
  )
}

# Reads config.yml from the data folder; missing keys fall back to defaults.
load_config <- function(dir) {
  cfg <- default_config()
  path <- file.path(dir, "config.yml")
  if (file.exists(path)) {
    user <- yaml::read_yaml(path)
    cfg[names(user)] <- user
  }
  cfg$standard_expenses <- unlist(cfg$standard_expenses) %||% character()
  cfg
}

`%||%` <- function(x, y) if (is.null(x)) y else x
