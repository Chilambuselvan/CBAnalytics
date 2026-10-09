# CB Analytics

A Shiny dashboard for personal bank account statements: where the money goes
each month, spend per company and segment, and progress on money lent out.

Real financial data is **never** stored in this repository. The app reads it
from a folder on your own computer. Without that folder it runs on fake data
in `data-sample/`, so anyone can develop and test it.

## Run it

Install the packages once:

```r
install.packages(c("shiny", "bslib", "dplyr", "readr", "plotly", "DT",
                   "yaml", "openxlsx", "testthat"))
```

Then open the project in RStudio and click **Run App** on `app.R`, or run
`shiny::runApp()`. A blue banner shows when you are looking at sample data.

## Use your own data (owner only)

1. Put everything private in one folder outside git, or in `Data/` (it is
   git-ignored):
   - the bank's CSV exports (any file name ending in `.csv`)
   - `KeywordMapping.xlsx` or `KeywordMapping.csv`
   - `config.yml` - copy `data-sample/config.yml` and edit it
2. Tell the app where it is. Add this line to `~/.Renviron` (run
   `usethis::edit_r_environ()` or create the file), then restart R:
   ```
   CB_DATA_DIR=D:/STUDIES/01_DataAnalytics/CBAnalytics/Data
   ```

### Keyword mapping

| Column | Required | Meaning |
|---|---|---|
| `SearchKeyword` | yes | Text (case-insensitive regular expression) found in the booking text |
| `Company` | yes | Name shown in the app |
| `Segment` | yes | Group, e.g. Groceries, Travel |
| `Priority` | no | When several keywords match, the highest wins; ties go to the longest keyword |
| `IsStandard` | no | `TRUE` for fixed monthly costs (rent, internet...). Without this column, `standard_expenses` in `config.yml` is used |

Each transaction is matched to at most one keyword, so nothing is counted twice.

### config.yml

See `data-sample/config.yml` for all options: app title, currency, CSV
encoding, fixed-cost companies, one-off `overrides` (e.g. "this 1490.55 is
Lufthansa") and `trackers` (gauges for money being paid back).

## Project layout

```
app.R                  page layout and tabs
R/config.R             finds the data folder, reads config.yml
R/load_data.R          reads and cleans the bank CSVs, removes duplicates
R/categorise.R         keyword matching and overrides
R/metrics.R            the calculations behind the charts
R/plots.R              chart helpers
R/mod_*.R              one Shiny module per tab
data-sample/           fake statements, keyword mapping and config
scripts/make_sample_data.R   regenerates data-sample/
tests/testthat/        tests, run on the sample data
legacy/                the original app, kept until the new one is checked
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

They also run automatically on GitHub for every push and pull request, using
only the sample data.

## Contributing

- Work on a branch and open a pull request.
- Never commit real statements or a real `config.yml`. `.gitignore` blocks
  them, but check `git status` before committing.
- If you need a new data situation, add it to `scripts/make_sample_data.R`,
  rerun it, and add a test.
