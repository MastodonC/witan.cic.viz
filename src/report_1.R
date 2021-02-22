
loadfonts()
### `font_import()` should be run initially to ensure required fonts are present

impute_birthday <- function(birth_month, min_start, max_cease) {
  earliest_possible <- max(max_cease - days(floor(18 * 365.25)) + 1, month_start(birth_month))
  latest_possible <- min(min_start, month_end(birth_month))
  date_between(earliest_possible, latest_possible)
}

report_1 <- function(actual_episodes_file) {
  set.seed(5)
  actual_episodes <- read.csv(actual_episodes_file, header = TRUE, 
                              stringsAsFactors = FALSE, na.strings ="NA")
  episodes$report_date <- ymd(episodes$report_date)
  episodes$ceased <- ymd(episodes$ceased)
}