
loadfonts()
### `font_import()` should be run initially to ensure required fonts are present

report_1 <- function(actual_episodes_file) {
  set.seed(5)
  actual_episodes <- read.csv(actual_episodes_file, header = TRUE, 
                              stringsAsFactors = FALSE, na.strings ="NA")
  episodes$report_date <- ymd(episodes$report_date)
  episodes$ceased <- ymd(episodes$ceased)
}