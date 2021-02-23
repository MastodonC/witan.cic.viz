library(dplyr)
library(lubridate)
library(tidyquant)
library(extrafont)
# loadfonts() # <- possibly not needed
### `font_import()` should be run initially to ensure required fonts are present

month_start <- function(month) {
  as.Date(paste0(month, "-01"))
}

month_end <- function(month) {
  as.Date(paste0(month, "-01")) + months(1) - days(1)
}

date_between <- function(start, end) {
  out <- numeric(length = length(start))
  for(i in seq_along(start)) {
    out[i] <- sample(seq(min(start[i], end[i]),
                         max(start[i], end[i]),
                         by = "day"), 1)
  }
  as.Date(out)
}

impute_birthday <- function(birth_month, min_start, max_cease) {
  earliest_possible <- max(max_cease - days(floor(18 * 365.25)) + 1, month_start(birth_month))
  latest_possible <- min(min_start, month_end(birth_month))
  date_between(earliest_possible, latest_possible)
}

year_diff <- function(start, stop) {
  as.numeric(difftime(stop, start, units = "days")) %/% 365.25
}

report_1 <- function(actual_episodes_file) {
  set.seed(5)
  actual_episodes <- read.csv(actual_episodes_file, header = TRUE, 
                              stringsAsFactors = FALSE, na.strings ="NA")
  actual_episodes$report_date <- ymd(actual_episodes$report_date)
  actual_episodes$ceased <- ymd(actual_episodes$ceased)
  end_date <- max(c(actual_episodes$ceased, actual_episodes$report_date), na.rm = TRUE)
  birthdays <- actual_episodes %>% 
    group_by(ID) %>% 
    summarise(birthday = impute_birthday(DOB[1], min(report_date), coalesce(max(ceased), end_date)))
  actual_episodes <- actual_episodes %>% inner_join(birthdays)
  actual_episodes <- actual_episodes %>% 
    group_by(phase_id) %>% 
    mutate(admission_age = year_diff(min(birthday), min(report_date))) %>% 
    ungroup
}

# actual_episodes_file <- '~/code/witan.cic/data/episodes.scrubbed.csv'
# report_1('~/code/witan.cic/data/episodes.scrubbed.csv')