library(dplyr)
library(lubridate)
library(tidyquant)
library(extrafont)
library(beepr)
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

theme_mastodon <- theme(plot.title = element_text(
  hjust = 0.5, size = 20,
  margin = margin(0,0,15,0)),
  axis.title = element_text(
    hjust = 0.5, size = 16),
  axis.text = element_text(
    hjust = 0.5, size = 10),
  axis.text.x = element_text(angle = -45),
  axis.title.x = element_text(margin = margin(15,0,0,0)),
  axis.title.y = element_text(margin = margin(0,10,0,0)),
  plot.margin = margin(10,20,10,10),
  panel.grid = element_line(color = "#eeeeee"))

report_1 <- function(actual_episodes_file, projected_episodes_file) {
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
  
  projected_episodes <- read.csv(projected_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="")
  projected_episodes$Start <- ymd(projected_episodes$Start)
  projected_episodes$End <- ymd(projected_episodes$End)
  projected_episodes$Birthday <- ymd(projected_episodes$Birthday)
  
  colours <- c("#4E79A7", "#F28E2B", "grey", "#F28E2B", "#4E79A7", "black")
  names(colours) <- c("lower.ci", "q1", "median", "q3", "upper.ci", "actual")
  
  dates <- seq(as.Date("2016-01-01"), as.Date("2020-02-01"), by = "week") ## TODO make intelligent or as args
  
  projected <- data.frame(date = c(), lower.ci = c(), q1 = c(), median = c(), q3 = c(), upper.ci = c())
  for (date in dates) {
    counts_by_simulation <- projected_episodes %>%
      filter(Start <= date & (is.na(End) | End >= date)) %>%
      group_by(Simulation) %>%
      summarise(n = n())
    quants <- quantile(counts_by_simulation$n, probs = c(0.05, 0.25, 0.5, 0.75, 0.975))
    projected <- rbind(projected, data.frame(date = c(date), lower.ci = c(quants[1]), q1 = c(quants[2]), median = c(quants[3]), q3 = c(quants[4]), upper.ci = c(quants[5])))
  }
  projected$date <- as.Date(projected$date)
  projected <- projected %>% filter(lower.ci != upper.ci)
  actuals <- data.frame(date = c(), variable = c(), value = c())
  for (date in dates) {
    counts <- actual_episodes %>%
      filter(report_date <= date & (is.na(ceased) | ceased > date)) %>%
      summarise(n = n())
    actuals <- rbind(actuals, data.frame(date = c(date), variable = c("actual"), value = c(counts[[1]])))
  }
  actuals$date <- as.Date(actuals$date)
  print(ggplot() +
          geom_line(data = actuals, aes(x = date, y = value)) +
          geom_line(data = projected, aes(x = date, y = median), linetype = 2) +
          geom_ribbon(data = projected, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
          geom_ribbon(data = projected, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
          theme_mastodon +
          scale_color_manual(values = colours) +
          labs(title = "CiC", x = "Date", y = "CiC"))
  beep()
}

pdf(output_file, paper = "a4r")
report_1(actual_episodes_file, projected_episodes_file)
dev.off()
embed_fonts(file = output_file, outfile = output_file)

# actual_episodes_file <- '~/code/witan.cic/data/episodes.scrubbed.csv'
# projected_episodes_file <- 'scc-episodes-2019-08-13-rewind-1yr-train-3yr-project-5yr-runs-100-seed-42-universe.csv'
# output_file <- "out.pdf"
# report_1('~/code/witan.cic/data/episodes.scrubbed.csv')