library(tidyverse)
library(lubridate)
library(tidyquant)
library(extrafont)
library(beepr)
library(ggthemes)
source("src/process_counts.R")
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
  plot.caption = element_text(hjust = 0.5),
  axis.title = element_text(
    hjust = 0.5, size = 16),
  axis.text = element_text(
    hjust = 0.5, size = 10),
  axis.text.x = element_text(angle = -45),
  axis.title.x = element_text(margin = margin(15,0,0,0)),
  axis.title.y = element_text(margin = margin(0,10,0,0)),
  plot.margin = margin(10,20,10,10),
  panel.grid = element_line(color = "#eeeeee"))

project_from <- as.Date("2020-03-31")
project_yrs <- 4
train_from <- project_from - years(3)
projection_end <- project_from + years(project_yrs)

report_1 <- function(actual_episodes_file, projected_episodes_file = NULL, counts_file = NULL) {
  
  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 10, "Report 1", cex=2.5)
  text(5, 8, "2021-XX-XX", cex=2.5) ## TODO take date from config
  text(5, 6, "\"LA\" CiC Projection", cex=2.5) ## TODO take LA name from config
  
  set.seed(5)
  colours <- c("gray", "#778899", "gray", "black", "blue")
  names(colours) <- c("Confidence Interval", "Interquartile Range", "Projected", "SSDA903", "MIS")
  dates <- seq(as.Date("2016-01-01"), projection_end, by = "week") ## TODO take dates from config file
  tableau_color_pal("Tableau 20")(20)
  
  actual_episodes <- read.csv(actual_episodes_file, header = TRUE, 
                              stringsAsFactors = FALSE, na.strings ="NA") %>% 
    mutate(report_date = ymd(report_date),
           ceased = ymd(ceased))
  end_date <- max(c(actual_episodes$ceased, actual_episodes$report_date), na.rm = TRUE)
  birthdays <- actual_episodes %>% 
    group_by(ID) %>% 
    summarise(birthday = impute_birthday(DOB[1], min(report_date), coalesce(max(ceased), end_date)))
  actual_episodes <- actual_episodes %>% 
    inner_join(birthdays) %>% 
    group_by(phase_id) %>% 
    mutate(admission_age = year_diff(min(birthday), min(report_date))) %>% 
    ungroup

  if(is.null(projected_episodes_file)) {
    projected_episodes <- NULL
  } else {
    projected_episodes <- read.csv(projected_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="") %>%
      mutate(Start = ymd(Start),
             End = ymd(End),
             Birthday = ymd(Birthday))
  }
  
  ### Total in CiC
  projected_totals <- data.frame(date = c(), lower.ci = c(), q1 = c(), median = c(), q3 = c(), upper.ci = c())
  if(!is.null(projected_episodes_file)) {
    for (date in dates) {
      counts_by_simulation <- projected_episodes %>%
        filter(Start <= date & (is.na(End) | End >= date)) %>%
        group_by(Simulation) %>%
        summarise(n = n())
      quants <- quantile(counts_by_simulation$n, probs = c(0.05, 0.25, 0.5, 0.75, 0.975))
      projected_totals <- rbind(projected_totals, data.frame(date = c(as.Date(date)), lower.ci = c(quants[1]), 
                                                             q1 = c(quants[2]), median = c(quants[3]), 
                                                             q3 = c(quants[4]), upper.ci = c(quants[5]),
                                                             variable = c("Projected")))
    }
    projected_totals <- projected_totals %>%
      mutate(date = ymd(date)) %>% 
      filter(lower.ci != upper.ci)
  }
  
  actual_totals <- data.frame(date = c(), variable = c(), value = c())
  for (date in dates[dates < end_date]) {
    counts <- actual_episodes %>%
      filter(report_date <= date & (is.na(ceased) | ceased > date)) %>%
      summarise(n = n())
    actual_totals <- rbind(actual_totals, data.frame(date = c(as.Date(date)), variable = c("SSDA903"), value = c(counts[[1]])))
  }

  if(!is.null(counts_file)) {
    counts <- process_counts(counts_file)
    actual_totals <- bind_rows(actual_totals, counts %>% 
                                 select(date, Total) %>% 
                                 mutate(variable = "MIS") %>% 
                                 rename(value = Total) %>% 
                                 mutate(value = as.numeric(as.character(value))))
  } 
  
  print(ggplot() +
          {if(!is.null(projected_episodes_file)) 
            list(
              geom_ribbon(data = projected_totals %>% mutate(variable = "Interquartile Range"), 
                          aes(x = date, ymin = q1, ymax = q3, colour = variable), linetype = 0, alpha = 0.4, show.legend = FALSE),
              geom_ribbon(data = projected_totals %>% mutate(variable = "Confidence Interval"), 
                          aes(x = date, ymin = lower.ci, ymax = upper.ci, colour = variable), linetype = 0, alpha = 0.2, show.legend = FALSE),
              geom_line(data = projected_totals, 
                        aes(x = date, y = median, colour = variable), 
                        linetype = 2))} +
          geom_line(data = actual_totals, aes(x = date, y = value, colour = variable)) +
          geom_vline(xintercept = train_from, color = "black", linetype = 3, alpha = 0.5) +
          geom_vline(xintercept = project_from, color = "black", linetype = 3, alpha = 0.5) +
          theme_mastodon +
          scale_color_manual(values = colours) +
          labs(title = "Total Children in Care", x = "Date", y = "No. children in care", 
               colour = "Dataset", caption = "This is some caption text")
        )

  beep()
  
}


### Run section ###
actual_episodes_file <- "/Users/Seb/code/witan.cic/data/scc/2021-03-11/suffolk-scrubbed-episodes-20210219.csv"
projected_episodes_file <- "/Users/Seb/code/witan.cic/data/scc/2021-03-11/scc-episodes-2019-03-31-rewind-1yr-train-3yr-project-5yr-runs-100-seed-42-marginal-age-out.csv"
counts_file <- '~/Downloads/2021-02-12 CiC Data for Chris Feb 20 sent.xlsx'
output_file <- "report_1.pdf"
pdf(output_file, paper = "a4r")
report_1(actual_episodes_file, projected_episodes_file, counts_file)
dev.off()
embed_fonts(file = output_file, outfile = output_file)
# -5yrs, +3yrs from projection date
# "joiner rate training period" added to legeend