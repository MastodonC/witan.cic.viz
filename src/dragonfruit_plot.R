library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggthemes)
library(tidyquant)
source("src/helpers.R")

generate_dragonfruit_plot <- function(input_dir, output_dir, projection_start) {
  
  historic_episodes_file <- "historic-episodes.csv"
  projection_episodes_file <- "projection-episodes.csv"
  dragonfruit_pdf <- "dragonfruit.pdf"
  
  projected_periods <- read.csv(file.path(input_dir, projection_episodes_file)) %>%
    filter(Episode == 1 & Provenance == "P") %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End), birthday = ymd(Birthday), provenance = Provenance, simulation = Simulation) %>%
    dplyr::select(period_id, simulation, period_start, period_end, birthday, provenance) %>%
    dplyr::mutate(projection_start_age = day_diff(birthday, projection_start) / 365.25,
                  projection_start_duration = day_diff(period_start, projection_start) / 365.25,
                  period_end_age = day_diff(birthday, period_end) / 365.25,
                  period_end_duration = day_diff(period_start, period_end) / 365.25)
  
  summary_plot <- function(data) {
    chart_data <- data %>%
      group_by(period_id) %>%
      dplyr::summarise(median = median(end),
                       l95 = quantile(end, probs = 0.025),
                       u95 = quantile(end, probs = 0.975),
                       l50 = quantile(end, probs = 0.25),
                       u50 = quantile(end, probs = 0.75),
                       min = min(end),
                       max = max(end),
                       start = min(start)) %>%
      arrange(start) %>%
      mutate(index = as.numeric(factor(period_id, levels = period_id)))
    
    chart_data %>%
      ggplot() +
      geom_point(aes(index, median)) +
      geom_point(aes(index, start), colour = "orange") +
      geom_linerange(aes(index, ymin = min, ymax = max), alpha = 0.1) +
      geom_linerange(aes(index, ymin = l95, ymax = u95), alpha = 0.2) +
      geom_linerange(aes(index, ymin = l50, ymax = u50), alpha = 0.5)
  }
  pdf(file = file.path(output_dir, dragonfruit_pdf), width=11, height=8.5)
  print(summary_plot(projected_periods %>%
                     mutate(end = period_end_duration, start = projection_start_duration)) +
          scale_y_continuous(breaks = 0:18) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) +
          labs(title = "Projected closed durations"))
  print(summary_plot(projected_periods %>%
                       mutate(end = period_end_age, start = projection_start_age)) +
          scale_y_continuous(breaks = 0:18) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) +
          labs(title = "Projected closed ages"))
  dev.off()
  
}

# input_dir <- '/Users/henry/Mastodon C/witan.cic/data/scc/2021-06-10/04-untrended-1000'
# output_dir <- '/Users/henry/Mastodon C/witan.cic/data/scc/2021-06-10/04-untrended-1000'
# generate_dragonfruit_plot(input_dir, output_dir, as.Date("2019-03-31"))

