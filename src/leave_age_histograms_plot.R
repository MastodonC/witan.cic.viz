library(dplyr)
library(ggplot2)
library(lubridate)
source("src/helpers.R")

# Provenance filters will default to just historic episodes unless specified
generate_leave_age_histograms_plot <- function(input_dir, output_dir, provenance_1 = "H",
                                               provenance_2 = "H") {
  projection_episodes <- file.path(input_dir, "projection-episodes.csv")
  projected_periods <- read.csv(projection_episodes) %>%
    filter(Episode == 1 & Provenance == provenance_1 | Episode == 1 & Provenance == provenance_2) %>%
    dplyr::mutate(period_start = ymd(Period.Start), period_end = ymd(Period.End), birthday = ymd(Birthday), provenance = Provenance, simulation = Simulation, duration = Period.Duration) %>%
    dplyr::select(ID, simulation, period_start, period_end, birthday, provenance, duration) %>%
    mutate(join_age_days = day_diff(birthday, period_start), leave_age_days = day_diff(birthday, period_end)) %>%
    mutate(join_age_years = join_age_days %/% 365.25, leave_age_years = leave_age_days %/% 365.25)

  join_age_labs <- paste("Join age", 0:17)
  names(join_age_labs) <- 0:17

  ggplot(projected_periods, aes(leave_age_years)) +
    geom_bar() +
    facet_wrap(vars(join_age_years), nrow = 3, scales = "free_y", labeller = labeller(join_age_years = join_age_labs)) +
    geom_vline(data = projected_periods %>% group_by(join_age_years) %>% summarise(median = median(leave_age_days) %/% 365.25),
               aes(xintercept = median), colour = "orange", linetype = 2) +
      theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
      scale_x_continuous(n.breaks = 7) +
      labs(x = "Leave age", y = "Probability distribution") +
    theme_mastodon

  if (provenance_1 == provenance_2) {
    filename <- "historic-leave-age-histograms.png"
  } else {
    filename <- "historic-and-adjusted-leave-age-histograms.png"
  }
  ggsave(file.path(output_dir, filename), width = 8, height = 5)
}

# input_dir <- '/Users/Seb/code/witan.cic.surrey/data/outputs/'
# output_dir <- '/Users/Seb/code/witan.cic.surrey/data/historic-charts/'
# 
# generate_leave_age_histograms_plot(input_dir, output_dir, "H", "P")

