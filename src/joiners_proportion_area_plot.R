library(dplyr)
library(ggplot2)

generate_joiners_proportion_area_plot <- function(input_dir, output_dir, historic_start, historic_end, period_start_date) {
  projection_episodes <- file.path(input_dir, "historic-episodes.csv")
  projected_periods <- read.csv(projection_episodes) %>%
    filter(Episode == 1) %>%
    dplyr::mutate(period_start = ymd(Period.Start), period_end = ymd(Period.End), birthday = ymd(Birthday), provenance = Provenance, simulation = 1) %>%
    dplyr::select(ID, simulation, period_start, period_end, birthday, provenance)
  
  new_joiner_label <- paste("Joined after", period_start_date)
  old_joiner_label <- paste("Joined before", period_start_date)
  
  n_cic <- data.frame(date = seq(historic_start, historic_end, by = "month")) %>%
    dplyr::inner_join(projected_periods, by = character()) %>%
    dplyr::filter(period_start <= date & period_end >= date) %>%
    dplyr::mutate(provenance = if_else(period_start >= period_start_date, new_joiner_label, old_joiner_label)) %>%
    dplyr::group_by(simulation, date, provenance) %>%
    dplyr::summarise(n_cic = n()) %>%
    dplyr::mutate(provenance = factor(provenance, levels = c(new_joiner_label, old_joiner_label)))
  
  ggplot() +
    geom_area(data = n_cic %>% group_by(date, provenance) %>%
                summarise(median = median(n_cic)),
              aes(date, median, fill = provenance),
              position = "stack") +
    scale_fill_manual(values = tableau_color_pal("Tableau 20")(4)[c(3,1)]) +
    labs(y = "CiC count", x = "Date", fill = "CiC count")
  ggsave(file.path(output_dir, "historic-joiner-proportion.png"), width = 8, height = 5)
  
  n_cic %>%
    group_by(date, provenance) %>%
    summarise(lower_95 = quantile(n_cic, 0.0275),
              lower_50 = quantile(n_cic, 0.25),
              median = median(n_cic),
              upper_50 = quantile(n_cic, 0.75),
              upper_95 = quantile(n_cic, 0.975)) %>%
    write.csv(file = file.path(output_dir, "historic-joiner-proportion.csv"), row.names = FALSE)
}

# input_dir <- ''
# output_dir <- ''
# historic_start <- as.Date("2014-03-01")
# historic_end <-  as.Date("2021-03-31")
# period_start_date <- as.Date("2017-03-01")
# 
# generate_joiners_proportion_area_plot(input_dir, output_dir,
#                                       historic_start, historic_end, period_start_date)
