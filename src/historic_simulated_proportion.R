library(dplyr)
library(ggplot2)

# Projection date is the date from which we start tracking simulated joiners
historic_simulated_proportion <- function(input_dir, output_dir, historic_start, historic_end, projection_start, projection_end, max_y) {
  projection_episodes <- file.path(input_dir, "projection-episodes.csv")
  projected_periods <- read.csv(projection_episodes) %>%
    filter(Episode == 1) %>%
    dplyr::mutate(period_start = ymd(Period.Start), period_end = ymd(Period.End), birthday = ymd(Birthday), provenance = Provenance, simulation = Simulation) %>%
    dplyr::select(ID, simulation, period_start, period_end, birthday, provenance)
  
  n_cic <- data.frame(date = seq(historic_start, projection_end, by = "month")) %>%
    inner_join(projected_periods, by = character()) %>%
    filter(period_start <= date & period_end >= date) %>%
    mutate(provenance = if_else(provenance == "S", "Simulated joiners", "Historic joiners")) %>%
    group_by(simulation, date, provenance) %>%
    summarise(n_cic = n()) %>%
    mutate(provenance = factor(provenance, levels = c("Simulated joiners", "Historic joiners")))
  
  ggplot() +
    geom_area(data = n_cic %>% group_by(date, provenance) %>%
                summarise(median = median(n_cic)),
              aes(date, median, fill = provenance),
              position = "stack") +
    geom_ribbon(data = n_cic %>%
                  filter(provenance == "Historic joiners") %>%
                  group_by(date) %>%
                  summarise(lower_95 = quantile(n_cic, 0.0275),
                            upper_95 = quantile(n_cic, 0.975)),
                aes(x = date, ymin = lower_95, ymax = upper_95), fill = "black", alpha = 0.2) +
    geom_ribbon(data = n_cic %>%
                  filter(provenance == "Historic joiners") %>%
                  group_by(date) %>%
                  summarise(lower_95 = quantile(n_cic, 0.25),
                            upper_95 = quantile(n_cic, 0.75)),
                aes(x = date, ymin = lower_95, ymax = upper_95), fill = "black", alpha = 0.2) +
    geom_ribbon(data = n_cic %>%
                  group_by(date, simulation) %>%
                  summarise(n_cic = sum(n_cic)) %>%
                  summarise(lower_95 = quantile(n_cic, 0.0275),
                            upper_95 = quantile(n_cic, 0.975)),
                aes(x = date, ymin = lower_95, ymax = upper_95), fill = "black", alpha = 0.2) +
    geom_ribbon(data = n_cic %>%
                  group_by(date, simulation) %>%
                  summarise(n_cic = sum(n_cic)) %>%
                  summarise(lower_95 = quantile(n_cic, 0.25),
                            upper_95 = quantile(n_cic, 0.75)),
                aes(x = date, ymin = lower_95, ymax = upper_95), fill = "black", alpha = 0.2) +
    geom_line(data = n_cic %>%
                group_by(date, simulation) %>%
                filter(date >= projection_start) %>%
                summarise(n_cic = sum(n_cic)) %>%
                summarise(y = median(n_cic)),
              aes(x = date, y = y), colour = "black", linetype = 2, alpha = 0.5) +
    scale_fill_manual(values = tableau_color_pal("Tableau 20")(4)[c(3,1)]) +
    labs(y = "CiC", x = "Date", fill = "Joiners") +
    ylim(c(0, max_y))
  ggsave(file.path(output_dir, "historic-simulated-proportion.png"), width = 8, height = 5)
  
  n_cic %>%
    group_by(date, provenance) %>%
    summarise(lower_95 = quantile(n_cic, 0.0275),
              lower_50 = quantile(n_cic, 0.25),
              median = median(n_cic),
              upper_50 = quantile(n_cic, 0.75),
              upper_95 = quantile(n_cic, 0.975)) %>%
    write.csv(file = file.path(output_dir, "historic-simulated-proportion.csv"), row.names = FALSE)
}

# historic_simulated_proportion("/Users/henry/Mastodon C/witan.cic/data/scc/2021-04-08/trended", "/Users/henry/Mastodon C/witan.cic/data/scc/2021-04-08/trended",
#                               as.Date("2015-01-01"), as.Date("2020-03-31"), as.Date("2019-03-31"), as.Date("2024-03-31"), 1300)
