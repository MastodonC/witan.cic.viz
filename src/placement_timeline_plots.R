library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggthemes)
library(tidyquant)
source("src/helpers.R")

placement_timeline_plots <- function(input_dir, output_dir) {
  
  historic_episodes_file <- "historic-episodes.csv"
  projection_episodes_file <- "projection-episodes.csv"
  
  projected_episodes <- read.csv(file.path(input_dir, projection_episodes_file)) %>%
    filter(Provenance != "S" & Simulation == 1) %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End),
                  birthday = ymd(Birthday), start = ymd(Start), end = ymd(End),
                  provenance = Provenance, placement = Placement, simulation = Simulation, episode = Episode) %>%
    dplyr::select(period_id, simulation, episode, period_start, period_end, start, end, birthday, provenance, placement)
  
  weekly_placements <- projected_episodes %>%
    dplyr::inner_join(data.frame(week = floor_date(seq(historic_start, historic_end, by = "week"), unit = "week")), by = character()) %>%
    dplyr::filter(start <= week & (end >= week | is.na(end))) %>%
    dplyr::group_by(week, simulation, period_id) %>%
    dplyr::slice(1) %>%
    as.data.frame
  
  placements <- c("A3", "A4", "A5", "A6", "H5", "K1", "K2", "M2", "M3", "P1", "P2",
                      "Q1","Q2", "R1", "R2", "R3", "R5", "S1", "T0", "T4", "Z1")
  placement_colours <- tableau_color_pal("Tableau 20")(20)
  placement_colours <- c(placement_colours, "#888888", "#FFFFFF")
  names(placement_colours) <- placements
  
  weekly_placements %>%
    filter(period_id %in% (weekly_placements %>% filter(placement == "K2") %>% pull(period_id))) %>%
    ggplot(aes(week, period_id, fill = placement)) +
    geom_tile(height = 0.9) +
    scale_fill_manual(values = placement_colours) +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank()) +
    labs(fill = "Placement")
}