library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggthemes)
library(tidyquant)
source("src/helpers.R")

placement_timeline_plots <- function(input_dir, output_dir, projection_start) {
  
  historic_episodes_file <- "historic-episodes.csv"
  projection_episodes_file <- "projection-episodes.csv"
  timeline_plot_pdf <- "timeline-plot.pdf"
  
  provenance_labels <- c("Historically closed cases", "Open cases")
  names(provenance_labels) <- c("H", "P")

  projected_episodes <- read.csv(file.path(input_dir, projection_episodes_file)) %>%
    filter(Provenance != "S" & Simulation == 1) %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End),
                  birthday = ymd(Birthday), start = ymd(Start), end = ymd(End),
                  provenance = Provenance, placement = Placement, simulation = Simulation, episode = Episode) %>%
    filter(start < projection_start) %>%
    mutate(provenance = if_else(period_end > projection_start, "P", "H"),
           end = pmin(end, projection_start), period_end = pmin(end, projection_start)) %>%
    dplyr::select(period_id, simulation, episode, period_start, period_end, start, end, birthday, provenance, placement)
  
  weekly_placements <- projected_episodes %>%
    dplyr::inner_join(data.frame(week = floor_date(seq(min(projected_episodes$start), max(projected_episodes$end, na.rm = TRUE), by = "week"), unit = "week")), by = character()) %>%
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
                panel.background = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_line(colour = "#DDDDDD")) +
    labs(fill = "Placement")
  
  order_by_join_age <- function(df) {
    levels <- df %>% group_by(period_id) %>%
      dplyr::slice(1) %>%
      mutate(join_age = day_diff(birthday, period_start)) %>%
      arrange(join_age) %>%
      pull(period_id)
    df$period_id <- factor(df$period_id, levels = levels)
    df
  }
  pdf(file = file.path(output_dir, timeline_plot_pdf))
  for (p in placements) {
    print(weekly_placements %>%
      filter(period_id %in% (weekly_placements %>% filter(placement == p) %>% pull(period_id))) %>%
      mutate(age = day_diff(birthday, week) %/% 7) %>%
      order_by_join_age() %>%
      ggplot(aes(age, period_id, fill = placement)) +
      geom_tile(height = 0.9) +
      scale_fill_manual(values = placement_colours) +
      labs(fill = "Placement", x = "Age", title = paste("Pathways passing through", p)) +
      facet_grid(rows = vars(provenance), labeller = labeller(provenance = provenance_labels), switch = "y", scales = "free_y") +
      scale_x_continuous(breaks = seq(0,936, by = 52), labels = 0:18, expand = c(0.01,0)) +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(colour = "#DDDDDD"),
            strip.placement = "outside"))
  }
  dev.off()
}

# placement_timeline_plots(input_dir, output_dir, as.Date("2020-03-31"))

