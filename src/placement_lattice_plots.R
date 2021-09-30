library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggthemes)
library(tidyquant)
source("src/helpers.R")

generate_placement_lattice_plots <- function(input_dir, output_dir, historic_start, historic_end, projection_start, projection_end) {
  
  historic_episodes_file <- "historic-episodes.csv"
  projection_episodes_file <- "projection-episodes.csv"
  lattice_by_age_csv <- "lattice-by-placement.csv"
  lattice_by_age_simulation_csv <- "lattice-by-placement-simulation.csv"
  lattice_top_line_csv <- "placement-lattice-top-line.csv"
  lattice_pdf <- "placement-lattice-plots.pdf"
  joiner_rates_pdf <- 'placement-joiner-rates.pdf'
  joiner_rates_csv <- 'placement-joiner-rates.csv'
  
  label_levels <- c("joiners", "movedin", "net", "movedout", "leavers", "cic")
  
  bootstrapped_actuals <- read.csv(file.path(input_dir, historic_episodes_file)) %>%
    filter(Provenance != "S") %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End),
                  birthday = ymd(Birthday), start = ymd(Start), end = ymd(End),
                  provenance = Provenance, placement = Placement, simulation = 1, episode = Episode) %>%
    dplyr::select(period_id, simulation, episode, period_start, period_end, start, end, birthday, provenance, placement)
  
  placements <- bootstrapped_actuals %>%
    group_by(placement) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    pull(placement)
  
  migrations <- bootstrapped_actuals %>%
    inner_join(bootstrapped_actuals, by = c("period_id", "simulation")) %>%
    filter(episode.x + 1 == episode.y) %>%
    mutate(placement_before = placement.x, placement_after = placement.y, date = end.x) %>%
    filter(placement_before != placement_after) %>%
    select(period_id, simulation, date, placement_before, placement_after)
  
  grouped_ledger <- rbind(
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), placement = placement[1], .groups = "drop") %>%
      dplyr::select(date, simulation, placement) %>%
      dplyr::mutate(metric = "joiners"),
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), placement = tail(placement, n = 1), .groups = "drop") %>%
      dplyr::select(date, simulation, placement) %>%
      dplyr::mutate(metric = "leavers"),
    migrations %>%
      dplyr::rename(placement = placement_after) %>%
      dplyr::select(date, simulation, placement) %>%
      dplyr::mutate(metric = "movedin"),
    migrations %>%
      dplyr::rename(placement = placement_before) %>%
      dplyr::select(date, simulation, placement) %>%
      dplyr::mutate(metric = "movedout")
  ) %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month, placement, metric, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    ungroup %>%
    complete(month, placement, metric, simulation, fill = list(n = 0)) %>%
    as.data.frame() %>%
    rbind(
      bootstrapped_actuals %>%
        dplyr::inner_join(data.frame(month = floor_date(seq(historic_start, historic_end, by = "month"), unit = "month")), by = character()) %>%
        dplyr::filter(start <= month & (end >= month | is.na(end))) %>%
        dplyr::group_by(month, placement, simulation) %>%
        dplyr::summarise(n = n_distinct(period_id)) %>%
        ungroup %>%
        complete(month, placement, simulation, fill = list(n = 0)) %>%
        dplyr::mutate(metric = "cic") %>%
        dplyr::select(month, placement, metric, simulation, n) %>%
        as.data.frame()
    ) %>%
    filter(month >= historic_start & month <= historic_end)

  grouped_ledger <- rbind(grouped_ledger,
                          dcast(simulation + month + placement ~ metric, value.var = "n", data = grouped_ledger, fill = 0) %>%
                            mutate(net = joiners + movedin - movedout - leavers) %>%
                            dplyr::select(simulation, month, placement, net) %>%
                            rename(n = net) %>%
                            mutate(metric = "net")) %>%
    mutate(source = "SSDA903")
  
  grouped_ledger$metric <- factor(grouped_ledger$metric, levels = label_levels)
  
  ## Simulated episodes
  
  simulated_episodes <- read.csv(file.path(input_dir, projection_episodes_file)) %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End),
                  birthday = ymd(Birthday), start = ymd(Start), end = ymd(End),
                  provenance = Provenance, placement = Placement, simulation = Simulation, episode = Episode) %>%
    dplyr::select(period_id, simulation, episode, period_start, period_end, start, end, birthday, provenance, placement) %>%
    mutate(simulation = factor(simulation)) # Required to fill blanks
  
  simulated_migrations <- simulated_episodes %>%
    inner_join(simulated_episodes, by = c("period_id", "simulation")) %>%
    filter(episode.x + 1 == episode.y) %>%
    mutate(placement_before = placement.x, placement_after = placement.y, date = end.x) %>%
    filter(placement_before != placement_after) %>%
    select(period_id, simulation, date, placement_before, placement_after)
  
  simulated_grouped_ledger <- rbind(
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), placement = placement[1], .groups = "drop") %>%
      dplyr::select(date, simulation, placement) %>%
      dplyr::mutate(metric = "joiners"),
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), placement = tail(placement, n = 1), .groups = "drop") %>%
      dplyr::select(date, simulation, placement) %>%
      dplyr::mutate(metric = "leavers"),
    simulated_migrations %>%
      dplyr::rename(placement = placement_after) %>%
      dplyr::select(date, simulation, placement) %>%
      dplyr::mutate(metric = "movedin"),
    simulated_migrations %>%
      dplyr::rename(placement = placement_before) %>%
      dplyr::select(date, simulation, placement) %>%
      dplyr::mutate(metric = "movedout")
  ) %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month, placement, metric, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    ungroup %>%
    complete(month, placement, metric, simulation, fill = list(n = 0)) %>%
    as.data.frame() %>%
    rbind(
      simulated_episodes %>%
        dplyr::inner_join(data.frame(month = floor_date(seq(projection_start, projection_end, by = "month"), unit = "month")), by = character()) %>%
        dplyr::filter(start <= month & (end >= month | is.na(end))) %>%
        dplyr::group_by(month, placement, simulation) %>%
        dplyr::summarise(n = n_distinct(period_id)) %>%
        ungroup %>%
        complete(month, placement, simulation, fill = list(n = 0)) %>%
        dplyr::mutate(metric = "cic") %>%
        dplyr::select(month, placement, metric, simulation, n) %>%
        as.data.frame()
    ) %>%
    filter(month > projection_start & month <= projection_end)
  
  simulated_grouped_ledger <- rbind(simulated_grouped_ledger,
                                    dcast(simulation + month + placement ~ metric, value.var = "n", data = simulated_grouped_ledger, fill = 0) %>%
                                      mutate(net = joiners + movedin - movedout - leavers ) %>%
                                      dplyr::select(simulation, month, placement, net) %>%
                                      rename(n = net) %>%
                                      mutate(metric = "net")) %>%
    mutate(source = "projection")
  
  simulated_grouped_ledger$metric <- factor(simulated_grouped_ledger$metric, levels = label_levels)

  simulated_ci <- simulated_grouped_ledger %>%
    group_by(month, placement, metric) %>%
    summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n),
              upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))

  state_labels <- c(
    'joiners' = 'Joiners',
    'movedin' = 'Moved in',
    'net' = 'Net',
    'movedout' = 'Moved out',
    'leavers' = 'Leavers',
    'cic' = 'CiC'
  )
  
  rbind(simulated_grouped_ledger %>%
          dcast(month + placement + metric + source ~ simulation, value.var = "n", fill = 0),
        grouped_ledger %>%
          select(-simulation) %>%
          inner_join(data.frame(simulation = 1:100), by = character()) %>%
          dcast(month + placement + metric + source ~ simulation, value.var = "n", fill = 0)) %>%
    write.csv(file = file.path(output_dir, lattice_by_age_simulation_csv), row.names = FALSE)
  
  pdf(file = file.path(output_dir, joiner_rates_pdf))
  
  print(simulated_grouped_ledger %>%
          filter(metric == "joiners") %>%
          dcast(month + placement ~ simulation, value.var = "n", fill = 0) %>%
          melt(id.vars = c("month", "placement"), variable.name = "simulation", value.name = "n") %>%
          dplyr::select(placement, simulation, n) %>%
          group_by(placement)%>%
          mutate(age_mean = mean(n)) %>%
          group_by(placement, age_mean, n) %>%
          summarise(count = n())%>%
          ungroup %>%
          ggplot(aes(n, count))+
          geom_bar(stat = "identity") +
          geom_vline(aes(xintercept = age_mean), colour = "orange", linetype = 2) +
          facet_wrap(vars(placement), scales = "free"))
  
  dev.off()
  
  simulated_grouped_ledger %>%
    filter(metric == "joiners") %>%
    dcast(month + placement ~ simulation, value.var = "n", fill = 0) %>%
    melt(id.vars = c("month", "placement"), variable.name = "simulation", value.name = "n") %>%
    dplyr::select(placement, simulation, n) %>%
    group_by(placement) %>%
    summarise(group_mean = mean(n)) %>%
    write.csv(file = file.path(output_dir, joiner_rates_csv), row.names = FALSE)
  
  rbind(simulated_grouped_ledger %>%
          group_by(month, placement, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
        grouped_ledger %>%
          group_by(month, placement, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))) %>%
    write.csv(file = file.path(output_dir, lattice_by_age_csv), row.names = FALSE)
  
  pdf(file = file.path(output_dir, lattice_pdf))
  
  colours <- tableau_color_pal("Tableau 20")(12)
  colours[11] <- "#000000"
  colours[12] <- "#000000"
  names(colours) <- c("joiners", "joiners simulated",
                      "movedin", "agedin simulated",
                      "net", "net simulated",
                      "movedout", "agedout simulated",
                      "leavers", "leavers simulated",
                      "cic", "cic simulated")
  
  categories <- placements
  category_labels <- paste(categories, "count of children")
  names(category_labels) <- categories

  print(ggplot() +
          geom_ribbon(data = simulated_ci %>% filter(metric == "cic" & placement %in% categories),
                      aes(month, ymin = lower_95, ymax = upper_95, fill = metric), alpha = 0.2) +
          geom_ribbon(data = simulated_ci %>% filter(metric == "cic" & placement %in% categories),
                      aes(month, ymin = lower_50, ymax = upper_50, fill = metric), alpha = 0.2) +
          geom_line(data = simulated_ci %>% filter(metric == "cic" & placement %in% categories),
                    aes(month, median, colour = metric), linetype = 3) +
          geom_line(data = grouped_ledger %>% filter(metric == "cic" & placement %in% categories),
                    aes(month, n, group = simulation, colour = metric),
                    stat = "identity", alpha = 1) +
          facet_wrap(vars(placement), scales = "free_y",
                     ncol = 2) +
          scale_colour_manual(values = colours) +
          scale_fill_manual(values = colours) +
          theme(legend.position = "none") +
          labs(x = "Date", y = "Children"))

  # print(ggplot() +
  #         geom_ribbon(data = simulated_ci %>% filter(metric == "cic" & placement %in% placement_sequence_2),
  #                     aes(month, ymin = lower_95, ymax = upper_95, fill = metric), alpha = 0.2) +
  #         geom_ribbon(data = simulated_ci %>% filter(metric == "cic" & placement %in% placement_sequence_2),
  #                     aes(month, ymin = lower_50, ymax = upper_50, fill = metric), alpha = 0.2) +
  #         geom_line(data = simulated_ci %>% filter(metric == "cic" & placement %in% placement_sequence_2),
  #                   aes(month, median, colour = metric), linetype = 3) +
  #         geom_line(data = grouped_ledger %>% filter(metric == "cic" & placement %in% placement_sequence_2),
  #                   aes(month, n, group = simulation, colour = metric),
  #                   stat = "identity", alpha = 1) +
  #         facet_wrap(vars(placement), scales = "free_y",
  #                    ncol = 2) +
  #         scale_colour_manual(values = colours) +
  #         scale_fill_manual(values = colours) +
  #         theme(legend.position = "none") +
  #         labs(x = "Date", y = "Children"))

  for (category in categories) {
    print(ggplot() +
            geom_ribbon(data = simulated_ci %>% filter(placement == category),
                        aes(month, ymin = lower_95, ymax = upper_95, fill = metric), alpha = 0.2) +
            geom_ribbon(data = simulated_ci %>% filter(placement == category),
                        aes(month, ymin = lower_50, ymax = upper_50, fill = metric), alpha = 0.2) +
            geom_line(data = simulated_ci %>% filter(placement == category),
                      aes(month, median, colour = metric), linetype = 3) +
            geom_line(data = grouped_ledger %>% filter(placement == category),
                      aes(month, n, group = simulation, colour = metric),
                      stat = "identity", alpha = 1) +
            facet_grid(vars(metric), vars(placement), labeller = labeller(metric = state_labels, placement = category_labels), scales = "free_y") +
            scale_colour_manual(values = colours) +
            scale_fill_manual(values = colours) +
            theme(legend.position = "none") +
            labs(x = "Date", y = "Children"))
  }
  
  ## Same but for all cic
  
  label_levels <- c("joiners", "net", "leavers", "cic")
  
  grouped_ledger <- rbind(
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), birthday = birthday[1], .groups = "drop") %>%
      dplyr::select(date, simulation) %>%
      dplyr::mutate(metric = "joiners"),
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), birthday = birthday[1], .groups = "drop") %>%
      dplyr::select(date, simulation) %>%
      dplyr::mutate(metric = "leavers")
  ) %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month, metric, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    ungroup %>%
    complete(month, metric, simulation, fill = list(n = 0)) %>%
    as.data.frame() %>%
    rbind(
      bootstrapped_actuals %>%
        dplyr::inner_join(data.frame(month = floor_date(seq(historic_start, historic_end, by = "month"), unit = "month")), by = character()) %>%
        dplyr::filter(period_start <= month & period_end >= month) %>%
        dplyr::group_by(month, simulation) %>%
        dplyr::summarise(n = n_distinct(period_id)) %>%
        ungroup %>%
        complete(month, simulation, fill = list(n = 0)) %>%
        dplyr::mutate(metric = "cic") %>%
        dplyr::select(month, metric, simulation, n) %>%
        as.data.frame()
    ) %>%
    filter(month >= historic_start & month <= historic_end)
  
  
  grouped_ledger <- rbind(grouped_ledger,
                          dcast(simulation + month ~ metric, value.var = "n", data = grouped_ledger, fill = 0) %>%
                            mutate(net = joiners - leavers ) %>%
                            dplyr::select(simulation, month, net) %>%
                            rename(n = net) %>%
                            mutate(metric = "net")) %>%
    mutate(source = "SSDA903")
  
  grouped_ledger$metric <- factor(grouped_ledger$metric, levels = label_levels)
  
  ## Simulated episodes
  
  simulated_episodes <- read.csv(file.path(input_dir, projection_episodes_file)) %>%
    filter(Episode == 1) %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End), birthday = ymd(Birthday), provenance = Provenance, simulation = Simulation) %>%
    dplyr::select(period_id, simulation, period_start, period_end, birthday, provenance)
  
  simulated_migrations <- simulated_episodes %>%
    group_by(period_id, simulation) %>%
    slice_head() %>%
    ungroup %>%
    inner_join(data.frame(age = 1:17), by = character()) %>%
    mutate(anniversary = birthday + years(age)) %>%
    filter(period_start <= anniversary & period_end > anniversary) %>%
    mutate(age_before = age - 1, age_after = age)
  
  simulated_grouped_ledger <- rbind(
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), birthday = birthday[1], .groups = "drop") %>%
      dplyr::select(date, simulation) %>%
      dplyr::mutate(metric = "joiners"),
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), birthday = birthday[1], .groups = "drop") %>%
      dplyr::select(date, simulation) %>%
      dplyr::mutate(metric = "leavers")
  ) %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month, metric, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    ungroup %>%
    complete(month, metric, simulation, fill = list(n = 0)) %>%
    as.data.frame() %>%
    rbind(
      simulated_episodes %>%
        dplyr::inner_join(data.frame(month = floor_date(seq(projection_start, projection_end, by = "month"), unit = "month")), by = character()) %>%
        dplyr::filter(period_start <= month & period_end >= month) %>%
        dplyr::group_by(month, simulation) %>%
        dplyr::summarise(n = n_distinct(period_id)) %>%
        ungroup %>%
        complete(month, simulation, fill = list(n = 0)) %>%
        dplyr::mutate(metric = "cic") %>%
        dplyr::select(month, metric, simulation, n) %>%
        as.data.frame()
    ) %>%
    filter(month > projection_start & month <= projection_end)
  
  simulated_grouped_ledger <- rbind(simulated_grouped_ledger,
                                    dcast(simulation + month ~ metric, value.var = "n", data = simulated_grouped_ledger, fill = 0) %>%
                                      mutate(net = joiners - leavers ) %>%
                                      dplyr::select(simulation, month, net) %>%
                                      rename(n = net) %>%
                                      mutate(metric = "net")) %>%
    mutate(source = "projection")
  
  simulated_grouped_ledger$metric <- factor(simulated_grouped_ledger$metric, levels = label_levels)
  
  simulated_ci <- simulated_grouped_ledger %>%
    group_by(month, metric) %>%
    summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n),
              upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))
  
  state_labels <- c(
    'joiners' = 'Joiners',
    'net' = 'Net',
    'leavers' = 'Leavers',
    'cic' = 'CiC'
  )
  
  colours <- tableau_color_pal("Tableau 20")(12)
  colours[11] <- "#000000"
  colours[12] <- "#000000"
  names(colours) <- c("joiners", "joiners simulated",
                      "agedin", "agedin simulated",
                      "net", "net simulated",
                      "agedout", "agedout simulated",
                      "leavers", "leavers simulated",
                      "cic", "cic simulated")
  
  print(
    ggplot() +
      geom_ribbon(data = simulated_ci,
                  aes(month, ymin = lower_95, ymax = upper_95, fill = metric), alpha = 0.2) +
      geom_ribbon(data = simulated_ci,
                  aes(month, ymin = lower_50, ymax = upper_50, fill = metric), alpha = 0.2) +
      geom_line(data = simulated_ci,
                aes(month, median, colour = metric), linetype = 3) +
      geom_line(data = grouped_ledger,
                aes(month, n, group = simulation, colour = metric),
                stat = "identity", alpha = 1) +
      facet_grid(rows = vars(metric), labeller = labeller(metric = state_labels), scales = "free_y") +
      scale_colour_manual(values = colours) +
      scale_fill_manual(values = colours) +
      theme(legend.position = "none") +
      labs(x = "Date", y = "Children", title = "Total count of children")
  )
  
  dev.off()
  
  rbind(simulated_grouped_ledger %>%
          filter(month >= projection_start & month <= projection_end) %>%
          group_by(month, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
        grouped_ledger %>%
          filter(month >= projection_start & month <= historic_end) %>%
          group_by(month, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))
  ) %>%
    write.csv(file = file.path(output_dir, lattice_top_line_csv), row.names = FALSE)
}


# input_dir <- ''
# output_dir <- ''
# historic_start <- as.Date("2014-03-01")
# historic_end <-  as.Date("2021-03-31")
# projection_start <- as.Date("2019-01-31")
# projection_end <- as.Date("2025-03-31")
# group_ages <- FALSE
# 
# generate_placement_lattice_plots(input_dir, output_dir,
#                        historic_start, historic_end,
#                        projection_start, projection_end)
