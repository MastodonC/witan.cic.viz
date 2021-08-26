library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggthemes)
library(tidyquant)
source("src/helpers.R")

generate_lattice_plots <- function(input_dir, output_dir, historic_start, historic_end, projection_start, projection_end, group_ages = FALSE) {

  historic_episodes_file <- "historic-episodes.csv"
  projection_episodes_file <- "projection-episodes.csv"
  lattice_by_age_csv <- "lattice-by-age-group.csv"
  lattice_by_age_simulation_csv <- "lattice-by-age-group-simulation.csv"
  lattice_top_line_csv <- "lattice-top-line.csv"
  lattice_pdf <- "lattice-plots.pdf"
  joiner_rates_pdf <- 'joiner-rates.pdf'
  joiner_rates_csv <- 'joiner-rates.csv'

  label_levels <- c("joiners", "agedin", "net", "agedout", "leavers", "cic")

  bootstrapped_actuals <- read.csv(file.path(input_dir, historic_episodes_file)) %>%
    filter(Episode == 1 & Provenance != "S") %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End), birthday = ymd(Birthday), provenance = Provenance, simulation = 1) %>%
    dplyr::select(period_id, simulation, period_start, period_end, birthday, provenance)

  migrations <- bootstrapped_actuals %>%
    group_by(period_id, simulation) %>%
    slice_head(1) %>%
    ungroup %>%
    inner_join(data.frame(age = 1:17), by = character()) %>%
    mutate(anniversary = birthday + years(age)) %>%
    filter(period_start <= anniversary & period_end > anniversary) %>%
    mutate(age_before = age - 1, age_after = age)

  grouped_ledger <- rbind(
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), birthday = birthday[1], .groups = "drop") %>%
      dplyr::mutate(age_group = age_category(year_diff(birthday, date), group_ages)) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(metric = "joiners"),
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), birthday = birthday[1], .groups = "drop") %>%
      dplyr::mutate(age_group = age_category(year_diff(birthday, date), group_ages)) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(metric = "leavers"),
    migrations %>%
      dplyr::mutate(category_before = age_category(age_before, group_ages),
                    category_after = age_category(age_after, group_ages)) %>%
      dplyr::filter(category_before != category_after) %>%
      dplyr::rename(date = anniversary, age_group = category_after) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(metric = "agedin"),
    migrations %>%
      dplyr::mutate(category_before = age_category(age_before, group_ages),
                    category_after = age_category(age_after, group_ages)) %>%
      dplyr::filter(category_before != category_after) %>%
      dplyr::rename(date = anniversary, age_group = category_before) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(metric = "agedout")
  ) %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month, age_group, metric, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    ungroup %>%
    complete(month, age_group, metric, simulation, fill = list(n = 0)) %>%
    rbind(
      bootstrapped_actuals %>%
        dplyr::inner_join(data.frame(month = floor_date(seq(historic_start, historic_end, by = "month"), unit = "month")), by = character()) %>%
        dplyr::filter(period_start <= month & period_end >= month) %>%
        dplyr::mutate(age_group = age_category(year_diff(birthday, month), group_ages)) %>%
        dplyr::group_by(month, age_group, simulation) %>%
        dplyr::summarise(n = n_distinct(period_id)) %>%
        dplyr::mutate(metric = "cic") %>%
        dplyr::select(month, age_group, metric, simulation, n)
    ) %>%
    filter(month >= historic_start & month <= historic_end)

  grouped_ledger <- rbind(grouped_ledger,
                          dcast(simulation + month + age_group ~ metric, value.var = "n", data = grouped_ledger, fill = 0) %>%
                            mutate(net = joiners + agedin - agedout - leavers) %>%
                            dplyr::select(simulation, month, age_group, net) %>%
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
    slice_head(1) %>%
    ungroup %>%
    inner_join(data.frame(age = 1:17), by = character()) %>%
    mutate(anniversary = birthday + years(age)) %>%
    filter(period_start <= anniversary & period_end > anniversary) %>%
    mutate(age_before = age - 1, age_after = age)

  simulated_grouped_ledger <- rbind(
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), birthday = birthday[1], .groups = "drop") %>%
      dplyr::mutate(age_group = age_category(year_diff(birthday, date), group_ages)) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(metric = "joiners"),
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), birthday = birthday[1], .groups = "drop") %>%
      dplyr::mutate(age_group = age_category(year_diff(birthday, date), group_ages)) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(metric = "leavers"),
    simulated_migrations %>%
      dplyr::mutate(category_before = age_category(age_before, group_ages),
                    category_after = age_category(age_after, group_ages)) %>%
      dplyr::filter(category_before != category_after) %>%
      dplyr::rename(date = anniversary, age_group = category_after) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(metric = "agedin"),
    simulated_migrations %>%
      dplyr::mutate(category_before = age_category(age_before, group_ages),
                    category_after = age_category(age_after, group_ages)) %>%
      dplyr::filter(category_before != category_after) %>%
      dplyr::rename(date = anniversary, age_group = category_before) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(metric = "agedout")
  ) %>%
    mutate(month = floor_date(date, unit = "month")) %>%
    group_by(month, age_group, metric, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    ungroup %>%
    complete(month, age_group, metric, simulation, fill = list(n = 0)) %>%
    rbind(
      simulated_episodes %>%
        dplyr::inner_join(data.frame(month = floor_date(seq(projection_start, projection_end, by = "month"), unit = "month")), by = character()) %>%
        dplyr::filter(period_start <= month & period_end >= month) %>%
        dplyr::mutate(age_group = age_category(year_diff(birthday, month), group_ages)) %>%
        dplyr::group_by(month, age_group, simulation) %>%
        dplyr::summarise(n = n_distinct(period_id)) %>%
        dplyr::mutate(metric = "cic") %>%
        dplyr::select(month, age_group, metric, simulation, n)
    ) %>%
    filter(month > projection_start & month <= projection_end)

  simulated_grouped_ledger <- rbind(simulated_grouped_ledger,
                                    dcast(simulation + month + age_group ~ metric, value.var = "n", data = simulated_grouped_ledger, fill = 0) %>%
                                      mutate(net = joiners + agedin - agedout - leavers ) %>%
                                      dplyr::select(simulation, month, age_group, net) %>%
                                      rename(n = net) %>%
                                      mutate(metric = "net")) %>%
    mutate(source = "projection")

  simulated_grouped_ledger$metric <- factor(simulated_grouped_ledger$metric, levels = label_levels)
  
  simulated_ci <- simulated_grouped_ledger %>%
    group_by(month, age_group, metric) %>%
    summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n),
              upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))

  state_labels <- c(
    'joiners' = 'Joiners',
    'agedin' = 'Aged in',
    'net' = 'Net',
    'agedout' = 'Aged out',
    'leavers' = 'Leavers',
    'cic' = 'CiC'
  )

  rbind(simulated_grouped_ledger %>%
          dcast(month + age_group + metric + source ~ simulation, value.var = "n", fill = 0),
        grouped_ledger %>%
          select(-simulation) %>%
          inner_join(data.frame(simulation = 1:100), by = character()) %>%
          dcast(month + age_group + metric + source ~ simulation, value.var = "n", fill = 0)) %>%
    write.csv(file = file.path(output_dir, lattice_by_age_simulation_csv), row.names = FALSE)

  pdf(file = file.path(output_dir, joiner_rates_pdf))

  print(simulated_grouped_ledger %>%
          mutate(age_group = factor(age_group, levels = paste("Age", 0:17))) %>%
          filter(metric == "joiners") %>%
          dcast(month + age_group ~ simulation, value.var = "n", fill = 0) %>%
          melt(id.vars = c("month", "age_group"), variable.name = "simulation", value.name = "n") %>%
          dplyr::select(age_group, simulation, n) %>%
          group_by(age_group)%>%
          mutate(age_mean = mean(n)) %>%
          group_by(age_group, age_mean, n) %>%
          summarise(count = n())%>%
          ungroup %>%
          ggplot(aes(n, count))+
          geom_bar(stat = "identity") +
          geom_vline(aes(xintercept = age_mean), colour = "orange", linetype = 2) +
          facet_wrap(vars(age_group), scales = "free"))

  dev.off()

  simulated_grouped_ledger %>%
    mutate(age_group = factor(age_group, levels = paste("Age", 0:17))) %>%
    filter(metric == "joiners") %>%
    dcast(month + age_group ~ simulation, value.var = "n", fill = 0) %>%
    melt(id.vars = c("month", "age_group"), variable.name = "simulation", value.name = "n") %>%
    dplyr::select(age_group, simulation, n) %>%
    group_by(age_group) %>%
    summarise(age_mean = mean(n)) %>%
    write.csv(file = file.path(output_dir, joiner_rates_csv), row.names = FALSE)

  rbind(simulated_grouped_ledger %>%
          group_by(month, age_group, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
        grouped_ledger %>%
          group_by(month, age_group, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))) %>%
    write.csv(file = file.path(output_dir, lattice_by_age_csv), row.names = FALSE)

  pdf(file = file.path(output_dir, lattice_pdf))

  colours <- tableau_color_pal("Tableau 20")(12)
  colours[11] <- "#000000"
  colours[12] <- "#000000"
  names(colours) <- c("joiners", "joiners simulated",
                      "agedin", "agedin simulated",
                      "net", "net simulated",
                      "agedout", "agedout simulated",
                      "leavers", "leavers simulated",
                      "cic", "cic simulated")

  categories <- if(group_ages) {age_categories} else {age_labels}
  category_labels <- paste(categories, "count of children")
  names(category_labels) <- categories

  for (category in categories) {
    print(ggplot() +
        geom_ribbon(data = simulated_ci %>% filter(age_group == category),
                    aes(month, ymin = lower_95, ymax = upper_95, fill = metric), alpha = 0.2) +
        geom_ribbon(data = simulated_ci %>% filter(age_group == category),
                    aes(month, ymin = lower_50, ymax = upper_50, fill = metric), alpha = 0.2) +
        geom_line(data = simulated_ci %>% filter(age_group == category),
                  aes(month, median, colour = metric), linetype = 3) +
        geom_line(data = grouped_ledger %>% filter(age_group == category),
                  aes(month, n, group = simulation, colour = metric),
                  stat = "identity", alpha = 1) +
        facet_grid(vars(metric), vars(age_group), labeller = labeller(metric = state_labels, age_group = category_labels), scales = "free_y") +
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
    rbind(
      bootstrapped_actuals %>%
        dplyr::inner_join(data.frame(month = floor_date(seq(historic_start, historic_end, by = "month"), unit = "month")), by = character()) %>%
        dplyr::filter(period_start <= month & period_end >= month) %>%
        dplyr::group_by(month, simulation) %>%
        dplyr::summarise(n = n_distinct(period_id)) %>%
        dplyr::mutate(metric = "cic") %>%
        dplyr::select(month, metric, simulation, n)
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
    slice_head(1) %>%
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
    rbind(
      simulated_episodes %>%
        dplyr::inner_join(data.frame(month = floor_date(seq(projection_start, projection_end, by = "month"), unit = "month")), by = character()) %>%
        dplyr::filter(period_start <= month & period_end >= month) %>%
        dplyr::group_by(month, simulation) %>%
        dplyr::summarise(n = n_distinct(period_id)) %>%
        dplyr::mutate(metric = "cic") %>%
        dplyr::select(month, metric, simulation, n)
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
          filter(month > projection_start & month <= projection_end) %>%
          group_by(month, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
        grouped_ledger %>%
          filter(month >= projection_start & month <= historic_end) %>%
          group_by(month, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
  ) %>%
    write.csv(file = file.path(output_dir, lattice_top_line_csv), row.names = FALSE)
}

# input_dir <- ''
# output_dir <- ''
# historic_start <- as.Date("2015-03-01")
# historic_end <-  as.Date("2021-03-31")
# projection_start <- as.Date("2020-03-31")
# projection_end <- as.Date("2025-03-01")
# group_ages <- FALSE
#
# generate_lattice_plots(input_dir, output_dir,
#                        historic_start, historic_end,
#                        projection_start, projection_end,
#                        FALSE)
