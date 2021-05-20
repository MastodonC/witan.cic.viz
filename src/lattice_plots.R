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

  ledger <- rbind(
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), birthday = birthday[1], .groups = "drop") %>%
      dplyr::mutate(age_group = age_category(year_diff(birthday, date), group_ages)) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(label = "joiners"),
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), birthday = birthday[1], .groups = "drop") %>%
      dplyr::mutate(age_group = age_category(year_diff(birthday, date), group_ages)) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(label = "leavers"),
    migrations %>%
      dplyr::mutate(category_before = age_category(age_before, group_ages),
                    category_after = age_category(age_after, group_ages)) %>%
      dplyr::filter(category_before != category_after) %>%
      dplyr::rename(date = anniversary, age_group = category_after) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(label = "agedin"),
    migrations %>%
      dplyr::mutate(category_before = age_category(age_before, group_ages),
                    category_after = age_category(age_after, group_ages)) %>%
      dplyr::filter(category_before != category_after) %>%
      dplyr::rename(date = anniversary, age_group = category_before) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(label = "agedout")
  )

  grouped_ledger <- ledger %>%
    mutate(group_date = floor_date(date, unit = "month")) %>%
    group_by(group_date, age_group, label, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop_last") %>%
    complete(group_date, age_group, label, fill = list(n = 0, sd = 0))

  grouped_ledger <- rbind(grouped_ledger,
                          dcast(simulation + group_date + age_group ~ label, value.var = "n", data = grouped_ledger, fill = 0) %>%
                            mutate(net = joiners + agedin - agedout - leavers ) %>%
                            dplyr::select(simulation, group_date, age_group, net) %>%
                            rename(n = net) %>%
                            mutate(label = "net"))

  grouped_ledger$label <- factor(grouped_ledger$label, levels = label_levels)

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

  simulated_ledger <- rbind(
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), birthday = birthday[1], .groups = "drop") %>%
      dplyr::mutate(age_group = age_category(year_diff(birthday, date), group_ages)) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(label = "joiners"),
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), birthday = birthday[1], .groups = "drop") %>%
      dplyr::mutate(age_group = age_category(year_diff(birthday, date), group_ages)) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(label = "leavers"),
    simulated_migrations %>%
      dplyr::mutate(category_before = age_category(age_before, group_ages),
                    category_after = age_category(age_after, group_ages)) %>%
      dplyr::filter(category_before != category_after) %>%
      dplyr::rename(date = anniversary, age_group = category_after) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(label = "agedin"),
    simulated_migrations %>%
      dplyr::mutate(category_before = age_category(age_before, group_ages),
                    category_after = age_category(age_after, group_ages)) %>%
      dplyr::filter(category_before != category_after) %>%
      dplyr::rename(date = anniversary, age_group = category_before) %>%
      dplyr::select(date, simulation, age_group) %>%
      dplyr::mutate(label = "agedout")
  )

  simulated_grouped_ledger <- simulated_ledger %>%
    mutate(group_date = floor_date(date, unit = "month")) %>%
    group_by(group_date, age_group, label, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop_last") %>%
    complete(group_date, age_group, label, fill = list(n = 0, sd = 0))

  simulated_grouped_ledger <- rbind(simulated_grouped_ledger,
                                    dcast(simulation + group_date + age_group ~ label, value.var = "n", data = simulated_grouped_ledger, fill = 0) %>%
                                      mutate(net = joiners + agedin - agedout - leavers ) %>%
                                      dplyr::select(simulation, group_date, age_group, net) %>%
                                      rename(n = net) %>%
                                      mutate(label = "net"))

  simulated_grouped_ledger$label <- factor(simulated_grouped_ledger$label, levels = label_levels)

  simulated_ci <- simulated_grouped_ledger %>%
    group_by(group_date, age_group, label) %>%
    summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), 
              upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))


  in_cic_actuals <- bootstrapped_actuals %>%
    inner_join(data.frame(group_date = seq(historic_start, historic_end, by = "month")), by = character()) %>%
    filter(period_start <= group_date & period_end >= group_date) %>%
    mutate(age_group = age_category(year_diff(birthday, group_date), group_ages)) %>%
    group_by(group_date, age_group, simulation) %>%
    summarise(n = n_distinct(period_id))

  in_cic_actuals_ci <- in_cic_actuals %>%
    dplyr::group_by(group_date, age_group) %>%
    dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = quantile(n, 0.5), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))

  in_cic_simulated <- simulated_episodes %>%
    inner_join(data.frame(group_date = seq(historic_end - years(1), projection_end, by = "month")), by = character()) %>%
    filter(period_start <= group_date & period_end >= group_date) %>%
    mutate(age_group = age_category(year_diff(birthday, group_date), group_ages)) %>%
    group_by(group_date, age_group, simulation) %>%
    summarise(n = n_distinct(period_id))

  in_cic_simulated_ci <- in_cic_simulated %>%
    dplyr::group_by(group_date, age_group) %>%
    dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = quantile(n, 0.5), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))

  state_labels <- c(
    'joiners' = 'Joiners',
    'agedin' = 'Aged in',
    'net' = 'Net',
    'agedout' = 'Aged out',
    'leavers' = 'Leavers',
    'cic' = 'CiC'
  )

  rbind(simulated_grouped_ledger %>%
          filter(group_date > projection_start & group_date <= projection_end) %>%
          rename(month = group_date, metric = label) %>%
          mutate(source = "projection") %>%
          dcast(month + age_group + metric + source ~ simulation, value.var = "n", fill = 0),
        grouped_ledger %>%
          filter(group_date >= historic_start & group_date <= historic_end) %>%
          rename(month = group_date, metric = label) %>%
          mutate(source = "SSDA903") %>%
          select(-simulation) %>%
          inner_join(data.frame(simulation = 1:100), by = character()) %>%
          dcast(month + age_group + metric + source ~ simulation, value.var = "n", fill = 0),
        in_cic_simulated %>%
          filter(group_date > projection_start & group_date <= projection_end) %>%
          rename(month = group_date) %>%
          mutate(metric = "cic", source = "projection") %>%
          dcast(month + age_group + metric + source ~ simulation, value.var = "n", fill = 0),
        in_cic_actuals %>%
          filter(group_date >= historic_start & group_date <= historic_end) %>%
          rename(month = group_date) %>%
          mutate(metric = "cic", source = "SSDA903") %>%
          select(-simulation) %>%
          inner_join(data.frame(simulation = 1:100), by = character()) %>%
          dcast(month + age_group + metric + source ~ simulation, value.var = "n", fill = 0)) %>%
    write.csv(file = file.path(output_dir, lattice_by_age_simulation_csv), row.names = FALSE)

  pdf(file = file.path(output_dir, joiner_rates_pdf))

  print(simulated_grouped_ledger %>%
          mutate(age_group = factor(age_group, levels = paste("Age", 0:17))) %>%
          filter(group_date > projection_start & group_date <= projection_end) %>%
          rename(month = group_date, metric = label) %>%
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
    filter(group_date > projection_start & group_date <= projection_end) %>%
    rename(month = group_date, metric = label) %>%
    filter(metric == "joiners") %>%
    dcast(month + age_group ~ simulation, value.var = "n", fill = 0) %>%
    melt(id.vars = c("month", "age_group"), variable.name = "simulation", value.name = "n") %>%
    dplyr::select(age_group, simulation, n) %>%
    group_by(age_group) %>%
    summarise(age_mean = mean(n)) %>%
    write.csv(file = file.path(output_dir, joiner_rates_csv), row.names = FALSE)

  rbind(simulated_grouped_ledger %>%
          filter(group_date > projection_start & group_date <= projection_end) %>%
          rename(month = group_date, metric = label) %>%
          mutate(source = "projection") %>%
          group_by(month, age_group, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
        grouped_ledger %>%
          filter(group_date >= historic_start & group_date <= historic_end) %>%
          rename(month = group_date, metric = label) %>%
          mutate(source = "SSDA903") %>%
          group_by(month, age_group, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
        in_cic_actuals_ci %>%
          filter(group_date >= historic_start & group_date <= historic_end) %>%
          rename(month = group_date) %>%
          mutate(metric = "cic", source = "SSDA903") %>%
          dplyr::select(month, age_group, metric, source, lower_95, lower_50, median, upper_50, upper_95),
        in_cic_simulated_ci %>%
          filter(group_date > projection_start & group_date <= projection_end) %>%
          rename(month = group_date) %>%
          mutate(metric = "cic", source = "projection") %>%
          dplyr::select(month, age_group, metric, source, lower_95, lower_50, median, upper_50, upper_95)
  ) %>%
    write.csv(file = file.path(output_dir, lattice_by_age_csv), row.names = FALSE)

  pdf(file = file.path(output_dir, lattice_pdf))

  colours <- tableau_color_pal("Tableau 20")(10)
  names(colours) <- c("joiners", "joiners simulated",
                      "agedin", "agedin simulated",
                      "net", "net simulated",
                      "agedout", "agedout simulated",
                      "leavers", "leavers simulated")

  categories <- if(group_ages) { age_categories}else {age_labels}

  for (category in categories) {
    print(ggplot() +
        geom_ribbon(data = simulated_ci %>% filter(group_date >= projection_start & group_date <= projection_end & age_group == category),
                    aes(group_date, ymin = lower_95, ymax = upper_95, fill = label), alpha = 0.2) +
        geom_ribbon(data = simulated_ci %>% filter(group_date >= projection_start & group_date <= projection_end & age_group == category),
                    aes(group_date, ymin = lower_50, ymax = upper_50, fill = label), alpha = 0.2) +
        geom_line(data = simulated_ci %>% filter(group_date >= projection_start & group_date <= projection_end & age_group == category),
                  aes(group_date, median, colour = label), linetype = 3) +
        geom_line(data = grouped_ledger %>% filter(age_group == category & group_date >= projection_start & group_date <= historic_end),
                  aes(group_date, n, group = simulation, colour = label),
                  stat = "identity", alpha = 1) +
        geom_line(data = in_cic_simulated_ci %>% filter(group_date >= projection_start & age_group == category) %>% mutate(label = factor("cic", levels = label_levels)), aes(group_date, median), linetype = 3) +
        geom_ribbon(data = in_cic_simulated_ci %>% filter(group_date >= projection_start & age_group == category) %>% mutate(label = factor("cic", levels = label_levels)), aes(group_date, ymin = lower_95, ymax = upper_95), alpha = 0.2) +
        geom_ribbon(data = in_cic_simulated_ci %>% filter(group_date >= projection_start & age_group == category) %>% mutate(label = factor("cic", levels = label_levels)), aes(group_date, ymin = lower_50, ymax = upper_50), alpha = 0.2) +
        geom_line(data = in_cic_actuals_ci %>% filter(group_date >= projection_start & age_group == category) %>% mutate(label = factor("cic", levels = label_levels)), aes(group_date, median)) +
        facet_grid(vars(label), vars(age_group), labeller = labeller(label = state_labels), scales = "free_y") +
        scale_colour_manual(values = colours) +
        scale_fill_manual(values = colours) +
        theme(legend.position = "none") +
        labs(x = "Date", y = "Children"))
    }

  ## Same but for all cic

  label_levels <- c("joiners", "net", "leavers", "cic")

  ledger <- rbind(
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), birthday = birthday[1], .groups = "drop") %>%
      dplyr::select(date, simulation) %>%
      dplyr::mutate(label = "joiners"),
    bootstrapped_actuals %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), birthday = birthday[1], .groups = "drop") %>%
      dplyr::select(date, simulation) %>%
      dplyr::mutate(label = "leavers")
  )

  grouped_ledger <- ledger %>%
    mutate(group_date = floor_date(date, unit = "month")) %>%
    group_by(group_date, label, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop_last") %>%
    complete(group_date, label, fill = list(n = 0, sd = 0))

  grouped_ledger <- rbind(grouped_ledger,
                          dcast(simulation + group_date ~ label, value.var = "n", data = grouped_ledger, fill = 0) %>%
                            mutate(net = joiners - leavers ) %>%
                            dplyr::select(simulation, group_date, net) %>%
                            rename(n = net) %>%
                            mutate(label = "net"))

  grouped_ledger$label <- factor(grouped_ledger$label, levels = label_levels)

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

  simulated_ledger <- rbind(
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = min(period_start), birthday = birthday[1], .groups = "drop") %>%
      dplyr::select(date, simulation) %>%
      dplyr::mutate(label = "joiners"),
    simulated_episodes %>%
      dplyr::group_by(period_id, simulation) %>%
      dplyr::summarise(date = max(period_end), birthday = birthday[1], .groups = "drop") %>%
      dplyr::select(date, simulation) %>%
      dplyr::mutate(label = "leavers")
  )

  simulated_grouped_ledger <- simulated_ledger %>%
    mutate(group_date = floor_date(date, unit = "month")) %>%
    group_by(group_date, label, simulation) %>%
    dplyr::summarise(n = n(), .groups = "drop_last") %>%
    complete(group_date, label, fill = list(n = 0, sd = 0))

  simulated_grouped_ledger <- rbind(simulated_grouped_ledger,
                                    dcast(simulation + group_date ~ label, value.var = "n", data = simulated_grouped_ledger, fill = 0) %>%
                                      mutate(net = joiners - leavers ) %>%
                                      dplyr::select(simulation, group_date, net) %>%
                                      rename(n = net) %>%
                                      mutate(label = "net"))

  simulated_grouped_ledger$label <- factor(simulated_grouped_ledger$label, levels = label_levels)

  simulated_ci <- simulated_grouped_ledger %>%
    group_by(group_date, label) %>%
    summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n),
              upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))


  in_cic_actuals <- bootstrapped_actuals %>%
    inner_join(data.frame(group_date = seq(historic_start, historic_end, by = "month")), by = character()) %>%
    filter(period_start <= group_date & period_end >= group_date) %>%
    group_by(group_date, simulation) %>%
    summarise(n = n_distinct(period_id))

  in_cic_actuals_ci <- in_cic_actuals %>%
    dplyr::group_by(group_date) %>%
    dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = quantile(n, 0.5), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))

  in_cic_simulated <- simulated_episodes %>%
    inner_join(data.frame(group_date = seq(historic_end - years(1), projection_end, by = "month")), by = character()) %>%
    filter(period_start <= group_date & period_end >= group_date) %>%
    group_by(group_date, simulation) %>%
    summarise(n = n_distinct(period_id))

  in_cic_simulated_ci <- in_cic_simulated %>%
    dplyr::group_by(group_date) %>%
    dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = quantile(n, 0.5), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975))

  state_labels <- c(
    'joiners' = 'Joiners',
    'net' = 'Net',
    'leavers' = 'Leavers',
    'cic' = 'CiC'
  )

  colours <- tableau_color_pal("Tableau 20")(10)
  names(colours) <- c("joiners", "joiners simulated",
                      "agedin", "agedin simulated",
                      "net", "net simulated",
                      "agedout", "agedout simulated",
                      "leavers", "leavers simulated")

  print(
    ggplot() +
      geom_ribbon(data = simulated_ci %>% filter(group_date >=projection_start & group_date <= projection_end),
                  aes(group_date, ymin = lower_95, ymax = upper_95, fill = label), alpha = 0.2) +
      geom_ribbon(data = simulated_ci %>% filter(group_date >= projection_start & group_date <= projection_end),
                  aes(group_date, ymin = lower_50, ymax = upper_50, fill = label), alpha = 0.2) +
      geom_line(data = simulated_ci %>% filter(group_date >= projection_start & group_date <= projection_end),
                aes(group_date, median, colour = label), linetype = 3) +
      geom_line(data = grouped_ledger %>% filter(group_date >= projection_start & group_date <= historic_end),
                aes(group_date, n, group = simulation, colour = label),
                stat = "identity", alpha = 1) +
      geom_line(data = in_cic_simulated_ci %>% filter(group_date >= projection_start) %>% mutate(label = factor("cic", levels = label_levels)), aes(group_date, median), linetype = 3) +
      geom_ribbon(data = in_cic_simulated_ci %>% filter(group_date >= projection_start) %>% mutate(label = factor("cic", levels = label_levels)), aes(group_date, ymin = lower_95, ymax = upper_95), alpha = 0.2) +
      geom_ribbon(data = in_cic_simulated_ci %>% filter(group_date >= projection_start) %>% mutate(label = factor("cic", levels = label_levels)), aes(group_date, ymin = lower_50, ymax = upper_50), alpha = 0.2) +
      geom_line(data = in_cic_actuals_ci %>% filter(group_date >= projection_start) %>% mutate(label = factor("cic", levels = label_levels)), aes(group_date, median)) +
      facet_grid(rows = vars(label), labeller = labeller(label = state_labels), scales = "free_y") +
      scale_colour_manual(values = colours) +
      scale_fill_manual(values = colours) +
      theme(legend.position = "none") +
      labs(x = "Date", y = "Children")
  )

  dev.off()

  rbind(simulated_grouped_ledger %>%
          filter(group_date > projection_start & group_date <= projection_end) %>%
          rename(month = group_date, metric = label) %>%
          mutate(source = "projection") %>%
          group_by(month, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
        grouped_ledger %>%
          filter(group_date >= projection_start & group_date <= historic_end) %>%
          rename(month = group_date, metric = label) %>%
          mutate(source = "SSDA903") %>%
          group_by(month, metric, source) %>%
          dplyr::summarise(lower_95 = quantile(n, 0.025), lower_50 = quantile(n, 0.25), median = median(n), upper_50 = quantile(n, 0.75), upper_95 = quantile(n, 0.975)),
        in_cic_actuals_ci %>%
          filter(group_date >= projection_start & group_date <= historic_end) %>%
          rename(month = group_date) %>%
          mutate(metric = "cic", source = "SSDA903") %>%
          dplyr::select(month, metric, source, lower_95, lower_50, median, upper_50, upper_95),
        in_cic_simulated_ci %>%
          filter(group_date > projection_start & group_date <= projection_end) %>%
          rename(month = group_date) %>%
          mutate(metric = "cic", source = "projection") %>%
          dplyr::select(month, metric, source, lower_95, lower_50, median, upper_50, upper_95)
  ) %>%
    write.csv(file = file.path(output_dir, lattice_top_line_csv), row.names = FALSE)
}

# generate_lattice_plots('',
#                        '',
#                        as.Date("2014-03-01"), as.Date("2020-03-31"),
#                        as.Date("2019-03-31"), as.Date("2024-03-01"),
#                        FALSE)
