library(dplyr)
library(ggplot2)
library(ggthemes)

source("src/helpers.R")

cic_by_age_group <- function(input_dir, output_dir, from_date, to_date, group_ages) {
  
  projection_episodes_file <- "projection-episodes.csv"
  cic_by_age_group_pdf <- "cic-placement-age-group.pdf"
  
  episodes <- read.csv(file.path(input_dir, projection_episodes_file)) %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End),
                  birthday = ymd(Birthday), start = ymd(Start), end = ymd(End), provenance = Provenance,
                  simulation = Simulation, placement = Placement) %>%
    dplyr::select(period_id, simulation, period_start, period_end, birthday, provenance, start, end, placement)
  
  placements <- episodes %>%
    group_by(placement) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    pull(placement)
  
  chart_data <- episodes %>%
    dplyr::inner_join(data.frame(month = floor_date(seq(from_date, to_date, by = "month"), unit = "month")), by = character()) %>%
    dplyr::filter(start <= month & end >= month) %>%
    dplyr::mutate(age_group = age_category(year_diff(birthday, month), group_ages)) %>%
    dplyr::group_by(month, simulation, period_id) %>%
    dplyr::slice(1) %>%
    dplyr::group_by(month, age_group, placement, simulation) %>%
    dplyr::summarise(n = n_distinct(period_id), .groups = "drop_last") %>%
    dplyr::summarise(n = median(n))
  
  categories <- if(group_ages) {age_categories} else {age_labels}
  category_labels <- paste(categories, "count of children")
  names(category_labels) <- categories
  
  chart_data$age_group <- factor(chart_data$age_group, levels = categories)

  pdf(file = file.path(output_dir, cic_by_age_group_pdf))

  print(ggplot(chart_data %>% group_by(month, age_group) %>% summarise(n = sum(n)), aes(month, n, fill = age_group)) +
    geom_bar(stat = "identity") +
    labs(x = "Month", y = "Children in care", fill = "Age group", title = "Children in care") +
    scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)))

  for (the.placement in placements) {
    print(ggplot(chart_data %>% filter(placement == the.placement), aes(month, n, fill = age_group)) +
      geom_bar(stat = "identity") +
      labs(x = "Month", y = "Children in care", fill = "Age group", title = paste("Children in", the.placement)) +
      scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)))
  }

  dev.off()
}

# input_dir <- ''
# output_dir <- ''
# from_date <- as.Date("2015-03-31")
# to_date <- as.Date("2025-03-31")
# group_ages <- TRUE
# 
# cic_by_age_group(input_dir, output_dir, from_date, to_date, group_ages)
