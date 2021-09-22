library(dplyr)
library(ggplot2)
library(ggthemes)

source("src/helpers.R")

cic_placement_age_group <- function(input_dir, output_dir, from_date, to_date, group_ages) {
  
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
    dplyr::inner_join(data.frame(month = floor_date(seq(month_start(from_date), month_start(to_date), by = "month"), unit = "month")), by = character()) %>%
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

  # print(ggplot(chart_data %>% group_by(month, age_group) %>% summarise(n = sum(n)), aes(month, n, fill = age_group)) +
  #   geom_bar(stat = "identity") +
  #   labs(x = "Month", y = "Children in care", fill = "Age group", title = "Children in care by age") +
  #   scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  #   xlim(c(from_date, as.Date("2024-03-31"))) + ylim(c(0, 500)) # Required for 'what happens next?'
  #   )
  # 
  # print(ggplot(chart_data %>% group_by(month, age_group) %>% summarise(n = sum(n)), aes(month, n, fill = age_group)) +
  #         geom_bar(stat = "identity", position = "fill") +
  #         labs(x = "Month", y = "Children in care", fill = "Age group", title = "Children in care by age") +
  #         scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  #         scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%")) +
  #         xlim(c(from_date, as.Date("2024-03-31"))) # Required for 'what happens next?'
  # )

  print(ggplot(chart_data %>% group_by(month, age_group) %>% summarise(n = sum(n)), aes(month, n, fill = age_group)) +
          geom_bar(stat = "identity") +
          labs(x = "Month", y = "Children in care", fill = "Age group", title = "Children in care by age") +
          scale_fill_manual(values = tableau_color_pal("Tableau 20")(20))
  )

  print(ggplot(chart_data %>% group_by(month, age_group) %>% summarise(n = sum(n)), aes(month, n, fill = age_group)) +
          geom_bar(stat = "identity", position = "fill") +
          labs(x = "Month", y = "Children in care", fill = "Age group", title = "Children in care by age") +
          scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
          scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%"))
  )

  for (the.placement in placements) {
    print(ggplot(chart_data %>% filter(placement == the.placement), aes(month, n, fill = age_group)) +
      geom_bar(stat = "identity") +
      labs(x = "Month", y = "Children in care", fill = "Age group", title = paste("Children in", the.placement)) +
      scale_fill_manual(values = tableau_color_pal("Tableau 20")(20))
      )

    print(ggplot(chart_data %>% filter(placement == the.placement), aes(month, n, fill = age_group)) +
            geom_bar(stat = "identity", position = "fill") +
            labs(x = "Month", y = "Children in care", fill = "Age group", title = paste("Children in", the.placement)) +
            scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
            scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%"))
    )
  }

  all.placements <- c("A3", "A4", "A5", "A6", "H5", "K1", "K2", "M2", "M3", "P1", "P2",
                      "Q1","Q2", "R1", "R2", "R3", "R5", "S1", "T0", "T4", "Z1",
                      'Join')
  placement.colours <- tableau_color_pal("Tableau 20")(20)
  placement.colours <- c(placement.colours, "#888888", "#FFFFFF")
  names(placement.colours) <- all.placements

  print(ggplot(chart_data %>% group_by(month, placement) %>% summarise(n = sum(n)), aes(month, n, fill = placement)) +
          geom_bar(stat = "identity") +
          labs(x = "Month", y = "Children in care", fill = "Placement", title = "Children in care by placement") +
          scale_fill_manual(values = placement.colours))
  
  print(ggplot(chart_data %>% group_by(month, placement) %>% summarise(n = sum(n)), aes(month, n, fill = placement)) +
          geom_bar(stat = "identity", position = "fill") +
          labs(x = "Month", y = "Children in care", fill = "Placement", title = "Children in care by placement") +
          scale_fill_manual(values = placement.colours) +
          scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%"))
        )
  
  for (age.group in categories) {
    print(ggplot(chart_data %>% filter(age_group == age.group), aes(month, n, fill = placement)) +
            geom_bar(stat = "identity") +
            labs(x = "Month", y = "Children in care", fill = "Placement", title = paste(age.group, "children")) +
            scale_fill_manual(values = placement.colours)
    )
    
    print(ggplot(chart_data %>% filter(age_group == age.group), aes(month, n, fill = placement)) +
            geom_bar(stat = "identity", position = "fill") +
            labs(x = "Month", y = "Children in care", fill = "Placement", title = paste(age.group, "children")) +
            scale_fill_manual(values = placement.colours) +
            scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%"))
    )
  }
  
  dev.off()
}

# input_dir <- ''
# output_dir <- ''
# from_date <- as.Date("2014-03-31")
# to_date <- as.Date("2027-03-31")
# group_ages <- TRUE
#  
# cic_placement_age_group(input_dir, output_dir, from_date, to_date, group_ages)
