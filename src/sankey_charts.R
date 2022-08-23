library(dplyr)
library(networkD3)
library(htmlwidgets)
library(webshot)

sankey_chart <- function(input_dir, output_dir, filter_date = "") {

  historic_episodes_file <- "historic-episodes.csv"
  historic_episodes <- read.csv(file.path(input_dir, historic_episodes_file))

  if ("" != filter_date) {
    historic_episodes <- historic_episodes %>%
      mutate(Period.End = as.Date(Period.End)) %>%
      filter(Period.End >= as.Date(filter_date)) %>%
      filter(End != "")
  }

  historic_sequences <- historic_episodes %>% group_by(ID) %>%
    arrange(ID, Start) %>%
    filter(lag(Placement) != Placement | is.na(lag(Placement))) %>%
    dplyr::select(ID, Admission.Age, Placement) %>%
    group_by(ID) %>%
    mutate(sequence = 1:length(ID)) %>%
    rename(admission_age = Admission.Age, placement = Placement)

  sankey_transitions_temp <- historic_sequences %>%
    inner_join(historic_sequences %>% select(ID, placement, sequence), by = "ID") %>%
    filter(sequence.y == sequence.x + 1)

  sankey_transitions <- rbind(sankey_transitions_temp,
                              historic_sequences %>%
                                group_by(ID) %>%
                                arrange(desc(sequence)) %>%
                                slice(1) %>%
                                rename(placement.x = placement, sequence.x = sequence) %>%
                                mutate(placement.y = "OUT", sequence.y = sequence.x + 1)) %>%
    rename(placement = placement.x, next_placement = placement.y, sequence = sequence.x) %>%
    group_by(admission_age, placement, next_placement, sequence) %>%
    summarise(n = n()) %>%
    ungroup

  sankey_placements <- function(transitions, level) {
    if (level == 1) {
      transitions %>%
        filter(sequence == 1) %>%
        dplyr::select(placement) %>%
        unique %>%
        arrange(placement) %>%
        mutate(id = row_number() - 1) %>%
        as.data.frame
    } else {
      rbind(transitions %>%
              filter(sequence == level - 1) %>%
              dplyr::select(next_placement) %>%
              rename(placement = next_placement),
            transitions %>%
              filter(sequence == level) %>%
              dplyr::select(placement)) %>%
        unique %>%
        arrange(placement) %>%
        mutate(id = row_number() - 1) %>%
        as.data.frame
    }
  }

  sankey_from_transitions <- function(transitions) {
    nodes <- data.frame(placement = c(), id = c(), level = c())
    start <- sankey_placements(transitions, 1)
    nodes <- cbind(start, level = 1)
    for (level in 2:7) {
      new_nodes <- sankey_placements(transitions, level)
      if (nrow(new_nodes) > 0) {
        new_nodes <- cbind(new_nodes, level = level)
        new_nodes$id <- new_nodes$id + nrow(nodes)
        nodes <- rbind(nodes, new_nodes)
      }
    }

    links <- transitions %>%
      inner_join(nodes, by = c("sequence" = "level", "placement" = "placement")) %>%
      inner_join(nodes %>% mutate(level = level - 1), by = c("sequence" = "level", "next_placement" = "placement")) %>%
      rename(source = id.x, target = id.y, value = n) %>%
      as.data.frame

    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                            Target = "target", Value = "value", NodeID = "placement",
                            NodeGroup = "placement",
                            units = "TWh", fontSize = 12, nodeWidth = 30,
                            sinksRight = FALSE,
                            colourScale = JS('d3.scaleOrdinal()
                    .domain(["A3","A4","A5","A6","H5","K1","K2","M2","M3","P1","P2","Q1","Q2","R1","R2","R3","R5","S1", "T1", "Z1","OUT"])
                                     .range(["#4E79A7","#A0CBE8","#F28E2B","#FFBE7D","#59A14F","#8CD17D","#B6992D","#F1CE63","#499894","#86BCB6",
                                     "#E15759","#FF9D9A","#79706E","#BAB0AC","#D37295","#FABFD2","#B07AA1","#D4A6C8","#9D7660","#D7B5A6", "#FFFFFF"])'))

  }

  transitions <- sankey_transitions %>% group_by(placement, next_placement, sequence) %>% summarise(n = sum(n)) %>% ungroup
  transitions %>% filter(placement == next_placement)
  sankey <- sankey_from_transitions(transitions)

  temp_file = tempfile(fileext = ".html")
  saveWidget(sankey, file=temp_file)
  webshot(temp_file, file.path(output_dir, "sankey-all.pdf"))

  for (age in 0:17) {
    transitions <- sankey_transitions %>% filter(admission_age == age)
    sankey <- sankey_from_transitions(transitions)
    saveWidget(sankey, file=temp_file)
    webshot(temp_file, file.path(output_dir, paste0("sankey-age-", age, ".pdf")))
  }
}

# input_dir <- ''
# output_dir <- ''
# sankey_chart(input_dir, output_dir)
