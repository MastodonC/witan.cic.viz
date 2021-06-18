
source("src/helpers.R")

generate_layer_cake_plots <- function(input_dir, output_dir) {
  
  historic_episodes_file <- "historic-episodes.csv"
  projection_episodes_file <- "projection-episodes.csv"

  episodes <- read.csv(file.path(input_dir, projection_episodes_file)) %>%
    dplyr::mutate(period_id = ID, period_start = ymd(Period.Start), period_end = ymd(Period.End),
                  birthday = ymd(Birthday), start = ymd(Start), end = ymd(End),
                  admission_age = Admission.Age, placement_pathway = Placement.Pathway, period_duration = Period.Duration,
                  provenance = Provenance, placement = Placement, simulation = Simulation, episode = Episode, offset = Offset) %>%
    dplyr::select(period_id, simulation, episode, period_start, period_end, start, end, birthday, provenance, placement,
                  admission_age, placement_pathway, period_duration, offset)
  
  projected_episodes <- episodes %>%
    filter(simulation < 10) %>%
    dplyr::mutate(offset_end = as.integer(day_diff(period_start, end)),
           provenance = ifelse(is.na(provenance), "S", provenance))
  
  top_age_pathways <- projected_episodes %>% filter(provenance == "H") %>% group_by(period_id) %>% slice(1) %>%
    group_by(admission_age, placement_pathway) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
    as.data.frame
  top_age_pathways <- top_age_pathways[1:10,]
  
  offsets <- seq(0, max(projected_episodes$period_duration), 7)
  episodes.weekly <- data.frame(offset = offsets) %>%
    inner_join(projected_episodes, by = character()) %>%
    filter(offset.x >= offset.y & offset.x <= offset_end) %>%
    mutate(placement_p = placement)
  
  all.placements <- c("A3", "A4", "A5", "A6", "H5", "K1", "K2", "M2", "M3", "P1", "P2",
                      "Q1","Q2", "R1", "R2", "R3", "R5", "S1", "T0", "T4", "Z1",
                      'Join')
  library(ggthemes)
  my.colours <- tableau_color_pal("Tableau 20")(20)
  my.colours <- c(my.colours, "#888888", "#FFFFFF")
  
  all.colours <- c(my.colours, my.colours)
  all.placements <- c(all.placements, paste0(all.placements, ".P"))
  names(all.colours) <- all.placements
  
  
  pdf(file = output_file_layercake, paper = "a4r", width=11, height=8.5)
  for (i in 1:nrow(top_age_pathways)) {
    i <- 1
    episodes.filtered <- episodes.weekly %>% filter(placement_pathway == top_age_pathways[i,"placement_pathway"] &
                                                      admission_age == top_age_pathways[i,"admission_age"]) %>%
      mutate(period_id = paste0(period_id, ".", simulation)) %>%
      dplyr::select(offset.y, simulation, period_id, placement, period_duration, provenance)
    
    matches <- episodes.filtered %>% filter(!is.na(Matched.ID)) %>% dplyr::distinct(ID, Matched.ID, Match.Offset, Simulation)
    
    ordered <- episodes.filtered %>% group_by(ID) %>%
      summarise(Period.Duration = max(Period.Duration)) %>%
      arrange(Period.Duration)
    
    provenance_labels <- c("Historic Closed", "Historic Open", "Matched Closed", "Projected Closed", "Simulated")
    names(provenance_labels) <- c("H", "PA", "M", "PB", "S")
    
    episodes.filtered$ID <- factor(episodes.filtered$ID, levels = ordered$ID)
    episodes.filtered$Provenance <- factor(episodes.filtered$Provenance, levels = c("H", "PA", "M", "PB", "S"))
    print(episodes.filtered %>%
            filter(Simulation < 20) %>%
            filter((Provenance == "H" & Simulation == 1) | Provenance != "H") %>%
            ggplot(aes(offset, ID, fill = Placement)) +
            geom_tile() +
            scale_fill_manual(values = all.colours) +
            theme_mastodon +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            facet_grid(cols = vars(Provenance), scales = "free_y", labeller = labeller(Provenance = provenance_labels)) +
            labs(x = "Days in care", y = "Period in care",
                 title = paste0("Join age ",top_age_pathways[i,"Admission.Age"], ", pathway ", top_age_pathways[i,"Placement.Pathway"])))
  }
  dev.off()
  embed_fonts(file = output_file_layercake,outfile = output_file_layercake)
}