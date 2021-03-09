library(readxl)
library(janitor)

process_counts <- function(file) {
  read_xlsx(file) %>%
    slice(c(3, 44:61)) %>%
    rename(age = `...1`) %>%
    mutate(age = str_replace(age, "Age ", ""),
           age = str_replace(age, " CiC exl UASC", ""),
           age = str_replace(age, " Non UASC", "")) %>%
    as.matrix() %>%
    t() %>%
    as.data.frame() %>%
    row_to_names(row_number = 1) %>% 
    rownames_to_column("date") %>%
    mutate(date = str_replace(date, "Open ", "") %>% 
             parse_date_time(orders = c("%d/%m/%y")))
}
