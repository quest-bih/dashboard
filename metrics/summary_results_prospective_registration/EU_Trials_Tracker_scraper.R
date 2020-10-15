library(tidyverse)
library(rvest)


extract_var <- function(var_name, text)
{
  var_value <- text %>%
    str_extract(paste0(var_name, " = [:digit:]{2,3}")) %>%
    str_remove(paste0(var_name, " = "))

  return(var_value)
}

charite_url <-  "https://eu.trialstracker.net/sponsor/charite-universitatsmedizin-berlin"

webpage <- read_lines(charite_url) %>% paste(collapse = " ")

var_names <- c("total_unreported", "total_reported", "total_due",
  "not_yet_due_trials", "inconsistent_trials", "total_trials")

var_values <- var_names %>%
  map_chr(extract_var, text = webpage) %>%
  as.integer()

EU_data_charite <- tibble(total_unreported = var_values[1],
                          total_reported = var_values[2],
                          total_due = var_values[3],
                          not_yet_due_trials = var_values[4],
                          inconsistent_trials = var_values[5],
                          total_trials = var_values[6],
                          retrieval_date = as.character(Sys.Date())) %>%
  mutate(perc_reported = total_reported/total_due)

write_csv(EU_data_charite, "./shiny_app/data/EU_trialstracker.csv")
