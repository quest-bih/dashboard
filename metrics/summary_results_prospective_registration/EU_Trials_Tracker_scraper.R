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
  map_chr(extract_var, text = webpage)

EU_data_charite <- tibble(var_names, var_values) %>%
  rbind(c("retrieval_date", as.character(Sys.Date())))

write_csv(EU_data_charite, "./results/EU_trialstracker.csv")
