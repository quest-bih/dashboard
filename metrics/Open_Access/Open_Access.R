library(tidyverse)
library(tictoc)
library(unpaywallR)

publications <- read_csv("./main/publication_table_library_2016_20.csv")

email <- "nico.riedel@bihealth.de"
OA_save_file <- "./results/Open_Access.csv"
years <- publications$year %>% unique() %>% sort()


write_lines("doi,color,year", OA_save_file)

for(current_year in years) {
  tic()

  print(paste0("year: ", current_year))
  doi_batch <- publications %>%
    filter(year == current_year) %>%
    filter(!is.na(doi))
  doi_batch <- doi_batch$doi
  print(paste0("DOI number: ", length(doi_batch)))

  unpaywall_results <- unpaywallR::dois_OA_colors(doi_batch, email, clusters = 1)
  unpaywall_results["year"] <- current_year
  write_csv(unpaywall_results, OA_save_file, append = TRUE)

  toc()
}

