library(tidyverse)
library(tictoc)

publication_ids <- read_csv("./results/Charite_publication_ids.csv")

email <- "nico.riedel@bihealth.de"
OA_save_file <- "./results/Open_Access.csv"
years <- publication_ids$e_pub_year %>% unique() %>% sort()


write_lines("doi,color,year", OA_save_file)

for(current_year in years) {
  tic()

  print(paste0("year: ", current_year))
  doi_batch <- publication_ids %>%
    filter(e_pub_year == current_year)
  doi_batch <- doi_batch$doi
  print(paste0("DOI number: ", length(doi_batch)))

  unpaywall_results <- unpaywallR::dois_OA_colors(doi_batch, email, clusters = 2)
  unpaywall_results["year"] <- current_year
  write_csv(unpaywall_results, OA_save_file, append = TRUE)

  toc()
}

