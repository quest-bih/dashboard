library(tidyverse)

#load PURE dataset
load("./main/status_quo_table.RData")

#filter dataset for all relevant Charité publications
years <- c(2015, 2016, 2017, 2018)
publications <- status_quo_table_save %>%
  filter(e_pub_year %in% years) %>%
  filter(Article == TRUE) %>%
  filter(`charite authors` > 0 | `BIH authors` > 0)

publications_ids <- publications %>%
  select(doi, pmid, e_pub_year)

write_csv(publications_ids, "./results/Charite_publication_ids.csv")
saveRDS(publications, "./results/Charite_publication_table.rds")
