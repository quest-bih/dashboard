library(tidyverse)

#load PURE dataset
load("./main/status_quo_table_2020.RData")
status_quo_table <- status_quo_table_save

#filter dataset for all relevant Charité publications
years <- c(2015, 2016, 2017, 2018, 2019, 2020)
publications <- status_quo_table %>%
  filter(e_pub_year %in% years) %>%
  filter(Article == TRUE) %>%
  filter(`charite authors` > 0 | `BIH authors` > 0)

publications_ids <- publications %>%
  select(doi, pmid, e_pub_year)

write_csv(publications_ids, "./results/Charite_publication_ids.csv")
write_rds(publications, "./results/Charite_publication_table.rds", compress = "gz")
