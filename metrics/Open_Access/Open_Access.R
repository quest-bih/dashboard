library(tidyverse)
library(tictoc)
library(unpaywallR)

publications <- read_csv("./main/publication_table.csv")
# publications_2021 <- read_csv("./main/charite_pubs_2021.csv") %>%
#   mutate(journal = toupper(journal_title),
#          year = e_pub_year,
#          publisher = NA,
#          e_issn = eissn) %>%
#   select(doi, pmid, title, journal, year, publisher, issn, e_issn) %>%
#   drop_na(doi)

# publications_all <- publications %>%
#   rows_append(publications_2021)

email <- "vladislav.nachev@charite.de"
OA_save_file <- "./results/Open_Access.csv"
years <- publications$year %>% unique() %>% sort()
# years <- years[-length(years)]
#
# publications <- publications_all %>%
#   select(doi, year) %>%
#   drop_na()

write_lines("doi,color,issn,journal,publisher,date,year", OA_save_file)

for(current_year in years) {
  tic()

  print(paste0("year: ", current_year))
  doi_batch <- publications %>%
    filter(year == current_year) %>%
    filter(!is.na(doi))
  doi_batch <- sample(doi_batch$doi)
  print(paste0("DOI number: ", length(doi_batch)))

  unpaywall_results <- unpaywallR::dois_OA_colors(doi_batch, email, clusters = 1)
  unpaywall_results["year"] <- current_year
  write_csv(unpaywall_results, OA_save_file, append = TRUE)

  toc()
}

################
# test code area
################
publications1 <- read_csv("./results/Open_Access_part1.csv") %>%
  filter(!is.na(color),
         year == 2021)

publications <- read_csv("./results/Open_Access.csv")

publications <- publications %>%
  filter(year == 2021)


part1_doi <- publications %>%
  filter(!is.na(color)) %>%
  pull(doi)

publications1 <- publications1 %>%
  mutate(new_doi = !doi %in% part1_doi)
publications1 <- rows_update(publications1, publications, by = "doi")

doi_batch <- c("10.1055/s-0041-1723313", "10.1016/j.freeradbiomed.2020.12.347")

up_res <- unpaywallR::dois_OA_colors(doi_batch, email, clusters = 1)

up_res <- roadoi::oadoi_fetch(dois = doi_batch, email = email)

length(unique(publications$doi)) == nrow(publications)
length(unique(publications_2021$doi)) == nrow(publications_2021)
length(unique(publications_all$doi)) == nrow(publications_all)
publications

publications_all %>%
  count(doi, sort = TRUE)
