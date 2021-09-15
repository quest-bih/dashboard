library(tidyverse)

#load new publication dataset & merge the two publication lists
publications_18_20 <- read_csv("./main/publication_table_library_2018_20.csv")
publications_16_17 <- read_csv("./main/publication_table_library_2016_17.csv") %>%
  rename(doi = DOI,
         title = Titel,
         corresponding_author = `Corresp. Author`,
         journal = `TI abrev. J9`,
         issn = ISSN,
         eissn = `E-ISSN`,
         year = `Publ. Year`,
         database = Identifier,
         pmid = PMID,
         publisher = Publisher,
         authors = Autor,
         document_type = `Doc.Type`) %>%
  mutate(oa_indicator = NA,
         oa_status = NA,
         doi = ifelse(doi == "0", NA, doi),
         database = database %>% str_remove(":[:digit:]{15}")) %>%
  select(colnames(publications_18_20)) %>%
  filter(!(doi %in% publications_18_20$doi),
         year != 0)

publications <- rbind(publications_16_17, publications_18_20) %>%
  arrange(doi)

publications %>% write_csv("./main/publication_table_library_2016_20.csv")
