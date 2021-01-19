library(tidyverse)
library(DBI)

### Read in the data set that has OA and TRN columns
original <- read_csv(
    "2021-01-11_pp-dataset-trn.csv",
    col_types="ccdddcccccdcccdllllllcddccccDlcccccccccccccccccccc"
)

### Generate the following CSV using the CSV above and the R script at the following address:
### https://codeberg.org/bgcarlisle/PubmedIntersectionCheck

### Use the Pubmed search string from:
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3104815/

animals <- read_csv("2021-01-19_08-37-04-checked-pmids.csv")

joined_filename <- Sys.time() %>%
    str_replace_all(":", "-") %>%
    str_replace_all(" ", "_") %>%
    paste0("-joined.csv")

joined <- original %>%
    left_join(animals, by=c("pmid_dimensions" = "pmid")) %>%
    rename(animals = found)

joined$pmid_dimensions
joined$animals

joined %>%
    write_csv(joined_filename)

### Now join with Sciscore data

con <- dbConnect(RSQLite::SQLite(), "~/Academic/2020-12-01-robustness/2019-11-17-sciscore_pmcoai_all2.db")

sciscore_reports <- con %>%
    dbReadTable("sciscore_reports")

joined$pmid_dimensions <- joined$pmid_dimensions %>%
    as.character()

joinedwithsciscore <- joined %>%
    left_join(sciscore_reports, by=c("pmid_dimensions" = "pmid"))

joinedwithsciscore_filename <- Sys.time() %>%
    str_replace_all(":", "-") %>%
    str_replace_all(" ", "_") %>%
    paste0("-joinedwithsciscore.csv")

joinedwithsciscore %>%
    write_csv(joinedwithsciscore_filename)
