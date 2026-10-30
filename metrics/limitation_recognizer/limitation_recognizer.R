library(tidyverse)
library(here)
library(jsonlite)

### since the tool is a jar file, incorporated into the pipeline, the data can be generated either by running
### a pathscreener.py or limitationscreener.py script over a folder of PDFs, generating either a database or a json output file

#### processing the json output


limitations <- read_json(here("results", "limitationresults.json"))

# limitations <- read_json("C:/Users/nachevv/Downloads/limitationresults_2024.json")

lims <- limitations |>
  enframe(name = "filename", value = "sentences") |>
  transmute(doi = str_remove(filename, "\\.pdf") |>
           str_replace_all("\\+", "\\/"),
           sentences = unlist(sentences),
           has_limitations = sentences != "[]")

# lims |>
#   write_excel_csv(here("results", "limitations_old.csv"))

limitations_old <- read_csv(here("results", "limitations_old.csv"))

limitations_results <- limitations_old |>
  rows_upsert(lims, by = "doi")


limitations_results |>
  distinct(doi, .keep_all = TRUE) |>
  write_excel_csv(here("results", "limitations_old.csv"))

# lims_2022 <- read_json(here("results", "limitationresults_2022.json")) |>
#   enframe(name = "filename", value = "sentences") |>
#   transmute(doi = str_remove(filename, "\\.pdf") |>
#               str_replace_all("\\+", "\\/"),
#             sentences = unlist(sentences),
#             has_limitations = sentences != "[]")
#
# lims_2022 |>
#   filter(!doi %in% lims$doi)
#
# limitations <- read_json(here("results", "limitationresults_old.json"))
#
# lims |>
#   write_excel_csv(here("results", "limitations_old.csv"))
#
# limitations |>
#   enframe(name = "filename", value = "sentences") |>
#   transmute(doi = str_remove(filename, "\\.pdf") |>
#               str_replace_all("\\+", "\\/"),
#             sentences = unlist(sentences),
#             has_limitations = sentences != "[]") |>
#   distinct(doi, .keep_all = TRUE) |>
#   write_json(here("results", "limitations_old_deduped.json"))
