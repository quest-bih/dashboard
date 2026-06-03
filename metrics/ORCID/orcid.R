library(tidyverse)
library(ContriBOT)
library(furrr)
library(progressr)

plan(multisession)
handlers(global = TRUE)

message("Extraction of ORCID hyperlinks with with ContriBOT...")
# converstion to text should have happened in the Open_Data detection already
pdf_folder <- "C:/Datenablage/charite_dashboard/2024/PDFs"

orcid_hyperlinks <- extract_orcids_from_folder(pdf_folder)
orcids_extracted <- tibble(doi = list.files(pdf_folder) |>
                            str_remove(".pdf") |> str_replace_all("\\+", "\\/"),
                          orcid_hyperlinks)
orcids_extracted |>
  write_csv(here("results", "orcids_extracted_2024.csv"))
