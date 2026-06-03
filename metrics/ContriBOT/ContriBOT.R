library(tidyverse)
library(furrr)
library(oddpub)
library(ContriBOT)
library(here)
library(progressr)

plan(multisession)
handlers(global = TRUE)

print("Detection of Contribution statements with ContriBOT...")
# converstion to text should have happened in the Open_Data detection already
txt_folder <- "C:/Datenablage/charite_dashboard/2023/PDFs_to_text/"
# txt_folder <- "dev/rescreen_txt"

print("Load txt files...")

pdf_text_corpus <- oddpub::pdf_load(txt_folder, lowercase = FALSE, remove_regex = NULL)
append_flag <- FALSE
#only screen new PDFs
if (file.exists(here("results", "ContriBOT.csv"))) {
  already_screened_PDFs <- read_csv(here("results", "ContriBOT.csv"))
  pdf_text_corpus <- pdf_text_corpus[!(names(pdf_text_corpus) %in% already_screened_PDFs$doi)]
  append_flag <- TRUE
}

if (length(pdf_text_corpus) > 0) {
  message("Run ContriBOT...")
  contribot_results <- ContriBOT::extract_contributions(pdf_text_corpus)

  credit_results <- classify_contributions(contribot_results, article, contrib_statement)
  unified_results <- contribot_results |>
    mutate(credit_estimate = NA_character_,
           contrib_estimate = NA_character_,
           narrative_estimate = NA_character_,
           responsibility_estimate = NA_character_) |>
    rows_upsert(credit_results, by = "article") |>
    mutate(doi = stringr::str_replace_all(article, "\\+", "\\/") |>
             stringr::str_remove(".txt")) |>
    select(-article)

    write_csv(unified_results, here("results", "ContriBOT_2023.csv"),
              append = append_flag)
}

print("completed!")


### TODO: retroactively screen prior years 2016-2022!!!
