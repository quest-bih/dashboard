library(tidyverse)
library(janitor)
library(furrr)
library(progressr)
library(here)
library(oddpub)
library(pseekr)

future::plan(multisession)
progressr::handlers(global = TRUE)

print("PSE detection with PSEekR...")
# txt_folder <- "S:/Partner/BIH/QUEST/CENTER/3-Service-Infra-Governance/Data Science/PDFs_to_text/2021/"
# txt_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/PDFs_to_text/"
txt_folder <- "C:/Datenablage/charite_dashboard/2023/PDFs_to_text/"
# txt_folder <- "C:/Users/Vladi/OneDrive - Charité - Universitätsmedizin Berlin/PDFs_22_to_text/"
# txt_folder <- "dev/rescreen_txt"

print("Load txt files...")

#only screen new PDFs
if (file.exists(here("results", "PSE_detections.csv")))
{
  already_screened_PDFs <- read_csv(here("results", "PSE_detections.csv"))

  pdf_text_corpus <- oddpub::pdf_load(txt_folder)
  pdf_text_corpus <- pdf_text_corpus[!(names(pdf_text_corpus) %in% already_screened_PDFs$article)]

  if (length(pdf_text_corpus) > 0) {
    pseekr_results <- pseekr::pseek_text(pdf_text_corpus)
    pseekr_results <- pseekr_results |>
      mutate(doi = str_remove(article, ".txt") |>
               str_replace_all("\\+", "\\/"), .before = article)
    write_csv(pseekr_results, here("results", "PSE_detections.csv"), append = TRUE)
  }

} else {
  pdf_text_corpus <- oddpub::pdf_load(txt_folder)

  print("Run PSEekR...")
  pseekr_results <- pseekr::pseek_text(pdf_text_corpus)
  pseekr_results <-
    write_csv(pseekr_results, here("results", "PSE_detections.csv"))

}

print("completed!")

