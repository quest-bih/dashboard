library(tidyverse)
library(furrr)
library(oddpub)
library(ContriBOT)
library(here)


future::plan(multisession)

print("Detection of Contribution statements with ContriBOT...")
# converstion to text should have happened in the Open_Data detection already
txt_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs_to_text/"
print("Load txt files...")

#only screen new PDFs
if (file.exists(here("results", "ContriBOT.csv")))
{
  already_screened_PDFs <- read_csv(here("results", "ContriBOT.csv"))

  pdf_text_corpus <- oddpub::pdf_load(txt_folder)
  pdf_text_corpus <- pdf_text_corpus[!(names(pdf_text_corpus) %in% already_screened_PDFs$article)]

  if (length(pdf_text_corpus) > 0) {
    contribot_results <- (pdf_text_corpus)
    write_csv(contribot_results, here("results", "ContriBOT.csv"), append = TRUE)
  }

} else {
  pdf_text_corpus <- oddpub::pdf_load(txt_folder)

  print("Run ContriBOT...")
  oddpub_results <- ContriBOT::contribution_detection(pdf_text_corpus)
  write_csv(contribot_results, here("results", "ContriBOT.csv"))
}

print("completed!")
