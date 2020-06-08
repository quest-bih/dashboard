library(tidyverse)
library(pdfRetrieve)

email <- "nico.riedel@bihealth.de"

#load PURE dataset
load("./main/status_quo_table.RData")

#filter dataset
year <- 2018
publications <- status_quo_table_save %>%
  filter(e_pub_year == year) %>%
  filter(Article == TRUE) %>%
  filter(`charite authors` > 0 | `BIH authors` > 0)

dois <- publications$doi[publications$doi != ""]

#download pdfs
pdf_folder <- paste0("C:/Datenablage/charite_dashboard/", year, "/")
pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(dois, email, pdf_folder, sleep = 20)
