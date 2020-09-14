library(tidyverse)
library(pdfRetrieve)

email <- "nico.riedel@bihealth.de"

#load PURE dataset
load("./main/status_quo_table.RData")

#filter dataset
year <- 2019
publications <- status_quo_table_save %>%
  filter(e_pub_year == year) %>%
  filter(Article == TRUE) %>%
  filter(`charite authors` > 0 | `BIH authors` > 0)

dois <- publications$doi[publications$doi != ""]

#download pdfs
pdf_folder <- paste0("C:/Datenablage/charite_dashboard/PDFs/", year, "/")
pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(dois, email, pdf_folder, sleep = 10)


#------------------------------------------------------------------------------------------------
# download Open Data publ for manual check separately
#------------------------------------------------------------------------------------------------

open_data_publ <- read_csv("./results/OD_manual_check/Open_Data_2015-18_manual_check.csv")
pdf_folder <- paste0("C:/Datenablage/charite_dashboard/PDFs/2015-18/")
pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(open_data_publ$doi, email, pdf_folder, sleep = 10)

open_code_missing_publ <- read_csv("./results/OD_manual_check/Open_Code_missing_check.csv")
pdf_folder <- paste0("C:/Datenablage/charite_dashboard/PDFs/Open_Code_missing/")
pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(open_code_missing_publ$doi, email, pdf_folder, sleep = 10)


#------------------------------------------------------------------------------------------------
# try to download missing publications from 2015-18 again
# as they had very low download rates
#------------------------------------------------------------------------------------------------

dashboard_metrics <- read_csv("./shiny_app/data/dashboard_metrics.csv") %>%
  rename(year = e_pub_year)

for(filter_year in 2017:2019)
{
  missing_pdfs <- dashboard_metrics %>%
    filter(year == filter_year) %>%
    filter(pdf_downloaded == FALSE)
  missing_dois <- missing_pdfs$doi[!is.na(missing_pdfs$doi)] %>% sample()

  pdf_folder <- paste0("C:/Datenablage/charite_dashboard/PDFs/", filter_year, "_missing/")
  pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(missing_dois, email, pdf_folder, sleep = 10)
}
