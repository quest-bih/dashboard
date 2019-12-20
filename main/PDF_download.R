library(tidyverse)
library(pdfRetrieve)

email <- "nico.riedel@bihealth.de"

#load PURE dataset
load("../data/status_quo_table.RData")

#filter dataset
year <- 2016
publications <- status_quo_table_save %>%
  filter(e_pub_year == year) %>%
  filter(Article == TRUE) %>%
  filter(`charite authors` > 0 | `BIH authors` > 0)

dois <- publications$doi[publications$doi != ""]

#download pdfs
pdf_folder <- paste0("C:/Datenablage/charite_dashboard/", year, "/")
pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(dois, email, pdf_folder, sleep = 20)


#load (preliminary!) FACT data
FACT_2018 <- read_delim("C:/Users/riedeln/Downloads/FACT_2018_ZS_27062019.csv", delim = ";") #only read from this folder because I can't load it currently from the local folder as one of the folders in the path includes special character (Charité)

dois_FACT <- FACT_2018$DOI
dois_FACT <- dois_FACT[!is.na(dois_FACT)]
dois_FACT_additional <- dois_FACT[!(dois_FACT %in% dois)]

#download pdfs
pdf_folder <- "C:/Datenablage/Open_Data_LOM/2018/PDFs/"
#pdf_retrieval_results_FACT <- pdfRetrieve::pdf_retrieve(dois_FACT_additional, email, pdf_folder, sleep = 15)

