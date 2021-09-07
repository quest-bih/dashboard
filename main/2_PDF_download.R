library(tidyverse)
library(pdfRetrieve)

email <- "nico.riedel@bihealth.de"

publications <- read_csv("./main/publication_table_library_2016_20.csv")

#need doi for download
publications_download <- publications %>%
  filter(doi != "") %>%
  filter(!is.na(doi)) %>%
  filter(!(doi %>% str_detect("keine DOI"))) %>%
  filter(doi != "0")

set.seed(539)
dois <- sample(publications_download$doi)

#download pdfs
pdf_folder <- paste0("C:/Datenablage/charite_dashboard/unified_dataset/PDFs/")
pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(dois, email, pdf_folder, sleep = 5,
                                                   overwrite_files = FALSE)


#------------------------------------------------------------------------------------------------
# validate downloaded PDFs
#------------------------------------------------------------------------------------------------

#test validity of all PDF files by reading the first lines
#and removing PDFs with no valid PDF formatting
is_valid_PDF <- function(PDF_file) {
  first_line <- readLines(PDF_file, n = 1)
  if(length(first_line) == 0) { #if file empty
    is_valid <- FALSE
  } else {
    is_valid <- str_sub(first_line, 1,5) == "%PDF-"
  }

  return(is_valid)
}

delete_invalid_PDF <- function(PDF_file) {
  is_valid <- is_valid_PDF(PDF_file)
  if(!is_valid) {
    file.remove(PDF_file)
    remove_msg <- "PDF removed"
  } else {
    remove_msg <- "PDF valid"
  }
  print(paste0(PDF_file, ": ", remove_msg))

  return(!is_valid)
}

pdf_files <- paste0(pdf_folder,
                    list.files(pdf_folder))
pdf_removed <- pdf_files %>% map_chr(delete_invalid_PDF)


#also remove Elsevier PDFs with just one page
#as only the first page gets returned by the API for articles
#without access rights

elsevier_pdfs <- paste0(pdf_folder,
                        list.files(pdf_folder))
elsevier_pdfs <- elsevier_pdfs[elsevier_pdfs %>% str_detect("10.1016")]

delete_one_page_PDFs <- function(PDF_file) {
  page_num <- pdftools::pdf_info(PDF_file)$pages
  if(page_num == 1) {
    file.copy(PDF_file, "C:/Datenablage/charite_dashboard/unified_dataset/PDFs_removed/")
    file.remove(PDF_file)
    remove_msg <- "PDF removed"
  } else {
    remove_msg <- "PDF valid"
  }
  print(paste0(PDF_file, ": ", remove_msg))

  return(page_num == 1)
}
elsevier_pdf_removed <- elsevier_pdfs %>% map_chr(delete_one_page_PDFs)

