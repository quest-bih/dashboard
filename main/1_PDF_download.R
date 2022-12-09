library(tidyverse)
library(pdfRetrieve)

email <- "vladislav.nachev@charite.de"

publications <- read_csv("./main/publication_table.csv")

# pdf_folder <- "S:/Partner/BIH/QUEST/CENTER/3-Service-Infra-Governance/Data Science/PDFs/2021/"
pdf_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/PDFs/"

#need doi for download
publications_download <- publications %>%
  filter(doi != "") %>%
  filter(!is.na(doi),
         year == 2021) %>%
  filter(!(doi %>% str_detect("^k"))) %>%
  filter(doi != "0") %>%
  mutate(found = tolower(paste0(gsub("/", "+", doi), ".pdf")) %in%
           tolower(list.files(pdf_folder)),
         doi = tolower(doi)) %>%
  select(found, everything())

downloaded_files <- publications_download %>%
  filter(found == TRUE) %>%
  mutate(file_name = paste0(gsub("/", "+", doi), ".pdf"))

missing_manual <- downloaded_files %>%
  select(doi) %>%
  left_join(manual_check_results %>% mutate(doi = tolower(doi))) %>%
  filter(is_open_data,
         is.na(open_data_manual_check))

missing_manual %>% write_csv("./results/missing_manual.csv")


dest_folder <- "C:/Datenablage/charite_dashboard/2021/PDFs"


# copy_PDFs_from_doi(downloaded_files$doi, pdf_folder, dest_folder)

dois_sub <- publications_download %>%
  filter(found == FALSE) %>%
  # filter out Elsevier
  filter(!str_detect(doi, "10.1016"))

set.seed(539)
dois <- sample(dois_sub$doi)

#download pdfs (only if not yet downloaded)

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

############### help function for copying files

copy_PDFs_from_doi <- function(dois, source_path, destination_path) {

  ## Create the downloads folder if it doesn't exist
  if (!file.exists(destination_path)) {
    dir.create(destination_path)
  }

  files_to_copy <- paste0(source_path, gsub("/", "+", dois), ".pdf")

  # copy the files to the new folder
  file.copy(files_to_copy, destination_path, overwrite = FALSE)
}

