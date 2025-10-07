library(tidyverse)
library(pdfRetrieve)
library(furrr)

email <- Sys.getenv("EMAIL")

publications <- read_csv("./main/publication_table.csv")

# pdf_folder <- "S:/Partner/BIH/QUEST/CENTER/3-Service-Infra-Governance/Data Science/PDFs/2021/"
pdf_folder <- "C:/Datenablage/charite_dashboard/2024/PDFs_oa/"

#need doi for download
publications_download <- publications |>
  filter(doi != "") |>
  filter(!is.na(doi),
         year == 2021) |>
  filter(!(doi |> str_detect("^k"))) |>
  filter(doi != "0") |>
  mutate(found = tolower(paste0(gsub("/", "+", doi), ".pdf")) %in%
           tolower(list.files(pdf_folder)),
         doi = tolower(doi)) |>
  select(found, everything())

downloaded_files <- publications_download |>
  filter(found == TRUE) |>
  mutate(file_name = paste0(gsub("/", "+", doi), ".pdf"))

missing_manual <- downloaded_files |>
  select(doi) |>
  left_join(manual_check_results |> mutate(doi = tolower(doi))) |>
  filter(is_open_data,
         is.na(open_data_manual_check))

missing_manual |> write_csv("./results/missing_manual.csv")


dest_folder <- "C:/Datenablage/charite_dashboard/2024/PDFs"


# copy_PDFs_from_doi(downloaded_files$doi, pdf_folder, dest_folder)

dois_sub <- publications_download |>
  filter(found == FALSE) |>
  # filter out Elsevier
  filter(!str_detect(doi, "10.1016"))

set.seed(539)
dois <- sample(dois_sub$doi)

#download pdfs (only if not yet downloaded)

pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(dois, email, pdf_folder, sleep = 1,
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
pdf_removed <- pdf_files |> map_chr(delete_invalid_PDF)


# #also remove Elsevier PDFs with just one page
# #as only the first page gets returned by the API for articles
# #without access rights
#
# elsevier_pdfs <- paste0(pdf_folder,
#                         list.files(pdf_folder))
# elsevier_pdfs <- elsevier_pdfs[elsevier_pdfs |> str_detect("10.1016")]
#
# delete_one_page_PDFs <- function(PDF_file) {
#   page_num <- pdftools::pdf_info(PDF_file)$pages
#   if(page_num == 1) {
#     file.copy(PDF_file, "C:/Datenablage/charite_dashboard/unified_dataset/PDFs_removed/")
#     file.remove(PDF_file)
#     remove_msg <- "PDF removed"
#   } else {
#     remove_msg <- "PDF valid"
#   }
#   print(paste0(PDF_file, ": ", remove_msg))
#
#   return(page_num == 1)
# }
# elsevier_pdf_removed <- elsevier_pdfs |> map_chr(delete_one_page_PDFs)

############### help function for copying files

copy_pdfs_from_doi <- function(dois, source_path, destination_path, overwrite = FALSE) {

  ## Create the downloads folder if it doesn't exist
  if (!file.exists(destination_path)) {
    dir.create(destination_path)
  }

  files_to_copy <- paste0(source_path, gsub("/", "+", dois), ".pdf")

  # copy the files to the new folder
  file.copy(files_to_copy, destination_path, overwrite = overwrite)
}

move_short_paper <- function(pdf_file, destination_folder, min_page_num) {

  filename <- basename(pdf_file)
  dest_filename <- file.path(destination_folder, filename)
  page_num <- pdftools::pdf_info(pdf_file)$pages

  if (page_num < min_page_num) {
    file.rename(from = pdf_file,
                to = dest_filename)
    message(glue::glue("{pdf_file} has {page_num} pages. Moved {filename} to {dest_filename}./n"))
    return(TRUE)
  } else {
    message(glue::glue("{pdf_file} was not too short and left in its original folder./n"))
    return(FALSE)
  }
}

move_short_papers <- function(pdf_folder, destination_folder, min_page_num) {

  ## Create the downloads folder if it doesn't exist
  if (!file.exists(destination_folder)) {
    dir.create(destination_folder)
  }


  files_to_screen <- list.files(pdf_folder, pattern = ".(pdf|PDF)", full.names = TRUE)
  p <- progressr::progressor(along = files_to_screen)

  files_to_screen |>
    furrr::future_map_lgl(\(x) {
      p()
      move_short_paper(x, destination_folder, min_page_num)
    })
}

pdf_folder <- "C:/Datenablage/charite_dashboard/2024/PDFs_oa"
pdf_folder <- "C:/Datenablage/charite_dashboard/2024/shorts"
dest_folder <- "C:/Datenablage/charite_dashboard/2024/shorts2"
# list.files(pdf_folder, full.names = TRUE)[62]
# file.path(pdf_folder)


library(easyRPubMed)

dois_shorts <- tibble(doi = list.files(pdf_folder) |>
                        doi_pdf2stripped())

meta_shorts <- dois_shorts |>
  get_metadata(doi, api_key = Sys.getenv("NCBI_KEY"))


dois <- tibble(doi = list.files(pdf_folder) |>
                 doi_pdf2stripped())
3270/2/2
820/2
dois |> slice(168)
meta_all <- dois |>
  # slice(168) |>
  get_metadata(doi, chunksize = 50, api_key = Sys.getenv("NCBI_KEY"))

plan(multisession)
# plan(sequential)
progressr::handlers(global = TRUE)
shorties <- move_short_papers(pdf_folder, dest_folder, min_page_num = 4)

exclusion_types <- c("Address",
                     "(B|b)iography",
                     "Directory",
                     "Editor",
                     "Supplem",
                     "Abstract",
                     "Historic",
                     "Tutorial",
                     "Interview",
                     "Lecture",
                     "Legislation",
                     "News",
                     "Handout",
                     "Periodical",
                     "Pesonal",
                     "Portrait",
                     "Preprint",
                     "Errat|Corrigend",
                     "Retract",
                     "Integrity",
                     "Media",
                     "Webcast",
                     "Proceedings",
                     "Consensus",
                     "Guideline",
                     "Congress",
                     "Protocol"
) |>
  paste0(collapse = "|")

filtered_publications <- meta_all |>
  list_rbind() |>
  filter(
    ##### inclusion:
    str_detect(doi, "^10"), # having doi
    ##### exclusions:
    !str_detect(pubtype, exclusion_types), # from PubMed article type in exclusion category
    # !is.na(abstract), # without abstract
    !is.na(title), # without title
    !str_detect(title, "\\[(German|Italian|Russian|Spanish|Chinese|Japanese)"), # foreign language
    !str_detect(title, "Errat|Author|Re(\\:|\\b|ply)|In Mem|onference|(?<=:).*(P|p)rotocol\\b|(P|p)rotocol\\b(?=:)|Summary")
  )


junk_dois <- meta_all |>
  list_rbind() |>
  filter(!doi %in% filtered_publications$doi, doi != "") |>
  pull(doi) |>
  doi_stripped2pdf()


pdf_folder <- "C:/Datenablage/charite_dashboard/2024/PDFs_oa"
short_folder <- "C:/Datenablage/charite_dashboard/2024/shorts"
dest_folder <- "C:/Datenablage/charite_dashboard/2024/junk"

file.rename(from = file.path(pdf_folder, junk_dois),
            to = file.path(dest_folder, junk_dois))

# remove junk from txt

txt_to_remove <- c(list.files(dest_folder), list.files(short_folder)) |>
  str_replace("pdf", "txt")

txt_folder <- "C:/Datenablage/charite_dashboard/2024/PDFs_to_text"
file.remove(file.path(txt_folder, txt_to_remove))

# delete_one_page_PDFs <- function(PDF_file) {
#   page_num <- pdftools::pdf_info(PDF_file)$pages
#   if(page_num == 1) {
#     file.copy(PDF_file, "C:/Datenablage/charite_dashboard/unified_dataset/PDFs_removed/")
#     file.remove(PDF_file)
#     remove_msg <- "PDF removed"
#   } else {
#     remove_msg <- "PDF valid"
#   }
#   print(paste0(PDF_file, ": ", remove_msg))
#
#   return(page_num == 1)
# }
# elsevier_pdf_removed <- elsevier_pdfs |> map_chr(delete_one_page_PDFs)
