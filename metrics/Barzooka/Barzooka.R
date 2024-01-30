library(tidyverse)
library(here)
library(pdfRetrieve)


email <- Sys.getenv("EMAIL")

# determine which files to screen in adition to the ODDPUb screened files. As Barzooka
# screens figures, the German-language articles excluded from ODDPub screening are being
# added back here

publications <- vroom(here("main", "publications_table.csv"))

dashboard_metrics <- vroom(here("shiny_app", "data", "dashboard_metrics.csv"))

# how many not screened w/ barzooka after 2020?
dashboard_metrics |>
  filter(year > 2020) |>
  count(year, !is.na(bar))

# how many not screened with ODDPub
dashboard_metrics |>
  # filter(year > 2020) |>
  count(year, !is.na(is_open_data))

missing_barzooka_dois_22 <- dashboard_metrics |>
  filter(year > 2020,
         is.na(bar),
         str_detect(doi, "10\\.")) |>
  select(doi) |>
  mutate(in_junk = doi %in% junk_dois,
         not_en = str_detect(doi, "10\\.1055|10\\.1026|10\\.1007|10.1111|10\\.52571|10\\.13109|10\\.4414|10\\.1159"))
# |>
#   mutate(in_publications = doi %in% publications$doi) # all pubs came from library list

junk_folder1 <- "C:/Datenablage/charite_dashboard/unified_dataset/junk/"
junk_folder2 <- "C:/Datenablage/charite_dashboard/2022/junk/"

junk_pdfs <- c(list.files(junk_folder1),
               list.files(junk_folder2))

junk_dois <- junk_pdfs |>
  str_replace_all("\\+", "/") |>
  str_remove(".pdf")

dest_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs_Barzooka/"
missing_barzooka_dois_22 |>
  copy_PDFs_from_doi(junk_folder2, dest_folder)

missing_barzooka_dois_22 |>
  filter(in_junk == TRUE) |>
  pull(doi) |>
  copy_PDFs_from_doi(junk_folder1, dest_folder)

dois <- missing_barzooka_dois_22 |>
  filter(in_junk == FALSE)


dois_sub <- missing_barzooka_dois_22 |>
  mutate(found = tolower(paste0(gsub("/", "+", doi), ".pdf")) %in%
  tolower(list.files(dest_folder))) |>
  filter(found == FALSE)

set.seed(539)
dois <- sample(dois_sub$doi)

#download pdfs (only if not yet downloaded)

pdf_retrieval_results <- pdfRetrieve::pdf_retrieve(dois, email, dest_folder, sleep = 1,
                                                   overwrite_files = FALSE)


from_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs_Barzooka/"
dest_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs_last_batch/"

missing_barzooka_dois_22 |>
  filter(not_en == FALSE) |>
  pull(doi) |>
  copy_PDFs_from_doi(from_folder, dest_folder)
