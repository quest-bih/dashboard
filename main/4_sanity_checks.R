# sanity checks

library(tidyverse)
library(readxl)
library(here)
library(janitor)

dashboard_metrics <- read_csv(here("shiny_app", "data", "dashboard_metrics.csv"))


od_2021 <- dashboard_metrics |>
  filter(year == 2021, open_data_manual_check == TRUE)


raw <- dashboard_metrics |>
  count(year, open_data_manual_check, is.na(is_open_data))

old_metrics <- read_csv("C:/Users/nachevv/Downloads/dashboard_21.csv") |>
  select(-`...1`, doi, pdf_downloaded, is_open_data:bar)




qa_od <- od_2021 |>
  select(doi, pdf_downloaded, is_open_data:bar) |>
  left_join(old_metrics, by = "doi")

# new downloaded dois
qa_od |>
  filter(pdf_downloaded.x != pdf_downloaded.y) |>
  pull(doi)

# od that wasn't od in old
qa_od |>
  filter(open_data_manual_check.x != open_data_manual_check.y) |>
  pull(doi)

# od that was missing in old (n = 40)!
qa_od |>
  filter(is.na(open_data_manual_check.y)) |>
  pull(doi)

old_dois_21 <- old_metrics |>
  filter(year > 2020) |>
  pull(doi) |>
  tolower()

old_dois_21_no_pdf <- old_metrics |>
  filter(year > 2020, pdf_downloaded == FALSE) |>
  pull(doi) |>
  tolower()

new_dois_21 <- dashboard_metrics |>
  filter(year == 2021) |>
  pull(doi) |>
  tolower()

setdiff(old_dois_21, new_dois_21)
setdiff(new_dois_21, old_dois_21)

new_pdfs_21 <- dashboard_metrics |>
  filter(year == 2021, pdf_downloaded == TRUE,
         doi %in% old_dois_21_no_pdf)

new_pdfs_21 |>
  count(is_open_data, open_data_manual_check)

# screened that wasn't screened in old
# n = 530
# 976 - 530

dashboard_metrics |>
  get_dupes(doi)


qa_all <- dashboard_metrics |>
  filter(str_detect(doi, "10\\.")) |>
  select(doi, pdf_downloaded, is_open_data:bar) |>
  left_join(old_metrics, by = "doi")


old_manual <- read_csv2(here("results", "OD_manual_tidy_old.csv")) |>
  mutate(doi = tolower(doi))

dupes <- read_csv2(here("results", "OD_manual_tidy.csv")) |>
  mutate(doi = tolower(doi)) |>
  filter(doi %in% old_manual$doi)


dupes <- qa_all |>
  filter(open_data_manual_check.x != open_data_manual_check.y |
           doi %in% dupes$doi,
         !is.na(open_data_manual_check.y)) |>
  select(doi, year, open_data_manual_check.x, open_data_manual_check.y) |>
  rename(open_data_manual_check_22 = open_data_manual_check.x,
         open_data_manual_check_old = open_data_manual_check.y)


dupes |>
  write_excel_csv("dupes21_22.csv")

pdf_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs/"
pdf_cloud_folder <- "C:/Users/nachevv/OneDrive - Charité - Universitätsmedizin Berlin/PDFs_22/"
pdfs_downloaded <- list.files(pdf_folder)
pdfs_cloud <- list.files(pdf_cloud_folder)

setdiff(pdfs_cloud, pdfs_downloaded)
setdiff(pdfs_downloaded, pdfs_cloud)

open_code_22_manual <- read_xlsx(here("results", "oddpub_code_results_manual.xlsx")) |>
  pull(doi) |>
  pdfRetrieve::doi_stripped2pdf()

setdiff(pdfs_cloud, open_code_22_manual)
setdiff(pdfs_downloaded, pdfs_cloud)
setdiff(open_code_22_manual, pdfs_cloud)

#### oddpub check
validated <- read_csv2(here("results", "OD_manual_tidy.csv")) |>
  mutate(doi = tolower(doi))

oddpub_results <- read_csv(here("results", "Open_Data.csv"))

qa_oddpub_results <- oddpub_results |>
  left_join(validated, by = "doi") |>
  select(doi, is_open_data, open_data_category, open_data_manual_check, contains("data_statements"), everything()) |>
  filter(open_data_manual_check != is_open_data)



qa_limitations <- dashboard_metrics |>
  # filter(year == 2021) |>
  count(has_contrib, has_limitations, has_coi, is_open_data)

qa_lims <- dashboard_metrics |>
  filter(has_limitations == FALSE,
         is.na(has_coi) == TRUE, is_open_data == FALSE)

qa_screened <- dashboard_metrics |>
  # filter(year == 2021) |>
  filter(is_open_data == TRUE, is.na(has_contrib) | is.na(has_coi) | is.na(has_limitations))

unified_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/PDFs"
folder_2021 <- "C:/Datenablage/charite_dashboard/2021/PDFs"
folder_2022 <- "C:/Datenablage/charite_dashboard/2022/PDFs"
folder_2023 <- "C:/Datenablage/charite_dashboard/2023/PDFs"
folder_2024 <- "C:/Datenablage/charite_dashboard/2024/PDFs"
junk_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/junk"
junk_2022_folder <- "C:/Datenablage/charite_dashboard/2022/junk"
junk_2023_folder <- "C:/Datenablage/charite_dashboard/2023/junk"
junk_2024_folder <- "C:/Datenablage/charite_dashboard/2024/junk"

junk_in_2021 <- list.files(folder_2021)[list.files(folder_2021) %in% list.files(junk_folder)]

# file.rename(from = file.path(folder_2021, junk_in_2021),
#             to = file.path(junk_folder, junk_in_2021))

junk_unified <- list.files(junk_folder) |>
  pdfRetrieve::doi_pdf2stripped()

junk_2022 <- list.files(junk_2022_folder) |>
  pdfRetrieve::doi_pdf2stripped()

junk_2023 <- list.files(junk_2023_folder) |>
  pdfRetrieve::doi_pdf2stripped()

junk_2024 <- list.files(junk_2024_folder) |>
  pdfRetrieve::doi_pdf2stripped()

junk_dois <- c(junk_unified, junk_2022, junk_2023, junk_2024)

qa_junk <- dashboard_metrics |>
  filter(doi %in% junk_dois) |>
  select(doi, is_open_data, has_contrib, has_coi, has_limitations)

rescreen_dois <- c("10.1007/s12185-021-03196-6",
                   "10.1136/bmjopen-2019-032864",
                   "10.1515/pp-2020-0148")


downloaded_unified_dois <- list.files(unified_folder) |>
  pdfRetrieve::doi_pdf2stripped()

downloaded_2021 <- list.files(folder_2021) |>
  pdfRetrieve::doi_pdf2stripped()

downloaded_2022 <- list.files(folder_2022) |>
  pdfRetrieve::doi_pdf2stripped()

downloaded_2023 <- list.files(folder_2023) |>
  pdfRetrieve::doi_pdf2stripped()
downloaded_2024 <- list.files(folder_2024) |>
  pdfRetrieve::doi_pdf2stripped()

downloaded_unified_dois <- c(downloaded_2023, downloaded_2022, downloaded_2021, downloaded_unified_dois)

qa_unified <- dashboard_metrics |>
  mutate(is_junk = doi %in% junk_dois,
         downloaded = doi %in% downloaded_unified_dois) |>
  select(doi, is_junk, downloaded, year, is_open_data, has_contrib, has_coi, has_limitations)

qa_unified |>
  count(has_contrib, has_limitations, has_coi, is_open_data)

Open_Data_manual_check_template <- read_delim("results/Open_Data_manual_check_template.csv",
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
  mutate(line_n = 1:n()) |>
  select(line_n, everything())

dois_not_downloaded <- Open_Data_manual_check_template |>
  filter(!doi %in% downloaded_unified_dois)

# Open_Data_manual_check_template |>
#   filter(!doi %in% ) |>
#   write_excel_csv2(here("results", "Open_Data_manual_check_template.csv"), progress = TRUE)


qa_contrib <- qa_unified |>
  filter(is.na(has_contrib), !is.na(is_open_data))

# qa_contrib <- qa_unified |>
#   filter(!is.na(has_contrib), is.na(is_open_data))

oddpub_results |>
  filter(open_data_statements != "") |>
  write_excel_csv(here("oddpub_older_missed.csv"))


#### remove old junk from dashboard

Open_Data_manual_check_template <- read_delim("results/Open_Data_manual_check_template.csv",
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
  mutate(line_n = 1:n()) |>
  select(line_n, everything())


