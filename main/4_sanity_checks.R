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


#new od cases
validated |>
