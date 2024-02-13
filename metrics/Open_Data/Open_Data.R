library(tidyverse)
library(vroom)
library(assertthat)
library(readxl)
library(janitor)
library(furrr)
library(here)

future::plan(multisession)

print("Open Data detection with oddpub...")
# pdf_folder <-"S:/Partner/BIH/QUEST/CENTER/3-Service-Infra-Governance/Data Science/PDFs/2021/"
# txt_folder <- "S:/Partner/BIH/QUEST/CENTER/3-Service-Infra-Governance/Data Science/PDFs_to_text/2021/"
# pdf_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/PDFs/"
# txt_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/PDFs_to_text/"
pdf_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs/"
# pdf_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs_last_chunk/"
txt_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs_to_text/"
# txt_folder <- "C:/Users/Vladi/OneDrive - Charité - Universitätsmedizin Berlin/PDFs_22_to_text/"

print("Convert pdfs to text...")
conversion_success <- oddpub::pdf_convert(pdf_folder, txt_folder)
list.files(pdf_folder)[!conversion_success]

print("Load txt files...")

#only screen new PDFs
if (file.exists("./results/Open_Data.csv"))
{
  already_screened_PDFs <- read_csv("results", "Open_Data.csv")

  pdf_text_corpus <- oddpub::pdf_load(txt_folder)
  pdf_text_corpus <- pdf_text_corpus[!(names(pdf_text_corpus) %in% already_screened_PDFs$article)]

  if (length(pdf_text_corpus) > 0) {
    oddpub_results <- oddpub::open_data_search(pdf_text_corpus)
    write_csv(oddpub_results, here("results", "Open_Data.csv"), append = TRUE)
  }

} else {
  pdf_text_corpus <- oddpub::pdf_load(txt_folder)

  print("Run oddpub...")
  oddpub_results <- oddpub::open_data_search(pdf_text_corpus)
  write_csv(oddpub_results, here("results", "Open_Data.csv"))

}

print("completed!")

#add columns for manual check & write to csv -
#this table is then used as a basis for the manual check of the results

template_filename <- "./results/Open_Data_manual_check_template.csv"

oddpub_results <- vroom(here("results", "Open_Data.csv"))


oddpub_results_manual_check <- oddpub_results |>
  mutate(
    # open_data_manual_check = NA,
         # open_data_category_manual = "",
         # open_code_category_manual = "",
         # open_code_category_manual =
#           case_when(
#            str_detect(open_code_statements, "github") ~ "github",
#            str_detect(open_code_statements, "upon request|on reasonable request|if requested|available from.*corresponding authors*") ~ "upon request",
#            str_detect(open_code_statements, "git.|repository|osf|[Z,z]enodo|doi.|available.*\\[|figshare|bitbucket|modeldb|see.*urls|open.source|to reproduce") ~ "other repository or website",
#            str_detect(open_code_statements, "[S,s]upplement(?!ed)|journal.* web page|supporting information|appendix|additional file") ~ "supplement",
#            TRUE ~ NA_character_
#          ),
         # open_code_manual_check = NA,
#         if_else(
#            is.na(open_code_category_manual) | open_code_category_manual == "upon request",
#            FALSE,
#            is_open_code
#          ),
         doi = article |>  str_remove(fixed(".txt")) |>
                           str_replace_all(fixed("+"), "/")
         # data_access = NA
         ) |>
  select(doi, is_open_data, is_open_code)
# |>
#   select(doi, is_open_data, open_data_category, open_data_manual_check,
#          open_data_category_manual, open_data_statements, is_open_code,
#          open_code_manual_check, open_code_category_manual, open_code_statements, data_access)

oddpub_results |>
  write_excel_csv2("C:/my_programs/screening_2023.csv")


scr_22 <- read_csv2("C:/my_programs/screening_2023.csv")
scr_22 |>
  count(is_open_data)



# oddpub_results_manual_check |>
#   filter(is_open_code) |>
#   count(open_code_category_manual)


# old_results <- read_csv("S:/Partner/BIH/QUEST/CENTER/3-Service-Infra-Governance/Inzentivierung und Indikatoren/LoM/Open Data LOM Charité/b/open data 2023/Berechnung/oddpub_results/new_oddpub_2021.csv") |>
#   mutate(doi = article |> str_remove(fixed(".txt")) |>
#            str_replace_all(fixed("+"), "/"))
#
# publications <- read_csv("./main/publication_table.csv")
#
# new_articles <- oddpub_results_manual_check |>
#   left_join(publications, by = "doi") |>
#   filter(year == 2021) |>
#   anti_join(old_results, by = "doi")
#
# new_articles |> count(is_open_data)
# write_csv(new_articles, paste0("./results/new_articles_2021.csv"))

#this is the file with the manually checked results, already update the template file with them,
#such that only the remaining cases need to be checked manually
# manual_check_results <- vroom("./results/Open_Code_manual_detections.csv")
manual_check_results <- vroom("./results/Open_Data_manual_check_results.csv")
# manual_check_results <- vroom("./results/Open_Data_manual_check_template.csv") |>
#   mutate(doi = tolower(doi))
#### add restricted to currently old version of manual_check from 2020

od_2022_restricted <- read_xlsx("./results/OD-LOM_2022.xlsx") |>
  filter(data_access == "restricted",
         assessment != "no_open_data") |>
  select(doi = article, data_access) |>
  mutate(doi = tolower(doi))

manual_only_od <- vroom("./results/OD_manual_tidy.csv") |>
  select(doi, contains("data"), -contains("has"))

categories <- manual_only_od |>
  select(doi, open_data_category_manual)

manual_check_results <- manual_check_results |>
  left_join(categories, by = "doi") |>
  rowwise() |>
  mutate(open_data_category_manual.x =
           paste(na.omit(open_data_category_manual.x, open_data_category_manual.y), collapse = "|")
           ) |>
  # select(-open_data_category_manual.y) |>
  rename(open_data_category_manual = open_data_category_manual.x) |>
  mutate(open_data_category_manual = case_when(
    !is.na(open_data_category_manual.y) ~ open_data_category_manual.y,
    str_detect(open_data_category_manual, "field|discip") &
      str_detect(open_data_category_manual, "general") ~
      "disciplinary and general-purpose repositories",
    str_detect(open_data_category_manual, "field|discip") ~
      "disciplinary repository",
    str_detect(open_data_category_manual, "general") ~
      "general-purpose repository",
    TRUE ~ open_data_category_manual
  )) |>
  select(-open_data_category_manual.y)
#
# manual_check_results |>
#   write_excel_csv2("./results/Open_Data_manual_check_results2.csv")

manual_check_results |>
  count(is_open_data)

# manual_only_od |>
#   count(has_restricted, has_only_restricted)

manual_check_results <- manual_check_results |>
  mutate(data_access = case_when(
    open_data_manual_check == FALSE ~ NA_character_,
    doi %in% od_2020_restricted$doi ~ "restricted",
    .default = data_access))


manual_check_results <- manual_check_results |>
  rows_update(manual_only_od, by = "doi")


mcs <- manual_check_results |>
  filter(!is.na(open_data_manual_check))

mcs |>
  count(is_open_data, open_data_manual_check)

missing_articles <- manual_check_results |>
  filter(is_open_data, is.na(open_data_manual_check))

manual_check_results <- manual_check_results |>
  filter(!doi %in% manual_only_od$doi) |>
  bind_rows(manual_only_od)
# |>
  # left_join(od_2020_restricted) |>
  # mutate(data_access = if_else(open_data_manual_check == FALSE, NA_character_, data_access))

# oddpub_results_manual_check <-
oddpub_results_manual_check <- oddpub_results_manual_check |>
  mutate(doi = tolower(doi)) |>
  left_join(manual_check_results)

missing_manual <- oddpub_results_manual_check |> filter(is_open_data,
       is.na(open_data_manual_check))

#
# manual_check_results <- vroom("./results/OD_manual_2021.csv") |>
# select(doi, open_data_manual_check = open_data_assessment, own_or_reuse_data, data_access, repository) |>
# replace(. =="NULL", NA) |>
# mutate(open_data_category_manual = case_when(
#   repository
# ))
#
#
# categories_old <- dashboard_metrics |>
#   select(doi, od_m = open_data_category_manual)
#
# test <- manual_check_results |>
#   left_join(categories_old) |>
#   select(od_m, open_data_category, everything()) |>
#   filter(open_data_category == "data availability statement")
#
#
# test |>
#   count(open_data_assessment)
#
# test2 <- test |>
#   filter(open_data_assessment == "open_data")

manual_check_results |>
  count(open_data_category_manual)

# oddpub_results_manual_check <- oddpub_results_manual_check |>
#   select(-contains("has_"))

##### update previous oddpub_results_manual_check with

oddpub_results_manual_check |>
  get_dupes(doi)

oddpub_results_manual_check <- rows_update(oddpub_results_manual_check,
                                           manual_check_results, by = "doi")

#now the new Open Data cases in the saved template file have to be checked manually
# oddpub_results_manual_check |> write_excel_csv2(template_filename)
manual_check_results |> write_excel_csv2(template_filename)

#assertions for validity check of the manual results
OD_manual_num <- sum(manual_check_results$open_data_manual_check, na.rm = TRUE)
OD_manual_categories_num <- sum(!is.na(manual_check_results$open_data_category_manual))
assert_that(OD_manual_num == OD_manual_categories_num)

OC_manual_num <- sum(manual_check_results$open_code_manual_check, na.rm = TRUE)
OC_manual_categories_num <- sum(!is.na(manual_check_results$open_code_category_manual))
assert_that(OC_manual_num == OC_manual_categories_num)

# pdfs <- list.files(pdf_folder) |>
#   str_remove(".pdf")
# txts <- list.files(txt_folder) |>
#   str_remove(".txt")
#
# setdiff(pdfs, txts)
#
# setdiff(txts, pdfs)
#
# PDFs <- tibble(files = pdfs)

od2024 <- oddpub_results |>
  # filter(is_open_data == TRUE) |>
  mutate(doi = article |>
           str_remove(".pdf") |>
           str_replace_all("\\+", "/"))

write_csv(od2024, here("results", "Open_Data_240119.csv"))

ff <- list.files(pdf_folder)[!conversion_success]
source_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs/"
files_to_copy <- paste0(source_folder, ff)

dest_folder <- "C:/Datenablage/charite_dashboard/2022/PDFs_last_chunk/"

file.copy(from = files_to_copy, to = dest_folder)

text_files <- list.files("C:/Users/nachevv/OneDrive - Charité - Universitätsmedizin Berlin/PDFs_22_to_text/") |>
  str_remove(".txt")

length(text_files)

pdf_files <- list.files(pdf_folder) |>
  str_remove(".pdf")

setdiff(text_files, pdf_files)



