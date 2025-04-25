library(assertthat)
library(haven)
library(tidyverse)
library(vroom)
library(here)
library(easyRPubMed)
library(janitor)
library(readxl)

#----------------------------------------------------------------------------------------
# load results
#----------------------------------------------------------------------------------------

# dashboard_metrics |>
#   filter(year == 2023) |>
#   count(is_open_data, open_data_manual_check)

publications <- read_csv(here("main", "publication_table_old.csv")) |>
  mutate(doi = tolower(doi))
publications |>
  count(year)

dupes <- get_dupes(publications, doi)

publications_2023 <- read_xlsx(here("main", "Publikationsdatensatz Charité_2023_241119VN.xlsx")) |>
  filter(str_detect(Duplikate, "DOI|kein") | is.na(Duplikate),
         is.na(Keine_Charite_Affiliation)
         ) |>
  transmute(doi = tolower(DOI),
         pmid = `Pubmed Id`,
         title = `Article Title`,
         journal = `Source Title`,
         year =
           case_when(
           # doi == "10.1016/j.accpm.2021.101015" ~ `Publication Year` - 1,
           !is.na(`Early Access Date`) ~ year(`Early Access Date`),
             # str_detect(`Early Access Date`, " ") ~ year(my(`Early Access Date`)),
           # !str_detect(`Early Access Date`, " ") ~ as.numeric(`Early Access Date`) |>
             # as.Date(origin = "1899-12-30") |>
             # year(),
           # str_detect(Duplikat, "halb") ~ year(`Date of Export`) - 1,

           .default = `Publication Year`
         ),
         publisher = Publisher,
         issn = ISSN,
         e_issn = eISSN,
         oa_indicator = `Open Access Designations`,
         oa_status = `OA-Status`,
         corresponding_author_charite = !is.na(CA)
         # ,
         # orcids_lib = ORCIDs # ORCIDs that library has are not necessarily in the paper!
         ) |>
  select(all_of(names(publications)))

dupes <- get_dupes(publications_2023, doi) # should only be articles w/o doi

publications_2023 |>
  count(year)

only_new_dois <- publications_2023 |>
  filter(!doi %in% publications$doi |
        str_detect(doi, "keine") | is.na(doi))

publications <- publications |>
  rows_append(only_new_dois) |>
  # filter(str_detect(doi, "10"), doi %in% publications_2022$doi) |>
  rows_upsert(publications_2023 |>
                filter(str_detect(doi, "10")) |>
                select(doi, oa_status), by = "doi")

count(publications, year)

# old_dupes <- publications_2023 |>
#   filter(doi %in% publications_2022$doi)

# corresponding_authorship <- read_csv("T:/Dokumente/publications_CA.csv")

# publications <- publications |>
#   left_join(corresponding_authorship, by = "doi")


write_excel_csv(publications, here("main", "publication_table.csv"))


publications <- read_csv(here("main", "publication_table.csv"))

publications |>
  count(year, oa_status) |>
  pivot_wider(names_from = year, values_from = n)

#results files
# results_files <- list.files(here("results"), full.names = TRUE)
# results_files <- paste0(results_folder, results_files)


open_data_22 <- read_csv(here("results", "Open_Data_2022.csv")) |>
  # mutate(doi = str_replace_all(article, "\\+", "\\/") |>
  #          str_remove(".txt") |>
  #          tolower()) |>
  select(doi, everything(), -article)

open_data_23 <- read_csv(here("results", "Open_Data.csv")) |>
  # mutate(doi = str_replace_all(article, "\\+", "\\/") |>
  #          str_remove(".txt") |>
  #          tolower()) |>
  select(doi, everything(), -article)

open_code_22_manual <- read_xlsx(here("results", "oddpub_code_results_manual_2022.xlsx")) |>
  select(doi, contains("open_code"), "cas") |>
  mutate(open_code_manual_check = as.logical(open_code_manual_check))

# open_data_23 |>
#   filter(is_open_code == TRUE) |>
#   write_excel_csv2("T:/Dokumente/open_code_2023.csv")


#manually checked Open Data results + additional cases that were not detected by algorithm
#but found in manual searches or submitted by researchers for the LOM/IOM
# open_data_retro <- read_csv(here("results", "Open_Data_retroactive.csv")) |>
#   mutate(doi = tolower(doi)) |>
#   select(doi, everything(), -article)

open_data_23_manual <- read_csv2(here("results", "OD_manual_tidy.csv")) |>
  mutate(doi = tolower(doi)) |>
  select(doi, open_data_manual_check, restricted_manual_check, open_data_category_manual, data_access)

open_data_23_manual |>
  left_join(publications, by = "doi") |>
  count(year)

open_code_23_manual <- read_xlsx(here("results", "oddpub_code_results_manual.xlsx")) |>
  select(doi, contains("code"), language) |>
  mutate(is_open_code = as.logical(is_open_code),
         open_code_manual_check = as.logical(open_code_manual_check)) |>
  filter(doi %in% open_data_23$doi)

# qa_old_results <- open_data_results |>
#   rows_upsert(open_data_retro, by = "doi") |>
#   rows_upsert(open_data_22, by = "doi") |>
#   rows_upsert(open_data_22_manual, by = "doi") |>
#   rows_upsert(open_code_22_manual, by = "doi") |>
#   mutate(open_code_category_manual = case_when(
#     str_detect(open_code_category_manual, "github") ~ "github",
#     str_detect(open_code_category_manual, "supplement") ~ "supplement",
#     open_code_manual_check == TRUE ~ "other repository/website"
#   ),
#   open_code_manual_check = case_when(
#     str_detect(open_code_category_manual, "supplement") ~ FALSE,
#     .default = open_code_manual_check
#   ),
#   open_data_manual_check = case_when(
#     str_detect(open_data_category_manual, "supplement") ~ FALSE,
#     .default = open_data_manual_check
#   ),
#   restrictions = case_when(
#     str_detect(data_access, "yes") & str_detect(data_access, "restricted") ~ "partial",
#     str_detect(data_access, "restricted") ~ "full",
#     .default = "no restricted data"
#   ),
#   restricted_manual_check = case_when(
#     restrictions == "no restricted data" ~ FALSE,
#     restrictions == "full" ~ TRUE,
#     .default = restricted_manual_check
#   )
#   ) |>
#   left_join(dashboard_metrics, by = "doi") |>
#   # filter(is_open_code.x != is_open_code.y) |>
#   # select(doi, year, contains("is_open_code"), contains("code"), everything())
#   filter(is_open_data.x != is_open_data.y) |>
#   select(doi, year, contains("is_open_data"), contains("data"), everything())
# |>
#   # left_join(open_data_retro, by = "doi") |>
#   # filter(is_open_data.x != is_open_data.y) |>
#   # filter(is_open_code.x != is_open_code.y | is.na(is_open_code.x)) |>
#   left_join(publications, by = "doi") |>
#   select(doi, year, contains("is_open"), contains("data"), everything())

# qa_old_results |>
#   count(year, is.na(open_code_manual_check))

open_data_results <- read_csv2(here("results", "Open_Data_manual_check_results.csv")) |>
  mutate(language = as.character(language)) |>
  # mutate(
  #        restricted_manual_check = NA,
  #        # is_reuse = NA, ### missing from previous screenings
  #        # is_open_data_das = NA, ### missing from previous screenings
  #        # is_open_code_cas = NA, ### missing from previous screenings
  #        is_code_supplement = NA, ### missing from previous screenings
  #        is_code_reuse = NA, ### missing from previous screenings
  #        # das = as.character(das),
  #        # cas = as.character(cas),
  #        language = NA_character_) |>
  # rows_upsert(open_data_retro, by = "doi") |>
  # rows_upsert(open_data_22, by = "doi") |>
  # rows_upsert(open_code_23_manual, by = "doi") |>
  # rows_upsert(open_data_22_manual, by = "doi") |>
  # rows_upsert(open_code_22_manual, by = "doi") |>
  rows_upsert(open_data_23_manual, by = "doi") |>
  rows_upsert(open_code_23_manual, by = "doi") |>
  mutate(open_code_category_manual = case_when(
    str_detect(open_code_category_manual, "github") ~ "github",
    str_detect(open_code_category_manual, "supplement") ~ "supplement",
    open_code_manual_check == TRUE ~ "other repository/website"
  ),
  open_code_manual_check = case_when(
    str_detect(open_code_category_manual, "supplement") ~ FALSE,
    .default = open_code_manual_check
    ),
  open_data_manual_check = case_when(
    str_detect(open_data_category_manual, "supplement") ~ FALSE,
    .default = open_data_manual_check
    ),
  restrictions = case_when(
    str_detect(data_access, "yes") & str_detect(data_access, "restricted") ~ "partial",
    str_detect(data_access, "restricted") ~ "full",
    .default = "no restricted data"
  ),
  restricted_manual_check = case_when(
    restrictions == "no restricted data" ~ FALSE,
    restrictions == "full" ~ TRUE,
    .default = restricted_manual_check
  )
  )

open_data_results |>
  write_excel_csv2(here("results", "Open_Data_manual_check_results_new.csv"))


open_data_results |>
  left_join(publications |> select(doi, year)) |>
  count(year, is.na(das))

missing_years <- open_data_results |>
  left_join(publications |> select(doi, year)) |>
  filter(is.na(year))

open_data_results |>
  left_join(publications |> select(doi, year)) |>
  filter(year > 2020) |>
  count(year, open_code_manual_check)

# missing_years <- open_data_results |>
#   left_join(publications |> select(doi, year)) |>
#   filter(is.na(year))

# when finished update the template to contain the data from previous year !!!!
# open_data_results |>
#   write_excel_csv2(here("results", "Open_Data_manual_check_template.csv"))

# template_miss <- template |>
#   filter(!doi %in% open_data_results$doi)


# old_das_cas <- vroom(here("results", "Open_Data_retroactive.csv")) |>
#   select(doi, das, cas)
#
# open_data_results <- open_data_results |>
#   rows_upsert(old_das_cas, by = "doi")

### some LOM articles here, which will not be joined to main table!

open_data_results |>
  filter(is.na(is_open_data),
         open_data_category_manual == "disciplinary and general-purpose repositories") |>
  pull(doi)

open_data_results |>
  filter(open_data_manual_check) |>
  count(is_open_data, open_data_category_manual)

open_data_results |>
  count(is_open_code, open_code_manual_check)

open_data_results |>
  count(open_data_manual_check, open_data_category_manual)

#ContriBOT results

# contribot_results <- read_csv(here("results", "ContriBOT_2023.csv"))
# contribot_results_retro <- read_csv(here("results", "ContriBOT_retroactive.csv"))
# contribot_results <- contribot_results_retro |>
#   rows_upsert(contribot_results, by = "doi")
contribot_results <- read_csv(here("results", "ContriBOT.csv"))

# contribot_results <- contribot_results_old |>
#   rows_upsert(contribot_results, by = "doi")

# contribot_results |>
#   write_excel_csv(here("results", "ContriBOT.csv"))

orcid_screening_results <- read_csv(here("results", "orcids_extracted_2023.csv"))

# orcid_screening_results_retro <- read_csv(here("results", "orcids_retroactive.csv")) |>
#   mutate(doi = str_extract(file, "10\\..*") |>
#            str_remove(".pdf") |>
#            str_replace_all("\\+", "/")) |>
#   select(-file)

# orcid_screening_results <- orcid_screening_results_retro |>
#   rows_upsert(orcid_screening_results, by = "doi")

orcid_screening_results_old <- read_csv(here("results", "orcids_extracted_old.csv"))

orcid_screening_results <- orcid_screening_results_old |>
  rows_upsert(orcid_screening_results, by = "doi")

orcid_screening_results |>
  write_excel_csv(here("results", "orcids_extracted_old.csv"))

#Barzooka results
barzooka_results <- read_csv(here("results", "Barzooka.csv"))
barzooka_old <- read_csv(here("results", "Barzooka_old.csv"))

barzooka_results <- barzooka_old |>
  rename(flowconcept = flowno,
         flowinex = flowyes) |>
  rows_upsert(barzooka_results, by = "paper_id")

barzooka_results <- barzooka_results |>
  rename(doi = paper_id) |>
  select(doi, bar, pie, bardot, box, dot, hist, violin) |>
  distinct(doi, .keep_all = TRUE)

missing_dois <- setdiff(contribot_results$doi, publications$doi)

publications |>
  filter(doi %in% missing_dois)

rtransparent_2023 <- read_csv(here("results", "rtransparent_2023.csv")) |>
  transmute(doi = tolower(doi),
            has_coi = is_coi_pred,
            has_funding = is_funded_pred
  )

rtransparent_old <- read_csv(here("results", "rtransparent_old.csv"))

rtransparent_results <- rtransparent_old |>
  rows_upsert(rtransparent_2023, by = "doi")

rtransparent_results |>
  write_excel_csv(here("results", "rtransparent_old.csv"))


dupes <- rtransparent_old |>
  distinct(doi, .keep_all = TRUE) |>
  get_dupes(doi)

limitations_2023 <- read_csv(here("results", "limitations.csv"))

limitations_old <- read_csv(here("results", "limitations_old.csv"))

limitations_results <- limitations_old |>
  rows_upsert(limitations_2023, by = "doi")

limitations_results |>
  write_excel_csv(here("results", "limitations_old.csv"))

#----------------------------------------------------------------------------------------
# combine results
#----------------------------------------------------------------------------------------

#check if there are no duplicated dois in the results files (problem for left_join)
assert_that(length(open_data_results$doi) == length(unique(open_data_results$doi)))
assert_that(length(barzooka_results$doi) == length(unique(barzooka_results$doi)))


dashboard_metrics <- publications |>
  left_join(open_data_results, by = "doi") |>
  left_join(contribot_results, by = "doi") |>
  left_join(orcid_screening_results, by = "doi") |>
  left_join(rtransparent_results, by = "doi") |>
  left_join(limitations_results, by = "doi") |>
  rename(oa_color = oa_status) |>
  mutate(has_any_orcid = has_orcid | !is.na(orcids)) |>
  left_join(barzooka_results, by = "doi") |>
  mutate(pdf_downloaded = !is.na(bar) | !is.na(has_contrib))



## set excluded articles that were screened to NA
junk_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/junk"
junk_2022_folder <- "C:/Datenablage/charite_dashboard/2022/junk"
junk_2023_folder <- "C:/Datenablage/charite_dashboard/2023/junk"

junk_unified <- list.files(junk_folder) |>
  pdfRetrieve::doi_pdf2stripped()

junk_2022 <- list.files(junk_2022_folder) |>
  pdfRetrieve::doi_pdf2stripped()

junk_2023 <- list.files(junk_2023_folder) |>
  pdfRetrieve::doi_pdf2stripped()

junk_prior <- dashboard_metrics |>
  filter(is.na(has_contrib), !is.na(is_open_data),
       year < 2021) |>
  pull(doi)

junk_dois <- c(junk_unified, junk_2022, junk_2023, junk_prior)

dashboard_metrics_junk <- dashboard_metrics |>
  filter(doi %in% junk_dois) |>
  select(doi, starts_with("is_open"), starts_with("has"), restrictions, orcids) |>
  mutate(across(!doi, \(x) x = NA))


dashboard_metrics <- dashboard_metrics |>
  rows_update(dashboard_metrics_junk, by = "doi")

dashboard_metrics |>
  filter(pdf_downloaded == TRUE) |>
  count(has_any_orcid, has_orcid)

nas_orcids <- dashboard_metrics |>
  filter(is.na(has_any_orcid))

nas_orcids |>
  count(year)

orcid_screening_results |>
  count(is.na(orcids))

per_year <- dashboard_metrics |>
  count(year, open_data_category_manual) |>
  group_by(year) |>
  mutate(perc = n / sum(n) * 100)

per_year <- dashboard_metrics |>
  count(year, open_code_category_manual) |>
  group_by(year) |>
  mutate(perc = n / sum(n) * 100)


oc_dois <- open_code_22_manual |>
  filter(open_code_manual_check == TRUE,
         open_code_category_manual != "supplement") |>
  pull(doi)

dashboard_metrics |>
  filter(doi %in% oc_dois) |>
  count(year, open_code_manual_check)

# missing_dois <- setdiff(oc_dois, dashboard_metrics$doi)

no_orcid_hyperlinks <- dashboard_metrics |>
  filter(has_orcid, is.na(orcids))

# miss_year <- dashboard_metrics |>
#   filter(is.na(year))
# dashboard_metrics |>
#   count(OA_color)

#----------------------------------------------------------------------------------------
# further preprocessing & validation steps
#----------------------------------------------------------------------------------------

#data plausibility/quality check for the updated PDF dataset -> are there any new
#cases that we missed? For old PDF dataset, there were 0 cases, now again 0 cases
check_tbl <- dashboard_metrics |>
  filter(
    (is_open_data & is.na(open_data_manual_check)) |
           (is_open_code & is.na(open_code_manual_check)),
    year > 2021) |>
  select(doi, year, is_open_data, open_data_category, is_open_code,
         open_data_statements, open_code_statements,
         open_data_manual_check, open_data_category_manual,
         open_code_manual_check, open_code_category_manual)

assert_that(nrow(check_tbl) == 0)

if (nrow(check_tbl) > 0) {
  check_tbl |>
    filter(year == 2023) |>
    write_excel_csv2(here("results", "pdf_update_cases.csv"))
}



#----------------------------------------------------------------------------------------
# save resulting table with relevant columns only
#----------------------------------------------------------------------------------------

#only select columns relevant for shiny table
shiny_table <- dashboard_metrics |>
  select(doi, pmid, title, journal, year,
         publisher, issn, e_issn,
         corresponding_author_charite,
         pdf_downloaded, oa_color,
         is_open_data, is_open_data_das, open_data_manual_check, open_data_category_manual,
         is_open_code, is_open_code_cas, open_code_manual_check, open_code_category_manual,
         open_data_statements, open_code_statements,
         das, cas,
         restrictions,
         has_contrib, contrib_statement,
         has_orcid, orcid_statement,
         orcids, has_any_orcid,
         has_coi, has_funding,
         has_limitations,
         bar, pie, bardot, box, dot, hist, violin)

shiny_table |>
  count(is.na(has_any_orcid), is.na(has_coi))

cois_missing <- shiny_table |>
  filter(!is.na(has_any_orcid), is.na(has_coi))

write_csv(shiny_table, here("shiny_app", "data", "dashboard_metrics.csv"))

shiny_table |>
  count(year, restrictions)

# dashboard_metrics |>
#   count(year, is.na(is_open_data))
#
# dashboard_metrics |>
#   count(year, is.na(has_contrib))
#
# year_21 <- wrong_years |>
#   filter(year == 2021)
# year_22 <- dashboard_metrics |>
#   filter(year == 2022)
#
# year_22 |>
#   count(is.na(has_contrib))
#
# nas_22 <- year_22 |>
#   filter(is.na(has_contrib))
#
# wrong_years <- dashboard_metrics |>
#   filter(year != 2022,
#          !is.na(has_contrib))

# dashboard_metrics <- read_csv(here("shiny_app", "data", "dashboard_metrics.csv"))
#----------------------------------------------------------------------------------------
# now for the metrics that are already aggregated by year
#----------------------------------------------------------------------------------------

preprints_dataset_shiny <- vroom(here("results", "preprints_oa.csv")) |>
  select(doi, title, journal_title, year, has_published_version)
write_csv(preprints_dataset_shiny, here("shiny_app", "data", "preprints_dataset_shiny.csv"))

prosp_reg_dataset_shiny <- vroom(here("results", "prosp_reg_dataset_shiny.csv")) |>
  rename(registration_date = study_first_submitted_date) |>
  select(nct_id, start_date, registration_date, has_prospective_registration)
write_csv(prosp_reg_dataset_shiny, here("shiny_app", "data", "prosp_reg_dataset_shiny.csv"))

EU_trialstracker_dataset_shiny <- vroom(here("results", "EU_trialstracker.csv")) |>
  group_by(retrieval_date) |>
  slice(1) |>
  mutate(perc_reported = round(total_reported/total_due, 3)) |>
  arrange(desc(retrieval_date))
write_csv(EU_trialstracker_dataset_shiny, here("shiny_app", "data", "EU_trialstracker_past_data.csv"))

preprints <- vroom(here("results", "preprints_oa.csv")) |>
  group_by(year) |>
  summarize(n_preprints = n(),
            n_preprints_with_articles = sum(has_published_version, na.rm = TRUE),
            perc_preprints_with_articles = n_preprints_with_articles / n_preprints)

prospective_registration <- vroom(here("results", "prospective_registration.csv"))

max_year <- max(prospective_registration$year)

shiny_table_aggregate_metrics <- tibble(year = 2006:max_year) |>
  left_join(prospective_registration) |>
  left_join(preprints)

write_csv(shiny_table_aggregate_metrics, here("shiny_app", "data", "dashboard_metrics_aggregate.csv"))

#----------------------------------------------------------------------------------------
# fair assessment
#----------------------------------------------------------------------------------------

fair_table <- vroom(here("results", "fair_assessment_2021.csv"), show_col_types = FALSE)
write_csv(fair_table, here("shiny_app", "data", "fair_assessment_2021.csv"))

#----------------------------------------------------------------------------------------
# berlin science survey
#----------------------------------------------------------------------------------------

bss_stata <- read_dta(here("results", "bss-pilot22-char.dta"))
write_dta(bss_stata, here("shiny_app", "data", "bss-pilot22-char.dta"))

