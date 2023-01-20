library(assertthat)
library(haven)
library(tidyverse)
library(vroom)

#----------------------------------------------------------------------------------------
# load results
#----------------------------------------------------------------------------------------

publications <- read_csv("./main/publication_table.csv")

#results files
results_folder <- "results/"
results_files <- list.files(results_folder)
results_files <- paste0(results_folder, results_files)


#manually checked Open Data results + additional cases that were not detected by algorithm
#but found in manual searches or submitted by researchers for the LOM
# open_data_results <- vroom("./results/Open_Data_manual_check_results.csv")
# open_data_manual_detection <- vroom("./results/Open_Data_manual_detections.csv")
# open_data_manual_detection <- vroom("./results/Open_Code_manual_detections.csv")

# open_data_results <- rows_update(open_data_results, open_data_manual_detection, by = "doi")
open_data_results <- vroom("./results/Open_Data_manual_check_results2.csv") %>% # temporary until Anastasiia completes manual screening
  mutate(open_code_category_manual = case_when(
    str_detect(open_code_category_manual, "github") ~ "github",
    str_detect(open_code_category_manual, "supplement") ~ "supplement",
    open_code_manual_check == TRUE ~ "other repository/website"
  ),
  open_code_manual_check = case_when(
    str_detect(open_code_category_manual, "supplement") ~ FALSE,
    TRUE ~ open_code_manual_check),
  open_data_manual_check = case_when(
    str_detect(open_data_category_manual, "supplement") ~ FALSE,
    TRUE ~ open_data_manual_check),
  restrictions = case_when(
    str_detect(data_access, "yes") & str_detect(data_access, "restricted") ~ "partial",
    str_detect(data_access, "restricted") ~ "full",
    TRUE ~ "no restricted data"
  )

  )

# open_data_results %>%
#   filter(open_data_manual_check) %>%
#   count(is_open_data, open_data_category_manual)
# open_data_results %>%
#   count(is_open_code, open_code_manual_check)
open_data_results %>%
  count(open_data_manual_check, open_data_category_manual)

#Barzooka results
barzooka_results <- read_csv("./results/Barzooka.csv") %>%
  rename(doi = paper_id) %>%
  select(doi, bar, pie, bardot, box, dot, hist, violin) %>%
  distinct(doi, .keep_all = TRUE)

#----------------------------------------------------------------------------------------
# combine results
#----------------------------------------------------------------------------------------

#check if there are no duplicated dois in the results files (problem for left_join)
assert_that(length(open_data_results$doi) == length(unique(open_data_results$doi)))
assert_that(length(barzooka_results$doi) == length(unique(barzooka_results$doi)))


dashboard_metrics <- publications %>%
  left_join(open_data_results, by = "doi") %>%
  left_join(barzooka_results, by = "doi") %>%
  mutate(pdf_downloaded = !is.na(bar)) %>%
  rename(OA_color = oa_status)

per_year <- dashboard_metrics %>%
  count(year, open_data_category_manual) %>%
  group_by(year) %>%
  mutate(perc = n / sum(n) * 100)

# miss_year <- dashboard_metrics %>%
#   filter(is.na(year))
# dashboard_metrics %>%
#   count(OA_color)

#----------------------------------------------------------------------------------------
# further preprocessing & validation steps
#----------------------------------------------------------------------------------------

#data plausibility/quality check for the updated PDF dataset -> are there any new
#cases that we missed? For old PDF dataset, there were 0 cases, now again 0 cases
check_tbl <- dashboard_metrics %>%
  filter(
    (is_open_data & is.na(open_data_manual_check)) |
           (is_open_code & is.na(open_code_manual_check))) %>%
  select(doi, year, is_open_data, open_data_category, is_open_code,
         open_data_statements, open_code_statements,
         open_data_manual_check, open_data_category_manual,
         open_code_manual_check, open_code_category_manual)
assert_that(dim(check_tbl)[1] == 0)
# write_csv(check_tbl, "./results/OD_manual_check/pdf_update_cases.csv")


#----------------------------------------------------------------------------------------
# save resulting table with relevant columns only
#----------------------------------------------------------------------------------------

#only select columns relevant for shiny table
shiny_table <- dashboard_metrics %>%
  select(doi, pmid, title, journal, year,
         publisher, issn, e_issn,
         pdf_downloaded, OA_color,
         is_open_data, open_data_manual_check, open_data_category_manual,
         is_open_code, open_code_manual_check, open_code_category_manual,
         open_data_statements, open_code_statements,
         restrictions,
         bar, pie, bardot, box, dot, hist, violin)

write_csv(shiny_table, "shiny_app/data/dashboard_metrics.csv")

#----------------------------------------------------------------------------------------
# now for the metrics that are already aggregated by year
#----------------------------------------------------------------------------------------

preprints_dataset_shiny <- read_csv("./results/preprints.csv") %>%
  select(doi, title, journal.title, year)
write_csv(preprints_dataset_shiny, "./shiny_app/data/preprints_dataset_shiny.csv")

prosp_reg_dataset_shiny <- read_csv("./results/prosp_reg_dataset_shiny.csv") %>%
  rename(registration_date = study_first_submitted_date) %>%
  select(nct_id, start_date, registration_date, has_prospective_registration)
write_csv(prosp_reg_dataset_shiny, "./shiny_app/data/prosp_reg_dataset_shiny.csv")

orcid_dataset_shiny <- read_csv("./results/orcid.csv") %>%
  distinct(date, .keep_all = TRUE) %>%
  mutate(orcid_count = if_else(str_length(orcid_count) > 5, str_sub(orcid_count, 1, 4),
                               orcid_count),
         orcid_count = as.numeric(orcid_count))
write_csv(orcid_dataset_shiny, "./shiny_app/data/orcid_results.csv")

EU_trialstracker_dataset_shiny <- read_csv("./results/EU_trialstracker.csv") %>%
  group_by(retrieval_date) %>%
  slice(1) %>%
  mutate(perc_reported = round(total_reported/total_due, 3)) %>%
  arrange(desc(retrieval_date))
write_csv(EU_trialstracker_dataset_shiny, "./shiny_app/data/EU_trialstracker_past_data.csv")

preprints <- read_csv("./results/preprints.csv") %>%
  group_by(year) %>%
  summarize(preprints = n())

prospective_registration <- read_csv("./results/prospective_registration.csv")


shiny_table_aggregate_metrics <- tibble(year = 2006:2021) %>%
  left_join(prospective_registration) %>%
  left_join(preprints)

write_csv(shiny_table_aggregate_metrics, "shiny_app/data/dashboard_metrics_aggregate.csv")

#----------------------------------------------------------------------------------------
# fair assessment
#----------------------------------------------------------------------------------------

fair_table <- read_csv("./results/fair_assessment_2021.csv", show_col_types = FALSE)
write_csv(fair_table, "./shiny_app/data/fair_assessment_2021.csv")

#----------------------------------------------------------------------------------------
# berlin science survey
#----------------------------------------------------------------------------------------

bss_stata <- read_dta("./results/bss-pilot22-char.dta")
write_dta(bss_stata, "./shiny_app/data/bss-pilot22-char.dta")

