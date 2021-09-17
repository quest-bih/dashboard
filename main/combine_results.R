library(tidyverse)
library(assertthat)

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
open_data_results <- read_delim("./results/Open_Data_manual_check_results.csv", delim = ";")
open_data_manual_detection <- read_csv("./results/Open_Data_manual_detections.csv")
open_data_results <- rows_update(open_data_results, open_data_manual_detection, by = "doi")

#Barzooka results
barzooka_results <- read_csv("./results/Barzooka.csv") %>%
  rename(doi = paper_id) %>%
  select(doi, bar, pie, bardot, box, dot, hist, violin) %>%
  distinct(doi, .keep_all = TRUE)

#Open Access results
open_access_results <- read_csv("./results/Open_Access.csv") %>%
  rename(OA_color = color) %>%
  distinct(doi, .keep_all = TRUE) %>%
  select(doi, OA_color)

#Sciscore results
sciscore <- read_csv("metrics/Sciscore/sciscore_reports.csv")


#----------------------------------------------------------------------------------------
# combine results
#----------------------------------------------------------------------------------------

#check if there are no duplicated dois in the results files (problem for left_join)
assert_that(length(open_data_results$doi) == length(unique(open_data_results$doi)))
assert_that(length(barzooka_results$doi) == length(unique(barzooka_results$doi)))
assert_that(length(open_access_results$doi) == length(unique(open_access_results$doi)))


dashboard_metrics <- publications %>%
  left_join(open_data_results, by = "doi") %>%
  left_join(barzooka_results, by = "doi") %>%
  left_join(open_access_results, by = "doi") %>%
  mutate(pdf_downloaded = !is.na(bar))

#number of papers that are already screened by sciscore
print(paste0("number of papers that are already screened by sciscore: ",
             sum(!is.na(dashboard_metrics$sciscore))))
print(paste0("not yet screened by sciscore: ",
             sum(is.na(dashboard_metrics$sciscore))))


#----------------------------------------------------------------------------------------
# further preprocessing & validation steps
#----------------------------------------------------------------------------------------

#data plausibility/quality check for the updated PDF dataset -> are there any new
#cases that we missed? For old PDF dataset, there were 0 cases, now again 0 cases
check_tbl <- dashboard_metrics %>%
  filter((is_open_data & is.na(open_data_manual_check)) |
           (is_open_code & is.na(open_code_manual_check))) %>%
  select(doi, year, is_open_data, open_data_category, is_open_code,
         open_data_statements, open_code_statements,
         open_data_manual_check, open_data_category_manual,
         open_code_manual_check, open_code_category_manual)
assert_that(dim(check_tbl)[1] == 0)
#write_csv(check_tbl, "./results/OD_manual_check/pdf_update_cases.csv")


#----------------------------------------------------------------------------------------
# save resulting table with relevant columns only
#----------------------------------------------------------------------------------------

#only select columns relevant for shiny table
shiny_table <- dashboard_metrics %>%
  select(doi, pmid, year,
         pdf_downloaded, OA_color,
         is_open_data, open_data_manual_check, open_data_category_manual,
         is_open_code, open_code_manual_check, open_code_category_manual,
         open_data_statements, open_code_statements,
         bar, pie, bardot, box, dot, hist, violin)

write_csv(shiny_table, "shiny_app/data/dashboard_metrics.csv")


#----------------------------------------------------------------------------------------
# now for the metrics that are already aggregated by year
#----------------------------------------------------------------------------------------

preprints_dataset_shiny <- read_csv("./results/preprints.csv") %>%
  select(doi, title, journal.title, year)
write_csv(preprints_dataset_shiny, "./shiny_app/data/preprints_dataset_shiny.csv")


orcid_dataset_shiny <- read_csv("./results/orcid.csv") %>%
  distinct(date, .keep_all = TRUE)
write_csv(orcid_dataset_shiny, "./shiny_app/data/orcid_results.csv")


preprints <- read_csv("./results/preprints.csv") %>%
  group_by(year) %>%
  summarize(preprints = n())

prospective_registration <- read_csv("./results/prospective_registration.csv")

shiny_table_aggregate_metrics <- tibble(year = 2006:2020) %>%
  left_join(prospective_registration) %>%
  left_join(preprints)

write_csv(shiny_table_aggregate_metrics, "shiny_app/data/dashboard_metrics_aggregate.csv")
