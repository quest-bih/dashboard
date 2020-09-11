library(tidyverse)

#----------------------------------------------------------------------------------------
# load results
#----------------------------------------------------------------------------------------

#load publication dataset
publications <- readRDS("./results/Charite_publication_table.rds")

#results files
results_folder <- "results/"
results_files <- list.files(results_folder)
results_files <- paste0(results_folder, results_files)


#Open Data results
open_data_files <- results_files[results_files %>% str_detect("Open_Data")]
open_data_results <- map(open_data_files, read_csv)
open_data_results <- do.call(rbind, open_data_results) %>%
  rename(doi = article) %>%
  distinct(doi, .keep_all = TRUE)

#convert dois to standard format
open_data_results$doi <- open_data_results$doi %>%
  str_remove(fixed(".txt")) %>%
  str_replace_all(fixed("+"), fixed("/"))


#manually checked Open Data results
open_data_manual <- results_files[results_files %>% str_detect("OD_manual_check_")] %>%
                    map(read_csv)
open_data_manual <- do.call(rbind, open_data_manual) %>%
  mutate(doi = (doi %>% (function(x) x %>%
                           str_remove(fixed(".txt")) %>%
                           str_replace_all(fixed("+"), "/")))) %>%
  distinct(doi, .keep_all = TRUE) %>%
  filter(in_PURE) %>%
  select(doi, open_data_manual_check, open_data_category_manual,
         open_code_manual_check, open_code_category_manual,
         open_data_statements, open_code_statements)

#Open Access results
open_access_files <- results_files[results_files %>% str_detect("Open_Access")]
open_access_results <- read_csv(open_access_files) %>%
  select(-year) %>%
  rename(OA_color = color) %>%
  filter(!is.na(doi)) %>%
  distinct(doi, .keep_all = TRUE)


#Barzooka results
barzooka_files <- results_files[results_files %>% str_detect("Barzooka")]
barzooka_results <- map(barzooka_files, read_csv)
barzooka_results <- do.call(rbind, barzooka_results) %>%
  select(paper_id, bar, pie, bardot, box, dot, hist, violin) %>%
  rename(doi = paper_id) %>%
  distinct(doi, .keep_all = TRUE)


#----------------------------------------------------------------------------------------
# combine results
#----------------------------------------------------------------------------------------

#check if there are no duplicated dois in the results files (problem for left_join)
length(open_data_results$doi) == length(unique(open_data_results$doi))
length(open_data_manual$doi) == length(unique(open_data_manual$doi))
length(open_access_results$doi) == length(unique(open_access_results$doi))
length(barzooka_results$doi) == length(unique(barzooka_results$doi))


dashboard_metrics <- publications %>%
  left_join(open_data_results, by = "doi") %>%
  left_join(open_data_manual, by = "doi") %>%
  left_join(open_access_results, by = "doi") %>%
  left_join(barzooka_results, by = "doi") %>%
  mutate(pdf_downloaded = !is.na(is_open_data))


#still need to filter out some article types, as the filtering by 'article'
#was not sufficient, because one publication can have multiple types
dashboard_metrics <- dashboard_metrics %>%
  filter(Comment == FALSE) %>%
  filter(Editorial == FALSE) %>%
  filter(Erratum == FALSE) %>%
  filter(`Retracted Publication` == FALSE) %>%
  filter(`Retraction of Publication` == FALSE) %>%
  filter(`Conference Abstract` == FALSE) %>%
  filter(`Conference Paper` == FALSE) %>%
  filter(Note == FALSE)


#some of the open data cases were only manually detected
#need to update the is_open_data status for them
od_manual_pos <- (!is.na(dashboard_metrics$open_data_manual_check) &
                    dashboard_metrics$open_data_manual_check == "TRUE")
dashboard_metrics[od_manual_pos,]$is_open_data <- TRUE

no_access_pos <- (!is.na(dashboard_metrics$open_data_manual_check) &
                    dashboard_metrics$open_data_manual_check == "no access")
dashboard_metrics[no_access_pos,]$open_data_manual_check <- "FALSE"

no_od_pos <- (is.na(dashboard_metrics$open_data_manual_check) &
                !is.na(dashboard_metrics$is_open_data))
dashboard_metrics[no_od_pos,]$open_data_manual_check <- "FALSE"

no_check <- (is.na(dashboard_metrics$is_open_data) &
               !is.na(dashboard_metrics$open_data_manual_check))
dashboard_metrics[no_check,]$open_data_manual_check <- NA

#need to do the same cleaning of the manual data for open code
oc_manual_pos <- (!is.na(dashboard_metrics$open_code_manual_check) &
                    dashboard_metrics$open_code_manual_check == TRUE)
dashboard_metrics[oc_manual_pos,]$is_open_code <- TRUE

no_oc_pos <- (is.na(dashboard_metrics$open_code_manual_check) &
                !is.na(dashboard_metrics$is_open_code))
dashboard_metrics[no_oc_pos,]$open_code_manual_check <- FALSE

no_check_oc <- (is.na(dashboard_metrics$is_open_code) &
               !is.na(dashboard_metrics$open_code_manual_check))
dashboard_metrics[no_check_oc,]$open_code_manual_check <- NA


#only select columns relevant for shiny table
shiny_table <- dashboard_metrics %>%
  select(doi, pmid, title, journal_title,
         e_pub_year, pdf_downloaded, OA_color,
         is_open_data, open_data_manual_check, open_data_category_manual,
         is_open_code, open_code_manual_check, open_code_category_manual,
         open_data_statements, open_code_statements,
         bar, pie, bardot, box, dot, hist, violin)

write_csv(shiny_table, "shiny_app/data/dashboard_metrics.csv")


#----------------------------------------------------------------------------------------
# now for the metrics that are already aggregated by year
#----------------------------------------------------------------------------------------

preprints <- read_csv("./results/preprints.csv") %>%
  group_by(year) %>%
  summarize(preprints = n())

policy_citations <- read_csv("./results/policy_citations.csv") %>%
  group_by(year) %>%
  summarize(policy_citations = n())

total_publ_dimensions = read_csv("./results/Charite_publication_ids_dimensions_2006_19.csv") %>%
  group_by(year) %>%
  summarize(total_publ_dimensions = n())

prospective_registration <- read_csv("./results/prospective_registration.csv")
summary_results_12_month <- read_csv("./results/summary_results_12_month.csv")
summary_results_24_month <- read_csv("./results/summary_results_24_month.csv")


shiny_table_aggregate_metrics <- tibble(year = 2006:2019) %>%
  left_join(prospective_registration) %>%
  left_join(summary_results_12_month) %>%
  left_join(summary_results_24_month) %>%
  left_join(policy_citations) %>%
  left_join(total_publ_dimensions) %>%
  left_join(preprints)

write_csv(shiny_table_aggregate_metrics, "shiny_app/data/dashboard_metrics_aggregate.csv")
