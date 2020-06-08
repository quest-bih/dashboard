library(tidyverse)
library(lubridate)

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
AACT_folder <- "C:/Datenablage/AACT/AACT dataset Nov 2019/" #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values")

load_AACT_dataset <- function(AACT_folder, AACT_dataset_names) {
  AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".txt")
  AACT_datasets <- AACT_dataset_files %>%
    map(read_delim, delim = "|", guess_max = 10000)
  names(AACT_datasets) <- AACT_dataset_names

  return(AACT_datasets)
}

AACT_datasets <- load_AACT_dataset(AACT_folder, AACT_dataset_names)


#----------------------------------------------------------------------------------------------------------------------
# add manually checked dataset for German institutions
#----------------------------------------------------------------------------------------------------------------------

german_lead_trials <- read_csv("metrics/summary_results_prospective_registration/german_UMCs_lead_trials_manually_checked.csv")

split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[,col])
german_lead_trials_ncts <- split_tibble(german_lead_trials, "city") %>% map(2)

charite_trials <- german_lead_trials_ncts$Berlin


#----------------------------------------------------------------------------------------------------------------------
# reduce the CTgov dataset to those studies that are indeed affiliated
# and filter for primary completion years & study status
#----------------------------------------------------------------------------------------------------------------------

CTgov_sample_full <- AACT_datasets$studies %>%
  left_join(AACT_datasets$calculated_values %>%
              select(nct_id, were_results_reported), by = "nct_id")

#add calculated columns to dataset - time to summary results & prospective registration
CTgov_sample_full <- CTgov_sample_full %>%
  mutate(days_prim_compl_to_summary = results_first_submitted_date - primary_completion_date) %>%
  mutate(days_compl_to_summary = results_first_submitted_date - completion_date) %>%
  mutate(days_reg_to_start = start_date - study_first_submitted_date)

#calculate the metrics of interest
CTgov_sample_full <- CTgov_sample_full %>%
  mutate(has_prospective_registration = days_reg_to_start > -60) %>%
  mutate(summary_result_12_month = days_compl_to_summary < 365) %>%
  mutate(summary_result_24_month = days_compl_to_summary < 2*365) %>%
  mutate(summary_result_12_month = replace_na(summary_result_12_month, FALSE)) %>%
  mutate(summary_result_24_month = replace_na(summary_result_24_month, FALSE))


#now filter the trials for each institution into separate tables to summarize the results
CTgov_sample_Charite <- list()
CTgov_sample_Charite[[1]] <- CTgov_sample_full %>%
  filter(nct_id %in% charite_trials)


#results for the prospective registration metric
summary_prosp_reg <- CTgov_sample_Charite %>%
  map(filter, start_date > "2006-01-01") %>%
  map(filter, start_date < "2019-11-29") %>%
  map(function(x) table(year(x[["start_date"]]), x[["has_prospective_registration"]])) %>%
  map(function(x) tibble(year = rownames(x), no_prosp_reg = x[,1],
                         has_prosp_reg = x[,2], perc_prosp_reg = x[,2]/rowSums(x)))

summary_table_prosp_reg <- summary_prosp_reg[[1]]

write_csv(summary_table_prosp_reg, "results/prospective_registration.csv")


#results for the summary results registration metric
summary_sum_res_12 <- CTgov_sample_Charite %>%
  map(filter, completion_date > "2006-01-01") %>%
  map(filter, completion_date < "2018-11-29") %>%
  map(filter, study_type == "Interventional") %>%
  map(filter, overall_status %in% c("Completed", "Unknown status", "Terminated", "Suspended")) %>%
  map(function(x) table(year(x[["completion_date"]]), x[["summary_result_12_month"]])) %>%
  map(function(x) tibble(year = rownames(x), no_sum_res_12 = x[,1],
                         has_sum_res_12 = x[,2], perc_sum_res_12 = x[,2]/rowSums(x)))

summary_table_sum_res_12 <- summary_sum_res_12[[1]]

write_csv(summary_table_sum_res_12, "results/summary_results_12_month.csv")


#results for the summary results registration metric
summary_sum_res_24 <- CTgov_sample_Charite %>%
  map(filter, completion_date > "2006-01-01") %>%
  map(filter, completion_date < "2017-11-29") %>%
  map(filter, study_type == "Interventional") %>%
  map(filter, overall_status %in% c("Completed", "Unknown status", "Terminated", "Suspended")) %>%
  map(function(x) table(year(x[["completion_date"]]), x[["summary_result_24_month"]])) %>%
  map(function(x) tibble(year = rownames(x), no_sum_res_24 = x[,1],
                         has_sum_res_24 = x[,2], perc_sum_res_24 = x[,2]/rowSums(x)))

summary_table_sum_res_24 <- summary_sum_res_24[[1]]

write_csv(summary_table_sum_res_24, "results/summary_results_24_month.csv")

