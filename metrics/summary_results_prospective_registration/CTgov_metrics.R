library(tidyverse)
library(lubridate)

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
AACT_folder <- "C:/Datenablage/AACT/AACT dataset 20200603/" #insert the AACT download folder here

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
# Load search terms for the affiliations/cities
#----------------------------------------------------------------------------------------------------------------------

city_search_terms <- c("Charite", "Charité")
city_search_terms <-  paste0("\\b", city_search_terms, "\\b", collapse = "|")

#----------------------------------------------------------------------------------------------------------------------
#  search for studies with primary sponsor affiliations matching the search terms
#----------------------------------------------------------------------------------------------------------------------

grep_fast <- function(pattern, x)
{
  return(which(str_detect(x, pattern)))
}

get_nct <- function(affil_indices, dataset)
{
  ncts <- dataset %>%
    slice(affil_indices) %>%
    select(nct_id)
  return(ncts[[1]])
}

city_grep <- function(dataset, colname, grep_terms)
{
  indices <- map(tolower(grep_terms), grep_fast, x=tolower(dataset[[colname]]))
  city_ncts <- map(indices, get_nct, dataset=dataset)
  names(city_ncts) <- names(grep_terms)

  return(city_ncts)
}

#joining of the different grep results
affil_join <- function(affil_nct_list)
{
  affil_indices_joined <- affil_nct_list %>%
    pmap(c) %>%
    map(unique) %>%
    map(sort)
}

#lead sponsor if affil found in fields (PI/sponsor/responsible_party)
search_NCTS_lead_sponsor <- function(AACT_datasets, city_search_terms)
{
  #search the different affilation datasets for the city search terms
  grep_PI <- city_grep(AACT_datasets$overall_officials, "affiliation", city_search_terms)
  grep_sponsor <- city_grep(AACT_datasets$sponsors %>% filter(lead_or_collaborator == "lead"),
                            "name", city_search_terms)
  grep_resp_party_org <- city_grep(AACT_datasets$responsible_parties, "organization", city_search_terms)
  grep_resp_party_affil <- city_grep(AACT_datasets$responsible_parties, "affiliation", city_search_terms)

  #combine the results for the different fields
  grep_results_primary <- list(grep_PI, grep_sponsor, grep_resp_party_org, grep_resp_party_affil)
  affil_ncts_primary <- affil_join(grep_results_primary)

  return(affil_ncts_primary)
}

institutions_ncts_primary <- search_NCTS_lead_sponsor(AACT_datasets, city_search_terms)[[1]]


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
  filter(nct_id %in% institutions_ncts_primary) %>%
  filter(study_type == "Interventional")


#results for the prospective registration metric
summary_prosp_reg <- CTgov_sample_Charite %>%
  map(filter, start_date > "2006-01-01") %>%
  map(filter, start_date < "2019-12-31") %>%
  map(function(x) table(year(x[["start_date"]]), x[["has_prospective_registration"]])) %>%
  map(function(x) tibble(year = rownames(x), no_prosp_reg = x[,1],
                         has_prosp_reg = x[,2], perc_prosp_reg = x[,2]/rowSums(x)))

summary_table_prosp_reg <- summary_prosp_reg[[1]]

write_csv(summary_table_prosp_reg, "results/prospective_registration.csv")


#results for the summary results registration metric
summary_sum_res_12 <- CTgov_sample_Charite %>%
  map(filter, completion_date > "2006-01-01") %>%
  map(filter, completion_date < "2018-12-31") %>%
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
  map(filter, completion_date < "2017-12-31") %>%
  map(filter, study_type == "Interventional") %>%
  map(filter, overall_status %in% c("Completed", "Unknown status", "Terminated", "Suspended")) %>%
  map(function(x) table(year(x[["completion_date"]]), x[["summary_result_24_month"]])) %>%
  map(function(x) tibble(year = rownames(x), no_sum_res_24 = x[,1],
                         has_sum_res_24 = x[,2], perc_sum_res_24 = x[,2]/rowSums(x)))

summary_table_sum_res_24 <- summary_sum_res_24[[1]]

write_csv(summary_table_sum_res_24, "results/summary_results_24_month.csv")

