library(tidyverse)
library(lubridate)

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
# AACT_folder <- "C:/Datenablage/AACT/AACT dataset 20210222/" #insert the AACT download folder here
AACT_folder <- "C:/Datenablage/AACT/AACT_dataset_220920/" #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values")

load_AACT_dataset_from_txt <- function(AACT_folder, AACT_dataset_names) {
  AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".txt")
  AACT_datasets <- AACT_dataset_files %>%
    map(read_delim, delim = "|", guess_max = 10000)
  names(AACT_datasets) <- AACT_dataset_names

  return(AACT_datasets)
}

#
# load_AACT_dataset_from_csv <- function(AACT_folder, AACT_dataset_names) {
#   AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".csv")
#   AACT_datasets <- AACT_dataset_files %>%
#     map(read_csv, guess_max = 10000)
#   names(AACT_datasets) <- AACT_dataset_names
#
#   return(AACT_datasets)
# }

AACT_datasets <- load_AACT_dataset_from_txt(AACT_folder, AACT_dataset_names)


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
  mutate(days_compl_to_summary = results_first_submitted_date - completion_date) %>%
  mutate(days_reg_to_start = start_date - study_first_submitted_date)

#calculate the metrics of interest
CTgov_sample_full <- CTgov_sample_full %>%
  mutate(has_prospective_registration = floor_date(start_date, unit = "month") >=
                                        floor_date(study_first_submitted_date, unit = "month")) %>%
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
  map(filter, start_date < "2021-12-31") %>%
  map(function(x) table(year(x[["start_date"]]), x[["has_prospective_registration"]])) %>%
  map(function(x) tibble(year = rownames(x), no_prosp_reg = x[,1],
                         has_prosp_reg = x[,2], perc_prosp_reg = round(x[,2]/rowSums(x), 3)))

summary_table_prosp_reg <- summary_prosp_reg[[1]]

write_csv(summary_table_prosp_reg, "results/prospective_registration.csv")


#save table with individual trials to display them in the Shiny app
prosp_reg_dataset_shiny <- CTgov_sample_Charite[[1]] %>%
  filter(start_date >= "2006-01-01") %>%
  filter(start_date <= "2021-12-31") %>%
  select(nct_id, start_date, study_first_submitted_date,
         days_reg_to_start, has_prospective_registration)

write_csv(prosp_reg_dataset_shiny, "results/prosp_reg_dataset_shiny.csv")


#----------------------------------------------------------------------------------------------------------------------
# get timely publication numbers from the combined iv dataset
#----------------------------------------------------------------------------------------------------------------------

iv_dataset <- read_csv("https://zenodo.org/record/5141343/files/iv_main_dataset.csv?download=1")

iv_dataset_charite <- iv_dataset %>%
  #get minimum of days to pub or to summary result
  mutate(days_to_publ = pmin(days_cd_to_publication,
                             days_cd_to_summary, na.rm = TRUE),
         iv_version = case_when(
           iv_version == 1 & !is_dupe  ~ "IV1",
           iv_version == 1 & is_dupe  ~ "IV1_dupl",
           iv_version == 2 & !is_dupe  ~ "IV2",
           iv_version == 2 & is_dupe  ~ "IV2_dupl")) %>%
  filter(lead_cities %>% str_detect("Berlin"),
         #get newest results for each trial
         iv_version %in% c("IV1", "IV2", "IV2_dupl"))


calc_summary <- function(iv_data, years)
{
  cutoff_date_IV1 <- dmy("01.12.2017") - months(years * 12)
  cutoff_date_IV2 <- dmy("01.09.2020") - months(years * 12)

  has_long_followup_IV1 <- (iv_data$completion_date < cutoff_date_IV1)
  has_long_followup_IV2 <- (iv_data$completion_date < cutoff_date_IV2)
  is_IV1 <- iv_data[["iv_version"]] %in% c("IV1", "IV1_dupl")
  is_IV2 <- iv_data[["iv_version"]] %in% c("IV2", "IV2_dupl")

  has_long_followup <- (has_long_followup_IV1 & is_IV1) | (has_long_followup_IV2 & is_IV2)

  iv_summary <- iv_data[has_long_followup,] %>%
    group_by(completion_year) %>%
    summarize(trials_with_publ = sum(days_to_publ < years*365,
                                     na.rm = TRUE),
              total_trials     = n(),
              percentage_publ  = (trials_with_publ/
                                    total_trials) %>%
                round(3))

  colnames(iv_summary) <- c("completion_year",
                       paste0("trials_with_publication_", years, "_years"),
                       paste0("total_trials_", years, "_years"),
                       paste0("percentage_published_", years, "_years"))

  return(iv_summary)
}

iv_results_2years <- iv_dataset_charite %>% calc_summary(2)
iv_results_5years <- iv_dataset_charite %>% calc_summary(5)

iv_results_years <- iv_results_2years %>%
  left_join(iv_results_5years, by = "completion_year")

write_csv(iv_results_years, "./shiny_app/data/IntoValue_Results_years.csv")

