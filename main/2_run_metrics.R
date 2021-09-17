library(tidyverse)

#run the different metrics - some need additional

#Open Data - needs downloaded PDFs
source("metrics/Open_Data/Open_Data.R")

#Open Access
source("metrics/Open_Access/Open_Access.R")

#orcid
source("metrics/ORCID/orcid.R")

#Barzooka - need to insert PDF location for downloaded PDFs
"metrics/Barzooka/dashboard_run_barzooka.py"

#preprints - need Dimensions API access
"metrics/preprints/preprints.ipynb"

#EU Trialstracker
#currently only downloads the most recent numbers
source("metrics/summary_results_prospective_registration/EU_Trials_Tracker_scraper.R")

#prospective registration & timely reporting
#prosp. reg. needs AACT dataset
#timely reporting from published dataset: https://doi.org/10.5281/zenodo.5141343
source("metrics/summary_results_prospective_registration/CTgov_metrics.R")
