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
  rename(doi = article)

#convert dois to standard format
open_data_results$doi <- open_data_results$doi %>%
  str_remove(fixed(".txt")) %>%
  str_replace_all(fixed("+"), fixed("/"))


#Open Access results
open_access_files <- results_files[results_files %>% str_detect("Open_Access")]
open_access_results <- read_csv(open_access_files) %>%
  select(-year) %>%
  rename(OA_color = color) %>%
  filter(!is.na(doi))


#Barzooka results
barzooka_files <- results_files[results_files %>% str_detect("Barzooka")]
barzooka_results <- map(barzooka_files, read_csv)
barzooka_results <- do.call(rbind, barzooka_results) %>%
  select(paper_id, bar, pie, bardot, box, dot, hist, violin) %>%
  rename(doi = paper_id)


#----------------------------------------------------------------------------------------
# combine results
#----------------------------------------------------------------------------------------

#check if there are no duplicated dois in the results files (problem for left_join)
length(open_data_results$doi) == length(unique(open_data_results$doi))
length(open_access_results$doi) == length(unique(open_access_results$doi))
length(barzooka_results$doi) == length(unique(barzooka_results$doi))


dashboard_metrics <- publications %>%
  left_join(open_data_results) %>%
  left_join(open_access_results) %>%
  left_join(barzooka_results) %>%
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


#only select columns relevant for shiny table
shiny_table <- dashboard_metrics %>%
  select(doi, pmid, title, journal_title,
         e_pub_year, pdf_downloaded,
         is_open_data, is_open_code, OA_color,
         bar, pie, bardot, box, dot, hist, violin)

write_csv(shiny_table, "shiny_app/data/dashboard_metrics.csv")
