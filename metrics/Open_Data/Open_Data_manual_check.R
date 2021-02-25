#prepare the automated results for the manual check for false positives and Open Data/Code categories

library(tidyverse)

#load PURE dataset
load("./main/status_quo_table_2020.RData")

open_data_manual <- read_csv("./results/OD_manual_check/Open_Data_2015-18_manual_check.csv")
open_data_oddpub <- read_csv("./results/OD_manual_check/Open_Data_2015-18_cat.csv") %>%
  rename(is_open_data_oddpub = is_open_data) %>%
  select(-year)

open_data_oddpub <- open_data_oddpub %>%
    mutate(doi = article %>% (function(x) x %>%
                                str_remove(fixed(".txt")) %>%
                                str_replace_all(fixed("+"), "/"))) %>%
  select(-article)


open_data_manual_comb <- open_data_manual %>%
  left_join(open_data_oddpub) %>%
  mutate(in_PURE = doi %in% status_quo_table_save$doi)

write_csv(open_data_manual_comb, "./results/OD_manual_check/Open_Data_2015-18_manual_check_comb.csv")


#-------------------------------------------------------------------------------------------------------
# Open Data 2020
#-------------------------------------------------------------------------------------------------------

OD2020 <- read_csv("results/OD_manual_check/Open_Data_2020_manual_check.csv")
OD2020 <- OD2020 %>%
  filter(is_open_data_manual | is_open_code_manual) %>%
  mutate(year = "2020") %>%
  mutate(in_PURE = doi %in% status_quo_table_save$doi) %>%
  rename(is_open_data_oddpub = is_open_data,
         is_open_code_oddpub = is_open_code,
         open_data_manual_check = is_open_data_manual,
         open_code_manual_check = is_open_code_manual) %>%
  select(doi, year, in_PURE, is_open_data_oddpub, open_data_manual_check, open_data_category,
         open_data_category_manual, is_open_code_oddpub, open_code_manual_check,
         open_code_category_manual, open_data_statements, open_code_statements)

write_csv(OD2020, "results/OD_manual_check_2020.csv")
