library(tidyverse)
library(assertthat)

print("Open Data detection with oddpub...")

pdf_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/PDFs/"
txt_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/PDFs_to_text/"

print("Convert pdfs to text...")
conversion_success <- oddpub::pdf_convert(pdf_folder, txt_folder)

print("Load txt files...")

#only screen new PDFs
if(file.exists("./results/Open_Data.csv"))
{
  already_screened_PDFs <- read_csv("./results/Open_Data.csv")

  pdf_text_corpus <- oddpub::pdf_load(txt_folder)
  pdf_text_corpus <- pdf_text_corpus[!(names(pdf_text_corpus) %in% already_screened_PDFs$article)]

  if(length(pdf_text_corpus) > 0) {
    oddpub_results <- oddpub::open_data_search_parallel(pdf_text_corpus)
    write_csv(oddpub_results, paste0("./results/Open_Data.csv"), append = TRUE)
  }

} else {
  pdf_text_corpus <- oddpub::pdf_load(txt_folder)

  print("Run oddpub...")
  oddpub_results <- oddpub::open_data_search_parallel(pdf_text_corpus)
  write_csv(oddpub_results, paste0("./results/Open_Data.csv"))
}

print("completed!")


#add columns for manual check & write to csv -
#this table is then used as a basis for the manual check of the results

template_filename <- "./results/Open_Data_manual_check_template.csv"

oddpub_results <- read_csv("./results/Open_Data.csv")
oddpub_results_manual_check <- oddpub_results %>%
  mutate(open_data_manual_check = NA,
         open_data_category_manual = "",
         open_code_manual_check = NA,
         open_code_category_manual = "",
         doi = article %>% str_remove(fixed(".txt")) %>%
                           str_replace_all(fixed("+"), "/")) %>%
  select(doi, is_open_data, open_data_category, open_data_manual_check,
         open_data_category_manual, open_data_statements, is_open_code,
         open_code_manual_check, open_code_category_manual, open_code_statements)


#this is the file with the manually checked results, already update the template file with them,
#such that only the remaning cases need to be checked manually
manual_check_results <- read_delim("./results/Open_Data_manual_check_results.csv", delim = ";")
oddpub_results_manual_check <- rows_update(oddpub_results_manual_check,
                                           manual_check_results, by = "doi")

#now the new Open Data cases in the saved template file have to be checked manually
oddpub_results_manual_check %>% write_csv(template_filename)


#assertions for validity check of the manual results
OD_manual_num <- sum(manual_check_results$open_data_manual_check, na.rm = TRUE)
OD_manual_categories_num <- sum(!is.na(manual_check_results$open_data_category_manual))
assert_that(OD_manual_num == OD_manual_categories_num)

OC_manual_num <- sum(manual_check_results$open_code_manual_check, na.rm = TRUE)
OC_manual_categories_num <- sum(!is.na(manual_check_results$open_code_category_manual))
assert_that(OC_manual_num == OC_manual_categories_num)

