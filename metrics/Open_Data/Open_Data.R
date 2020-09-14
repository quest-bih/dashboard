library(tidyverse)

#conversion of PDFs
pdf_main_folder <- "C:/Datenablage/charite_dashboard/PDFs"
txt_main_folder <- "C:/Datenablage/charite_dashboard/PDFs_to_text"

pdf_folders <- list.dirs(pdf_main_folder)[-1]

for(pdf_folder in pdf_folders)
{
  subfolder_name <- pdf_folder %>% str_split("/") %>% map_chr(tail, 1)
  txt_folder <- paste0(txt_main_folder, "/", subfolder_name)
  if(!dir.exists(txt_folder)) {
    dir.create(txt_folder)
  }
  conversion_success <- oddpub::pdf_convert(pdf_folder, txt_folder)
}


#run ODDPub
txt_folders <- list.dirs(txt_main_folder)[-1]

for(txt_folder in txt_folders)
{
  year <- txt_folder %>% str_split("/") %>% map_chr(last)
  print(year)

  pdf_text_corpus <- oddpub::pdf_load(paste0(txt_folder, "/"))
  oddpub_results <- oddpub::open_data_search_parallel(pdf_text_corpus)
  oddpub_results["year"] <- year
  write_csv(oddpub_results, paste0("./results/Open_Data_", year, ".csv"))
}
