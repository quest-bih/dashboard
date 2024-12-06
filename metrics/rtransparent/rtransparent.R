library(tidyverse)
library(rtransparent)
library(oddpub)
library(furrr)
library(here)

txt_folder <- "C:/Datenablage/charite_dashboard/2023/PDFs_to_text/"

plan(multisession)

txt_corpus <- pdf_load(txt_folder, lowercase = FALSE)

txt_corpus <- txt_corpus |>
  furrr::future_map(\(x) paste(x, collapse = "\n"), .progress = TRUE)

res <- furrr::future_map(txt_corpus, rt_all, .progress = TRUE)

results <- res |>
  set_names(names(txt_corpus)) |>
  list_rbind(names_to = "filename") |>
  mutate(doi = str_replace_all(filename, "\\+", "\\/") |>
           str_remove(".txt")) |>
  select(doi, everything(), -filename, -article, -pmid) |>
  write_excel_csv(here("results", "rtransparent_2023.csv"))




# results <- rr
# text <- rt_read_pdf("C:/Datenablage/charite_dashboard/2023/PDFs/10.3389+fnagi.2023.1204134.pdf")
#
# text <- rt_read_pdf("C:/Datenablage/charite_dashboard/unified_dataset/PDFs/10.1007+s00213-020-05691-w.pdf")
# results <- rt_all(txt_corpus[["10.1001+jamapsychiatry.2023.2949.txt"]])
