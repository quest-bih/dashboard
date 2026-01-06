library(tidyverse)
# devtools::install_github("serghiou/rtransparent")
library(rtransparent)
library(oddpub)
library(furrr)
library(here)
library(progressr)

txt_folder <- "C:/Datenablage/charite_dashboard/2024/PDFs_to_text/"
# txt_folder <- "C:/Datenablage/charite_dashboard/unified_dataset/PDFs_to_text/"
# txt_folder <- "dev/rescreen_txt"

plan(multisession)
# plan(sequential)
handlers(global = TRUE)

txt_corpus <- pdf_load(txt_folder, lowercase = FALSE)

# txt_corpus <- txt_corpus |>
#   furrr::future_map(\(x) paste(x, collapse = "\n"), .progress = TRUE)

# rt_all(txt_corpus[["10.1038+s41569-022-00729-2.txt"]])

# res <- furrr::future_map(txt_corpus, rt_all, .progress = TRUE)

# results <- res |>
#   set_names(names(txt_corpus)) |>
#   list_rbind(names_to = "filename") |>
#   mutate(doi = str_replace_all(filename, "\\+", "\\/") |>
#            str_remove(".txt")) |>
#   select(doi, everything(), -filename, -article, -pmid)
#
# results |>
#   write_excel_csv(here("results", "rtransparent_2023.csv"))


if (file.exists(here("results", "rtransparent_old.csv"))) {
  already_screened_PDFs <- read_csv(here("results", "rtransparent_old.csv")) |>
    mutate(article = paste0(doi, ".txt") |> str_replace_all("\\/", "\\+"))

  txt_corpus <- txt_corpus[!(names(txt_corpus) %in% already_screened_PDFs$article)]

  txt_corpus <- txt_corpus |>
    furrr::future_map(\(x) paste(x, collapse = "\n"), .progress = TRUE)

  if (length(txt_corpus) > 0) {
    message("Run rtransparent...")
    res <- furrr::future_map(txt_corpus, rt_all, .progress = TRUE)

    results <- res |>
      set_names(names(txt_corpus)) |>
      list_rbind(names_to = "filename") |>
      mutate(doi = str_replace_all(filename, "\\+", "\\/") |>
               str_remove(".txt")) |>
      select(doi, everything(), -filename, -article, -pmid)

    binary_results <- results |>
      transmute(doi = tolower(doi),
                has_coi = is_coi_pred,
                has_funding = is_funded_pred
      )
    write_excel_csv(binary_results, here("results", "rtransparent_old.csv"), append = TRUE)

  }

} else {
  txt_corpus <- txt_corpus |>
    furrr::future_map(\(x) paste(x, collapse = "\n"), .progress = TRUE)
  message("Run rtransparent...")
  res <- furrr::future_map(txt_corpus, rt_all, .progress = TRUE)

  results <- res |>
    set_names(names(txt_corpus)) |>
    list_rbind(names_to = "filename") |>
    mutate(doi = str_replace_all(filename, "\\+", "\\/") |>
             str_remove(".txt")) |>
    select(doi, everything(), -filename, -article, -pmid)

  write_excel_csv(results, here("results", "rtransparent_full.csv"))

}

write_excel_csv(results, here("results", "rtransparent_full.csv"))

# results <- rr
# text <- rt_read_pdf("C:/Datenablage/charite_dashboard/2023/PDFs/10.3389+fnagi.2023.1204134.pdf")
#
# text <- rt_read_pdf("C:/Datenablage/charite_dashboard/unified_dataset/PDFs/10.1007+s00213-020-05691-w.pdf")
# results <- rt_all(txt_corpus[["10.1001+jamapsychiatry.2023.2949.txt"]])

# read_csv(here("results", "rtransparent_old.csv")) |>
#   distinct(doi, .keep_all = TRUE) |>
#   write_excel_csv(here("results", "rtransparent_old.csv"))

results <- rt_all(txt_corpus[["10.1001+jamanetworkopen.2024.25114.txt"]])
