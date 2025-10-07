library(openalexR)
library(europepmc)
library(tidyverse)
library(here)
library(janitor)
library(fuzzyjoin)
library(progressr)

handlers(global = TRUE)

query_works_2025 <- oa_query(
  entity = "works",
  # authorships.institutions.id = "I4210139777", # BIH id at openAlex
  authorships.institutions.ror = c("https://ror.org/001w7jn25", "https://ror.org/0493xsw21"),
  #charite: https://ror.org/001w7jn25
  from_publication_date = "2016-01-01",
  to_publication_date = "2024-12-31"
  # version = "submittedVersion"
  # type = "preprint"
)


res <- oa_request(query_url = query_works_2024, mailto = Sys.getenv("EMAIL"))

df <- oa2df(res, entity = "works")

df |>
  count(type, sort = TRUE)

# preprints_unkn <- df |>
#   filter(!str_detect(url, "10\\.1101|preprint|\\/rs\\.|xiv|\\/au\\.|zenodo|ssrn"),
#          is.na(version) | version == "submittedVersion",
#          type == "preprint")

deversion_doi <- function(doi_string) {
  doi_string |>
    stringr::str_remove("\\Wv\\d$|(?<=(f1000research|elife)\\.\\d{2,6})\\.\\d$")
}

dedupe_by_col <- function(oa_tib, group_col) {

  dupes <- janitor::get_dupes(oa_tib, {{ group_col }})
  # print(nrow(dupes))

  dupes_deduped <- dupes |>
    dplyr::arrange({{ group_col}}, doi) |>
    dplyr::group_by({{ group_col }}) |>
    dplyr::mutate(only_rs = sum(stringr::str_detect(doi, "10\\.21203")) == 2,
                  one_ssrn = sum(stringr::str_detect(doi, "ssrn")) == 1,
                  is_dupe = {{ group_col }} == dplyr::lag({{ group_col }}) |
                    {{ group_col }} == dplyr::lead({{ group_col }}),
                  is_dupe = is_dupe & dplyr::case_when(
                    only_rs == TRUE & is.na(dplyr::lag(doi)) ~ TRUE,
                    stringr::str_detect(doi, "pdf|http\\:") ~ TRUE,
                    one_ssrn == TRUE & stringr::str_detect(doi, "ssrn") ~ TRUE,
                    only_rs == FALSE & stringr::str_detect(doi, "10\\.21203") == TRUE ~ TRUE,
                    stringr::str_detect(doi, "chemrxiv|peerj") & !stringr::str_detect(doi, "v\\d$") ~ TRUE,
                    .default = FALSE
                  )) |>
    dplyr::filter(is_dupe == FALSE) |>
    dplyr::select(-only_rs, -is_dupe, -dupe_count, -one_ssrn) |>
    dplyr::ungroup()

  # print(nrow(dupes_deduped))

  if (nrow(dupes) > 0 & nrow(dupes) != nrow(dupes_deduped) * 2) {
    warning(paste("Deduplication failed! Check cases:",
                   dupes_deduped |>
                    janitor::get_dupes({{ group_col }}) |>
                    dplyr::pull(doi)
                    ))
  }

  oa_tib |>
     dplyr::filter(!{{ group_col }} %in% (dupes_deduped |> dplyr::pull({{ group_col }}))) |>
     dplyr::bind_rows(dupes_deduped)


}

parsed <- df |>
  filter(
    !is.na(doi),
    is.na(version) | version == "submittedVersion",
    # !is.na(abstract),
    type == "preprint" ,
    is_retracted == FALSE,
    !str_detect(landing_page_url, "ems|10\\.14293|egusphere|morressier|pubmed|espost|10\\.1016|protocols|elife|biofilms")
    ) |>
  distinct(display_name, .keep_all = TRUE) |>
  mutate(authors = map_chr(authorships, \(au) pull(au, display_name) |> paste0(collapse = "; ")),
         affils = map_chr(authorships, \(au) pull(au, affiliation_raw) |> paste0(collapse = "; ")),
         affils_st = map_chr(authorships, \(aff) pull(aff, affiliations) |> map(\(affil) affil |>
                                                                                  pull(display_name))  |>
                               unique() |>
                               paste0(collapse = "; ")),
         doi = str_remove(landing_page_url, ".*\\/(?=10\\.)"),
         # url = str_replace(doi, "(?<=v)\\d$", "1")
         url = deversion_doi(doi)
         ) |>
  distinct(url, .keep_all = TRUE) |>
  select(title = display_name, doi, url, authors, affils, affils_st, publication_date, publication_year, journal = source_display_name,
         license, type, version, is_oa, pdf_url) |>
  mutate(journal_st = case_when(
    str_detect(doi, "10.1101") & str_length(doi) < 27 ~ "bioRxiv (Cold Spring Harbor Laboratory)",
    str_detect(doi, "10.1101") & str_length(doi) > 25 ~ "medRxiv (Cold Spring Harbor Laboratory)",
    # str_detect(doi, "10\\.20944|") | str_detect(doi, "preprints\\.org") ~ "Preprints.org",
    str_detect(doi, "10\\.20944") | str_detect(doi, "preprints\\.\\d{4,}") ~ "JMIR Publications",
    str_detect(doi, "10.2139") ~ "Social Science Research Network",
    str_detect(doi, "bxiv") ~ "Beilstein Archives",
    str_detect(doi, "10.37044") ~ "BioHackrXiv",
    str_detect(doi, "techrxiv") ~ "TechRxiv",
    str_detect(doi, "10.21203") ~ "Research Square",
    str_detect(doi, "10.22541") ~ "Authorea",
    str_detect(doi, "10.26434") ~ "Biological and Medicinal Chemistry (ChemRxiv)",
    str_detect(doi, "10.31234") ~ "PsyArXiv",
    str_detect(doi, "10.31222") ~ "MetaArXiv",
    str_detect(doi, "10.31219") ~ "OSF Preprints",
    str_detect(doi, "10.31231") ~ "MindRxiv",
    str_detect(doi, "10.31232") ~ "Nutrixiv",
    str_detect(doi, "10.31235") ~ "SocArXiv",
    str_detect(doi, "10.33774") ~ "Theoretical and Computational Chemistry (ChemRxiv)",
    str_detect(doi, "10.7287") ~ "PeerJ Preprints",
    str_detect(doi, "refubium") ~ "Refubium",
    .default = journal
  ),
    has_quest_bih = str_detect(affils, "QUEST|Berlin Institute of Health")) |>
  filter(!str_detect(affils, "Charité Research Organisation GmbH")) |>
  arrange(publication_date, title) |>
  dedupe_by_col(title)

# str_extract(doi, ".*(?=\\/)")

## check for potentially missed preprints here, everything left after checking should be filtered out
unknown_preprints <- parsed |>
  filter(is.na(journal_st))

parsed |>
  count(journal_st, sort = TRUE)



parsed |>
  count(publication_year)

charite_regex <- c("Universit(ä|ae?)ts(m|M)edizin",
                   "Berlin Institute of Health",
                   "University Medic?ine,? Berlin",
                   "Charit(e|é) University (School of )?Medicine",
                   "Charit(e|é) University (Medicine|Hospital)",
                   "Charit(e|é)(-| )?Universit",
                   "Charit(e|é) and Humboldt",
                   "Charit(e|é),? Medical School",
                   "Charit(e|é) (- )?Medical University Berlin",
                   "Charit(e|é) - Berlin",
                   "Charit(e|é) (- )?Univers(t)?itätsmedizin Berlin",
                   "Charit(e|é) Hospital, University of Berlin",
                   "Charité, Charitéplatz 1",
                   "Charit(e|é), Berlin",
                   "Campus Charité",
                   "University Medical Center-Charité",
                   "Charit(e|é), Unive",
                   "Charit(e|é) CCM",
                   "Campus Charite Mitte",
                   "Charite, Chariteplatz",
                   "Experimental and Clinical Research Center") |>
  paste(collapse = "|")


not_charite_regex <- c("Vivantes GmbH",
                       "Charitéplatz",
                       "Charitéstr",
                       "FMP im Charité")


not_charite <- parsed |>
  filter(!str_detect(affils, charite_regex),
         !str_detect(affils_st, charite_regex),
         !url %in% c("10.1101/234492",
                     "10.1101/2022.10.03.510596"))

chplz1 <- not_charite |>
  filter(str_detect(affils, "Charitéplatz|Charitéstr") |
           str_detect(affils_st, "Charitéplatz|Charitéstr"))

not_charite_not_chplz1 <- not_charite |>
   filter(!doi %in% chplz1$doi)

str_detect("Charité Universtitätsmedizin Berlin", charite_regex)

dch <- not_charite |>
  filter(!doi %in% not_charite_not_chplz1$doi)


"10.1101/234492" # missing charite affil but has charite affil nonetheless
#NABU is at Charitestrasse
# noncharite affils, chariteplatz 1
# is Einstein Center for Neurosciences Charite or not? Aparently not

not_charite |> filter(str_detect(doi, "10.1101/2021.10.30.21265692")) |>  pull(affils)

aut <- df |>  filter(str_detect(landing_page_url, "10.1101/2021.10.30.21265692")) |> pull(authorships) |> list_rbind()

preprints_oa <- parsed |>
  filter(!url %in% not_charite$url,
         !is.na(journal_st)) |>
  select(title, doi, url, research_org_names = affils, research_orgs = affils_st,
         year = publication_year, journal_title = journal_st)

parsed_old <- read_csv(here("results", "preprints_oa_old.csv"))

preprints_oa_new <- parsed_old |>
  rows_upsert(preprints_oa, by = "doi") |>
  mutate(has_published_version = FALSE) |>
  dedupe_by_col(title)

preprints_oa_new <- preprints_oa_new |>
  write_excel_csv(here("results", "preprints_oa.csv"))

preprints_oa_new <- read_csv(here("results", "preprints_oa.csv"))

preprints_oa_new |>
  # dedupe_by_col(title) |>
  count(year)

parsed_old |>
  count(year)

new_preprints_only <- preprints_oa_new |>
  filter(!doi %in% parsed_old$doi)


########### checking which preprints have publications associated with them
### function taken from: https://zenodo.org/records/13133119
get_epmc_metadata <- function(query){
  hits <- epmc_hits(query = query)
  raw <- epmc_search(query = query, output = "raw", limit = hits, sort = "date")
  parsed <- raw %>% {
    tibble(
      id = map_chr(., "id", .default = NA_character_),
      doi = map_chr(., "doi", .default = NA_character_),
      source = map_chr(., "source", .default = NA_character_),
      firstPublicationDate = as_date(map_chr(., "firstPublicationDate",
                                             .default = NA_character_)),
      firstIndexDate = as_date(map_chr(., "firstIndexDate",
                                       .default = NA_character_)),
      publisher = map_chr(., pluck, "bookOrReportDetails", "publisher",
                          .default = NA_character_),
      citedByCount = map_int(., "citedByCount", .default = NA_integer_),
      version = map_int(., "versionNumber", .default = NA_integer_),
      inEPMC = map_chr(., "inEPMC", .default = NA_integer_),
      commentCorrection = map(., pluck, "commentCorrectionList",
                              "commentCorrection", .default = NA_character_),
      authorId = map(., pluck, "authorIdList", "authorId", .default = NA_character_)
    )
  } %>%
    mutate(
      hasPublishedVersion = str_detect(paste(commentCorrection),
                                       "Preprint of"),
      hasPreprint = str_detect(paste(commentCorrection),
                               "Preprint in"),
      hasOrcid = str_detect(paste(authorId), "ORCID"),
      inEPMC = ifelse(inEPMC == "Y", TRUE, FALSE)
    ) %>%
    select(-c(commentCorrection, authorId))
  return(parsed)
}

# for(year in 2016:2023){
#   query_preprint <- str_c('SRC:PPR AND FIRST_PDATE:',
#                           year)
#   metadata <- get_epmc_metadata(query = query_preprint)
#   write_tsv(metadata, str_c('./data/preprint_metadata', year, '.tsv'))
# }


get_preprint_metadata_by_year <- function(year) {
  query_preprint <- str_c('SRC:PPR AND FIRST_PDATE:',
                          year)
  get_epmc_metadata(query = query_preprint)
}

europepmc_preprint_metadata <- 2016:2024 |>
  map(get_preprint_metadata_by_year)

europepmc_preprints <- europepmc_preprint_metadata |>
  list_rbind()

europepmc_preprints |> saveRDS(here("results", "epmc_metadata_2016_2024.rds"))

europepmc_preprints <- readRDS(here("results", "epmc_metadata_2016_2024.rds"))

epmc_preprints_with_articles <- europepmc_preprints |>
  filter(hasPublishedVersion == TRUE) |>
  rename(has_published_version = hasPublishedVersion)

epmc_preprint_dois <- epmc_preprints_with_articles |>
  mutate(doi = deversion_doi(doi)) |>
  distinct(doi) |>
  pull(doi)

preprints_only <- preprints_oa_new |>
  filter(!url %in% epmc_preprint_dois,
         !str_detect(doi, "10.22541")) |> # exclude authorea due to false titles there!
  mutate(title = tolower(title) |>
           str_remove(": a systematic review and meta-analysis"))

publications <- read_csv(here("main", "publication_table.csv"))

articles_only <-  publications |>
  filter(!is.na(title)) |> # remove articles without a title
  mutate(title = tolower(title) |>
           str_remove(": a systematic review and meta-analysis")) |>
  select(doi, title, year)

titles_check <- preprints_only |>
  stringdist_left_join(articles_only, by = "title", distance_col = "dist", max_dist = 25) |>
  select(doi = doi.x, doi_article = doi.y, contains("year"), contains("title"), dist) |>
  arrange(desc(dist)) |>
  # titles_check <- titles_check |>
  mutate(title_length.x = str_length(title.x),
         title_length.y = str_length(title.y),
         preprint_first = year.x <= year.y,
         prop_mismatch = 2 * dist / (title_length.x + title_length.y))

titles_matched <- titles_check |>
  filter(!is.na(dist),
         prop_mismatch < 0.433,
         preprint_first == TRUE | dist == 0,
         !str_detect(title.x, "global, regional, and national burden") | dist == 0) |>
  group_by(doi) |>
  arrange(prop_mismatch) |>
  summarise(doi = first(doi)) |>
  pull(doi)


preprints_oa <- read_csv(here("results", "preprints_oa.csv"))
#
# preprints_oa_not_yet_matched <- preprints_oa |>
#   filter(has_published_version == FALSE)

preprints_oa_new_matches <- preprints_oa |>
  mutate(has_published_version = has_published_version == TRUE |
           doi %in% c(epmc_preprint_dois, titles_matched))

preprints_oa_new_matches |>
  count(has_published_version)

preprints_oa_new_matches |>
  write_excel_csv(here("results", "preprints_oa.csv"))
