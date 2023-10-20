library(openalexR)
library(tidyverse)
library(here)
library(vroom)
library(janitor)

query_works_2023 <- oa_query(
  entity = "works",
  # authorships.institutions.id = "I4210139777", # BIH id at openAlex
  authorships.institutions.ror = c("https://ror.org/001w7jn25", "https://ror.org/0493xsw21"),
  #charite: https://ror.org/001w7jn25
  from_publication_date = "2016-01-01",
  to_publication_date = "2023-01-01"
  # version = "submittedVersion"
  # type = "posted-content"
)


res <- oa_request(query_url = query_works_2023, mailto = Sys.getenv("EMAIL"))

df <- oa2df(res, entity = "works")


preprints_unkn <- df |>
  filter(!str_detect(url, "10\\.1101|preprint|\\/rs\\.|rxiv|\\/au\\.|zenodo|ssrn"),
         is.na(version) | version == "submittedVersion",
         type == "posted-content")

parsed <- df |>
  filter(
    is.na(version) | version == "submittedVersion",
    type == "posted-content" | str_detect(doi, "ssrn"),
    !str_detect(url, "ems|10\\.14293|egusphere|morressier|pubmed|espost|10\\.1016")) |>
  group_by(display_name) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(authors = map_chr(author, \(au) pull(au, au_display_name) |> paste0(collapse = "; ")),
         affils = map_chr(author, \(au) pull(au, au_affiliation_raw) |> paste0(collapse = "; ")),
         affils_st = map_chr(author, \(au) pull(au, institution_display_name) |> paste0(collapse = "; ")),
         doi = str_remove(url, ".*\\/(?=10\\.)"),
         url = str_replace(doi, "(?<=v)\\d$", "1")) |>
  group_by(url) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(title = display_name, doi, url, authors, affils, affils_st, publication_date, publication_year, journal = so, license, type, version, is_oa, pdf_url) |>
  mutate(journal_st = case_when(
    str_detect(doi, "10.1101") & str_length(doi) < 27 ~ "bioRxiv (Cold Spring Harbor Laboratory)",
    str_detect(doi, "10.1101") & str_length(doi) > 25 ~ "medRxiv (Cold Spring Harbor Laboratory)",
    str_detect(doi, "10.20944") | str_detect(doi, "preprints\\.org") ~ "Preprints.org",
    str_detect(doi, "10.2139") ~ "Social Science Research Network",
    str_detect(doi, "10.21203") ~ "Research Square",
    str_detect(doi, "10.22541") ~ "Authorea",
    str_detect(doi, "10.26434") ~ "Biological and Medicinal Chemistry (ChemRxiv)",
    str_detect(doi, "10.31234") ~ "PsyArXiv",
    str_detect(doi, "10.33774") ~ "Theoretical and Computational Chemistry (ChemRxiv)",
    str_detect(doi, "10.7287") ~ "PeerJ Preprints",
    str_detect(doi, "refubium") ~ "Refubium",
    .default = journal
  ),
    has_quest_bih = str_detect(affils, "QUEST|Berlin Institute of Health")) |>
  filter(!str_detect(affils, "Charité Research Organisation GmbH")) |>
  arrange(publication_date, title)

str_extract(doi, ".*(?=\\/)")


parsed |>
  count(journal_st)

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
                   "Experimental and Clinical Research Center",
                   "NeuroCure") |>
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
# is Einstein Center for Neurosciences Charite or not?

not_charite |> filter(str_detect(doi, "10.1101/2021.10.30.21265692")) |>  pull(affils)

aut <- df |>  filter(str_detect(url, "10.1101/2021.10.30.21265692")) |> pull(author) |> list_rbind()

preprints_oa <- parsed |>
  filter(!url %in% not_charite$url) |>
  select(title, doi, url, research_org_names = affils, research_orgs = affils_st,
         year = publication_year, journal_title = journal_st)
preprints_oa |>
  write_excel_csv2(here("results", "preprints_oa.csv"))

preprints_oa |>
  count(year)

### comparison with old method, using Dimensions.
# current method delivers good quality and has a higher coverage, so abandon old method
# and recalculate previous years

parsed <- vroom(here("results", "preprints_oa.csv"))

preprints_dim <- vroom(here("results", "preprints.csv"))


# query_works_arxiv <- oa_query(
#   entity = "works",
#   doi = c("10.48550/arxiv.2208.14217",
#           # "10.2139/ssrn.3388826",
#           # "10.2139/ssrn.3416695",
#           # "10.2139/ssrn.3378696",
#           "10.1101/2021.01.31.428997",
#           "10.2139/ssrn.3774817",
#
#           # "10.1101/034983",
#           # "10.1101/536383",
#           # "10.1101/528877",
#           # "10.1101/239160",
#           "10.1101/2021.06.08.447489")
#   )
#
# res_a <- oa_request(query_url = query_works_arxiv, mailto = Sys.getenv("EMAIL"))
#
# df_a <- oa2df(res_a, entity = "works")

preprints_dim |>
  count(year)


pr_dim_old <- vroom(here("results", "preprints_old.csv"))


oa_not_in_dim <- parsed |>
  filter(!doi %in% preprints_dim$doi,
         publication_year == 2021) |>
  arrange(title)


pr_dim_old |>
  count(year)

dim_not_in_oa <- preprints_dim |>
  filter(!doi %in% parsed$doi,
         year == 2021) |>
  arrange(title)



dupes <- preprints_dim |>
  filter(!doi %in% parsed$doi,
         year == 2021) |>
  get_dupes(title)
