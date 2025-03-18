library(tidyverse)

exclusion_types <- c("Address",
                     "(B|b)iography",
                     "Directory",
                     "Editor",
                     "Supplem",
                     "Abstract",
                     "Historic",
                     "Tutorial",
                     "Interview",
                     "Lecture",
                     "Legislation",
                     "News",
                     "Handout",
                     "Periodical",
                     "Pesonal",
                     "Portrait",
                     "Preprint",
                     "Errat|Corrigend",
                     "Retract",
                     "Integrity",
                     "Media",
                     "Webcast",
                     "Proceedings",
                     "Consensus",
                     "Guideline",
                     "Congress",
                     "Protocol"
) |>
  paste0(collapse = "|")

filtered_publications <- some_publication_list_with_metadata |>
  mutate(doi = tolower(DOI)) |>
  filter(
    ##### inclusion:
    str_detect(doi, "^10"), # having doi
    ##### exclusions:
    !str_detect(pubtype, exclusion_types), # from PubMed article type in exclusion category
    !is.na(abstract), # without abstract
    !is.na(title), # without title
    !str_detect(title, "\\[(German|Italian|Russian|Spanish|Chinese|Japanese)"), # foreign language
    !str_detect(title, "Errat|Author|Re(\\:|\\b|ply)|In Mem|onference|(?<=:).*(P|p)rotocol\\b|(P|p)rotocol\\b(?=:)|Summary")
  )
