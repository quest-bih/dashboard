library(rorcid)
library(tidyverse)

orcid_id_num <- orcid(query="current-institution-affiliation-name:
                      (Charité OR Charite OR (Universitätsmedizin AND Berlin)
                      OR (Berlin AND Institute AND of AND Health))") %>%
  attr("found")

write(paste(Sys.Date(), orcid_id_num, sep = ","), file="results/orcid.csv", append = TRUE)
