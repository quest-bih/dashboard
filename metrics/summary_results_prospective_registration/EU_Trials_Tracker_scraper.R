library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(httr)
library(curl)
library(here)

extract_var <- function(var_name, text)
{
  var_value <- text |>
    str_extract(paste0(var_name, " = [:digit:]{1,3}")) |>
    # str_extract(paste0(var_name, " = [:digit:]{2,3}")) %>% # some of latest numbers single digits!
    str_remove(paste0(var_name, " = "))

  return(var_value)
}

charite_url <-  "https://eu.trialstracker.net/sponsor/charite-universitatsmedizin-berlin"

webpage <- read_lines(charite_url) |>  paste(collapse = " ")

# webpage <- read_html(charite_url) |>  rvest::html_text()
var_names <- c("total_unreported", "total_reported", "total_due",
  "not_yet_due_trials", "inconsistent_trials", "total_trials")

var_values <- var_names |>
  map_chr(extract_var, text = webpage) |>
  as.integer()

EU_data_charite <- tibble(retrieval_date = as.character(Sys.Date()),
                          total_unreported = var_values[1],
                          total_reported = var_values[2],
                          total_due = var_values[3],
                          not_yet_due_trials = var_values[4],
                          inconsistent_trials = var_values[5],
                          total_trials = var_values[6])

# write_csv(EU_data_charite, "./results//EU_trialstracker.csv",
#           append = TRUE)


### historic scraping via github https://github.com/ebmdatalab/euctr-tracker-data/

oauth_endpoints("github")

EU_trackR <- oauth_app(appname = "EU_trials_trackR",
                                key = Sys.getenv("GITHUB_CLIENT_ID"),
                                secret = Sys.getenv("GITHUB_CLIENT_SECRET"))

github_token <- oauth2.0_token(oauth_endpoints("github"), EU_trackR)


gtoken <- config(token = github_token)


url <- "https://github.com/ebmdatalab/euctr-tracker-data/commits/master/all_sponsors.json"

history <- read_html(url)

commits <- history |>
  html_elements("react-app") |>
  html_element("script") |>
  html_text() |>
  jsonlite::fromJSON()

charite_trials <- commits$payload$commitGroups$commits |>
  list_rbind() |>
  select(retrieval_date = committedDate, SHAs = oid) |>
  mutate(retrieval_date = as.Date(retrieval_date))



charite_trials <- charite_trials |>
  mutate(url = paste0("https://github.com/ebmdatalab/euctr-tracker-data/raw/",
                      SHAs,
                      "/all_sponsors.json"))

scraped_trials <- tibble()

for (url in charite_trials$url) {
  req <- GET(url, gtoken)
  stop_for_status(req)

  # Extract content from a request
  json1 <- content(req) |>  str_replace_all("NaN", "0")


  # Convert to a data.frame
  gitDF <- jsonlite::fromJSON(json1, flatten = TRUE) |>
    filter(slug == "charite-universitatsmedizin-berlin") |>
    select(all_of(var_names))
  scraped_trials <- bind_rows(scraped_trials, gitDF)
  Sys.sleep(2) #pause to let connection work
  closeAllConnections()
  gc()

}

scraped_trials <- scraped_trials |>
  mutate(retrieval_date = charite_trials$retrieval_date) |>
  filter(total_trials > 100,
         retrieval_date > "2024-01-10") |>
  select(retrieval_date, everything())

write_csv(scraped_trials, "./results//EU_trialstracker.csv",
          append = TRUE)

