#----------------------------------------------------------------------
# oddpub data loading & preprocessing functions
#----------------------------------------------------------------------
# library(tidyverse)
# data_table <- read_csv("./shiny_app/data/dashboard_metrics.csv")
make_oddpub_plot_data <- function(data_table)
{
  # filter out supplements only
  data_table <- data_table %>%
    filter(is.na(open_data_category_manual)|open_data_category_manual != "supplement")
  od_manual_pos <- (!is.na(data_table$open_data_manual_check) & data_table$open_data_manual_check)
  data_table[od_manual_pos,]$is_open_data <- TRUE


  oddpub_plot_data <- data_table %>%
      group_by(year) %>%
      summarize(open_data_manual_count = sum(open_data_manual_check, na.rm = TRUE),
                open_data_neg_count = sum(!is_open_data | !open_data_manual_check, na.rm = TRUE),
                open_data_NA_count = sum(is.na(is_open_data), na.rm = TRUE),

                open_code_manual_count = sum(open_code_manual_check, na.rm = TRUE),
                open_code_neg_count = sum(!is_open_code | !open_code_manual_check, na.rm = TRUE),
                open_code_NA_count = sum(is.na(is_open_code), na.rm = TRUE),

                OD_disciplinary_count = sum(open_data_category_manual == "disciplinary repository", na.rm = TRUE),
                OD_general_purpose_count = sum(open_data_category_manual == "general-purpose repository", na.rm = TRUE),
                OD_disciplinary_and_general_count = sum(open_data_category_manual == "disciplinary and general-purpose repositories", na.rm = TRUE),
                OD_disc_restricted_count = sum(open_data_category_manual == "disciplinary repository" &
                                                 restrictions == "full", na.rm = TRUE),
                OD_gen_restricted_count = sum(open_data_category_manual == "general-purpose repository" &
                                                restrictions == "full", na.rm = TRUE),
                OD_disc_and_gen_restricted_count = sum(open_data_category_manual == "disciplinary and general-purpose repositories" &
                                                         restrictions == "full", na.rm = TRUE),

                # OD_supplement_count = sum(open_data_category_priority == "supplement", na.rm = TRUE),
                OC_github_count = sum(open_code_category_manual == "github", na.rm = TRUE),
                OC_other_count = sum(open_code_category_manual == "other repository/website", na.rm = TRUE),
                # OC_supplement_count = sum(open_code_category_priority == "supplement", na.rm = TRUE),
                total = sum(!is.na(is_open_data) | (open_data_manual_check == TRUE), na.rm = TRUE)) %>%

      mutate(open_data_manual_perc = round(open_data_manual_count/total * 100, 1),
             open_code_manual_perc = round(open_code_manual_count/total * 100, 1),
             OD_disciplinary_perc = round(OD_disciplinary_count/total * 100, 1),
             OD_general_purpose_perc = round(OD_general_purpose_count/total * 100, 1),
             OD_disciplinary_and_general_perc = round(OD_disciplinary_and_general_count/total * 100, 1),
             OD_disc_restricted_perc = round(OD_disc_restricted_count/total * 100, 1),
             OD_gen_restricted_perc = round(OD_gen_restricted_count/total * 100, 1),
             OD_disc_and_gen_restricted_perc = round(OD_disc_and_gen_restricted_count/total * 100, 1),
             OD_disc_nonrestricted_perc = round((OD_disciplinary_count - OD_disc_restricted_count)/total * 100, 1),
             OD_gen_nonrestricted_perc = round((OD_general_purpose_count - OD_gen_restricted_count)/total * 100, 1),
             OD_disc_and_gen_nonrestricted_perc = round((OD_disciplinary_and_general_count - OD_disc_and_gen_restricted_count)/total * 100, 1),
      # mutate(OD_supplement_perc = round(OD_supplement_count/total * 100, 1)) %>%
             OC_github_perc = round(OC_github_count/total * 100, 1),
             OC_other_perc = round(OC_other_count/total * 100, 1))
  # %>%
  #     mutate(OC_supplement_perc = round(OC_supplement_count/total * 100, 1))

  return(oddpub_plot_data)
}
