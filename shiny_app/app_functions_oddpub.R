#----------------------------------------------------------------------
# oddpub data loading & preprocessing functions
#----------------------------------------------------------------------

make_oddpub_plot_data <- function(data_table)
{
  oddpub_plot_data <- data_table %>%
    #HERE WE FILTER SUCH THAT WE ONLY KEEP THE DOWNLOADED PUBLICATIONS
    filter(!is.na(is_open_data) | (open_data_manual_check == TRUE)) %>%
    #only take the categories mentioned first
    mutate(open_data_category_priority = (open_data_category_manual %>% (function(x)
      x %>% str_split(",") %>% map_chr(head, 1)))) %>%
    mutate(open_code_category_priority = (open_code_category_manual %>% (function(x)
      x %>% str_split(",") %>% map_chr(head, 1)))) %>%
    group_by(year) %>%
    summarize(open_data_count = sum(is_open_data, na.rm = TRUE),
              open_code_count = sum(is_open_code, na.rm = TRUE),
              open_data_manual_count = sum(open_data_manual_check, na.rm = TRUE),
              open_code_manual_count = sum(open_code_manual_check, na.rm = TRUE),
              OD_field_specific_count = sum(open_data_category_priority == "field-specific repository", na.rm = TRUE),
              OD_general_purpose_count = sum(open_data_category_priority == "general-purpose repository", na.rm = TRUE),
              OD_supplement_count = sum(open_data_category_priority == "supplement", na.rm = TRUE),
              OC_github_count = sum(open_code_category_priority == "github", na.rm = TRUE),
              OC_other_count = sum(open_code_category_priority == "other repository/website", na.rm = TRUE),
              OC_supplement_count = sum(open_code_category_priority == "supplement", na.rm = TRUE),
              total = n()) %>% #DO WE WANT TO BE N=ALL DOWNLOADED PUBLICATIONS OR N=ALL PUBLICATIONS IN THE DATASET?
    #BECAUSE WITH THE CURRENT CALCULATION WE ASSUME THAT THE PERCENTAGE OF OPEN DATA IS THE SAME FOR ALL THE
    #PUBLICATIONS THAT COULD NOT BE DOWNLOADED, WHICH MIGHT NOT BE TRUE
    mutate(open_data_perc = open_data_count/total * 100) %>%
    mutate(open_code_perc = open_code_count/total * 100) %>%
    mutate(open_data_manual_perc = open_data_manual_count/total * 100) %>%
    mutate(open_code_manual_perc = open_code_manual_count/total * 100) %>%
    mutate(OD_field_specific_perc = OD_field_specific_count/total * 100) %>%
    mutate(OD_general_purpose_perc = OD_general_purpose_count/total * 100) %>%
    mutate(OD_supplement_perc = OD_supplement_count/total * 100) %>%
    mutate(OC_github_perc = OC_github_count/total * 100) %>%
    mutate(OC_other_perc = OC_other_count/total * 100) %>%
    mutate(OC_supplement_perc = OC_supplement_count/total * 100)

#
#   open_data_categories <- data_table %>%
#     filter(open_data_manual_check == TRUE) %>%
#     mutate(open_data_category_priority = (open_data_category_manual %>% (function(x)
#       x %>% str_split(",") %>% map_chr(head, 1))))

  return(oddpub_plot_data)
}
