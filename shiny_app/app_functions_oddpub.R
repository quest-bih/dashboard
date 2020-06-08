#----------------------------------------------------------------------
# Open Access data loading & preprocessing functions
#----------------------------------------------------------------------

make_oddpub_plot_data <- function(data_table)
{
  oddpub_plot_data <- data_table %>%
    filter(!is.na(is_open_data)) %>%
    group_by(year) %>%
    summarize(open_data_count = sum(is_open_data),
              open_code_count = sum(is_open_code),
              total = n()) %>%
    mutate(open_data_perc = open_data_count/total) %>%
    mutate(open_code_perc = open_code_count/total)

  return(oddpub_plot_data)
}
