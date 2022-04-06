#----------------------------------------------------------------------
# Open Access data loading & preprocessing functions
#----------------------------------------------------------------------


make_OA_plot_data_total <- function(data_table)
{
  OA_plot_data <- data_table %>%
    mutate(OA_color = replace_na(OA_color, "NA")) %>%
    group_by(year, OA_color) %>%
    summarize(count = n()) %>%
    calculate_OA_percentages(c("gold", "green", "hybrid", "bronze", "closed", "NA")) %>%
    rename(category = OA_color)

  return(OA_plot_data)
}


make_OA_plot_data <- function(data_table)
{
  OA_plot_data <- data_table %>%
    #do not filter the missing OA publications to be in line with library numbers
    #filter(!is.na(OA_color)) %>%
    group_by(year, OA_color) %>%
    summarize(count = n()) %>%
    calculate_OA_percentages(c("gold", "green", "hybrid", "bronze")) %>%
    rename(category = OA_color)

  return(OA_plot_data)
}


calculate_OA_percentages <- function(OA_data, categories)
{
  #number of publications
  publ_all <- OA_data %>%
    group_by(year) %>%
    summarise(all = sum(count))

  #number of publ in each OA category
  publ_OA_colors <- OA_data %>%
    filter(OA_color %in% categories) %>%
    group_by(OA_color, year) %>%
    summarise(OA = sum(count))

  #OA percentages
  OA_perc <- publ_OA_colors %>%
    left_join(publ_all) %>%
    mutate(perc = round(OA/all *100, 1)) %>%
    ungroup()

  return(OA_perc)
}


#RGB colors for OA color plotting
set_OA_colors <- function(plot_data, show_OA_colors)
{
  col_list <- c("#f7be16", "#008950", "#410b5b")

  if(show_OA_colors) {
    plot_data <- plot_data %>%
      filter(OA_color != "all_OA") %>%
      arrange(OA_color)

    plot_data$bar_col <- OA_color_to_rgb(plot_data$OA_color)

    plot_data$bar_col <- factor(plot_data$bar_col, levels = col_list)

  } else {
    plot_data <- plot_data %>%
      filter(OA_color == "all_OA")

    plot_data$bar_col <- ifelse(plot_data$city == "All cities combined", "#e9a602", "#90B2C4") # #E97F02
  }

  return(plot_data)
}


OA_color_to_rgb <- function(color_vec)
{
  col_list <- c("#f7be16", "#008950", "#410b5b")
  names(col_list) <- c("gold", "green", "hybrid")

  return(col_list[color_vec])
}
