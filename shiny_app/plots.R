#------------------------------------------------------------------------
# Open Science plots
#------------------------------------------------------------------------

# Open Access
plot_OA_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~gold, name = "Gold", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~green, name = 'Green',
              marker = list(color = color_palette[6],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~hybrid, name = 'Hybrid',
              marker = list(color = color_palette[7],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~bronze, name = 'Bronze',
              marker = list(color = color_palette[4],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~closed, name = 'Closed',
              marker = list(color = color_palette[5],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~`NA`, name = 'No data available',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Publications</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

plot_OA_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~gold, name = "Gold", type = 'bar',
                          marker = list(color = color_palette[3],
                                        line = list(color = 'rgb(0,0,0)',
                                                    width = 1.5))) %>%
  add_trace(y = ~green, name = 'Green',
            marker = list(color = color_palette[6],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
  add_trace(y = ~hybrid, name = 'Hybrid',
            marker = list(color = color_palette[7],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
  layout(barmode = 'stack',
         legend=list(title=list(text='<b> Category </b>')),
         yaxis = list(title = '<b>Percentage Open Access</b>',
                      range = c(0, 100)),
         xaxis = list(title = '<b>Year</b>',
                      dtick = 1),
         paper_bgcolor = color_palette[9],
         plot_bgcolor = color_palette[9])
}


# Open Data
plot_OD_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~OD_field_specific_perc,
          name = "field-specific repository", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~OD_general_purpose_perc,
              name = 'general-purpose repository or other website',
              marker = list(color = color_palette[6],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~OD_supplement_perc, name = 'supplement',
              marker = list(color = color_palette[7],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Percentage of publications</b>',
                        range = c(0, 20)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

plot_OD_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~open_data_manual_count,
          name = "Open Data", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~open_data_neg_count,
              name = 'No Open Data',
              marker = list(color = color_palette[5],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~open_data_NA_count,
              name = 'PDF not available',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Number of publications</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


# Open Code
plot_OC_perc <- function(plot_data, color_palette)
{
    plot_ly(plot_data, x = ~year, y = ~OC_github_perc,
            name = "GitHub", type = 'bar',
            marker = list(color = color_palette[3],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
      add_trace(y = ~OC_other_perc,
                name = 'other repository/website',
                marker = list(color = color_palette[6],
                              line = list(color = 'rgb(0,0,0)',
                                          width = 1.5))) %>%
      add_trace(y = ~OC_supplement_perc, name = 'supplement',
                marker = list(color = color_palette[7],
                              line = list(color = 'rgb(0,0,0)',
                                          width = 1.5))) %>%
      layout(barmode = 'stack',
             legend=list(title=list(text='<b> Category </b>')),
             yaxis = list(title = '<b>Percentage of publications</b>',
                          range = c(0, 10)),
             xaxis = list(title = '<b>Year</b>',
                          dtick = 1),
             paper_bgcolor = color_palette[9],
             plot_bgcolor = color_palette[9])
}

plot_OC_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~open_code_manual_count,
          name = "Open Data", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~open_code_neg_count,
              name = 'No Open Data',
              marker = list(color = color_palette[5],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~open_code_NA_count,
              name = 'PDF not available',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Number of publications</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


# Preprints
plot_preprints <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~preprints, type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    layout(yaxis = list(title = '<b>Number of preprints</b>',
                        range = c(0, 100)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


#------------------------------------------------------------------------
# Clinical trials plots
#------------------------------------------------------------------------

# Summary results
plot_summary_results_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~perc_sum_res_12,
          name = "12 months", type = 'bar',
          marker = list(color = color_palette[2],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~perc_sum_res_24, name = '24 months',
              marker = list(color = color_palette[3],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'group',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Percentage of trials</b>',
                        range = c(0, 100)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

plot_summary_results_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_sum_res_12,
          name = "12 months", type = 'bar',
          marker = list(color = color_palette[2],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~has_sum_res_24_only, name = '24 month',
              marker = list(color = color_palette[3],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~no_sum_res_24, name = 'no timely summary results',
              marker = list(color = color_palette[5],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Number of trials</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


# Prospective registration
plot_prosp_reg_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~perc_prosp_reg, type = 'bar',
          marker = list(color = color_palette[2],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    layout(yaxis = list(title = '<b>Percentage of trials</b>',
                        range = c(0, 100)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

plot_prosp_reg_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_prosp_reg,
          name = 'prospective registration', type = 'bar',
          marker = list(color = color_palette[2],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 2))) %>%
    add_trace(y = ~no_prosp_reg,
              name = 'no prospective registration',
              marker = list(color = color_palette[5],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 2))) %>%
    layout(barmode = 'stack',
           yaxis = list(title = '<b>Number of trials</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


#------------------------------------------------------------------------
# Visualizations plots
#------------------------------------------------------------------------

# problematic graphs
plot_barzooka_problem <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_bar,
          name = "bar graph", type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[2], width = 3),
          marker = list(color = color_palette[2], size = 8)) %>%
    add_trace(y = ~has_pie, name = 'pie chart', mode = 'lines+markers',
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) %>%
    layout(legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Graph types per 1000 publications</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

# more informative graphs
plot_barzooka_inform <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_informative,
          name = "any informative", type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[1], width = 3),
          marker = list(color = color_palette[1], size = 8)) %>%
    add_trace(y = ~has_bardot, name = 'bar graph with dots', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) %>%
    add_trace(y = ~has_box, name = 'box plot', mode = 'lines+markers',
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) %>%
    add_trace(y = ~has_dot, name = 'dot plot', mode = 'lines+markers',
              line = list(color = color_palette[4]),
              marker = list(color = color_palette[4])) %>%
    add_trace(y = ~has_hist, name = 'histogram', mode = 'lines+markers',
              line = list(color = color_palette[5]),
              marker = list(color = color_palette[5])) %>%
    add_trace(y = ~has_violin, name = 'violin plot', mode = 'lines+markers',
              line = list(color = color_palette[6]),
              marker = list(color = color_palette[6])) %>%
    layout(legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Graph types per 1000 publications</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}
