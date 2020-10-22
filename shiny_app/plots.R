
#------------------------------------------------------------------------
# Open Science plots
#------------------------------------------------------------------------

# Open Access
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
           legend = list(title = list(text = '<b> Category </b>',
                                      xanchor = "right")),
           yaxis = list(title = '<b>Percentage Open Access</b>',
                        range = c(0, 100)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

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
    add_trace(y = ~`NA`, name = 'No data <br>available',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Publications</b>',
                        range = c(0, 6000)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


# Open Data
plot_OD_perc <- function(plot_data, color_palette, zoom_in)
{
  if(zoom_in) {
    yrange <- c(0, 20)
  } else {
    yrange <- c(0, 100)
  }

  plot_ly(plot_data, x = ~year, y = ~OD_field_specific_perc,
          name = "field-specific repository", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~OD_general_purpose_perc,
              name = 'multipurpose repository <br>or website',
              marker = list(color = color_palette[6],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~OD_supplement_perc, name = 'supplement',
              marker = list(color = color_palette[7],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend = list(title = list(text = '<b> Category </b>'),
                         xanchor = "right"),
           yaxis = list(title = '<b>Percentage of publications</b>',
                        range = yrange),
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
              name = 'Not accessible',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Number of publications</b>',
                        range = c(0, 6000)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


# Open Code
plot_OC_perc <- function(plot_data, color_palette, zoom_in)
{
  if(zoom_in) {
    yrange <- c(0, 5)
  } else {
    yrange <- c(0, 100)
  }

  plot_ly(plot_data, x = ~year, y = ~OC_github_perc,
          name = "GitHub", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~OC_other_perc,
              name = 'other repository or website',
              marker = list(color = color_palette[6],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~OC_supplement_perc, name = 'supplement',
              marker = list(color = color_palette[7],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend = list(title = list(text = '<b> Category </b>',
                                      xanchor = "right")),
           yaxis = list(title = '<b>Percentage of publications</b>',
                        range = yrange),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

plot_OC_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~open_code_manual_count,
          name = "Open Code", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~open_code_neg_count,
              name = 'No Open Code',
              marker = list(color = color_palette[5],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~open_code_NA_count,
              name = 'Not accessible',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Number of publications</b>',
                        range = c(0, 6000)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


# Preprints
plot_preprints <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~preprints,
          name = 'Preprints',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) %>%
    add_trace(y = ~count, name = 'Journal articles', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) %>%
    layout(yaxis = list(title = '<b>Total number</b>',
                        range = c(0, 6000)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "right"))
}


#------------------------------------------------------------------------
# Clinical trials plots
#------------------------------------------------------------------------

# Summary results
plot_summary_results_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~as.Date(retrieval_date, format= "%Y-%m-%d"), y = ~round(perc_reported * 100, 1),
          name = 'Reported trials',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) %>%
    layout(yaxis = list(title = '<b>Percentage of trials</b>',
                        range = c(0, 100)),
           xaxis = list(title = '<b>Date</b>',
                        type = 'date'),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "right"))
}

plot_summary_results_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~as.Date(retrieval_date, format= "%Y-%m-%d"), y = ~total_reported,
          name = 'Reported trials',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) %>%
    add_trace(y = ~total_due, name = 'Due trials', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) %>%
    layout(yaxis = list(title = '<b>Number of trials</b>'),
           xaxis = list(title = '<b>Date</b>',
                        type = 'date'),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


# Trial publication
plot_intovalue_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~completion_year, y = ~round(percentage_published_2_years * 100,1),
          name = 'results within 2 years',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) %>%
    add_trace(y = ~round(percentage_published_5_years * 100,1),
              name = 'results within 5 years', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) %>%
    layout(yaxis = list(title = '<b>Percentage of trials</b>',
                        range = c(0, 100)),
           xaxis = list(title = '<b>Trial completion year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "right"))
}

plot_intovalue_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~completion_year, y = ~trials_with_publication_2_years,
          name = 'results within 2 years',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) %>%
    add_trace(y = ~trials_with_publication_5_years,
              name = 'results within 5 years', mode = 'lines+markers',
              line = list(color = color_palette[4]),
              marker = list(color = color_palette[4])) %>%
    add_trace(y = ~total_trials_2_years,
              name = 'total trials 2 years', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) %>%
    add_trace(y = ~total_trials_5_years,
              name = 'total trials 5 years', mode = 'lines+markers',
              line = list(color = color_palette[5]),
              marker = list(color = color_palette[5])) %>%
    layout(yaxis = list(title = '<b>Number of trials</b>',
                        range = c(0, 100)),
           xaxis = list(title = '<b>Trial completion year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "right"))
}


# Prospective registration
plot_prosp_reg_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~perc_prosp_reg, type = 'bar',
          marker = list(color = color_palette[3],
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
          marker = list(color = color_palette[3],
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
plot_barzooka_problem_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~round(has_bar/total*100, 1),
          name = "bar graph", type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[2], width = 3),
          marker = list(color = color_palette[2], size = 8)) %>%
    add_trace(y = ~round(has_pie/total*100, 1), name = 'pie chart', mode = 'lines+markers',
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) %>%
    layout(legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Percentage of publications</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

plot_barzooka_problem_total<- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_bar,
          name = "bar graph", type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[2], width = 3),
          marker = list(color = color_palette[2], size = 8)) %>%
    add_trace(y = ~has_pie, name = 'pie chart', mode = 'lines+markers',
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) %>%
    add_trace(y = ~total, name = 'accessible publications', mode = 'lines+markers',
              line = list(color = color_palette[5]),
              marker = list(color = color_palette[5])) %>%
    layout(legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Publications with graph type</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


# more informative graphs
plot_barzooka_inform_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~round(has_informative/total*100, 1),
          name = "any informative", type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[1], width = 3),
          marker = list(color = color_palette[1], size = 8)) %>%
    add_trace(y = ~round(has_bardot/total*100, 1), name = 'bar graph with dots', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) %>%
    add_trace(y = ~round(has_box/total*100, 1), name = 'box plot', mode = 'lines+markers',
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) %>%
    add_trace(y = ~round(has_dot/total*100, 1), name = 'dot plot', mode = 'lines+markers',
              line = list(color = color_palette[4]),
              marker = list(color = color_palette[4])) %>%
    add_trace(y = ~round(has_hist/total*100, 1), name = 'histogram', mode = 'lines+markers',
              line = list(color = color_palette[6]),
              marker = list(color = color_palette[6])) %>%
    add_trace(y = ~round(has_violin/total*100, 1), name = 'violin plot', mode = 'lines+markers',
              line = list(color = color_palette[7]),
              marker = list(color = color_palette[7])) %>%
    layout(legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Percentage of publications</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}

plot_barzooka_inform_total <- function(plot_data, color_palette)
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
              line = list(color = color_palette[6]),
              marker = list(color = color_palette[6])) %>%
    add_trace(y = ~has_violin, name = 'violin plot', mode = 'lines+markers',
              line = list(color = color_palette[7]),
              marker = list(color = color_palette[7])) %>%
    add_trace(y = ~total, name = 'accessible publications', mode = 'lines+markers',
              line = list(color = color_palette[5]),
              marker = list(color = color_palette[5])) %>%
    layout(legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Publications with graph type</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}
