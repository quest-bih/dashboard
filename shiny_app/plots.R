
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
    add_trace(y = ~bronze, name = 'Bronze',
              marker = list(color = color_palette[4],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           yaxis = list(title = '<b>Open Access</b>',
                        range = c(0, 100),
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
           yaxis = list(title = '<b>Publications</b>',
                        range = c(0, 6300)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
}


# Open Data
plot_OD_perc <- function(plot_data, color_palette)
  {
# plot_OD_perc <- function(plot_data, color_palette, zoom_in, show_restrictions)
# {
#   if(zoom_in) {
#     yrange <- c(0, 12)
#   } else {
    yrange <- c(0, 50)
#   }

  # plot_output <- plot_ly(plot_data, x = ~year, y = ~OD_disciplinary_perc,
  #         name = "disciplinary repository", type = 'bar',
  #         marker = list(color = color_palette[3],
  #                       line = list(color = 'rgb(0,0,0)',
  #                                   width = 1.5))) %>%
  #   add_trace(y = ~OD_general_purpose_perc,
  #             name = 'general-purpose repository',
  #             marker = list(color = color_palette[6],
  #                           line = list(color = 'rgb(0,0,0)',
  #                                       width = 1.5))) %>%
  #   add_trace(y = ~OD_disciplinary_and_general_perc,
  #             name = 'disciplinary and general<br>repository',
  #             marker = list(color = color_palette[2],
  #                           line = list(color = 'rgb(0,0,0)',
  #                                       width = 1.5)))
  # if (zoom_in == TRUE) {

    plot_output <- plot_data %>%
      # filter(year > 2019) %>%
      plot_ly(x = ~year, y = ~OD_disc_nonrestricted_perc,
                           name = "disciplinary repository", type = 'bar',
                           marker = list(color = color_palette[3],
                                         line = list(color = 'rgb(0,0,0)',
                                                     width = 1.5))) %>%
      add_trace(y = ~OD_disc_restricted_perc,
                name = 'disciplinary repository - restricted',
                marker = list(color = color_palette[3],
                              pattern = list(shape = "x"),
                              line = list(color = 'rgb(0,0,0)',
                                          width = 1.5))) %>%
      add_trace(y = ~OD_gen_nonrestricted_perc,
                name = 'general-purpose repository',
                marker = list(color = color_palette[6],
                              line = list(color = 'rgb(0,0,0)',
                                          width = 1.5))) %>%
      add_trace(y = ~OD_gen_restricted_perc,
                name = 'general-purpose repository - restricted',
                marker = list(color = color_palette[6],
                              pattern = list(shape = "x"),
                              line = list(color = 'rgb(0,0,0)',
                                          width = 1.5)),
                showlegend = FALSE) %>%
      add_trace(y = ~OD_disc_and_gen_nonrestricted_perc,
                name = 'disciplinary and general<br>repository',
                marker = list(color = color_palette[2],
                              line = list(color = 'rgb(0,0,0)',
                                          width = 1.5))) %>%
      add_trace(y = ~OD_disc_and_gen_restricted_perc,
                name = 'disciplinary and general<br>repository - restricted',
                marker = list(color = color_palette[2],
                              pattern = list(shape = "x"),
                              line = list(color = 'rgb(0,0,0)',
                                          width = 1.5)),
                showlegend = FALSE)
  # }

  plot_output %>%
    layout(barmode = 'stack',
           legend = list(
             orientation = "h",
             xanchor = "right",
             legendwidth = 1.3,
             # font = list(size = 11),
             bgcolor = 'rgba(0,0,0,0)',
             x = 0.05,
             y = 1.3),
           yaxis = list(title = '<b>Publications</b>',
                        range = yrange,
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
              name = 'Article<br>not accessible',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           yaxis = list(title = '<b>Number of publications</b>',
                        range = c(0, 6300)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
}


# Open Code
# plot_OC_perc <- function(plot_data, color_palette, zoom_in, show_supplements) {
plot_OC_perc <- function(plot_data, color_palette) {
  # if(zoom_in) {
  #   yrange <- c(0, 5)
  # } else {
    yrange <- c(0, 50)
  # }


  plot_output <- plot_ly(plot_data, x = ~year, y = ~OC_github_perc,
          name = "GitHub", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~OC_other_perc,
              name = 'other repository <br>or website',
              marker = list(color = color_palette[6],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5)))
  # if (show_supplements == TRUE) {
  #   plot_output <- plot_output %>%
  #     add_trace(y = ~OC_supplement_perc, name = 'supplement',
  #               marker = list(color = color_palette[7],
  #                             line = list(color = 'rgb(0,0,0)',
  #                                         width = 1.5)))
  # }

  plot_output %>%
    layout(barmode = 'stack',
           legend = list(xanchor = "left",
                         # font = list(size = 11),
                         bgcolor = 'rgba(0,0,0,0)',
                         x = 0.05,
                         y = 1),
           yaxis = list(title = '<b>Publications</b>',
                        range = yrange,
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
              name = 'Article<br>not accessible',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           yaxis = list(title = '<b>Number of publications</b>',
                        range = c(0, 6300)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
}


# Preprints
plot_preprints <- function(plot_data, color_palette)
{
  plot_ly(plot_data %>% filter(year > 2015), x = ~year, y = ~preprints,
          name = 'Preprints',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) %>%
    add_trace(y = ~count, name = 'Journal articles', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) %>%
    layout(yaxis = list(title = '<b>Total number</b>',
                        range = c(0, 8000)),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "left",
                         legendwidth = 1.3,
                         bgcolor = 'rgba(0,0,0,0)',
                         x = 0.05,
                         y = 1)) %>%
    config(displayModeBar = FALSE)
}

# Orcid
plot_orcid <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~as.Date(date, format= "%Y-%m-%d"), y = ~orcid_count,
          name = 'Registered ORCIDs',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 4),
          marker = list(color = color_palette[3], size = 1)) %>%
    layout(yaxis = list(title = '<b>Registered ORCIDs</b>',
                        range = c(0, 2000)),
           xaxis = list(title = '<b>Date</b>',
                        type = 'date'),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
    layout(yaxis = list(title = '<b>Trials</b>',
                        range = c(0, 100),
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Date</b>',
                        type = 'date'),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "right")) %>%
    config(displayModeBar = FALSE)
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
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
    layout(yaxis = list(title = '<b>Trials</b>',
                        range = c(0, 105),
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Trial completion year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "right",
                         bgcolor = 'rgba(0,0,0,0)')) %>%
    config(displayModeBar = FALSE)
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
           legend = list(xanchor = "right",
                         bgcolor = 'rgba(0,0,0,0)')) %>%
    config(displayModeBar = FALSE)
}


# Prospective registration
plot_prosp_reg_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~perc_prosp_reg, type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    layout(yaxis = list(title = '<b>Trials</b>',
                        range = c(0, 100),
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
    layout(yaxis = list(title = '<b>Publications</b>',
                        range = c(0, 28),
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
    layout(yaxis = list(title = '<b>Publications with graph type</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
    layout(yaxis = list(title = '<b>Publications</b>',
                        range = c(0, 28),
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
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
    layout(yaxis = list(title = '<b>Publications with graph type</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
    config(displayModeBar = FALSE)
}

#------------------------------------------------------------------------
# FAIR assessment
#------------------------------------------------------------------------

plot_fair_license_perc <- function(plot_data, color_palette)
{

  hovertext <-
    c(
      "<b>{license_fuji}</b>
          prop.: {perc*100}%
          n: {n}"
    )

  pal_lic <- colorRampPalette(c("#FFFFCC", "#F1BA50", "#F16A50"))
  pal_license <- c(rev(pal_lic(6)), "#879C9D")

  plot_data %>%
    plot_ly(
      x = ~ perc,
      y = ~ rep_type_2,
      color = ~ license_fuji,
      colors = pal_license,
      marker = list(line = list(color = "#000000",
                                width = 1)),
      text = ~ license_fuji,
      textposition = 'inside',
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "auto", size = 14), #color = #ffffff
      hoverinfo = "text",
      hovertext = ~ str_glue(hovertext)
    ) %>%
    add_bars() %>%
    layout(
      margin = list(l = 4, b = 10, pad = 0),
      barmode = "stack",
      xaxis = list(
        title = FALSE,
        side = "top",
        tickformat = ",.0%"
      ),
      yaxis = list(title = FALSE, side = "right"),
      uniformtext = list(minsize = 8, mode = "hide"),
      legend = list(
        orientation = 'h',
        traceorder = "normal",
        x = 0, #1
        xanchor = "left", # right
        font = list(size = 10)
      ),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9]
    ) %>%
    config(displayModeBar = FALSE)
}

plot_fair_license_total <- function(plot_data, color_palette)
{

  hovertext <-
    c(
      "<b>{license_fuji}</b>
          prop.: {perc*100}%
          n: {n}"
    )

  pal_lic <- colorRampPalette(c("#FFFFCC", "#F1BA50", "#F16A50"))
  pal_license <- c(rev(pal_lic(6)), "#879C9D")

  plot_data %>%
    plot_ly(
      x = ~ n,
      y = ~ rep_type_2,
      color = ~ license_fuji,
      colors = pal_license,
      marker = list(line = list(color = "#000000",
                                width = 1)),
      text = ~ license_fuji,
      textposition = 'inside',
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "auto", size = 14),
      hoverinfo = "text",
      hovertext = ~ str_glue(hovertext)
    ) %>%
    add_bars() %>%
    layout(
      margin = list(l = 4, b = 10, pad = 0),
      barmode = "stack",
      xaxis = list(
        title = FALSE,
        side = "top"
      ),
      yaxis = list(title = FALSE, side = "right"),
      uniformtext = list(minsize = 12, mode = "hide"),
      legend = list(
        orientation = 'h',
        traceorder = "normal",
        x = 1,
        xanchor = "right",
        font = list(size = 10)
      ),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9]
    ) %>%
    config(displayModeBar = FALSE)
}


plot_fair_principle_perc <- function(plot_data, color_palette)
{
  plot_data %>%
    mutate(jitter = value + round(runif(nrow(.),-0.02, 0.02), 3)) %>%
    ggplot(aes(
      x = name,
      y = jitter,
      fill = repository_type,
      group = name,
      text = paste0(
        "FAIR Principle: ",
        name,
        "<br>FAIR Score: ",
        round(value, 2) * 100,
        "%",
        "<br>Repository: ",
        repository_re3data
      )
    )) +
    geom_violin(trim = TRUE,
                scale = "width",
                na.rm = TRUE) +
    geom_quasirandom(
      width = 0.45, # 0.45
      bandwidth = 0.2, # 0.2
      varwidth = TRUE,
      size = 0.1, #0.4
      alpha = 0.4, # 0.4
      shape = 21, #21
      method = "quasirandom",
      na.rm = TRUE,
      color = "#000000"
    ) +
    # stat_summary(
    #   fun = median,
    #   geom = "errorbar",
    #   width = 0.3,
    #   size = 0.25,
    #   color = "#000000",
    #   na.rm = TRUE
    # ) +
    facet_wrap(~ repository_type) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank()
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c(color_palette[2], color_palette[3]))

    ggplotly(tooltip = "text") %>% #list("AB" = "name", "XY" = "value")
    layout(yaxis = list(
             title = list(text = "FAIR score", font = list(size = 12))),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
      config(displayModeBar = FALSE)
}

# For test, remove later
# plot_data <- test
# plot_fair_principle_perc(test, c("grey", "yellow"))
# ggplotly()


# Function for treemap plot for FAIR score by repository

plot_fair_treemap <- function(plot_data, color_palette, fair_perc, color_seq)
{
  # Prepare hovertext for str_glue()
  hovertext <-
    c(
      "<b>{repository_re3data}</b><span style='font-family:courier'>
          n = {n}
          FAIR = {fair_score}%
          F = {f_score}%
          A = {a_score}%
          I = {i_score}%
          R = {r_score}%</span>"
    )

  plot_data %>% plot_ly(
    labels = ~ repository,
    parents = ~ repository_type,
    values = ~ n,
    type = "treemap",
    branchvalues = "total",
    textinfo = "label",
    hoverinfo = "text",
    hovertext = ~ str_glue(hovertext),
    marker = list(
      colorscale = list(c(0, 0.35, 1), color_seq), # c("#AA493A", "#F1BA50", "#007265")
      cmin = 0,
      cmid = 0.35,
      cmax = 1,
      colors = ~ eval(as.symbol(fair_perc)) / 100, #fair_score
      showscale = TRUE,
      line = list(color = color_palette[9], width = 0.5),
      pad = list(b = 5, l = 5,r = 5,t = 25),
      colorbar = list(title = "FAIR<br>Score",
                      tickformat = ".0%",
                      tickfont = list(size = 10),
                      outlinecolor = color_palette[9])
    ),
    pathbar = list(visible = FALSE),
    outsidetextfont=list(size=20),
    insidetextfont=list(size=25)
  ) %>%
    layout(paper_bgcolor = color_palette[9],
           margin=list(l=0, r=0, b=10, t=0)) %>%
    config(displayModeBar = FALSE) #  plot_bgcolor = pal_bg
}

# Function for sunburst plot for FAIR score by guid

plot_fair_sunburst <- function(plot_data, color_palette, color_seq)
{
  # Prepare hovertext for str_glue()
  hovertext <-
    c(
      "<b>{guid_scheme_fuji}</b>
          n = {n}
          FAIR = {fair_score}%"
    )

  plot_data %>% plot_ly(
    ids = ~ ids,
    labels = ~ guid_scheme_fuji,
    parents = ~ repository_type,
    values = ~ n,
    type = "sunburst",
    branchvalues = "total",
    hoverinfo = "text",
    hovertext = ~ str_glue(hovertext),
    marker = list(
      colorscale = list(c(0, 0.35, 1), color_seq), # c("#AA493A", "#F1BA50", "#007265")
      cmin = 0,
      cmid = 0.35,
      cmax = 1,
      colors = ~ fair_score / 100,
      showscale = TRUE,
     # line = list(color = color_palette[9], width = 2),
     # pad = list(b = 5, l = 5,r = 5,t = 25),
      colorbar = list(title = "FAIR<br>Score",
                      tickformat = ".0%",
                      tickfont = list(size = 10),
                      outlinecolor = color_palette[9]))) %>%
    layout(paper_bgcolor = color_palette[9],
           margin=list(l=0, r=0, b=5, t=0)) %>%
    config(displayModeBar = FALSE)

}


# Function for sunburst plot for FAIR assessment according to F-UJI

plot_fair_principle_sunburst <- function(plot_data, color_palette, select_repository, color_seq)
{

  if (select_repository == "all repositories") {
    plot_data <- plot_data
  } else if (select_repository == "field-specific repositories") {
    plot_data <- plot_data %>% filter(repository_type == "field-specific repository")
  } else if (select_repository == "general-purpose repositories") {
    plot_data <- plot_data %>% filter(repository_type == "general-purpose repository")
  } else {
    plot_data <- plot_data %>% filter(repository_re3data %in% select_repository)
  }


  plot_data <- plot_data %>%
    #filter(repository_type == "field-specific repository") %>%
    #filter(repository_re3data == "figshare") %>%
    select(starts_with("fuji_percent"), starts_with("FsF")) %>%
    summarise(across(where(is.numeric), ~ mean(.)), n = n()) %>%
    pivot_longer(cols = c(starts_with("fuji_percent"), starts_with("FsF"))) %>%
    mutate(name = factor(
      name,
      levels = c(
        "fuji_percent",
        "fuji_percent_f",
        "fuji_percent_a",
        "fuji_percent_i",
        "fuji_percent_r",
        "fuji_percent_f1",
        "fuji_percent_f2",
        "fuji_percent_f3",
        "fuji_percent_f4",
        "fuji_percent_a1",
        "fuji_percent_i1",
        "fuji_percent_i2",
        "fuji_percent_i3",
        "fuji_percent_r1",
        "fuji_percent_r1_1",
        "fuji_percent_r1_2",
        "fuji_percent_r1_3",
        "FsF-F1-01D", "FsF-F1-02D", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
        "FsF-A1-01M", "FsF-A1-02M", "FsF-A1-03D",
        "FsF-I1-01M", "FsF-I2-01M", "FsF-I3-01M",
        "FsF-R1-01MD", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M", "FsF-R1.3-02D"
      ),
      labels =
        c("FAIR", "F", "A", "I", "R",
          "F1", "F2", "F3", "F4",
          "A1",
          "I1", "I2", "I3",
          "R1", "R1.1", "R1.2", "R1.3",
          "FsF-F1-01D", "FsF-F1-02D", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
          "FsF-A1-01M", "FsF-A1-02M", "FsF-A1-03D",
          "FsF-I1-01M", "FsF-I2-01M", "FsF-I3-01M",
          "FsF-R1-01MD", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M", "FsF-R1.3-02D")
    )) %>%
    arrange(name) %>%
    mutate(parent = name) %>%
    mutate(parent = str_replace(parent, "^FAIR$", NA_character_)) %>%
    mutate(parent = str_replace(parent, "^[FAIR]$", "FAIR")) %>%
    mutate(parent = str_replace_all(parent, "[0-9.]", "")) %>%
    mutate(parent = case_when(str_detect(name, "^FsF.*") ~ as.character(str_extract(name, "(?<=-)(.*)(?=-)")),
                              TRUE ~ parent)) %>%
    mutate(score = c(24,
                     7, 3, 4, 10,
                     2, 2, 1, 2,
                     3,
                     2, 1, 1,
                     4, 2, 2, 2,
                     1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 4, 2, 2, 1, 1)) %>%
    mutate(name = factor(name, levels = rev(levels(name)))) %>%
   # mutate(name = if(select_chart == "sunburst"){factor(name, levels = rev(levels(name)))}else{factor(name, levels = levels(name))}) %>%
    arrange(name) %>%
    mutate(principle = case_when(name == "FAIR" ~ "FAIR",
                                 name == "F" ~ "Findability",
                                 name == "A" ~ "Accessibility",
                                 name == "I" ~ "Interoperability",
                                 name == "R" ~ "Reusability",
                                 name == "F1" ~ "F1 — (Meta)data are assigned a globally unique and persistent identifier",
                                 name == "F2" ~ "F2 — Data are described with rich metadata (defined by R1 below)",
                                 name == "F3" ~ "F3 — Metadata clearly and explicitly include the identifier of the data they describe",
                                 name == "F4" ~ "F4 — (Meta)data are registered or indexed in a searchable resource",
                                 name == "A1" ~ "A1 — (Meta)data are retrievable by their identifier using a standardised communications protocol",
                                 name == "I1" ~ "I1 — (Meta)data use a formal, accessible, shared, and broadly applicable language for knowledge representation",
                                 name == "I2" ~ "I2 — (Meta)data use vocabularies that follow the FAIR principles",
                                 name == "I3" ~ "I3 — (Meta)data include qualified references to other (meta)data",
                                 name == "R1" ~ "R1 — Meta(data) are richly described with a plurality of accurate and relevant attributes",
                                 name == "R1.1" ~ "R1.1 — (Meta)data are released with a clear and accessible data usage license",
                                 name == "R1.2" ~ "R1.2 — (Meta)data are associated with detailed provenance",
                                 name == "R1.3" ~ "R1.3 — (Meta)data meet domain-relevant community standards",
                                 name == "FsF-F1-01D" ~ "FsF-F1-01D — Data are assigned a globally unique identifier.",
                                 name == "FsF-F1-02D" ~ "FsF-F1-02D — Data are assigned a persistent identifier.",
                                 name == "FsF-F2-01M" ~ "FsF-F2-01M — Metadata include descriptive core elements (creator, title, data identifier, publisher, publication date, summary and keywords) to support data findability.",
                                 name == "FsF-F3-01M" ~ "FsF-F3-01M — Metadata include the identifier of the data it describes.",
                                 name == "FsF-F4-01M" ~ "FsF-F4-01M — Metadata are offered in such a way that it can be retrieved programmatically.",
                                 name == "FsF-A1-01M" ~ "FsF-A1-01M — Metadata contain access level and access conditions of the data.",
                                 name == "FsF-A1-02M" ~ "FsF-A1-02M — Metadata are accessible through a standardized communication protocol.",
                                 name == "FsF-A1-03D" ~ "FsF-A1-03D — Data are accessible through a standardized communication protocol.",
                                 name == "FsF-I1-01M" ~ "FsF-I1-01M — Metadata are represented using a formal knowledge representation language.",
                                 name == "FsF-I2-01M" ~ "FsF-I2-01M — Metadata use semantic resources",
                                 name == "FsF-I3-01M" ~ "FsF-I3-01M — Metadata include links between the data and its related entities.",
                                 name == "FsF-R1-01MD" ~ "FsF-R1-01MD — Metadata specify the content of the data.",
                                 name == "FsF-R1.1-01M" ~ "FsF-R1.1-01M — Metadata include license information under which data can be reused.",
                                 name == "FsF-R1.2-01M" ~ "FsF-R1.2-01M — Metadata include provenance information about data creation or generation.",
                                 name == "FsF-R1.3-01M" ~ "FsF-R1.3-01M — Metadata follow a standard recommended by the target research community of the data.",
                                 name == "FsF-R1.3-02D" ~ "FsF-R1.3-02D — Data are available in a file format recommended by the target research community."
    )) %>%
    mutate(principle = str_wrap(principle, width = 50))

  hovertext <-
    c(
      "<b>{principle}</b>
    Score = {round(value, 1)}%
    Weight = {round(score/0.24, 1)}%"
    )

  caption <- glue::glue("n = {n}
                      Click on chart to see corresponding metrics
                      Size of wedges correspond to the weight of the FAIR assessment sections",
                        n = first(plot_data$n))


  plot_data %>% plot_ly(
    labels = ~ name,
    parents = ~ parent,
    values = ~ score,
    #text = ~ paste0(round(value, 0),"%"),
    type = "sunburst",
    maxdepth = 3,
    branchvalues = "total",
    rotation = 90,
    sort = FALSE,
    hoverinfo = "text",
    hovertext = ~ str_glue(hovertext),
    textinfo = "label",
    insidetextfont = list(size = 14),
    marker = list(
      colorscale = list(c(0, 0.35, 1), color_seq), # viridis c("#FDE725FF", "#21908CFF", "#440154FF") # red green c("#AA493A", "#F1BA50", "#007265")
      cmin = 0,
      cmid = 0.35,
      cmax = 1,
      colors = ~ value / 100,
      showscale = TRUE,
     # line = list(width = 1.5),
     # pad = list(b = 5, l = 5,r = 5,t = 25),
      colorbar = list(title = "FAIR<br>Score",
                      tickformat = ".0%",
                      tickfont = list(size = 10),
                      outlinecolor = color_palette[9])
    )
  ) %>%
    layout(paper_bgcolor = color_palette[9],
           margin=list(l=0, r=0, b=35, t=0),
           annotations =
             list(x = 0, y = -0.08, text = caption,
                  align = "left",
                  showarrow = F, xref='paper', yref='paper',
                  xanchor='auto', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=11))) %>%
    config(displayModeBar = FALSE)
}
