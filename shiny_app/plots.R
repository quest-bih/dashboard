
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
           yaxis = list(title = '<b>Percentage Open Access</b>',
                        range = c(0, 100)),
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
plot_OD_perc <- function(plot_data, color_palette, zoom_in)
{
  if(zoom_in) {
    yrange <- c(0, 20)
  } else {
    yrange <- c(0, 50)
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
           legend = list(xanchor = "right"),
           yaxis = list(title = '<b>Percentage of publications</b>',
                        range = yrange),
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
              name = 'Not accessible',
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
plot_OC_perc <- function(plot_data, color_palette, zoom_in)
{
  if(zoom_in) {
    yrange <- c(0, 5)
  } else {
    yrange <- c(0, 50)
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
           legend = list(xanchor = "right"),
           yaxis = list(title = '<b>Percentage of publications</b>',
                        range = yrange),
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
              name = 'Not accessible',
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
           legend = list(xanchor = "right")) %>%
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
                        range = c(0, 1700)),
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
    layout(yaxis = list(title = '<b>Percentage of trials</b>',
                        range = c(0, 100)),
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
    layout(yaxis = list(title = '<b>Percentage of trials</b>',
                        range = c(0, 105)),
           xaxis = list(title = '<b>Trial completion year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "right")) %>%
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
           legend = list(xanchor = "right")) %>%
    config(displayModeBar = FALSE)
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
    layout(yaxis = list(title = '<b>Percentage of publications</b>',
                        range = c(0, 28)),
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
    layout(yaxis = list(title = '<b>Percentage of publications</b>',
                        range = c(0, 28)),
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
      textfont = list(color = "#ffffff", size = 14)
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
        x = 1,
        xanchor = "right",
        font = list(size = 10)
      ),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9]
    ) %>%
    config(displayModeBar = FALSE)
}

plot_fair_license_total <- function(plot_data, color_palette)
{
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
      textfont = list(color = "#ffffff", size = 14)
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
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.3,
      size = 0.25,
      color = "#000000",
      na.rm = TRUE
    ) +
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
             title = list(text = "FAIR score according to F-UJI", font = list(size = 12))),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) %>%
      config(displayModeBar = FALSE)
}


plot_fair_treemap <- function(plot_data, color_palette)
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
      colorscale = list(c(0, 0.35, 1), c("#AA493A", "#F1BA50", "#007265")),
      cmin = 0,
      cmid = 0.35,
      cmax = 1,
      colors = ~ fair_score / 100,
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
           margin=list(l=0, r=0, b=5, t=0)) %>%
    config(displayModeBar = FALSE) #  plot_bgcolor = pal_bg
}


# plot_fair_principle_perc(make_fair_principle_plot_data(plot_data), color_palette)

# plot_data <- read_csv("shiny_app/data/fair_assessment.csv")
#
# color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
#                    "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
#                    "#DCE3E5")
#
# plot_fair_license_total(plot_data, color_palette)
# plot_fair_license_perc(plot_data, color_palette)
