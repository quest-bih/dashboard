
# Trial publication
plot_intovalue_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~completion_year, y = ~round(percentage_published_2_years * 100,1),
          name = 'results within 2 years',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8))  |>
    add_trace(y = ~round(percentage_published_5_years * 100,1),
              name = 'results within 5 years', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) |>
    layout(yaxis = list(title = '<b>Trials</b>',
                        range = c(0, 105),
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Trial completion year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(orientation = "h",
                         xanchor = "left",
                         bgcolor = "rgba(0,0,0,0)",
                         font = list(size = 11),
                         x = 0.01,
                         y = 1.6
           )) |>
    plotly::config(displayModeBar = FALSE)
}

plot_intovalue_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~completion_year, y = ~trials_with_publication_2_years,
          name = 'results within 2 years',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) |>
    add_trace(y = ~trials_with_publication_5_years,
              name = 'results within 5 years', mode = 'lines+markers',
              line = list(color = color_palette[4]),
              marker = list(color = color_palette[4])) |>
    add_trace(y = ~total_trials_2_years,
              name = 'total trials 2 years', mode = 'lines+markers',
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) |>
    add_trace(y = ~total_trials_5_years,
              name = "total trials 5 years", mode = "lines+markers",
              line = list(color = color_palette[5]),
              marker = list(color = color_palette[5])) |>
    layout(yaxis = list(title = "<b>Number of trials</b>",
                        range = c(0, 100)),
           xaxis = list(title = "<b>Trial completion year</b>"),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(orientation = "h",
                         xanchor = "left",
                         bgcolor = "rgba(0,0,0,0)",
                         font = list(size = 11),
                         x = 0.01,
                         y = 1.6
           )
    ) |>
    plotly::config(displayModeBar = FALSE)
}


