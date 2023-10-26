ctgovOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

ctgovServer <- function(id, ctgov_data, total, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({
      ctgov_plot_data <- ctgov_data |>
        arrange(year)

        #add total number of publications to the dataset
      if (total() == FALSE) { # relative numbers
        return(plot_prospreg(ctgov_plot_data, color_palette = color_palette))
        } else { # absolute numbers
        return(plot_prospreg_absolute(ctgov_plot_data, color_palette = color_palette))

      }
    })
  })
}

plot_prospreg <- function(plot_data, color_palette)
{
  plot_ly(ungroup(plot_data), x = ~year, y = ~round(prop_prosp_reg * 100), #type = 'bar',
          type = 'scatter', mode = 'lines+markers',
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) |>
    layout(yaxis = list(title = '<b>Trials</b>',
                        range = c(0, 100),
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}

plot_prospreg_absolute <- function(plot_data, color_palette)
{
  plot_ly(ungroup(plot_data), x = ~year, y = ~has_prosp_reg,
          name = 'prospective registration', type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 2))) |>
    add_trace(y = ~no_prosp_reg,
              name = 'no prospective registration',
              marker = list(color = color_palette[5],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 2))) |>
    layout(barmode = 'stack',
           yaxis = list(title = '<b>Number of trials</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(orientation = "h",
                         font = list(size = 11),
                         xanchor = "left",
                         bgcolor = "rgba(0,0,0,0)",
                         x = -0.01,
                         y = 1.6
           ))  |>
    plotly::config(displayModeBar = FALSE)
}


get_total_ctgov <- function(data_table, metric) {
  data_table |>
    summarise(across(.cols = c(has_prosp_reg, no_prosp_reg),
                     \(x) sum(x, na.rm = TRUE))) |>
    mutate(total_reg = has_prosp_reg + no_prosp_reg,
           perc_prosp_reg = round(has_prosp_reg/total_reg, 2) * 100) |>
    pull({{ metric }}) |>
    map_chr(\(x) paste(x, "%"))

}

get_current_ctgov <- function(data_table, metric, time_offset = 0) {
  data_table |>
    group_by(year) |>
    summarise(across(.cols = c(has_prosp_reg, no_prosp_reg),
                     \(x) sum(x, na.rm = TRUE))) |>
    filter(year == max(year) - time_offset) |>
    mutate(total_reg = has_prosp_reg + no_prosp_reg,
           perc_prosp_reg = round(has_prosp_reg/total_reg, 2) * 100) |>
    pull({{ metric }}) |>
    map_chr(\(x) paste(x, "%"))

}
