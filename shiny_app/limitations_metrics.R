limitationsOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}
# screened_data <- dashboard_metrics
limitationsServer <- function(id, screened_data, total, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({
      # screened_data <- dashboard_metrics
      limitations_data_plotly <- screened_data |>
        make_limitations_plot_data()

        if (total() == FALSE) { # relative numbers

          limitations_plot <- plot_limitations_perc(limitations_data_plotly, color_palette = color_palette)

        } else { # absolute numbers

          limitations_plot <- plot_limitations_total(limitations_data_plotly, color_palette = color_palette)
        }

      limitations_plot
    })
  })
}

get_current_limitations <- function(data_table, metric) {
  # data_table <- limitations_data_plotly
  data_table |>
    make_limitations_plot_data() |>
    dplyr::filter(year == max(year)) |>
    dplyr::pull({{ metric }}) |>
    map_chr(\(x) paste(x, "%"))
}
# data_table <- dashboard_metrics
make_limitations_plot_data <- function(data_table) {
  data_table |>
    dplyr::group_by(year) |>
    dplyr::summarize(total_screened = sum(!is.na(has_limitations), na.rm = TRUE),
                     has_limitations = sum(has_limitations, na.rm = TRUE),
                     has_no_limitations = total_screened - has_limitations,
                     # has_funding = sum(has_funding, na.rm = TRUE),
                     # has_no_funding = total_screened - has_funding,
                     total = n(),
                     not_screened = total - total_screened,
                     perc_has_limitations = round(has_limitations / total_screened * 100)
                     )
}


# dashboard_metrics |> make_limitations_plot_data()
# plot_data <- limitations_data_plotly

plot_limitations_perc <- function(plot_data, color_palette)
{
  plot_output <- plot_ly(plot_data, x = ~year, y = ~perc_has_limitations,
                         name = "Limitations acknowledged", type = "bar",
                         marker = list(color = color_palette[3],
                                       line = list(color = "rgb(0,0,0)",
                                                   width = 1.5)))

  plot_output |>
    layout(barmode = "stack",
           legend = list(xanchor = "left",
                         # font = list(size = 11),
                         bgcolor = "rgba(0,0,0,0)",
                         x = 0.05,
                         y = 1),
           yaxis = list(title = "<b>Publications</b>",
                        range = c(0, 100),
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)

}

plot_limitations_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_limitations,
          name = "Limitations acknowledged", type = "bar",
          marker = list(color = color_palette[3],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    add_trace(y = ~has_no_limitations,
              name = "No limitations acknowledged",
              marker = list(color = color_palette[5],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~not_screened,
              name = "Article<br>not screened",
              marker = list(color = color_palette[1],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    layout(barmode = "stack",
           yaxis = list(title = "<b>Number of publications</b>"
           ),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}
