rtransparentOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}
# screened_data <- dashboard_metrics
rtransparentServer <- function(id, screened_data, type, total, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({
      # screened_data <- dashboard_metrics
      transparency_data_plotly <- screened_data |>
        make_rtransparent_plot_data()

      if (type == "coi") { # if the coi output is to be visualized

        if (total() == FALSE) { # relative numbers

          transparency_plot <- plot_coi_perc(transparency_data_plotly, color_palette = color_palette)

        } else { # absolute numbers

          transparency_plot <- plot_coi_total(transparency_data_plotly, color_palette = color_palette)
        }

      } else { # if the orcid output is to be visualized instead

        if (total() == FALSE) { # relative numbers

          transparency_plot <- plot_funding_perc(transparency_data_plotly, color_palette = color_palette)

        } else { # absolute numbers

          transparency_plot <- plot_funding_total(transparency_data_plotly, color_palette = color_palette)
        }
      }
      transparency_plot
    })
  })
}

get_current_rtransparent <- function(data_table, metric) {
  # data_table <- transparency_data_plotly
  data_table |>
    make_rtransparent_plot_data() |>
    dplyr::filter(year == max(year)) |>
    dplyr::pull({{ metric }}) |>
    map_chr(\(x) paste(x, "%"))
}
# data_table <- dashboard_metrics
make_rtransparent_plot_data <- function(data_table) {
  data_table |>
    dplyr::group_by(year) |>
    dplyr::summarize(total_screened = sum(!is.na(has_coi), na.rm = TRUE),
                     has_coi = sum(has_coi, na.rm = TRUE),
                     has_no_coi = total_screened - has_coi,
                     has_funding = sum(has_funding, na.rm = TRUE),
                     has_no_funding = total_screened - has_funding,
                     total = n(),
                     not_screened = total - total_screened,
                     perc_has_coi = round(has_coi / total_screened * 100),
                     perc_has_funding = round(has_funding / total_screened * 100))
}


# dashboard_metrics |> make_rtransparent_plot_data()
# dashboard_metrics |> get_current_contribot(perc_has_contrib)
# get_current_contribot(dashboard_metrics, perc_has_contrib)
# plot_data <- transparency_data_plotly

plot_coi_perc <- function(plot_data, color_palette)
{
  plot_output <- plot_ly(plot_data, x = ~year, y = ~perc_has_coi,
                         name = "COI Statement", type = "bar",
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

plot_coi_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_coi,
          name = "COI Statement", type = "bar",
          marker = list(color = color_palette[3],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    add_trace(y = ~has_no_coi,
              name = "No COI Statement",
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

