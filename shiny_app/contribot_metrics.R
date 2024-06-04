contribotOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

contribotServer <- function(id, screened_data, type, total, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({
# screened_data <- dashboard_metrics
      authorship_data_plotly <- screened_data |>
        make_contribot_plot_data()

      if (type == "credit") { # if the credit output is to be visualized

        if (total() == FALSE) { # relative numbers

          Authorshipplot <- plot_contrib_perc(authorship_data_plotly, color_palette = color_palette)

        } else { # absolute numbers

          Authorshipplot <- plot_contrib_total(authorship_data_plotly, color_palette = color_palette)
        }

      } else { # if the orcid output is to be visualized instead

        if (total() == FALSE) { # relative numbers

          Authorshipplot <- plot_orcid_perc(authorship_data_plotly, color_palette = color_palette)

        } else { # absolute numbers

          Authorshipplot <- plot_orcid_total(authorship_data_plotly, color_palette = color_palette)
        }
      }
      Authorshipplot
      })
  })
}


get_current_contribot <- function(data_table, metric) {
  # data_table <- authorship_data_plotly
  data_table |>
    make_contribot_plot_data() |>
    dplyr::filter(year == max(year)) |>
    dplyr::pull({{ metric }}) |>
    map_chr(\(x) paste(x, "%"))
}

make_contribot_plot_data <- function(data_table) {
  data_table |>
    dplyr::group_by(year) |>
    dplyr::summarize(total_screened = sum(!is.na(has_contrib), na.rm = TRUE),
              has_contrib = sum(has_contrib, na.rm = TRUE),
              has_no_contrib = total_screened - has_contrib,
              has_orcid = sum(has_orcid, na.rm = TRUE),
              has_no_orcid = total_screened - has_orcid,
              total = n(),
              not_screened = total - total_screened,
              perc_has_contrib = round(has_contrib / total_screened * 100),
              perc_has_orcid = round(has_orcid / total_screened * 100))
}


# dashboard_metrics |> make_contribot_plot_data()
# dashboard_metrics |> get_current_contribot(perc_has_contrib)
# get_current_contribot(dashboard_metrics, perc_has_contrib)
# plot_data <- authorship_data_plotly

plot_contrib_perc <- function(plot_data, color_palette)
{
  plot_output <- plot_ly(plot_data, x = ~year, y = ~perc_has_contrib,
                         name = "Authorship Statement", type = "bar",
                         marker = list(color = color_palette[3],
                                       line = list(color = "rgb(0,0,0)",
                                                   width = 1.5)))
  #   add_trace(y = ~OC_other_perc,
  #             name = "other repository <br>or website",
  #             marker = list(color = color_palette[6],
  #                           line = list(color = "rgb(0,0,0)",
  #                                       width = 1.5)))
  plot_output |>
    layout(barmode = "stack",
           legend = list(xanchor = "left",
                         # font = list(size = 11),
                         bgcolor = "rgba(0,0,0,0)",
                         x = 0.05,
                         y = 1),
           yaxis = list(title = "<b>Publications</b>",
                        # range = c(0, 100),
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)

}

plot_contrib_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_contrib,
          name = "Authorship Statement", type = "bar",
          marker = list(color = color_palette[3],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    add_trace(y = ~has_no_contrib,
              name = "No Authorship Statement",
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
                        # range = c(0, 6300)
           ),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}

