orcidOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

orcidServer <- function(id, data, type, total, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({

      if (type == "total") {
        orcid_plot <- plot_orcid(data, color_palette)
      } else if (type == "pubs") {

        plot_data <- make_orcid_pubs_plot_data(data)

        if (total() == FALSE) { # relative numbers


          orcid_plot <- plot_orcid_pubs_perc(plot_data, color_palette)
        } else {
          orcid_plot <- plot_orcid_pubs_total(plot_data, color_palette)
        }

      }

      orcid_plot
    })

  })
}

# data_table <- dashboard_metrics
make_orcid_pubs_plot_data <- function(data_table) {
  check <-
  data_table |>
    dplyr::mutate(has_orcid_corresponding = has_any_orcid & corresponding_author_charite,
                  screened_corresponding = corresponding_author_charite & !is.na(has_contrib)) |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      has_corresponing_charite = sum(corresponding_author_charite, na.rm = TRUE),
      has_any_orcid = sum(has_any_orcid, na.rm = TRUE),
      has_orcid_corresponding = sum(has_orcid_corresponding, na.rm = TRUE),
      total_screened = sum(!is.na(has_contrib)),
      total_screened_corresponding = sum(screened_corresponding, na.rm = TRUE),
      total = n()) |>
    dplyr::mutate(no_orcids = total_screened - has_any_orcid,
                  no_orcid_corresponding = total_screened_corresponding - has_orcid_corresponding,
                  not_screened = total - total_screened,
                  not_screened_corresponding =  has_corresponing_charite - total_screened_corresponding,
                  perc_has_any_orcid = round(has_any_orcid / total_screened * 100),
                  perc_has_orcid_corresponding = round(has_orcid_corresponding / total_screened_corresponding * 100))
}


get_current_orcids_from_pubs <- function(data_table) {
  data_table |>
    make_orcid_pubs_plot_data() |>
    dplyr::filter(year == max(year)) |>
    dplyr::pull(perc_has_orcid_corresponding) |>
    purrr::map_chr(\(x) paste(x, "%"))

}
# plot_data <- make_orcid_pubs_plot_data(data_table)
plot_orcid <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~as.Date(date, format= "%Y-%m-%d"), y = ~orcid_count,
          name = "Registered ORCIDs",
          type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[3], width = 4),
          marker = list(color = color_palette[3], size = 1)) |>
    layout(yaxis = list(title = "<b>Registered ORCIDs</b>",
                        range = c(0, 2500)),
           xaxis = list(title = "<b>Date</b>",
                        type = "date"),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}


plot_orcid_pubs_perc <- function(plot_data, color_palette)
{
  yrange <- c(0, 100)

  plot_ly(plot_data, x = ~year, y = ~perc_has_orcid_corresponding,
          name = "ORCIDs in Publications", type = "bar",
          marker = list(color = color_palette[3],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    layout(barmode = "stack",
           legend = list(xanchor = "left",
                         # font = list(size = 11),
                         bgcolor = "rgba(0,0,0,0)",
                         x = 0.05,
                         y = 1),
           yaxis = list(title = "<b>Publications</b>",
                        range = yrange,
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)

}

plot_orcid_pubs_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_orcid_corresponding,
          name = "ORCIDs in Publications", type = "bar",
          marker = list(color = color_palette[3],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    add_trace(y = ~no_orcid_corresponding,
              name = "No ORCIDs in Publications",
              marker = list(color = color_palette[5],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~not_screened_corresponding,
              name = "Article<br>not screened",
              marker = list(color = color_palette[1],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    layout(barmode = "stack",
           yaxis = list(title = "<b>Number of publications with<br>a Charité correspondence author</b>"
                        # range = c(0, 25000)
           ),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)

}

