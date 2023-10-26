sumresOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

sumresServer <- function(id, sumres_data, total, color_palette) {
  moduleServer(id, function(input, output, session) {


    output$plot <- renderPlotly({
        sumres_plot_data <- sumres_data |>
          arrange(retrieval_date)
      #add total number of publications to the dataset
      if (total() == FALSE) { # relative numbers
        return(plot_sumres(sumres_plot_data, color_palette = color_palette))
      } else { # absolute numbers
        return(plot_sumres_absolute(sumres_plot_data, color_palette = color_palette))
        }
    })
  })
}

plot_sumres <- function(plot_data, color_palette)
{
  plot_ly(ungroup(plot_data), x = ~as.Date(retrieval_date, format= "%Y-%m-%d"), y = ~round(perc_reported * 100, 1),
          name = "Reported trials",
          type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8))  |>
    layout(yaxis = list(title = "<b>Trials</b>",
                        range = c(0, 105),
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Date</b>",
                        type = "date"),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "right")) |>
    plotly::config(displayModeBar = FALSE)
}

plot_sumres_absolute <- function(plot_data, color_palette)
{
  plot_ly(ungroup(plot_data), x = ~as.Date(retrieval_date, format= "%Y-%m-%d"), y = ~total_reported,
          name = "Reported trials",
          type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8))  |>
    add_trace(y = ~total_due, name = "Due trials", mode = "lines+markers",
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2]))  |>
    layout(yaxis = list(title = "<b>Number of trials</b>"),
           xaxis = list(title = "<b>Date</b>",
                        type = "date"),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])  |>
    plotly::config(displayModeBar = FALSE)
}

get_current_sumres <- function(tib) {
  tib |>
    filter(retrieval_date == max(retrieval_date)) |>
    summarise(perc_reported = round(sum(total_reported)/sum(total_due) * 100)) |>
    pull(perc_reported) |>
    map_chr(\(x) paste(x, "%"))
}

get_total_sumres <- function(tib) {
  tib |>
    summarise(perc_reported = round(sum(total_reported)/sum(total_due) * 100)) |>
    pull(perc_reported) |>
    map_chr(\(x) paste(x, "%"))
}
