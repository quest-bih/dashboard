creditOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

creditServer <- function(id, data, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({
      plot_credit(data, color_palette)
    })

  })
}



plot_credit <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~as.Date(date, format= "%Y-%m-%d"), y = ~orcid_count,
          name = "Registered ORCIDs",
          type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[3], width = 4),
          marker = list(color = color_palette[3], size = 1)) |>
    layout(yaxis = list(title = "<b>Registered ORCIDs</b>",
                        range = c(0, 2200)),
           xaxis = list(title = "<b>Date</b>",
                        type = "date"),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}
