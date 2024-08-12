preprintsOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

preprintsServer <- function(id, preprint_data, allpubs_data, total, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({

      preprints_plot_data <- allpubs_data |>

        count(year) |>
        rename(count = n) |>
        left_join(preprint_data, by = "year") |>
        replace_na(list(n_preprints = 0)) |>
        group_by(year) |>
        summarise(across(c(count, n_preprints), sum)) |>
        mutate(perc = round(n_preprints/(count + n_preprints) * 100, 2))

        #add total number of publications to the dataset
        if (total() == FALSE) { # relative numbers

          return(plot_preprints(preprints_plot_data, color_palette = color_palette))

          } else { # absolute numbers

            return(plot_preprints_absolute(preprints_plot_data, color_palette = color_palette))
          }
      })

  })
}

# Preprints plot

plot_preprints <- function(plot_data, color_palette)
{
  # plot_data <- plot_data |>
  #   arrange(year)

  plot_ly(plot_data, x = ~year, y = ~perc,
          name = "Preprints",
          type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8),
          text = ~paste0(n_preprints, " out of ", count + n_preprints)) |>
    layout(yaxis = list(title = "<b>Percent of total articles\nthat are preprints</b>",
                        range = c(0, 50),
                        ticksuffix = "%"
    ),
    xaxis = list(title = "<b>Year</b>",
                 dtick = 1),
    paper_bgcolor = color_palette[9],
    plot_bgcolor = color_palette[9],
    legend = list(xanchor = "left",
                  legendwidth = 1.3,
                  bgcolor = "rgba(0,0,0,0)",
                  x = 0.05,
                  y = 1.2)) |>
    plotly::config(displayModeBar = FALSE)
}

plot_preprints_absolute <- function(plot_data, color_palette)
{
  # plot_data <- plot_data |>
  #   arrange(year)

  plot_ly(plot_data, x = ~year, y = ~n_preprints,
          name = "Preprints",
          type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) |>
    add_trace(y = ~count, name = "Journal articles", mode = "lines+markers",
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) |>
    layout(yaxis = list(title = "<b>Total number</b>"
                        #, range = c(0, 6000)
                        ),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9],
           legend = list(xanchor = "left",
                         legendwidth = 1.3,
                         bgcolor = "rgba(0,0,0,0)",
                         x = 0.05,
                         y = 1.2)) |>
    plotly::config(displayModeBar = FALSE)
}
