preprintsOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

preprintsServer <- function(id, preprint_data, allpubs_data, type, total, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({

      preprints_plot_data <- allpubs_data |>
        count(year) |>
        rename(count = n) |>
        left_join(preprint_data, by = "year") |>
        replace_na(list(n_preprints = 0)) |>
        group_by(year) |>
        summarise(across(c(count, n_preprints, n_preprints_with_articles, perc_preprints_with_articles), sum)) |>
        mutate(perc_preprints = round(n_preprints/(count + n_preprints) * 100, 2),
               perc_preprints_with_articles = round(perc_preprints_with_articles * 100, 2))

        #add total number of publications to the dataset
        if (total() == FALSE) { # relative numbers

          if (type == "overall") {
            # return(plot_preprints(preprints_plot_data, color_palette = color_palette))
            return(plot_preprints(preprints_plot_data, color_palette = color_palette))
          } else {
            return(plot_preprints_published(preprints_plot_data, color_palette = color_palette))
          }

        } else { # absolute numbers

          if (type == "overall") {
            return(plot_preprints_absolute(preprints_plot_data, color_palette = color_palette))
          } else {
            return(plot_preprints_published_absolute(preprints_plot_data, color_palette = color_palette))
          }

        }
      })

  })
}

# Preprints plot

plot_preprints <- function(plot_data, color_palette)
{
  # plot_data <- plot_data |>
  #   arrange(year)

  plot_ly(plot_data, x = ~year, y = ~perc_preprints,
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


plot_preprints_published <- function(plot_data, color_palette)  {
  # plot_data <- plot_data |>
  #   arrange(year)

  plot_ly(plot_data, x = ~year, y = ~perc_preprints_with_articles,
          name = "Preprints published",
          type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8),
          text = ~paste0(n_preprints_with_articles, " out of ", n_preprints)) |>
    layout(yaxis = list(title = "<b>Percent of preprints<br>with publications</b>",
                        range = c(0, 100),
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

plot_preprints_published_absolute <- function(plot_data, color_palette) {
  # plot_data <- plot_data |>
  #   arrange(year)
  plot_ly(plot_data, x = ~year, y = ~n_preprints_with_articles,
          name = "Preprints also published in peer-reviewed journals",
          type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[3], width = 3),
          marker = list(color = color_palette[3], size = 8)) |>
    add_trace(y = ~n_preprints, name = "Preprints total", mode = "lines+markers",
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

get_preprints_perc <- function(tib, col, type = "current") {

  if (type == "current") {
    tib <- tib |>
      filter(year == max(year))
  }
  current <- tib |>
    summarise(across(contains("n_"),  sum)) |>
    mutate(perc_preprints = round(n_preprints/(n_articles + n_preprints)* 100),
           perc_preprints_with_articles = round(n_preprints_with_articles / n_preprints * 100)) |>
    pull({{ col }})

  paste0(current, " %")

}
