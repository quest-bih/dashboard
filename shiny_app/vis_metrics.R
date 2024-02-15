visOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

visServer <- function(id, vis_data, type, total, color_palette) {
  moduleServer(id, function(input, output, session) {

  barzooka_data <- make_barzooka_plot_data(vis_data)

    output$plot <- renderPlotly({
      if (total() == FALSE) { # relative numbers
        if (type == "problem") {
          Visplot <- plot_barzooka_problem_perc(barzooka_data, color_palette)
        } else if (type == "inform") {
          Visplot <- plot_barzooka_inform_perc(barzooka_data, color_palette)
        }
      } else { # absolute numbers
        if (type == "problem") {
          Visplot <- plot_barzooka_problem_total(barzooka_data, color_palette)
        } else if (type == "inform") {
          Visplot <- plot_barzooka_inform_total(barzooka_data, color_palette)
        }
      }
      Visplot
    })

  })
}


make_barzooka_plot_data <- function(data_table) {
  data_table |>
    filter(pdf_downloaded == TRUE) |>
    group_by(year) |>
    summarize(total = n(),
              has_bar = sum(bar > 0, na.rm = TRUE),
              has_pie = sum(pie > 0, na.rm = TRUE),
              has_bardot = sum(bardot > 0, na.rm = TRUE),
              has_box = sum(box > 0, na.rm = TRUE),
              has_dot = sum(dot > 0, na.rm = TRUE),
              has_hist = sum(hist > 0, na.rm = TRUE),
              has_violin = sum(violin > 0, na.rm = TRUE),
              has_informative = sum(bardot > 0 | box > 0 | dot > 0 | hist > 0 | violin > 0,
                                    na.rm = TRUE))
}

get_current_vis <- function(data_table, metric) {

  data_table |>
    # dashboard_metrics |>
    make_barzooka_plot_data() |>
    group_by(year) |>
    summarise(across(.cols = c(has_bar, has_informative, total),
                     \(x) sum(x, na.rm = TRUE))) |>
    filter(year == max(year)) |>
    mutate(perc_bar = round(has_bar/total, 2) * 100,
           perc_informative = round(has_informative/total, 2) * 100) |>
    pull({{ metric }}) |>
    map_chr(\(x) paste(x, "%"))

}

plot_barzooka_problem_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~round(has_bar/total*100, 1),
          name = "bar graph", type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[2], width = 3),
          marker = list(color = color_palette[2], size = 8)) |>
    add_trace(y = ~round(has_pie/total*100, 1), name = "pie chart", mode = "lines+markers",
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) |>
    layout(yaxis = list(title = "<b>Publications</b>",
                        range = c(0, 28),
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}

plot_barzooka_problem_total<- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_bar,
          name = "bar graph", type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[2], width = 3),
          marker = list(color = color_palette[2], size = 8)) |>
    add_trace(y = ~has_pie, name = "pie chart", mode = "lines+markers",
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) |>
    add_trace(y = ~total, name = "publications screened", mode = "lines+markers",
              line = list(color = color_palette[5]),
              marker = list(color = color_palette[5])) |>
    layout(yaxis = list(title = "<b>Publications with graph type</b>"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}


# more informative graphs
plot_barzooka_inform_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~round(has_informative/total*100, 1),
          name = "any informative", type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[1], width = 3),
          marker = list(color = color_palette[1], size = 8)) |>
    add_trace(y = ~round(has_bardot/total*100, 1), name = "bar graph with dots", mode = "lines+markers",
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) |>
    add_trace(y = ~round(has_box/total*100, 1), name = "box plot", mode = "lines+markers",
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) |>
    add_trace(y = ~round(has_dot/total*100, 1), name = "dot plot", mode = "lines+markers",
              line = list(color = color_palette[4]),
              marker = list(color = color_palette[4])) |>
    add_trace(y = ~round(has_hist/total*100, 1), name = "histogram", mode = "lines+markers",
              line = list(color = color_palette[6]),
              marker = list(color = color_palette[6])) |>
    add_trace(y = ~round(has_violin/total*100, 1), name = "violin plot", mode = "lines+markers",
              line = list(color = color_palette[7]),
              marker = list(color = color_palette[7])) |>
    layout(yaxis = list(title = "<b>Publications</b>",
                        range = c(0, 40),
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}

plot_barzooka_inform_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~has_informative,
          name = "any informative", type = "scatter", mode = "lines+markers",
          line = list(color = color_palette[1], width = 3),
          marker = list(color = color_palette[1], size = 8)) |>
    add_trace(y = ~has_bardot, name = "bar graph with dots", mode = "lines+markers",
              line = list(color = color_palette[2]),
              marker = list(color = color_palette[2])) |>
    add_trace(y = ~has_box, name = "box plot", mode = "lines+markers",
              line = list(color = color_palette[3]),
              marker = list(color = color_palette[3])) |>
    add_trace(y = ~has_dot, name = "dot plot", mode = "lines+markers",
              line = list(color = color_palette[4]),
              marker = list(color = color_palette[4])) |>
    add_trace(y = ~has_hist, name = "histogram", mode = "lines+markers",
              line = list(color = color_palette[6]),
              marker = list(color = color_palette[6])) |>
    add_trace(y = ~has_violin, name = "violin plot", mode = "lines+markers",
              line = list(color = color_palette[7]),
              marker = list(color = color_palette[7])) |>
    add_trace(y = ~total, name = "publications screened", mode = "lines+markers",
              line = list(color = color_palette[5]),
              marker = list(color = color_palette[5])) |>
    layout(yaxis = list(title = "<b>Publications with graph type</b>"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}
