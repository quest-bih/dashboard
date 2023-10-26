OAOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

OAServer <- function(id, allpubs_data, total, color_palette) {
  moduleServer(id, function(input, output, session) {


  output$plot <- renderPlotly({
    if (total() == FALSE) { # relative numbers
      OA_plot_data_plotly <- allpubs_data |>
        make_OA_plot_data() |>
        select(-OA, -all) |>
        pivot_wider(names_from = category, values_from = perc)

      OAplot <- plot_OA_perc(OA_plot_data_plotly, color_palette = color_palette)

      } else { # absolute numbers

        OA_plot_data_plotly <- allpubs_data |>
          make_OA_plot_data_total() |>
          select(-perc, -all) |>
          pivot_wider(names_from = category, values_from = OA)

        OAplot <- plot_OA_total(OA_plot_data_plotly, color_palette = color_palette)

        }
      OAplot
    })

  })
}


#----------------------------------------------------------------------
# Open Access data plot and helping functions
#----------------------------------------------------------------------

get_current_OA <- function(data_table) {

 data_table |>
    make_OA_plot_data() |>
    #do not count the bronze category for the total number of publ
    filter(category != "bronze") |>
    group_by(year) |>
    summarise(OA_perc = sum(perc)) |>
    filter(year == max(year)) |>
    pull(OA_perc) |>
    round() |>
    map_chr(\(x) paste(x, "%"))

}

get_total_OA <- function(data_table) {

  data_table |>
    make_OA_plot_data() |>
    #do not count the bronze category for the total number of publ
    filter(category != "bronze") |>
    group_by(year) |>
    summarise(OA_total = sum(OA),
              total = first(all)) |>
    summarise(OA_perc = sum(OA_total) / sum(total) * 100) |>
    pull(OA_perc) |>
    round() |>
    map_chr(\(x) paste(x, "%"))

}



make_OA_plot_data_total <- function(data_table)
{
  OA_plot_data <- data_table |>
    group_by(year, OA_color) |>
    summarise(count = n()) |>
    calculate_OA_percentages(c("gold", "green", "hybrid", "bronze", "closed")) |>
    rename(category = OA_color)

  return(OA_plot_data)
}


make_OA_plot_data <- function(data_table)
{
  OA_plot_data <- data_table |>
    #do not filter the missing OA publications to be in line with library numbers
    #filter(!is.na(OA_color)) |>
    group_by(year, OA_color) |>
    summarise(count = n()) |>
    calculate_OA_percentages(c("gold", "green", "hybrid", "bronze")) |>
    rename(category = OA_color)

  return(OA_plot_data)
}


calculate_OA_percentages <- function(OA_data, categories)
{
  #number of publications
  publ_all <- OA_data |>
    group_by(year) |>
    summarise(all = sum(count))

  #number of publ in each OA category
  publ_OA_colors <- OA_data |>
    filter(OA_color %in% categories) |>
    group_by(OA_color, year) |>
    summarise(OA = sum(count))

  #OA percentages
  OA_perc <- publ_OA_colors |>
    left_join(publ_all) |>
    mutate(perc = round(OA/all *100, 1)) |>
    ungroup()

  return(OA_perc)
}


# OA plots

plot_OA_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~gold, name = "Gold", type = "bar",
          marker = list(color = color_palette[3],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    add_trace(y = ~green, name = "Green",
              marker = list(color = color_palette[6],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~hybrid, name = "Hybrid",
              marker = list(color = color_palette[7],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~bronze, name = "Bronze",
              marker = list(color = color_palette[10],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5)),
              visible = "legendonly") |>
    layout(barmode = "stack",
           yaxis = list(title = "<b>Open Access</b>",
                        range = c(0, 100),
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)

}

plot_OA_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~gold, name = "Gold", type = "bar",
          marker = list(color = color_palette[3],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    add_trace(y = ~green, name = "Green",
              marker = list(color = color_palette[6],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~hybrid, name = "Hybrid",
              marker = list(color = color_palette[7],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~bronze, name = "Bronze",
              marker = list(color = color_palette[10],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~closed, name = "Closed",
              marker = list(color = color_palette[5],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    layout(barmode = "stack",
           yaxis = list(title = "<b>Publications</b>"
                        # range = c(0, 6300)
                        ),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)

}
