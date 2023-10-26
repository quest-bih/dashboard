ODOutput <- function(id, ...) {

  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

ODServer <- function(id, screened_data, type, total, color_palette) {
  moduleServer(id, function(input, output, session) {


    output$plot <- renderPlotly({

      OD_plot_data_plotly <- screened_data |>
        make_oddpub_plot_data(year)

      if (type == "data") { # if the data output is to be visualized

        if (total() == FALSE) { # relative numbers

          ODplot <- plot_OD_perc(OD_plot_data_plotly, color_palette = color_palette)

          } else { # absolute numbers

            ODplot <- plot_OD_total(OD_plot_data_plotly, color_palette = color_palette)
            }

      } else { # if the code output is to be visualized instead

        if (total() == FALSE) { # relative numbers

          ODplot <- plot_OC_perc(OD_plot_data_plotly, color_palette = color_palette)

          } else { # absolute numbers

            ODplot <- plot_OC_total(OD_plot_data_plotly, color_palette = color_palette)
          }
      }
      ODplot
      })

  })

  }

#----------------------------------------------------------------------
# Open Data and Code plot and helping functions
#----------------------------------------------------------------------

get_current_OD <- function(data_table) {

  data_table |>
  # screened_articles |>
    make_oddpub_plot_data(year) |>
    filter(year == max(year)) |>
    pull(open_data_perc) |>
    round() |>
    map_chr(\(x) paste(x, "%"))

}

get_current_OC <- function(data_table) {

  data_table |>
    # screened_articles |>
    make_oddpub_plot_data(year) |>
    filter(year == max(year)) |>
    pull(open_code_perc) |>
    round() |>
    map_chr(\(x) paste(x, "%"))

}

get_total_OD <- function(data_table) {

  data_table |>
  # screened_articles |>
    make_oddpub_plot_data(year) |>
    summarise(OD_perc = sum(open_data_count) / sum(total) * 100) |>
    pull(OD_perc) |>
    round() |>
    map_chr(\(x) paste(x, "%"))

}


get_total_OC <- function(data_table) {

  data_table |>
    # screened_articles |>
    make_oddpub_plot_data(year) |>
    summarise(OC_perc = sum(open_code_count) / sum(total) * 100) |>
    pull(OC_perc) |>
    round() |>
    map_chr(\(x) paste(x, "%"))

}

# Open Data
plot_OD_perc <- function(plot_data, color_palette)
{
  yrange <- c(0, 50)

  plot_output <- plot_data |>
    plot_ly(x = ~year, y = ~OD_disc_nonrestricted_perc,
            name = "disciplinary", type = 'bar',
            marker = list(color = color_palette[3],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) |>
    add_trace(y = ~OD_disc_restricted_perc,
              name = 'disciplinary - restricted',
              marker = list(color = color_palette[3],
                            pattern = list(shape = "x"),
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) |>
    add_trace(y = ~OD_gen_nonrestricted_perc,
              name = 'general-purpose',
              marker = list(color = color_palette[6],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) |>
    add_trace(y = ~OD_gen_restricted_perc,
              name = 'general-purpose - restricted',
              marker = list(color = color_palette[6],
                            pattern = list(shape = "x"),
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5)),
              showlegend = FALSE) |>
    add_trace(y = ~OD_disc_and_gen_nonrestricted_perc,
              name = 'disciplinary and general<br>',
              marker = list(color = color_palette[2],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) |>
    add_trace(y = ~OD_disc_and_gen_restricted_perc,
              name = 'disciplinary and general<br> - restricted',
              marker = list(color = color_palette[2],
                            pattern = list(shape = "x"),
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5)),
              showlegend = FALSE)

  plot_output |>
    layout(barmode = "stack",
           legend = list(
             orientation = "h",
             xanchor = "left",
             legendwidth = 1.3,
             # font = list(size = 11),
             bgcolor = "rgba(0,0,0,0)",
             x = 0.05,
             y = 1.3),
           yaxis = list(title = "<b>Publications</b>",
                        range = yrange,
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}

plot_OD_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~open_data_count,
          name = "Open Data", type = "bar",
          marker = list(color = color_palette[6],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    add_trace(y = ~open_data_neg_count,
              name = "No Open Data",
              marker = list(color = color_palette[5],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~open_data_NA_count,
              name = "Article<br>not screened",
              marker = list(color = color_palette[1],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    layout(barmode = "stack",
           yaxis = list(title = "<b>Number of publications</b>"
                        # range = c(0, 25000)
                        ),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}

# Open Code
plot_OC_perc <- function(plot_data, color_palette) {
  yrange <- c(0, 50)

  plot_output <- plot_ly(plot_data, x = ~year, y = ~OC_github_perc,
                         name = "GitHub", type = "bar",
                         marker = list(color = color_palette[3],
                                       line = list(color = "rgb(0,0,0)",
                                                   width = 1.5))) |>
    add_trace(y = ~OC_other_perc,
              name = "other repository <br>or website",
              marker = list(color = color_palette[6],
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
                        range = yrange,
                        ticksuffix = "%"),
           xaxis = list(title = "<b>Year</b>",
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9]) |>
    plotly::config(displayModeBar = FALSE)
}

plot_OC_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~open_code_count,
          name = "Open Code", type = "bar",
          marker = list(color = color_palette[3],
                        line = list(color = "rgb(0,0,0)",
                                    width = 1.5))) |>
    add_trace(y = ~open_code_neg_count,
              name = "No Open Code",
              marker = list(color = color_palette[5],
                            line = list(color = "rgb(0,0,0)",
                                        width = 1.5))) |>
    add_trace(y = ~open_code_NA_count,
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

make_oddpub_plot_data <- function(data_table, gr) {

  oddpub_plot_data <- data_table |>
    group_by({{ gr }}) |>
    summarize(open_data_count = sum(is_open_data, na.rm = TRUE),
              open_data_neg_count = sum(!is_open_data, na.rm = TRUE),
              open_data_NA_count = sum(is.na(is_open_data), na.rm = TRUE),

              open_code_count = sum(is_open_code, na.rm = TRUE),
              open_code_neg_count = sum(!is_open_code, na.rm = TRUE),
              open_code_NA_count = sum(is.na(is_open_code), na.rm = TRUE),
              OC_github_count = sum(open_code_category_manual == "github", na.rm = TRUE),
              OC_other_count = sum(open_code_category_manual == "other repository/website", na.rm = TRUE),
              total = sum(!is.na(is_open_data), na.rm = TRUE))  |>
    mutate(open_data_perc = round(open_data_count/total * 100, 1),
           open_code_perc = round(open_code_count/total * 100, 1),
           OC_github_perc = round(OC_github_count/total * 100, 1),
           OC_other_perc = round(OC_other_count/total * 100, 1))


  # TODO: add restriction info to the table when validated data is ready
  # oddpub_plot_data <- data_table %>%
  #   group_by(year) %>%
  #   summarize(open_data_manual_count = sum(open_data_manual_check, na.rm = TRUE),
  #             open_data_neg_count = sum(!is_open_data | !open_data_manual_check, na.rm = TRUE),
  #             open_data_NA_count = sum(is.na(is_open_data), na.rm = TRUE),
  #
  #             open_code_manual_count = sum(open_code_manual_check, na.rm = TRUE),
  #             open_code_neg_count = sum(!is_open_code | !open_code_manual_check, na.rm = TRUE),
  #             open_code_NA_count = sum(is.na(is_open_code), na.rm = TRUE),
  #
  #             OD_disciplinary_count = sum(open_data_category_manual == "disciplinary repository", na.rm = TRUE),
  #             OD_general_purpose_count = sum(open_data_category_manual == "general-purpose repository", na.rm = TRUE),
  #             OD_disciplinary_and_general_count = sum(open_data_category_manual == "disciplinary and general-purpose repositories", na.rm = TRUE),
  #             OD_disc_restricted_count = sum(open_data_category_manual == "disciplinary repository" &
  #                                              restrictions == "full", na.rm = TRUE),
  #             OD_gen_restricted_count = sum(open_data_category_manual == "general-purpose repository" &
  #                                             restrictions == "full", na.rm = TRUE),
  #             OD_disc_and_gen_restricted_count = sum(open_data_category_manual == "disciplinary and general-purpose repositories" &
  #                                                      restrictions == "full", na.rm = TRUE),
  #
  #             # OD_supplement_count = sum(open_data_category_priority == "supplement", na.rm = TRUE),
  #             OC_github_count = sum(open_code_category_manual == "github", na.rm = TRUE),
  #             OC_other_count = sum(open_code_category_manual == "other repository/website", na.rm = TRUE),
  #             # OC_supplement_count = sum(open_code_category_priority == "supplement", na.rm = TRUE),
  #             total = sum(!is.na(is_open_data) | (open_data_manual_check == TRUE), na.rm = TRUE)) %>%
  #
  #   mutate(open_data_manual_perc = round(open_data_manual_count/total * 100, 1),
  #          open_code_manual_perc = round(open_code_manual_count/total * 100, 1),
  #          OD_disciplinary_perc = round(OD_disciplinary_count/total * 100, 1),
  #          OD_general_purpose_perc = round(OD_general_purpose_count/total * 100, 1),
  #          OD_disciplinary_and_general_perc = round(OD_disciplinary_and_general_count/total * 100, 1),
  #          OD_disc_restricted_perc = round(OD_disc_restricted_count/total * 100, 1),
  #          OD_gen_restricted_perc = round(OD_gen_restricted_count/total * 100, 1),
  #          OD_disc_and_gen_restricted_perc = round(OD_disc_and_gen_restricted_count/total * 100, 1),
  #          OD_disc_nonrestricted_perc = round((OD_disciplinary_count - OD_disc_restricted_count)/total * 100, 1),
  #          OD_gen_nonrestricted_perc = round((OD_general_purpose_count - OD_gen_restricted_count)/total * 100, 1),
  #          OD_disc_and_gen_nonrestricted_perc = round((OD_disciplinary_and_general_count - OD_disc_and_gen_restricted_count)/total * 100, 1),
  #          # mutate(OD_supplement_perc = round(OD_supplement_count/total * 100, 1)) %>%
  #          OC_github_perc = round(OC_github_count/total * 100, 1),
  #          OC_other_perc = round(OC_other_count/total * 100, 1))



  # summarize(open_data_count = sum(is_open_data, na.rm = TRUE),
    #           open_data_neg_count = sum(!is_open_data, na.rm = TRUE),
    #           open_data_NA_count = sum(is.na(is_open_data), na.rm = TRUE),
    #
    #           open_code_count = sum(is_open_code, na.rm = TRUE),
    #           open_code_neg_count = sum(!is_open_code, na.rm = TRUE),
    #           open_code_NA_count = sum(is.na(is_open_code), na.rm = TRUE),

              # OD_disciplinary_count = sum(open_data_category_manual == "disciplinary repository", na.rm = TRUE),
              # OD_general_purpose_count = sum(open_data_category_manual == "general-purpose repository", na.rm = TRUE),
              # OD_disciplinary_and_general_count = sum(open_data_category_manual == "disciplinary and general-purpose repositories", na.rm = TRUE),
              # OD_disc_restricted_count = sum(open_data_category_manual == "disciplinary repository" &
              #                                  restrictions == "full", na.rm = TRUE),
              # OD_gen_restricted_count = sum(open_data_category_manual == "general-purpose repository" &
              #                                 restrictions == "full", na.rm = TRUE),
              # OD_disc_and_gen_restricted_count = sum(open_data_category_manual == "disciplinary and general-purpose repositories" &
              #                                          restrictions == "full", na.rm = TRUE),

              # OD_supplement_count = sum(open_data_category_priority == "supplement", na.rm = TRUE),
              # OC_github_count = sum(open_code_category == "github", na.rm = TRUE),
              # OC_other_count = sum(open_code_category == "other repository/website", na.rm = TRUE),
              # OC_supplement_count = sum(open_code_category_priority == "supplement", na.rm = TRUE),
              # total = sum(!is.na(is_open_data), na.rm = TRUE)) |>

    # mutate(open_data_perc = round(open_data_count/total * 100, 1),
           # open_code_perc = round(open_code_count/total * 100, 1),
           # OD_disciplinary_perc = round(OD_disciplinary_count/total * 100, 1),
           # OD_general_purpose_perc = round(OD_general_purpose_count/total * 100, 1),
           # OD_disciplinary_and_general_perc = round(OD_disciplinary_and_general_count/total * 100, 1),
           # OD_disc_restricted_perc = round(OD_disc_restricted_count/total * 100, 1),
           # OD_gen_restricted_perc = round(OD_gen_restricted_count/total * 100, 1),
           # OD_disc_and_gen_restricted_perc = round(OD_disc_and_gen_restricted_count/total * 100, 1),
           # OD_disc_nonrestricted_perc = round((OD_disciplinary_count - OD_disc_restricted_count)/total * 100, 1),
           # OD_gen_nonrestricted_perc = round((OD_general_purpose_count - OD_gen_restricted_count)/total * 100, 1),
           # OD_disc_and_gen_nonrestricted_perc = round((OD_disciplinary_and_general_count - OD_disc_and_gen_restricted_count)/total * 100, 1),
           # mutate(OD_supplement_perc = round(OD_supplement_count/total * 100, 1)) |>
           # OC_github_perc = round(OC_github_count/total * 100, 1),
           # OC_other_perc = round(OC_other_count/total * 100, 1))

  return(oddpub_plot_data)
}
