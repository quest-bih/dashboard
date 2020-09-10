#---------------------------------
# Open Science plots
#---------------------------------

# Open Access

plot_OA_total <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~gold, name = "Gold", type = 'bar',
          marker = list(color = color_palette[3],
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1.5))) %>%
    add_trace(y = ~green, name = 'Green',
              marker = list(color = color_palette[6],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~hybrid, name = 'Hybrid',
              marker = list(color = color_palette[7],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~bronze, name = 'Bronze',
              marker = list(color = color_palette[4],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~closed, name = 'Closed',
              marker = list(color = color_palette[5],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    add_trace(y = ~`NA`, name = 'No data available',
              marker = list(color = color_palette[1],
                            line = list(color = 'rgb(0,0,0)',
                                        width = 1.5))) %>%
    layout(barmode = 'stack',
           legend=list(title=list(text='<b> Category </b>')),
           yaxis = list(title = '<b>Publications</b>'),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = color_palette[9],
           plot_bgcolor = color_palette[9])
}


plot_OA_perc <- function(plot_data, color_palette)
{
  plot_ly(plot_data, x = ~year, y = ~gold, name = "Gold", type = 'bar',
                          marker = list(color = color_palette[3],
                                        line = list(color = 'rgb(0,0,0)',
                                                    width = 1.5))) %>%
  add_trace(y = ~green, name = 'Green',
            marker = list(color = color_palette[6],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
  add_trace(y = ~hybrid, name = 'Hybrid',
            marker = list(color = color_palette[7],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
  layout(barmode = 'stack',
         legend=list(title=list(text='<b> Category </b>')),
         yaxis = list(title = '<b>Percentage Open Access</b>',
                      range = c(0, 100)),
         xaxis = list(title = '<b>Year</b>',
                      dtick = 1),
         paper_bgcolor = color_palette[9],
         plot_bgcolor = color_palette[9])
}
