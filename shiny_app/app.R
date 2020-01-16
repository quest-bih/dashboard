library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggvis)
library(ggplot2)

source("app_functions_OA.R")

dashboard_metrics <- read_csv("data/dashboard_metrics.csv") %>%
  rename(year = e_pub_year)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("th")),
    menuItem("Open Access", tabName = "open_access", icon = icon("unlock-alt")),
    menuItem("Open Data & Open Code", tabName = "oddpub", icon = icon("database")),
    menuItem("Barzooka", tabName = "barzooka", icon = icon("chart-bar"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "open_access",
            h2("Open Access Metric"),
            box(plotOutput('plot_OA'))
    ),

    tabItem(tabName = "oddpub",
            h2("Open Data & Code Metric")
    ),

    tabItem(tabName = "barzooka",
            h2("Bar graphs for continuous data & more informative alternatives")
    )
  )
)


ui <- dashboardPage(dashboardHeader(title = "Charité Dashboard"),
                    sidebar,
                    body,
                    skin = "yellow")




server <- function(input, output)
{

  OA_plot_data <- dashboard_metrics %>%
    filter(!is.na(OA_color)) %>%
    group_by(year, OA_color) %>%
    summarize(count = n()) %>%
    calculate_OA_percentages() #%>%
    #set_OA_colors(TRUE) %>%
    #mutate(year = as.factor(year))

  output$plot_OA <- renderPlot({
    ggplot(OA_plot_data, aes(x=year, y=perc, fill = OA_color)) +
      geom_bar(stat="identity", color = "#3C5D70", width = 0.8, alpha = 0.6, size = 0.8) +
      scale_fill_manual(values=c("#f7be16", "#008950", "#410b5b")) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage Open Access publications") +
      ylim(0, 1) +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=18),
            legend.title=element_text(size=16),
            legend.text=element_text(size=14))

  }, )


}

shinyApp(ui, server)


