library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggvis)
library(ggplot2)

#----------------------------------------------------------------------------------------------------------------------
# load data & functions
#----------------------------------------------------------------------------------------------------------------------

source("app_functions_OA.R")
source("app_functions_oddpub.R")

dashboard_metrics <- read_csv("data/dashboard_metrics.csv") %>%
  rename(year = e_pub_year)

dashboard_metrics_aggregate <- read_csv("data/dashboard_metrics_aggregate.csv")


#----------------------------------------------------------------------------------------------------------------------
# preprocessing, need to move somewhere else later
#----------------------------------------------------------------------------------------------------------------------

show_year <- "2018"
metrics_show_year <- dashboard_metrics_aggregate %>% filter(year == show_year)

OA_data <- dashboard_metrics %>%
  make_OA_plot_data() %>%
  group_by(year) %>%
  summarize(OA_perc = sum(perc))

oddpub_data <- dashboard_metrics %>%
  make_oddpub_plot_data()

barzooka_data <- dashboard_metrics %>%
  group_by(year) %>%
  summarize(total = n(),
            has_bar = sum(bar > 0, na.rm = TRUE),
            has_pie = sum(pie > 0, na.rm = TRUE),
            has_bardot = sum(bardot > 0, na.rm = TRUE),
            has_box = sum(box > 0, na.rm = TRUE),
            has_dot = sum(dot > 0, na.rm = TRUE),
            has_hist = sum(hist > 0, na.rm = TRUE),
            has_violin = sum(violin > 0, na.rm = TRUE))

#----------------------------------------------------------------------------------------------------------------------
# ui
#----------------------------------------------------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("th")),
    menuItem("Plots", tabName = "plots", icon = icon("th"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "overview",
            h2("Charité Metrics Overview 2018"),
            h3("Open Science"),
            fluidRow(
              valueBox(metrics_show_year$preprints,
                       "preprints published", icon = icon("unlock-alt"), color = "yellow"),
              valueBox(paste(round(OA_data[[4,"OA_perc"]] *100, 0), "%"),
                       "Open Access articles", icon = icon("unlock-alt"), color = "yellow"),
              valueBox(paste(round(oddpub_data[[4,"open_data_perc"]] *100, 0), "%"),
                       "of publications have Open Data", icon = icon("unlock-alt"), color = "yellow"),
              valueBox(paste(round(oddpub_data[[4,"open_code_perc"]] *100, 0), "%"),
                       "of publications have Open Code", icon = icon("unlock-alt"), color = "yellow")
            ),
            h3("Clinical trials"),
            fluidRow(
              valueBox(paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_12"]] *100, 0), "%"),
                       "of completed trials clinical trials posted summary results on CT.gov within 12 month", icon = icon("th")),
              valueBox(paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_24"]] *100, 0), "%"),
                       "of completed trials clinical trials posted summary results on CT.gov within 24 month", icon = icon("th")),
              valueBox(paste(round(metrics_show_year$perc_prosp_reg *100, 0), "%"),
                       "of clinical trials prospectively registered on CT.gov", icon = icon("th"))
            ),
            h3("Vizualizations"),
            fluidRow(
              valueBox(barzooka_data$has_bar,
                       "publications with bar graphs", icon = icon("th"), color = "green"),
              valueBox(barzooka_data$has_pie,
                       "publications with pie graphs", icon = icon("th"), color = "green"),
              valueBox(barzooka_data$has_bardot,
                       "publications with bar graphs with dots", icon = icon("th"), color = "green"),
              valueBox(barzooka_data$has_box,
                       "publications with box plots", icon = icon("th"), color = "green"),
              valueBox(barzooka_data$has_dot,
                       "publications with dot plots", icon = icon("th"), color = "green"),
              valueBox(barzooka_data$has_hist,
                       "publications with histograms", icon = icon("th"), color = "green"),
              valueBox(barzooka_data$has_violin,
                       "publications with violin plots", icon = icon("th"), color = "green")
            ),
            h3("Other"),
            fluidRow(
              valueBox(round(metrics_show_year$policy_citations/metrics_show_year$total_publ_dimensions * 1000, 2),
                       "policy citations per 1000 publications", icon = icon("th"), color = "red")
            )
    ),
    tabItem(tabName = "plots",
            h2("Charité Metrics Overview"),
            fluidRow(
            box(title = "Open Access",
                plotOutput('plot_OA')),
            box(title = "Open Data & Code",
                plotOutput('plot_oddpub'))
            )
    )
  )
)


ui <- dashboardPage(dashboardHeader(title = "Charité Dashboard"),
                    sidebar,
                    body,
                    skin = "yellow")



#----------------------------------------------------------------------------------------------------------------------
# server
#----------------------------------------------------------------------------------------------------------------------

server <- function(input, output)
{

  OA_plot_data <- dashboard_metrics %>%
    make_OA_plot_data()

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


  oddpub_plot_data <- dashboard_metrics %>%
    make_oddpub_plot_data() %>%
    rename(`Open Data` = open_data_perc) %>%
    rename(`Open Code` = open_code_perc) %>%
    gather(`Open Data`, `Open Code`, key="category", value="perc")


  output$plot_oddpub <- renderPlot({
    ggplot(oddpub_plot_data, aes(x=year, y=perc, fill=category)) +
      geom_bar(stat="identity", position=position_dodge()) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage of publications") +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=18),
            legend.title=element_text(size=16),
            legend.text=element_text(size=14))

  }, )

}

shinyApp(ui, server)


