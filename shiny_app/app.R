library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggvis)
library(ggplot2)
library(shinythemes)
library(gridExtra)

#----------------------------------------------------------------------------------------------------------------------
# load data & functions
#----------------------------------------------------------------------------------------------------------------------

source("app_functions_OA.R")
source("app_functions_oddpub.R")
source("ui_elements.R")

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

# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     menuItem("Overview", tabName = "overview", icon = icon("th")),
#     menuItem("Plots", tabName = "plots", icon = icon("th"))
#   )
# )
#
#
# body <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "overview",
#             h2("Charité Metrics Overview 2018"),
#             h3("Open Science"),
#             fluidRow(
#               valueBox(metrics_show_year$preprints,
#                        "preprints published", icon = icon("unlock-alt"), color = "yellow"),
#               valueBox(paste(round(OA_data[[4,"OA_perc"]] *100, 0), "%"),
#                        "Open Access articles", icon = icon("unlock-alt"), color = "yellow"),
#               valueBox(paste(round(oddpub_data[[4,"open_data_perc"]] *100, 0), "%"),
#                        "of publications have Open Data", icon = icon("unlock-alt"), color = "yellow"),
#               valueBox(paste(round(oddpub_data[[4,"open_code_perc"]] *100, 0), "%"),
#                        "of publications have Open Code", icon = icon("unlock-alt"), color = "yellow")
#             ),
#             h3("Clinical trials"),
#             fluidRow(
#               valueBox(paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_12"]] *100, 0), "%"),
#                        "of completed trials clinical trials posted summary results on CT.gov within 12 month", icon = icon("th")),
#               valueBox(paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_24"]] *100, 0), "%"),
#                        "of completed trials clinical trials posted summary results on CT.gov within 24 month", icon = icon("th")),
#               valueBox(paste(round(metrics_show_year$perc_prosp_reg *100, 0), "%"),
#                        "of clinical trials prospectively registered on CT.gov", icon = icon("th"))
#             ),
#             h3("Vizualizations"),
#             fluidRow(
#               valueBox(barzooka_data$has_bar %>% last(),
#                        "publications with bar graphs", icon = icon("th"), color = "green"),
#               valueBox(barzooka_data$has_pie %>% last(),
#                        "publications with pie graphs", icon = icon("th"), color = "green"),
#               valueBox(barzooka_data$has_bardot %>% last(),
#                        "publications with bar graphs with dots", icon = icon("th"), color = "green"),
#               valueBox(barzooka_data$has_box %>% last(),
#                        "publications with box plots", icon = icon("th"), color = "green"),
#               valueBox(barzooka_data$has_dot %>% last(),
#                        "publications with dot plots", icon = icon("th"), color = "green"),
#               valueBox(barzooka_data$has_hist %>% last(),
#                        "publications with histograms", icon = icon("th"), color = "green"),
#               valueBox(barzooka_data$has_violin %>% last(),
#                        "publications with violin plots", icon = icon("th"), color = "green")
#             )
#     ),
#     tabItem(tabName = "plots",
#             h2("Charité Metrics Overview"),
#             fluidRow(
#             box(title = p("Open Access", style = "font-size: 150%;"),
#                 plotOutput('plot_OA')),
#             box(title = p("Open Data & Code", style = "font-size: 150%;"),
#                 plotOutput('plot_oddpub')),
#             box(title = p("Preprints", style = "font-size: 150%;"),
#                 plotOutput('plot_preprints')),
#             box(title = p("Clinical trials", style = "font-size: 150%;"),
#                 plotOutput('plot_CTgov')),
#             box(title = p("Vizualizations", style = "font-size: 150%;"),
#                 plotOutput('plot_barzooka'))
#             )
#     )
#   )
# )
#
#
# ui <- dashboardPage(dashboardHeader(title = "Charité Dashboard"),
#                     sidebar,
#                     body,
#                     skin = "yellow")
#

ui <- navbarPage(
  "Charité Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
  tabPanel("Start page",
           #overall_design_options,
           wellPanel(
                     br(),
                     h1(style = "margin-left:10cm", strong("Charité Dashboard"), align = "left"),
                     h4(style = "font-size:24px;margin-left:10cm", "This dashboard gives an overview over ..."),
                     br(),
                     actionButton(style = "margin-left:10cm; color: white; background-color: #aa1c7d;",
                                   'buttonLearnMore',
                                   'Learn more',
                                  onclick ="window.open('https://www.bihealth.org/en/research/quest-center/projects/projects-of-the-research-group-translational-bioethics/bravo/', '_blank')"),
                     #h4(style = "margin-left:10cm", HTML(paste0(a(href = 'https://osf.io/fh426/', "Learn more")))),
                     br()),

           wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
             h2(strong("Open Science"), align = "left"),
             h4("Explanation on Open Science ..."),
             fluidRow(
               column(3, metric_box("Open Access", paste(round(OA_data[[4,"OA_perc"]] *100, 0), "%"),
                                    "Open Access articles")),
               column(3, metric_box("Open Data", paste(round(oddpub_data[[4,"open_data_perc"]] *100, 0), "%"),
                                    "of publications have Open Data")),
               column(3, metric_box("Open Code", paste(round(oddpub_data[[4,"open_code_perc"]] *100, 0), "%"),
                                    "of publications have Open Code")),
               column(3, metric_box("Preprints", metrics_show_year$preprints,
                                    "preprints published"))
             )
           ),

           wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
             h2(strong("Clinical trials"), align = "left"),
             h4("Explanation on clinical trials ..."),
             fluidRow(
               column(3, metric_box("Summary Results", paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_12"]] *100, 0), "%"),
                                    "of completed trials posted summary results on CT.gov within 12 month")),
               column(3, metric_box("Summary Results", paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_24"]] *100, 0), "%"),
                                    "of completed trials posted summary results on CT.gov within 24 month")),
               column(3, metric_box("Prospective registration", paste(round(metrics_show_year$perc_prosp_reg *100, 0), "%"),
                                    "of clinical trials prospectively registered on CT.gov"))
             )
           ),

           wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
             h2(strong("Development over time"), align = "left"),
             fluidRow(
               column(4,
                      h4(strong("Open Access")),
                      plotOutput('plot_OA')
               ),
               column(4,
                      h4(strong("Open Data & Code")),
                      plotOutput('plot_oddpub')
               ),
               column(4,
                      h4(strong("Preprints")),
                      plotOutput('plot_preprints')
               )
             ),
             fluidRow(
               column(4,
                      h4(strong(HTML("Clinical trials - <br> Timely reporting"))),
                      plotOutput('plot_CTgov_1')
               ),
               column(4,
                      h4(strong(HTML("Clinical trials - <br> Prospective registration"))),
                      plotOutput('plot_CTgov_2')
               ),
               column(4,
                      h4(strong("Vizualizations")),
                      plotOutput('plot_barzooka')
               )
             )
           )
  ),
  tabPanel("Detailed Methods",

  ),
  tabPanel("Educational tools",

  )
)

#----------------------------------------------------------------------------------------------------------------------
# server
#----------------------------------------------------------------------------------------------------------------------

server <- function(input, output)
{

  color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#810050")
  background_color <- "#ecf0f1"


  OA_plot_data <- dashboard_metrics %>%
    make_OA_plot_data()

  output$plot_OA <- renderPlot({
    ggplot(OA_plot_data, aes(x=year, y=perc, fill = OA_color)) +
      geom_bar(stat="identity", color = "black", width = 0.8, size = 0.8) +
      scale_fill_manual(values=color_palette[c(3,6,7)]) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage Open Access") +
      ylim(0, 1) +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))

  }, height = 300, type = "cairo")



  oddpub_plot_data <- dashboard_metrics %>%
    make_oddpub_plot_data() %>%
    rename(`Open Data` = open_data_perc) %>%
    rename(`Open Code` = open_code_perc) %>%
    gather(`Open Data`, `Open Code`, key="category", value="perc")

  output$plot_oddpub <- renderPlot({
    ggplot(oddpub_plot_data, aes(x=year, y=perc, fill=category)) +
      geom_bar(stat="identity", position=position_dodge(), color = "black", size = 0.8) +
      scale_fill_manual(values = color_palette[c(2,3)]) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage of publications") +
      ylim(0, 1) +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))

  }, height = 300, type = "cairo")



  preprints_plot_data <- dashboard_metrics_aggregate %>%
    select(year, preprints) %>%
    filter(!is.na(preprints)) %>%
    filter(year >= 2015)

  output$plot_preprints <- renderPlot({
    ggplot(preprints_plot_data, aes(x=year, y=preprints)) +
      geom_bar(stat="identity", fill = color_palette[2], color = "black", size = 0.8) +
      theme_minimal() +
      xlab("Year") +
      ylab("Number of preprints") +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))

  }, height = 300, type = "cairo")



  CTgov_plot_data_1 <- dashboard_metrics_aggregate %>%
    select(year, perc_sum_res_12, perc_sum_res_24) %>%
    filter(!is.na(perc_sum_res_12)) %>%
    filter(year >= 2015) %>%
    rename(`12 month` = perc_sum_res_12) %>%
    rename(`24 month` = perc_sum_res_24) %>%
    gather(`12 month`, `24 month`, key="category", value="perc")

  output$plot_CTgov_1 <- renderPlot({
    ggplot(CTgov_plot_data_1, aes(x=year, y=perc, fill=category)) +
      geom_bar(stat="identity", position=position_dodge(), color = "black", size = 0.6) +
      scale_fill_manual(values = color_palette[2:4]) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage of trials") +
      ylim(0, 1) +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))

  }, height = 300, type = "cairo")



  CTgov_plot_data_2 <- dashboard_metrics_aggregate %>%
    select(year, perc_prosp_reg, perc_sum_res_12, perc_sum_res_24) %>%
    filter(!is.na(perc_prosp_reg)) %>%
    filter(year >= 2015) %>%
    rename(`Prospective registration` = perc_prosp_reg) %>%
    gather(`Prospective registration`, key="category", value="perc")

  output$plot_CTgov_2 <- renderPlot({
    ggplot(CTgov_plot_data_2, aes(x=year, y=perc)) +
      geom_bar(stat="identity", position=position_dodge(), color = "black", size = 0.6) +
      scale_fill_manual(values = color_palette[2:4]) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage of trials") +
      ylim(0, 1) +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))

  }, height = 300, type = "cairo")



  # policy_citations_plot_data <- dashboard_metrics_aggregate %>%
  #   select(year, policy_citations, total_publ_dimensions) %>%
  #   filter(!is.na(policy_citations)) %>%
  #   mutate(policy_citations_per_1000_publ = policy_citations/total_publ_dimensions*1000)
  #
  # output$plot_policy_citations <- renderPlot({
  #   ggplot(policy_citations_plot_data, aes(x=year, y=policy_citations_per_1000_publ)) +
  #     geom_bar(stat="identity") +
  #     theme_minimal() +
  #     xlab("Year") +
  #     ylab("Number of policy citations per 1000 publications") +
  #     theme(axis.text=element_text(size=14, face = "bold"),
  #           axis.title=element_text(size=16, face = "bold"),
  #           legend.title=element_text(size=14, face = "bold"),
  #           legend.text=element_text(size=12, face = "bold"),
  #           panel.grid=element_blank(),
  #           plot.background = element_rect(fill = background_color, colour = background_color))
  #
  # }, height = 300, type = "cairo")



  barzooka_plot_data <- barzooka_data %>%
    rename(`bar graph` = has_bar) %>%
    rename(`pie chart` = has_pie) %>%
    rename(`bar graph with dots` = has_bardot) %>%
    rename(`box plot` = has_box) %>%
    rename(`dot plot` = has_dot) %>%
    rename(`histogram` = has_hist) %>%
    rename(`violin plot` = has_violin) %>%
    gather(key, value, -year, -total)

  output$plot_barzooka <- renderPlot({
    ggplot(barzooka_plot_data, aes(x=year, y=value, color = key)) +
      geom_line(aes(color=key), size=1.2) +
      geom_point(size=3) +
      scale_color_manual(values = color_palette) +
      theme_minimal() +
      xlab("Year") +
      ylab("Publications with graph type") +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))
  }, height = 300, type = "cairo")

}

shinyApp(ui, server)


