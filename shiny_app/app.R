library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggvis)
library(ggplot2)
library(shinythemes)
library(shinyBS)
library(DT)

#----------------------------------------------------------------------------------------------------------------------
# load data & functions
#----------------------------------------------------------------------------------------------------------------------

source("app_functions_OA.R")
source("app_functions_oddpub.R")
source("ui_elements.R")
source("methods_descriptions.R", encoding = "UTF-8")
source("resources_descriptions.R", encoding = "UTF-8")
source("about_page.R", encoding = "UTF-8")

dashboard_metrics <- read_csv("data/dashboard_metrics.csv") %>%
  rename(year = e_pub_year)

dashboard_metrics_aggregate <- read_csv("data/dashboard_metrics_aggregate.csv") %>%
  mutate(perc_prosp_reg = perc_prosp_reg * 100) %>%
  mutate(perc_sum_res_12 = perc_sum_res_12 * 100) %>%
  mutate(perc_sum_res_24 = perc_sum_res_24 * 100)

#CT.gov dataset for the datatable
prosp_reg_dataset_shiny <- read_csv("data/prosp_reg_dataset_shiny.csv") %>%
  mutate_at(vars(nct_id, start_date, study_first_submitted_date,
                 days_reg_to_start, has_prospective_registration),
            as.character)
summary_results_dataset_shiny <- read_csv("data/sum_res_dataset_shiny.csv")

#----------------------------------------------------------------------------------------------------------------------
# preprocessing, need to move somewhere else later
#----------------------------------------------------------------------------------------------------------------------

show_year <- "2019"
metrics_show_year <- dashboard_metrics_aggregate %>% filter(year == show_year)

OA_data <- dashboard_metrics %>%
  make_OA_plot_data() %>%
  group_by(year) %>%
  summarize(OA_perc = sum(perc))

oddpub_data <- dashboard_metrics %>%
  make_oddpub_plot_data()

barzooka_data <- dashboard_metrics %>%
  filter(pdf_downloaded) %>%
  group_by(year) %>%
  summarize(total = n(),
            has_bar = sum(bar > 0, na.rm = TRUE)/total*1000,
            has_pie = sum(pie > 0, na.rm = TRUE)/total*1000,
            has_bardot = sum(bardot > 0, na.rm = TRUE)/total*1000,
            has_box = sum(box > 0, na.rm = TRUE)/total*1000,
            has_dot = sum(dot > 0, na.rm = TRUE)/total*1000,
            has_hist = sum(hist > 0, na.rm = TRUE)/total*1000,
            has_violin = sum(violin > 0, na.rm = TRUE)/total*1000,
            has_informative = sum(bardot > 0 | box > 0 | dot > 0 | hist > 0 | violin > 0,
                                  na.rm = TRUE)/total*1000)


#----------------------------------------------------------------------------------------------------------------------
# ui
#----------------------------------------------------------------------------------------------------------------------

ui <- navbarPage(
  "Charité Metrics Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
  tabPanel("Start page",
           #overall_design_options,
           wellPanel(
             br(),
             fluidRow(
               column(8,
                      h1(style = "margin-left:0cm", strong("Charité Metrics Dashboard"), align = "left"),
                      h4(style = "margin-left:0cm",
                         "This dashboard gives an overview over several metrics of open and responsible
                        research at the Charité. For more detailed information on the methods used to
                        calculate those metrics, for the dataset underlying the metrics, or for ressources to improve your own research practices
                        click one of the following buttons."),
                      h4(style = "margin-left:0cm",
                         "This dashboard is a pilot that is still under development. More metrics will be added in the future."),
                      br()),
               column(4,
                      hr(),
                      br(),
                      actionButton(style = "color: white; background-color: #aa1c7d;",
                                   'buttonMethods',
                                   'See methods'),
                      actionButton(style = "color: white; background-color: #aa1c7d;",
                                   'buttonResources',
                                   'See resources'),
                      actionButton(style = "color: white; background-color: #aa1c7d;",
                                   'buttonDatasets',
                                   'See dataset'),
                      br())
             )
           ),

           wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
                     h2(strong("Open Science"), align = "left"),
                     h4(paste0("These metrics show Open Science practices at Charité. The percentage of Charité
                original research publications that are published as Open Access articles are mesured as well as
                the percentage of publications that state that they share their research data or analysis code.
                Additionally, we count articles published on preprint servers like bioRxiv.")),
                     br(),
                     fluidRow(
                       column(3, metric_box("Open Access",
                                            paste(round((OA_data %>% filter(year == show_year))[["OA_perc"]], 0), "%"),
                                            "Open Access articles in 2019",
                                            plotlyOutput('plot_OA', height = "300px"))),
                       column(3, metric_box("Any Open Data",
                                            paste(round((oddpub_data %>%  filter(year == show_year))[["open_data_perc"]], 0), "%"),
                                            "of publications mention sharing of data in 2019",
                                            plotlyOutput('plot_oddpub_data', height = "300px"))),
                       column(3, metric_box("Any Open Code",
                                            paste(round((oddpub_data %>%  filter(year == show_year))[["open_code_perc"]], 0), "%"),
                                            "of publications mention sharing of code in 2019",
                                            plotlyOutput('plot_oddpub_code', height = "300px"))),
                       column(3, metric_box("Preprints",
                                            metrics_show_year$preprints,
                                            "preprints published in 2019",
                                            plotlyOutput('plot_preprints', height = "300px")))
                     )
           ),

           wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
                     h2(strong("Clinical trials"), align = "left"),
                     h4("These metrics look at clinical trials that are registered on ClinicalTrials.gov
                with Charité as the sponsor or with a priniciple investigator from Charité. We look
                both at the timely reporting of summary results (within 12 or 24 months) on
                ClinicalTrials.gov as well as prospective registration of the trials."),
                     br(),
                     fluidRow(
                       column(3, metric_box("Summary Results",
                                            paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_24"]], 0), "%"),
                                            "of trials completed in 2017 posted summary results on CT.gov within 24 months",
                                            plotlyOutput('plot_CTgov_1', height = "300px"))),
                       column(3, metric_box("Prospective registration",
                                            paste(round(metrics_show_year$perc_prosp_reg, 0), "%"),
                                            "of clinical trials started in 2019 are prospectively registered on CT.gov",
                                            plotlyOutput('plot_CTgov_2', height = "300px")))
                     )
           ),

           wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
                     h2(strong("Visualizations"), align = "left"),
                     h4("These metrics measure how often different suboptimal and more informative
                       graph types appear in the original research publications.
                       Bar graphs for continuous data are common in the biomedical literature
                       but are considered a suboptimal practice, as they conceal the underlying data points
                       and since many different data distributions can lead to the same bar graph. Also
                       pie charts are considered suboptimal, as they make it difficult to compare
                       the presented data. Different alternative graph types like dot plots,
                       violin plots, box plots or histograms can be used instead."),
                     br(),
                     fluidRow(
                       column(6, metric_box("Problematic graphs",
                                            filter(barzooka_data, year == show_year)$has_bar %>% round(0),
                                            "out of 1000 publications from 2019 use bar graphs for continuous data",
                                            plotlyOutput('plot_barzooka_problem', height = "300px"))),
                       column(6, metric_box("More informative graph types",
                                            filter(barzooka_data, year == show_year)$has_informative %>% round(0),
                                            "out of 1000 publications from 2019 use more informative graph types",
                                            plotlyOutput('plot_barzooka_inform', height = "300px")))
                     )
           )
  ),
  methods_panel,
  resources_panel,

  #possibly let users choose which dataset (publications/clinical trials) is shown
  #instead of showing both
  tabPanel("Datasets", value = "tabDatasets",
           h1("Datasets"),
           h4("The following tables contain the datasets underlying the numbers and plots
              shown for the metrics included in this Shiny app."),
           br(),
           bsCollapse(id = "datasetPanels_PublicationDataset",
                      bsCollapsePanel(strong("Publication dataset"),
                                      DT::dataTableOutput("data_table_publ"),
                                      style = "default")),
           br(),
           bsCollapse(id = "datasetPanels_PublicationDataset",
                      bsCollapsePanel(strong("Prospective registration dataset"),
                                      DT::dataTableOutput("data_table_prosp_reg"),
                                      style = "default")),
           br(),
           bsCollapse(id = "datasetPanels_PublicationDataset",
                      bsCollapsePanel(strong("Summary results dataset"),
                                      DT::dataTableOutput("data_table_sum_res"),
                                      style = "default")),
  ),
  about_page
)

#----------------------------------------------------------------------------------------------------------------------
# server
#----------------------------------------------------------------------------------------------------------------------

server <- function(input, output, session)
{

  #actionButton to switch tabs
  observeEvent(input$buttonMethods, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    #updateCollapse(session, "methodsPanels_OpenScience", open = "Preprints")
  })

  observeEvent(input$buttonResources, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabRessources")
  })

  observeEvent(input$buttonDatasets, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabDatasets")
  })


  #data table to show the underlying publ dataset
  output$data_table_publ <- DT::renderDataTable({
    DT::datatable(data = dashboard_metrics,
    extensions = 'Buttons',
    filter = 'top',
    options = list(dom = 'Blfrtip',
                   buttons =
                     list(list(
                       extend = "collection"
                       , buttons = c("csv", "excel")
                       , text = "Download"
                     ) ),
                   orderClasses = TRUE,
                   pageLength = 20,
                   lengthMenu = list(c(10, 20, 50, 100, -1),
                                     c(10, 20, 50, 100, "All")),
                   columnDefs = list(list(className = 'dt-left', targets = 2),
                                     list(className = 'dt-left', targets = 6),
                                     list(className = 'dt-left', targets = 8))
    ))
  })

  #data table to show the underlying dataset
  output$data_table_prosp_reg <- DT::renderDataTable({
    DT::datatable(data = prosp_reg_dataset_shiny,
                  extensions = 'Buttons',
                  filter = 'top',
                  options = list(dom = 'Blfrtip',
                                 buttons =
                                   list(list(
                                     extend = "collection"
                                     , buttons = c("csv", "excel")
                                     , text = "Download"
                                   ) ),
                                 orderClasses = TRUE,
                                 pageLength = 20,
                                 lengthMenu = list(c(10, 20, 50, 100, -1),
                                                   c(10, 20, 50, 100, "All"))
                  ))
  })

  #data table to show the underlying dataset
  output$data_table_sum_res <- DT::renderDataTable({
    DT::datatable(data = summary_results_dataset_shiny,
                  extensions = 'Buttons',
                  filter = 'top',
                  options = list(dom = 'Blfrtip',
                                 buttons =
                                   list(list(
                                     extend = "collection"
                                     , buttons = c("csv", "excel")
                                     , text = "Download"
                                   ) ),
                                 orderClasses = TRUE,
                                 pageLength = 20,
                                 lengthMenu = list(c(10, 20, 50, 100, -1),
                                                   c(10, 20, 50, 100, "All")),
                                 columnDefs = list(list(className = 'dt-left', targets = 2),
                                                   list(className = 'dt-left', targets = 6),
                                                   list(className = 'dt-left', targets = 8))
                  ))
  })





  color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#810050", "#000000")
  background_color <- "#ecf0f1"
  background_color_darker <- "#DCE3E5"


  #---------------------------------
  # Open Science plots
  #---------------------------------

  OA_plot_data <- dashboard_metrics %>%
    make_OA_plot_data()

  OA_plot_data_plotly <- OA_plot_data %>%
    select(-OA, -all) %>%
    pivot_wider(names_from = category, values_from = perc)

  output$plot_OA <- renderPlotly({
    plot_ly(OA_plot_data_plotly, x = ~year, y = ~gold, name = "Gold", type = 'bar',
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
             paper_bgcolor = background_color_darker,
             plot_bgcolor = background_color_darker)
  })



  oddpub_plot_data <- dashboard_metrics %>%
    make_oddpub_plot_data() %>%
    rename(`Open Data` = open_data_perc) %>%
    rename(`Open Code` = open_code_perc)


  output$plot_oddpub_data <- renderPlotly({
    plot_ly(oddpub_plot_data, x = ~year, y = ~`Open Data`, type = 'bar',
            marker = list(color = color_palette[3],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
      layout(yaxis = list(title = '<b>Percentage of publications</b>',
                          range = c(0, 100)),
             xaxis = list(title = '<b>Year</b>',
                          dtick = 1),
             paper_bgcolor = background_color_darker,
             plot_bgcolor = background_color_darker)
  })

  output$plot_oddpub_code <- renderPlotly({
    plot_ly(oddpub_plot_data, x = ~year, y = ~`Open Code`, type = 'bar',
            marker = list(color = color_palette[3],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
      layout(yaxis = list(title = '<b>Percentage of publications</b>',
                          range = c(0, 100)),
             xaxis = list(title = '<b>Year</b>',
                          dtick = 1),
             paper_bgcolor = background_color_darker,
             plot_bgcolor = background_color_darker)
  })



  preprints_plot_data <- dashboard_metrics_aggregate %>%
    select(year, preprints) %>%
    filter(!is.na(preprints)) %>%
    filter(year >= 2015)


  output$plot_preprints <- renderPlotly({
    plot_ly(preprints_plot_data, x = ~year, y = ~preprints, type = 'bar',
            marker = list(color = color_palette[3],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
      layout(yaxis = list(title = '<b>Percentage of preprints</b>',
                          range = c(0, 100)),
             xaxis = list(title = '<b>Year</b>',
                          dtick = 1),
             paper_bgcolor = background_color_darker,
             plot_bgcolor = background_color_darker)
  })


  #---------------------------------
  # Clinical trials plots
  #---------------------------------

  CTgov_plot_data_1 <- dashboard_metrics_aggregate %>%
    select(year, perc_sum_res_12, perc_sum_res_24) %>%
    filter(!is.na(perc_sum_res_12)) %>%
    filter(year >= 2015) %>%
    rename(`12 months` = perc_sum_res_12) %>%
    rename(`24 months` = perc_sum_res_24) #%>%
    #gather(`12 months`, `24 months`, key="category", value="perc")

  output$plot_CTgov_1 <- renderPlotly({
    plot_ly(CTgov_plot_data_1, x = ~year, y = ~`12 months`,
                               name = "12 months", type = 'bar',
            marker = list(color = color_palette[2],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
      add_trace(y = ~`24 months`, name = '24 months',
                marker = list(color = color_palette[3],
                              line = list(color = 'rgb(0,0,0)',
                                          width = 1.5))) %>%
      layout(barmode = 'group',
             legend=list(title=list(text='<b> Category </b>')),
             yaxis = list(title = '<b>Percentage of trials</b>',
                          range = c(0, 100)),
             xaxis = list(title = '<b>Year</b>',
                          dtick = 1),
             paper_bgcolor = background_color_darker,
             plot_bgcolor = background_color_darker)
  })



  CTgov_plot_data_2 <- dashboard_metrics_aggregate %>%
    select(year, perc_prosp_reg, perc_sum_res_12, perc_sum_res_24) %>%
    filter(!is.na(perc_prosp_reg)) %>%
    filter(year >= 2015) %>%
    rename(`Prospective registration` = perc_prosp_reg) %>%
    gather(`Prospective registration`, key="category", value="perc")


  output$plot_CTgov_2 <- renderPlotly({
    plot_ly(CTgov_plot_data_2, x = ~year, y = ~perc, type = 'bar',
            marker = list(color = color_palette[2],
                          line = list(color = 'rgb(0,0,0)',
                                      width = 1.5))) %>%
      layout(yaxis = list(title = '<b>Percentage of trials</b>',
                          range = c(0, 100)),
             xaxis = list(title = '<b>Year</b>',
                          dtick = 1),
             paper_bgcolor = background_color_darker,
             plot_bgcolor = background_color_darker)
  })


  #---------------------------------
  # Visualizations plots
  #---------------------------------

  output$plot_barzooka_problem <- renderPlotly({
    plot_ly(barzooka_data, x = ~year, y = ~has_bar,
            name = "bar graph", type = 'scatter', mode = 'lines+markers',
            line = list(color = color_palette[2], width = 3),
            marker = list(color = color_palette[2], size = 8)) %>%
      add_trace(y = ~has_pie, name = 'pie chart', mode = 'lines+markers',
                line = list(color = color_palette[3]),
                marker = list(color = color_palette[3])) %>%
      layout(legend=list(title=list(text='<b> Category </b>')),
             yaxis = list(title = '<b>Graph types per 1000 publications</b>'),
             xaxis = list(title = '<b>Year</b>',
                          dtick = 1),
             paper_bgcolor = background_color_darker,
             plot_bgcolor = background_color_darker)
  })


  output$plot_barzooka_inform <- renderPlotly({
    plot_ly(barzooka_data, x = ~year, y = ~has_informative,
            name = "any informative", type = 'scatter', mode = 'lines+markers',
            line = list(color = color_palette[1], width = 3),
            marker = list(color = color_palette[1], size = 8)) %>%
      add_trace(y = ~has_bardot, name = 'bar graph with dots', mode = 'lines+markers',
                line = list(color = color_palette[2]),
                marker = list(color = color_palette[2])) %>%
      add_trace(y = ~has_box, name = 'box plot', mode = 'lines+markers',
                line = list(color = color_palette[3]),
                marker = list(color = color_palette[3])) %>%
      add_trace(y = ~has_dot, name = 'dot plot', mode = 'lines+markers',
                line = list(color = color_palette[4]),
                marker = list(color = color_palette[4])) %>%
      add_trace(y = ~has_hist, name = 'histogram', mode = 'lines+markers',
                line = list(color = color_palette[5]),
                marker = list(color = color_palette[5])) %>%
      add_trace(y = ~has_violin, name = 'violin plot', mode = 'lines+markers',
                line = list(color = color_palette[6]),
                marker = list(color = color_palette[6])) %>%
      layout(legend=list(title=list(text='<b> Category </b>')),
             yaxis = list(title = '<b>Graph types per 1000 publications</b>'),
             xaxis = list(title = '<b>Year</b>',
                          dtick = 1),
             paper_bgcolor = background_color_darker,
             plot_bgcolor = background_color_darker)
  })

}

shinyApp(ui, server)


