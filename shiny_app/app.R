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
source("plots.R", encoding = "UTF-8")

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
            has_bar = sum(bar > 0, na.rm = TRUE),
            has_pie = sum(pie > 0, na.rm = TRUE),
            has_bardot = sum(bardot > 0, na.rm = TRUE),
            has_box = sum(box > 0, na.rm = TRUE),
            has_dot = sum(dot > 0, na.rm = TRUE),
            has_hist = sum(hist > 0, na.rm = TRUE),
            has_violin = sum(violin > 0, na.rm = TRUE),
            has_informative = sum(bardot > 0 | box > 0 | dot > 0 | hist > 0 | violin > 0,
                                  na.rm = TRUE))


#----------------------------------------------------------------------------------------------------------------------
# ui
#----------------------------------------------------------------------------------------------------------------------

ui <- navbarPage(
  "Charité Metrics Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
  tabPanel("Start page", value = "tabStart",
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
                     checkboxInput("checkbox_total_OS", "Show absolute numbers", value = FALSE),
                     br(),
                     fluidRow(
                       column(3, metric_box("Open Access",
                                            paste(round((OA_data %>% filter(year == show_year))[["OA_perc"]], 0), "%"),
                                            "Open Access articles in 2019",
                                            plotlyOutput('plot_OA', height = "300px"))),
                       column(3, metric_box("Any Open Data",
                                            paste(round((oddpub_data %>%  filter(year == show_year))[["open_data_manual_perc"]], 0), "%"),
                                            "of publications mention sharing of data in 2019",
                                            plotlyOutput('plot_oddpub_data', height = "300px"))),
                       column(3, metric_box("Any Open Code",
                                            paste(round((oddpub_data %>%  filter(year == show_year))[["open_code_manual_perc"]], 0), "%"),
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
                     checkboxInput("checkbox_total_CT", "Show absolute numbers", value = FALSE),
                     br(),
                     fluidRow(
                       column(3, metric_box("Summary Results",
                                            paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_24"]], 0), "%"),
                                            "of trials completed in 2017 posted summary results on CT.gov within 24 months",
                                            plotlyOutput('plot_summary_results', height = "300px"))),
                       column(3, metric_box("Prospective registration",
                                            paste(round(metrics_show_year$perc_prosp_reg, 0), "%"),
                                            "of clinical trials started in 2019 are prospectively registered on CT.gov",
                                            plotlyOutput('plot_prosp_reg', height = "300px")))
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
                     checkboxInput("checkbox_total_vis", "Show absolute numbers", value = FALSE),
                     br(),
                     fluidRow(
                       column(6, metric_box("Problematic graphs",
                                            paste((filter(barzooka_data, year == show_year)$has_bar/
                                                     filter(barzooka_data, year == show_year)$total*100) %>% round(0), "%"),
                                            "of publications from 2019 use bar graphs for continuous data",
                                            plotlyOutput('plot_barzooka_problem', height = "300px"))),
                       column(6, metric_box("More informative graph types",
                                            paste((filter(barzooka_data, year == show_year)$has_informative/
                                                     filter(barzooka_data, year == show_year)$total*100) %>% round(0), "%"),
                                            "of publications from 2019 use more informative graph types",
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
                                      style = "default"))
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





  color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                     "#303A3E", "#007265", "#810050", "#000000",
                     "#DCE3E5")
  background_color <- "#ecf0f1"
  background_color_darker <- "#DCE3E5"


  #---------------------------------
  # Open Science plots
  #---------------------------------

  # Open Access
  OA_plot_data_plotly <- dashboard_metrics %>%
    make_OA_plot_data() %>%
    select(-OA, -all) %>%
    pivot_wider(names_from = category, values_from = perc)

  OA_plot_data_plotly_total <- dashboard_metrics %>%
    make_OA_plot_data_total() %>%
    select(-perc, -all) %>%
    pivot_wider(names_from = category, values_from = OA)


  output$plot_OA <- renderPlotly({
     if(input$checkbox_total_OS) {
       return(plot_OA_total(OA_plot_data_plotly_total, color_palette))
     } else {
       return(plot_OA_perc(OA_plot_data_plotly, color_palette))
     }
  })


  # Open Data & Code
  oddpub_plot_data <- dashboard_metrics %>%
    make_oddpub_plot_data() %>%
    rename(`Open Data` = open_data_manual_perc) %>%
    rename(`Open Code` = open_code_manual_perc)

  output$plot_oddpub_data <- renderPlotly({
    if(input$checkbox_total_OS) {
      return(plot_OD_total(oddpub_plot_data, color_palette))
    } else {
      return(plot_OD_perc(oddpub_plot_data, color_palette))
    }
  })

  output$plot_oddpub_code <- renderPlotly({
    if(input$checkbox_total_OS) {
      return(plot_OC_total(oddpub_plot_data, color_palette))
    } else {
      return(plot_OC_perc(oddpub_plot_data, color_palette))
    }
  })


  # Preprints
  preprints_plot_data <- dashboard_metrics_aggregate %>%
    select(year, preprints) %>%
    filter(!is.na(preprints)) %>%
    filter(year >= 2015)

  output$plot_preprints <- renderPlotly({
    plot_preprints(preprints_plot_data, color_palette)
  })


  #---------------------------------
  # Clinical trials plots
  #---------------------------------

  CTgov_plot_data_1 <- dashboard_metrics_aggregate %>%
    select(year, perc_sum_res_12, perc_sum_res_24,
           has_sum_res_12, has_sum_res_24,
           no_sum_res_12, no_sum_res_24) %>%
    filter(!is.na(perc_sum_res_12)) %>%
    filter(year >= 2015) %>%
    # for stacked bar graph of total numbers need 0-12 month + 12-24 month
    mutate(has_sum_res_24_only = has_sum_res_24 - has_sum_res_12)

  #for latest year, we only can check for summary results within 12 month
  #need to use the number for trials without summ res in 12 month here
  CTgov_plot_data_1$no_sum_res_24[is.na(CTgov_plot_data_1$no_sum_res_24)] <-
    CTgov_plot_data_1$no_sum_res_12[is.na(CTgov_plot_data_1$no_sum_res_24)]


  output$plot_summary_results <- renderPlotly({
    if(input$checkbox_total_CT) {
      return(plot_summary_results_total(CTgov_plot_data_1, color_palette))
    } else {
      return(plot_summary_results_perc(CTgov_plot_data_1, color_palette))
    }
  })


  CTgov_plot_data_2 <- dashboard_metrics_aggregate %>%
    select(year, has_prosp_reg, no_prosp_reg, perc_prosp_reg) %>%
    filter(!is.na(perc_prosp_reg)) %>%
    filter(year >= 2015)

  output$plot_prosp_reg <- renderPlotly({
    if(input$checkbox_total_CT) {
      return(plot_prosp_reg_total(CTgov_plot_data_2, color_palette))
    } else {
      return(plot_prosp_reg_perc(CTgov_plot_data_2, color_palette))
    }
  })


  #---------------------------------
  # Visualizations plots
  #---------------------------------

  output$plot_barzooka_problem <- renderPlotly({
    if(input$checkbox_total_vis) {
      return(plot_barzooka_problem_total(barzooka_data, color_palette))
    } else {
      return(plot_barzooka_problem_perc(barzooka_data, color_palette))
    }
  })

  output$plot_barzooka_inform <- renderPlotly({
    if(input$checkbox_total_vis) {
      return(plot_barzooka_inform_total(barzooka_data, color_palette))
    } else {
      return(plot_barzooka_inform_perc(barzooka_data, color_palette))
    }
  })

}

shinyApp(ui, server)


