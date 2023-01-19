#----------------------------------------------------------------------------------------------------------------------
# Load libraries
#----------------------------------------------------------------------------------------------------------------------
library(DT)
library(ggbeeswarm)
library(ggvis)
library(ggplot2)
library(haven)
library(plotly)
library(scales)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(assertthat)

#----------------------------------------------------------------------------------------------------------------------
# load data & functions
#----------------------------------------------------------------------------------------------------------------------

source("impressum.R", encoding="UTF-8")
source("app_functions_OA.R")
source("app_functions_oddpub.R")
source("app_functions_fair.R")
source("ui_elements.R")
source("methods_descriptions.R", encoding = "UTF-8")
source("resources_descriptions.R", encoding = "UTF-8")
source("fair_panel.R", encoding = "UTF-8")
source("bss_panel.R", encoding = "UTF-8")
source("about_page.R", encoding = "UTF-8")
source("plots.R", encoding = "UTF-8")
source("datasets_panel.R")

dashboard_metrics <- read_csv("./data/dashboard_metrics.csv")

dashboard_metrics_aggregate <- read_csv("./data/dashboard_metrics_aggregate.csv") %>%
  mutate(perc_prosp_reg = perc_prosp_reg * 100) %>%
  round(1)

EU_trialstracker_dataset <- read_csv("./data/EU_trialstracker_past_data.csv")
intovalue_dataset <- read_csv("./data/IntoValue_Results_years.csv")


#datasets for the datatables
prosp_reg_dataset_shiny <- read_csv("./data/prosp_reg_dataset_shiny.csv") %>%
  mutate_at(vars(nct_id, start_date, registration_date,
                 has_prospective_registration),
            as.character)
preprints_dataset_shiny <- read_csv("./data/preprints_dataset_shiny.csv")

orcid_dataset <- read_csv("./data/orcid_results.csv")

# fair dataset
fair_dataset <- read_csv("./data/fair_assessment_2021.csv", show_col_types = FALSE) |>
  arrange(repository_re3data, article)

# fair dataset for datatables
fair_dataset_datatable <- fair_dataset |>
  select(-repository) |>
  mutate(repository_type = factor(repository_type,
                                  levels = c("field-specific repository",
                                             "general-purpose repository"),
                                  labels = c("disciplinary repository",
                                             "general-purpose repository"))) |>
  rename(article_doi = article,
         dataset_id = best_identifier,
         repository_name = repository_re3data,
         guid_scheme = guid_scheme_fuji) %>%
  arrange(repository_name, article_doi)

#----------------------------------------------------------------------------------------------------------------------
# preprocessing, need to move somewhere else later
#----------------------------------------------------------------------------------------------------------------------

show_year <- "2021"
metrics_show_year <- dashboard_metrics_aggregate %>% filter(year == show_year)

OA_data <- dashboard_metrics %>%
  make_OA_plot_data() %>%
  #do not count the bronze category for the total number of publ
  filter(category != "bronze") %>%
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

ui <-
  tagList(
  tags$head(tags$script(type="text/javascript", src = "code.js")),
  navbarPage(
  "Charité Metrics Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
  tabPanel("Start page", value = "tabStart",
           wellPanel(
             br(),
             fluidRow(
               column(8,
                      h1(style = "margin-left:0cm", strong("Charité Dashboard on Responsible Research"), align = "left"),
                      h4(style = "margin-left:0cm",
                         HTML('Charité has committed itself to establish, promote and maintain a
                              research environment which enhances the robustness of research and
                              the reproducibility of results
                              (<a href="https://www.charite.de/en/charite/about_us/strategic_direction_2030/">Rethinking Health – Charité 2030</a>).')),
                      h4(style = "margin-left:0cm",
                         "This dashboard gives an overview of several metrics of open and responsible
                        research at the Charité (including the Berlin Institute of Health).
                        For more detailed information on the methods used to
                        calculate those metrics, the dataset underlying the metrics, or resources
                        to improve your own research practices, click one of the following buttons."),
                      h4(style = "margin-left:0cm",
                         "This dashboard is a pilot that is still under development. More metrics will be added in the future."),
                      h4(style = "margin-left:0cm",
                         HTML('For more detailed open access metrics you can visit the
                         <a href="https://medbib-charite.github.io/oa-dashboard/">Charité Open Access Dashboard</a>
                         developed by the Charité Medical Library.')),

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
             ),
           ),

           # generate Open Science & Clinical trial metrics UI dynamically to determine column width during start of the app
           uiOutput("OpenScience_metrics"),

           uiOutput("CT_metrics"),

           uiOutput("Visualizations_metrics"),

           br(),
           br(),
           br(),
           hr(),
           bsCollapsePanel(strong("Impressum"),
                           impressum_text,
                           style = "default"),
           bsCollapsePanel(strong("Datenschutz"),
                           datenschutz_text,
                           style = "default")
  ),

  # FAIR data metrics are shown in separate dashboard tab
  fair_panel,
  # BSS data are shown in separate dashboard tab
  bss_panel,

  # Methods and resources are displayed in a fold-out menu due to lack of space in the navbar.
  navbarMenu("Methods/Resources/Data",
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
                      bsCollapsePanel("Publication dataset",
                                      DT::dataTableOutput("data_table_publ"),
                                      style = "default")),
           br(),
           bsCollapse(id = "datasetPanels_PreprintDataset",
                      bsCollapsePanel("Preprint dataset",
                                      DT::dataTableOutput("data_table_preprints"),
                                      style = "default")),
           br(),
           bsCollapse(id = "datasetPanels_PublicationDataset",
                      bsCollapsePanel("Prospective registration dataset",
                                      DT::dataTableOutput("data_table_prosp_reg"),
                                      style = "default")),
           br(),
           bsCollapse(id = "datasetPanels_PublicationDataset",
                      bsCollapsePanel("Timely publication dataset",
                                      HTML('This dataset was already published
                        <a href="https://doi.org/10.5281/zenodo.5141343">here</a>.'),
                                      style = "default")),

           br(),
           bsCollapse(id = "datasetPanels_PublicationDatasetFAIR",
                      bsCollapsePanel(title = "Data reusability (FAIR data) dataset",
                                      DT::dataTableOutput("data_table_FAIR"),
                                      style = "default")),
           br(),
           bsCollapse(id = "datasetPanels_PublicationDatasetBSS",
                      bsCollapsePanel(title = "Berlin Science Survey (BSS) dataset",
                                      DT::dataTableOutput("data_table_BSS"),
                                      style = "default")),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           hr(),
           bsCollapsePanel(strong("Impressum"),
                           impressum_text,
                           style = "default"),
           bsCollapsePanel(strong("Datenschutz"),
                           datenschutz_text,
                           style = "default")
  )
  ),
  about_page,

  # Javascript code necessary to provide window width as input variable
  # for the dynamic column width setting
  tags$head(tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        ')),
  # Change color of all selectize dropdowns
  tags$head(tags$style(HTML('.selectize-input.full{background: #DCE3E5; border: #DCE3E5;}'))),
  # Change color of selected selectize dropdowns
 tags$head(tags$style(HTML('#select_repository+ div>.selectize-input{background: #DCE3E5; border: #DCE3E5;}'))),
 tags$head(tags$style(HTML('#checkbox_FAIR+ div>.selectize-input{background: #DCE3E5; border: #DCE3E5;}')))
 #tags$head(tags$style(HTML('#prio-module_status + div>.selectize-input{background: #DCE3E5; border: #DCE3E5;}'))),
 #tags$head(tags$style(HTML('#prio-module_research + div>.selectize-input{background: #DCE3E5; border: #DCE3E5;}')))

)
)
#----------------------------------------------------------------------------------------------------------------------
# server
#----------------------------------------------------------------------------------------------------------------------

server <- function(input, output, session)
{

  # URI routing
  # (see: https://stackoverflow.com/questions/71541259/uri-routing-with-shiny-router-and-navbarpage-in-a-r-shiny-app/71807248?noredirect=1#comment126924825_71807248)
  observeEvent(session$clientData$url_hash, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    if(is.null(input$navbarTabs) || !is.null(currentHash) && currentHash != input$navbarTabs){
      freezeReactiveValue(input, "navbarTabs")
      updateTabsetPanel(session, "navbarTabs", selected = currentHash)
    }
  }, priority = 1)

  observeEvent(input$navbarTabs, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    pushQueryString <- paste0("#", input$navbarTabs)
    if(is.null(currentHash) || currentHash != input$navbarTabs){
      freezeReactiveValue(input, "navbarTabs")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)

  # dynamically determine column width of Open Science metrics at program start
  # four columns if resolution large enough, otherwise two columns
  output$OpenScience_metrics <- renderUI({
    req(input$width)
    if(input$width < 1400) {
      col_width <- 6
      alignment <- "left"
    } else {
      col_width <- 3
      alignment <- "right"
    }

    wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
              h2(strong("Open Science"), align = "left"),
              fluidRow(
                column(2, checkboxInput("checkbox_total_OS", strong("Show absolute numbers"), value = FALSE)),
                column(8, h5(strong("Double-click or select rectangular area inside any panel to zoom in")))
                # column(2, checkboxInput("checkbox_zoom_OS", strong("Zoom in"), value = FALSE)),
                ),
              fluidRow(
                column(col_width, metric_box(title = "Open Access",
                                     value = paste(round((OA_data %>% filter(year == show_year))[["OA_perc"]], 0), "%"),
                                     value_text = paste0("of publications were open access in ", show_year),
                                     plot = plotlyOutput('plot_OA', height = "300px"),
                                     info_id = "infoOA",
                                     info_title = "Open Access",
                                     info_text = open_access_tooltip)),
                column(col_width, metric_box(title = "Any Open Data",
                                     value = paste(round((oddpub_data %>%  filter(year == show_year))[["open_data_manual_perc"]], 0), "%"),
                                     value_text = paste0("of publications mentioned sharing of data in ", show_year),
                                     plot = plotlyOutput('plot_oddpub_data', height = "300px"),
                                     info_id = "infoOD",
                                     info_title = "Open Data",
                                     info_text = open_data_tooltip,
                                     info_alignment = alignment),
                       # checkboxInput("checkbox_restrictions", strong("Show cases with restricted access"), value = FALSE)
                ),
                column(col_width, metric_box(title = "Any Open Code",
                                     value = round((oddpub_data %>%  filter(year == show_year))[["open_code_manual_count"]], 0),
                                     value_text = paste0("publications mentioned sharing of code in ", show_year),
                                     plot = plotlyOutput('plot_oddpub_code', height = "300px"),
                                     info_id = "infoOC",
                                     info_title = "Open Code",
                                     info_text = open_code_tooltip)),
                column(col_width, metric_box(title = "Preprints",
                                     value = metrics_show_year$preprints,
                                     value_text = paste0("preprints published in ", show_year),
                                     plot = plotlyOutput('plot_preprints', height = "300px"),
                                     info_id = "infoPreprints",
                                     info_title = "Preprints",
                                     info_text = preprints_tooltip,
                                     info_alignment = "left"))
                )
    )
  })



  output$CT_metrics <- renderUI({
    req(input$width)
    if(input$width < 1400) {
      col_width <- 6
      alignment <- "left"
    } else {
      col_width <- 3
      alignment <- "right"
    }

    fluidRow(
      column(9,
             wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
              h2(strong("Clinical trials"), align = "left"),
              checkboxInput("checkbox_total_CT", strong("Show absolute numbers"), value = FALSE),
              fluidRow(
                column(4, metric_box(title = "Summary Results",
                                     value = paste(round(EU_trialstracker_dataset$perc_reported[1] * 100, 0), "%"),
                                     value_text = paste0("of due trials registered on the EU Clinical Trials Register reported results (as of ",
                                                         EU_trialstracker_dataset$retrieval_date[1] %>% str_replace_all("-", "/"), ")"),
                                     plot = plotlyOutput('plot_summary_results', height = "300px"),
                                     info_id = "infoSumRes",
                                     info_title = "Summary Results reporting",
                                     info_text = summary_results_tooltip)),

                column(4, metric_box(title = "Timely publication of results",
                                             value = paste(round(intovalue_dataset$percentage_published_2_years %>% last() * 100, 0), "%"),
                                             value_text = paste0("of trials registered on CT.gov or DRKS that ended in ",
                                                                 intovalue_dataset$completion_year %>% last(),
                                                                 " published results
                                                                 within 2 years"),
                                             plot = plotlyOutput('plot_intovalue', height = "300px"),
                                             info_id = "infoIntoValue",
                                             info_title = "Timely publication of results",
                                             info_text = intovalue_tooltip,
                                             info_alignment = alignment)),

                column(4, metric_box(title = "Prospective registration",
                                     value = paste(round(metrics_show_year$perc_prosp_reg, 0), "%"),
                                     value_text = paste0("of clinical trials started in ", show_year, " were prospectively registered on CT.gov"),
                                     plot = plotlyOutput('plot_prosp_reg', height = "300px"),
                                     info_id = "infoProspReg",
                                     info_title = "Prospective registration",
                                     info_text = prospective_registration_tooltip,
                                     info_alignment = alignment))))),
             column(3,
              wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
                        h2(strong("Persistent identifiers"), align = "left"),
                        fluidRow(
                          br(),
                          br(),
                        column(12, metric_box(title = "ORCID",
                                           value = orcid_dataset$orcid_count %>% last(),
                                           value_text = paste0("Charité researchers with an ORCID (as of ",
                                                               orcid_dataset$date %>% last() %>% str_replace_all("-", "/"), ")"),
                                           plot = plotlyOutput('plot_orcid', height = "300px"),
                                           info_id = "infoOrcid",
                                           info_title = "ORCID",
                                           info_text = orcid_tooltip,
                                           info_alignment = "left")))))
    )
  })

  output$Visualizations_metrics <- renderUI({

    #always show two tabs in one row for the visualization metrics
    col_width <- 6
    alignment <- "left"


    wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
            h2(strong("Visualizations"),
               align = "left"),
            checkboxInput("checkbox_total_vis", strong("Show absolute numbers"), value = FALSE),
            fluidRow(
              column(col_width, metric_box(title = "Problematic graph types",
                                   value = paste((filter(barzooka_data, year == show_year)$has_bar/
                                                    filter(barzooka_data, year == show_year)$total*100) %>% round(0), "%"),
                                   value_text = paste0("of publications from ", show_year, " used bar graphs for continuous data"),
                                   plot = plotlyOutput('plot_barzooka_problem', height = "300px"),
                                   info_id = "infoVisProblem",
                                   info_title = "Problematic graph types",
                                   info_text = vis_problem_tooltip)),
              column(col_width, metric_box(title = "More informative graph types for continuous data",
                                   value = paste((filter(barzooka_data, year == show_year)$has_informative/
                                                    filter(barzooka_data, year == show_year)$total*100) %>% round(0), "%"),
                                   value_text = paste0("of publications from ", show_year, " used more informative graph types"),
                                   plot = plotlyOutput('plot_barzooka_inform', height = "300px"),
                                   info_id = "infoVisInform",
                                   info_title = "More informative graph types",
                                   info_text = vis_inform_tooltip,
                                   info_alignment = "left"))
            ))
  })

  output$DataReusability_2_metrics <- renderUI({

    # Create choices for selectInput
    choices <- list(
      `Repository types` = as.list(c("all repositories", "general-purpose repositories", "field-specific repositories")) %>% setNames(c("All repositories", "All general-purpose repositories", "All disciplinary repositories")),
      `General-purpose repositories` = as.list(unique(fair_dataset$repository_re3data[fair_dataset$repository_type == "general-purpose repository"])) %>%
        setNames(unique(fair_dataset$repository_re3data[fair_dataset$repository_type == "general-purpose repository"])),
      `Field-specific repositories` = as.list(unique(fair_dataset$repository_re3data[fair_dataset$repository_type == "field-specific repository"])) %>%
        setNames(unique(fair_dataset$repository_re3data[fair_dataset$repository_type == "field-specific repository"]))
    )

    title <- "FAIR assessment by F-UJI"
    # title <- NULL
    value <- textOutput("select_perc")
    value_text <- textOutput("select_text")
    plot <- plotlyOutput('plot_fair_principle_sunburst', height = "400px")

    wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
              h2(strong("FAIR assessment"), align = "left"),
              fluidRow(
                column(8, selectInput("select_repository", label = "Select repository type or repository",
                                      choices = choices,
                                      selected = 1))),
              fluidRow(
                column(8, wellPanel(style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
                                    fluidRow(
                                      column(8, align="left", h4(strong(title)))
                                    ),
                                    h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", value),
                                    h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", value_text),
                                    plot)),
                column(4, includeMarkdown("texts/text_FAIR.md"))
              )
    )

  })

  output$select_perc <- renderText({
    if(input$select_repository == "all repositories") {
      glue::glue("{n} %", n = fair_dataset %>% pull(fuji_percent) %>% mean(na.rm = TRUE) %>% round(0))
    } else if (input$select_repository == "general-purpose repositories") {
      glue::glue("{n} %", n = fair_dataset %>% filter(repository_type == "general-purpose repository") %>% pull(fuji_percent) %>% mean(na.rm = TRUE) %>% round(0))
    } else if (input$select_repository == "field-specific repositories") {
      glue::glue("{n} %", n = fair_dataset %>% filter(repository_type == "field-specific repository") %>% pull(fuji_percent) %>% mean(na.rm = TRUE) %>% round(0))
    } else {
      glue::glue("{n} %", n = fair_dataset %>% filter(repository_re3data == input$select_repository) %>% pull(fuji_percent) %>% mean(na.rm = TRUE) %>% round(0))
    }
  })

  output$select_text <- renderText({
    if(input$select_repository == "field-specific repositories") {
      "is the average FAIR score of research data objects in disciplinary repositories"
    } else {
      glue::glue("is the average FAIR score of research data objects in {input$select_repository}")
    }
  })



  output$DataReusability_1_metrics <- renderUI({

    title <- "FAIR scores by repositories"
    # title <- NULL
    value <- textOutput("var")
    value_text <- textOutput("text")
    plot <- plotlyOutput('plot_fair_treemap', height = "400px")

    wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
              h2(strong("FAIR assessment by repositories"), align = "left"),
              fluidRow(
                column(8, selectInput("checkbox_FAIR", label = "Select FAIR principle",
                                      choices = list(`Select all FAIR principles` = list("all FAIR principles" = "fair_score"), `Select individual FAIR principles` = list("Findability" = "f_score", "Accessibility" = "a_score", "Interoperability" = "i_score", "Reusability" = "r_score")),
                                      selected = "fair_score"))
              ),
              fluidRow(
                column(8, wellPanel(style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
                                    fluidRow(
                                      column(8, align="left", h4(strong(title)))
                                    ),
                                    h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", value),
                                    h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", value_text),
                                    plot)),
                column(4, includeMarkdown("texts/text_repositories.md"))
              )
    )
  })

  output$text <- renderText({
    if(input$checkbox_FAIR == "fair_score") {
      "is the average FAIR score of research data objects"
    } else if (input$checkbox_FAIR == "f_score") {
      "is the average Findability score of research data objects"
    } else if (input$checkbox_FAIR == "a_score") {
      "is the average Accessibility score of research data objects"
    } else if (input$checkbox_FAIR == "i_score") {
      "is the average Interoperability score of research data objects"
    } else if (input$checkbox_FAIR == "r_score") {
      "is the average Reusability score of research data objects"
    }
  })

  output$var <- renderText({
    if(input$checkbox_FAIR == "fair_score") {
      glue::glue("{n} %", n = fair_dataset %>% pull(fuji_percent) %>% mean(na.rm = TRUE) %>% round(0))
    } else if (input$checkbox_FAIR == "f_score") {
      glue::glue("{n} %", n = fair_dataset %>% pull(fuji_percent_f) %>% mean(na.rm = TRUE) %>% round(0))
    } else if (input$checkbox_FAIR == "a_score") {
      glue::glue("{n} %", n = fair_dataset %>% pull(fuji_percent_a) %>% mean(na.rm = TRUE) %>% round(0))
    } else if (input$checkbox_FAIR == "i_score") {
      glue::glue("{n} %", n = fair_dataset %>% pull(fuji_percent_i) %>% mean(na.rm = TRUE) %>% round(0))
    } else if (input$checkbox_FAIR == "r_score") {
      glue::glue("{n} %", n = fair_dataset %>% pull(fuji_percent_r) %>% mean(na.rm = TRUE) %>% round(0))
    }
  })


  output$DataReusability_metrics <- renderUI({

    req(input$width)
    if(input$width < 1400 & input$width > 700) {
      col_width <- 6
      alignment <- "left"
      style_resp <- "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5" #"overflow: scroll; height = 500px; padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5"
    } else {
      col_width <- 4
      alignment <- "right"
      style_resp <- "padding-top: 0px; padding-bottom: 10px; background-color:#DCE3E5"
    }

    wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
              h2(strong("FAIR assessment (other metrics)"), align = "left"),
              checkboxInput("checkbox_total_FAIR", strong("Show absolute numbers"), value = FALSE),
              fluidRow(

                column(col_width, metric_box(style_resp = style_resp,
                                             title = "FAIR scores by principles",
                                             value = glue::glue("{n} %", n = fair_dataset %>% filter(repository_type == "field-specific repository") %>% pull(fuji_percent) %>% mean(na.rm = TRUE) %>% round(0)),
                                             value_text = "is the average FAIR score of datasets from 2021 in disciplinary repositories",
                                             plot = plotlyOutput('plot_fair_principle', height = "300px"),
                                             info_id = "infoFAIRprinciples",
                                             info_title = "FAIR scores by principles",
                                             info_text = fair_principles_tooltip,
                                             info_alignment = "bottom")),

                column(col_width, metric_box(style_resp = style_resp,
                                             title = "Dataset licenses",
                                             value = glue::glue("{n} %", n = round(nrow(fair_dataset[fair_dataset$repository_type == "general-purpose repository" & fair_dataset$license_fuji != "no license", ])/nrow(fair_dataset[fair_dataset$repository_type == "general-purpose repository", ])*100, 0)),
                                             value_text = "of datasets in general-purpose repositories specified a standard, machine readable license under which data can be reused",
                                             plot = plotlyOutput('plot_fair_license', height = "300px"),
                                             info_id = "infoFAIRlicenses",
                                             info_title = "Dataset licenses",
                                             info_text = fair_licenses_tooltip,
                                             info_alignment = "bottom")),
                column(col_width, metric_box(style_resp = style_resp,
                                             title = "FAIR scores by identifiers",
                                             value = glue::glue("{n} %", n = round(nrow(fair_dataset[fair_dataset$guid_scheme_fuji != "url", ])/nrow(fair_dataset)*100, 0)),
                                             value_text = "of 2021 datasets have a persistent identifier (e.g., DOI, Handle) associated with a higher FAIR score",
                                             plot = plotlyOutput('plot_fair_sunburst', height = "300px"),
                                             info_id = "infoFAIRidentifiers",
                                             info_title = "Dataset identifiers",
                                             info_text = fair_identifiers_tooltip,
                                             info_alignment = "bottom"))
              )
    )
  })

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

  #actionButton on FAIR tab to switch tabs
  observeEvent(input$buttonMethodsFAIR, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_FAIR", open = "FAIR data")
  })

  observeEvent(input$buttonDatasetFAIR, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabDatasets")
    updateCollapse(session, "datasetPanels_PublicationDatasetFAIR",
                   open = "Data reusability (FAIR data) dataset")
  })

  #actionButton on BSS tab to switch to tabs
  observeEvent(input$buttonMethodsBSS, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_BSS",
                   open = "methodsPanels_BSS")
  })

  observeEvent(input$buttonDatasetBSS, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabDatasets")
    updateCollapse(session, "datasetPanels_PublicationDatasetBSS",
                   open = "Berlin Science Survey (BSS) dataset")
  })


  #tooltip buttons -> methods section
  observeEvent(input$infoOA, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_OpenScience", open = "Open Access")
  })

  observeEvent(input$infoOD, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_OpenScience", open = "Open Data and Open Code")
  })

  observeEvent(input$infoOC, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_OpenScience", open = "Open Data and Open Code")
  })

  observeEvent(input$infoPreprints, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_OpenScience", open = "Preprints")
  })

  observeEvent(input$infoOrcid, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_persistent_ids", open = "ORCID")
  })

  observeEvent(input$infoSumRes, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_ClinicalTrials", open = "Summary results reporting")
  })

  observeEvent(input$infoIntoValue, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_ClinicalTrials", open = "Timely publication of results")
  })

  observeEvent(input$infoProspReg, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_ClinicalTrials", open = "Prospective registration")
  })

  observeEvent(input$infoVisProblem, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabRessources")
  })

  observeEvent(input$infoVisInform, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabRessources")
  })

  observeEvent(input$infoFAIRrepository, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_FAIR", open = "FAIR data")
  })

    observeEvent(input$infoFAIRrepository, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_FAIR", open = "FAIR data")
  })

  observeEvent(input$infoFAIRprinciples, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_FAIR", open = "FAIR data")
  })

  observeEvent(input$infoFAIRlicenses, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_FAIR", open = "FAIR data")
  })

  observeEvent(input$infoFAIRidentifiers, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
    updateCollapse(session, "methodsPanels_FAIR", open = "FAIR data")
  })


  #data table to show the underlying datasets
  output$data_table_publ <- DT::renderDataTable({
    make_datatable(dashboard_metrics)
  })

  output$data_table_preprints <- DT::renderDataTable({
    make_datatable(preprints_dataset_shiny)
  })

  output$data_table_prosp_reg <- DT::renderDataTable({
    make_datatable(prosp_reg_dataset_shiny)
  })

  output$data_table_FAIR <- DT::renderDataTable({
    make_datatable(fair_dataset_datatable)
  })

  output$data_table_BSS <- DT::renderDataTable({
    make_datatable_BSS(bss_labeled_dataset)
  })

  color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                     "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
                     "#DCE3E5")

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
    rename(`Open Data` = open_data_manual_perc,
           `Open Code` = open_code_manual_perc)


  output$plot_oddpub_data <- renderPlotly({
    if(input$checkbox_total_OS) {
      return(plot_OD_total(oddpub_plot_data, color_palette))
    } else {
      return(plot_OD_perc(oddpub_plot_data, color_palette
                          # input$checkbox_zoom_OS,
                          # input$checkbox_restrictions
                          ))
    }
  })

  output$plot_oddpub_code <- renderPlotly({
    if(input$checkbox_total_OS) {
      return(plot_OC_total(oddpub_plot_data, color_palette))
    } else {
      return(plot_OC_perc(oddpub_plot_data, color_palette
                          # input$checkbox_zoom_OS
                          ))
    }
  })


  # Preprints

  #add total number of publications to the dataset
  preprint_publ_total <- dashboard_metrics %>%
    group_by(year) %>%
    summarize(count = n())

  preprints_plot_data <- dashboard_metrics_aggregate %>%
    select(year, preprints) %>%
    filter(!is.na(preprints)) %>%
    filter(year >= 2015) %>%
    left_join(preprint_publ_total, by = "year")

  output$plot_preprints <- renderPlotly({
    plot_preprints(preprints_plot_data, color_palette)
  })

  # Orcid
  output$plot_orcid <- renderPlotly({
    plot_orcid(orcid_dataset, color_palette)
  })


  #---------------------------------
  # Clinical trials plots
  #---------------------------------

  output$plot_summary_results <- renderPlotly({
    if(input$checkbox_total_CT) {
      return(plot_summary_results_total(EU_trialstracker_dataset, color_palette))
    } else {
      return(plot_summary_results_perc(EU_trialstracker_dataset, color_palette))
    }
  })

  output$plot_intovalue <- renderPlotly({
    if(input$checkbox_total_CT) {
      return(plot_intovalue_total(intovalue_dataset, color_palette))
    } else {
      return(plot_intovalue_perc(intovalue_dataset, color_palette))
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

  #---------------------------------
  # Data reusability (FAIR) plots
  #---------------------------------

  # FAIR license

  fair_license_plot_data <- fair_dataset %>%
    make_fair_license_plot_data()

  output$plot_fair_license <- renderPlotly({
    if(input$checkbox_total_FAIR) {
      return(plot_fair_license_total(fair_license_plot_data, color_palette))
    } else {
      return(plot_fair_license_perc(fair_license_plot_data, color_palette))
    }
  })

  # FAIR principle

  fair_principle_plot_data <- fair_dataset %>%
    make_fair_principle_plot_data()

  output$plot_fair_principle <- renderPlotly({
    plot_fair_principle_perc(fair_principle_plot_data, color_palette)
  })


  # FAIR repository treemap

  fair_treemap_plot_data <- fair_dataset %>%
    make_fair_treemap_plot_data()

  output$plot_fair_treemap <- renderPlotly({
    fair_perc <- glue::glue_collapse(input$checkbox_FAIR)

    if(input$checkbox_colorblind){
      color_seq <- c("#440154", "#25858E", "#FDE725")
    } else {
      color_seq <- c("#AA493A", "#F1BA50", "#007265")
    }

    plot_fair_treemap(fair_treemap_plot_data, color_palette, fair_perc, color_seq)
  })

  # FAIR identifiers sunburst

  fair_sunburst_plot_data <- fair_dataset %>%
    make_fair_sunburst_plot_data()

  output$plot_fair_sunburst <- renderPlotly({

    if(input$checkbox_colorblind){
      color_seq <- c("#440154", "#25858E", "#FDE725")
    } else {
      color_seq <- c("#AA493A", "#F1BA50", "#007265")
    }

    plot_fair_sunburst(fair_sunburst_plot_data, color_palette, color_seq)
  })

  # FAIR principles sunburst

  output$plot_fair_principle_sunburst <- renderPlotly({
    select_repository <- input$select_repository

    if(input$checkbox_colorblind){
      color_seq <- c("#440154", "#25858E", "#FDE725") # db colors c("#634587", "#F1BA50", "#007265") # viridis c("#440154", "#25858E", "#FDE725")
    } else {
      color_seq <- c("#AA493A", "#F1BA50", "#007265")
    }

    plot_fair_principle_sunburst(fair_dataset, color_palette, select_repository, color_seq)
  })

  #---------------------------------
  # Berlin Science Survey (BSS) server modules
  #---------------------------------

  moduleServer_plot("prio")
  moduleServer_plot("practices")
  moduleServer_plot("environment")

}

shinyApp(ui, server)


