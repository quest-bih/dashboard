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
library(vroom)

#----------------------------------------------------------------------------------------------------------------------
# load data & functions
#----------------------------------------------------------------------------------------------------------------------

Sys.setlocale("LC_TIME", "English_Germany.utf8")

source("impressum.R", encoding="UTF-8")
source("metric_box.R")
source("OA_metrics.R")
source("ODC_metrics.R")
source("preprints_metrics.R")
source("euctr_metrics.R")
source("ctgov_metrics.R")
source("vis_metrics.R")
source("orcid_metrics.R")
source("contribot_metrics.R")
source("app_functions_fair.R")
source("ui_elements.R")
source("methods_descriptions.R", encoding = "UTF-8")
source("resources_descriptions.R", encoding = "UTF-8")
source("fair_panel.R", encoding = "UTF-8")
source("bss_panel.R", encoding = "UTF-8")
source("about_page.R", encoding = "UTF-8")
source("iv_plot.R", encoding = "UTF-8")
source("datasets_panel.R")

dashboard_metrics <- vroom("./data/dashboard_metrics.csv")
# dashboard_metrics <- read_csv("./shiny_app/data/dashboard_metrics.csv")
dashboard_metrics_aggregate <- vroom("./data/dashboard_metrics_aggregate.csv")
# dashboard_metrics_aggregate <- read_csv("./shiny_app/data/dashboard_metrics_aggregate.csv") |>


# EU_trialstracker_dataset <- read_csv("./data/EU_trialstracker_past_data.csv")
sumres_data <- vroom("./data/EU_trialstracker_past_data.csv")
intovalue_dataset <- vroom("./data/IntoValue_Results_years.csv")


#datasets for the datatables
prosp_reg_dataset_shiny <- vroom("./data/prosp_reg_dataset_shiny.csv") |>
  # prosp_reg_dataset_shiny <- read_csv("./shiny_app/data/prosp_reg_dataset_shiny.csv") |>
  mutate_at(vars(nct_id, start_date, registration_date,
                 has_prospective_registration),
            as.character)
<<<<<<< HEAD
preprints_dataset_shiny <- read_csv("./data/preprints_dataset_shiny.csv")
orcid_dataset <- read_csv("./data/orcid_results.csv")

# fair dataset
fair_dataset <- read_csv("./data/fair_assessment_2022.csv", show_col_types = FALSE) |>
=======
preprints_dataset_shiny <- vroom("./data/preprints_dataset_shiny.csv")

# fair dataset
fair_dataset <- vroom("./data/fair_assessment_2021.csv", show_col_types = FALSE) |>
>>>>>>> modularized
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
         guid_scheme = guid_scheme_fuji) |>
  arrange(repository_name, article_doi)


show_dashboard <- function(...) {
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
                               HTML('This dashboard gives an overview of several metrics of open and responsible
                            research at the Charité (including the Berlin Institute of Health).
                            For a detailed discussion about monitoring core Open Science practices see
                            (<a href = "https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001949">Cobey et al. 2023</a>).
                            For more detailed information on the methods used to calculate those metrics, the dataset
                            underlying the metrics, or resources to improve your own research practices, click one of
                                 the following buttons on the right.')),
                            # h4(style = "margin-left:0cm",
                            #"This dashboard is a pilot that is still under development. More metrics will be added in the future."),
                            h4(style = "margin-left:0cm",
                               HTML('For more detailed open access metrics you can visit the
                            <a href="https://medbib-charite.github.io/oa-dashboard/">Charité Open Access Dashboard</a>
                                 developed by the Charité Medical Library.')),
                            br()
                     ),
                     column(4,
                            hr(),
                            br(),
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
                            br(),
                            br(),
                            h4(style = "margin-left:18mm", strong("Latest Update: April 2024")))
                   ),
                   fluidRow(column(1,
                                   selectInput("citationStyle",
                                               h5(HTML("<b>Cite us:</b>")),
                                               c("APA",
                                                 "MLA",
                                                 "Chicago"),
                                               width = "100px")),
                            column(11,
                                   hr(),
                                   # br(),
                                   htmlOutput("citation_text"))
                   ),
                 ),

                 # generate Open Science & Clinical trial metrics UI dynamically to determine column width during start of the app
                 uiOutput("OpenScience_metrics") |>
                   shinycssloaders::withSpinner(color = "#007265"),

                 uiOutput("CT_metrics") |>
                   shinycssloaders::withSpinner(color = "#007265"),

                 uiOutput("Broader_transparency_metrics") |>
                   shinycssloaders::withSpinner(color = "#007265"),

                 uiOutput("Visualizations_metrics") |>
                   shinycssloaders::withSpinner(color = "#007265"),
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

    RVs <- reactiveValues(total_os = FALSE, total_ct = FALSE,
                          total_bt = FALSE, total_vis = FALSE)

    observe({
      RVs$total_os <- input$checkbox_total_OS
    }) |>
      bindEvent(input$checkbox_total_OS)

    observe({
      RVs$total_bt <- input$checkbox_total_BT
    }) |>
      bindEvent(input$checkbox_total_BT)

    observe({
      RVs$total_ct <- input$checkbox_total_CT
    }) |>
      bindEvent(input$checkbox_total_CT)

    observe({
      RVs$total_vis <- input$checkbox_total_Vis
    }) |>
      bindEvent(input$checkbox_total_Vis)

    preprintsServer("plot_preprints", dashboard_metrics_aggregate, dashboard_metrics, reactive(RVs$total_os), color_palette)
    OAServer("plot_OA", dashboard_metrics, reactive(RVs$total_os), color_palette)
    ODServer("plot_OD", dashboard_metrics, "data", reactive(RVs$total_os), color_palette)
    ODServer("plot_OC", dashboard_metrics, "code", reactive(RVs$total_os), color_palette)
    ODServer("plot_DAS", dashboard_metrics, "das", reactive(RVs$total_os), color_palette)
    sumresServer("plot_sumres", sumres_data, reactive(RVs$total_ct), color_palette)
    ctgovServer("plot_prosp_reg", dashboard_metrics_aggregate, reactive(RVs$total_ct), color_palette)
    visServer("plot_barzooka_problem", dashboard_metrics, "problem", reactive(RVs$total_vis), color_palette)
    visServer("plot_barzooka_inform", dashboard_metrics, "inform", reactive(RVs$total_vis), color_palette)
    orcidServer("plot_orcid_pubs", dashboard_metrics, "pubs", reactive(RVs$total_bt), color_palette)
    contribotServer("plot_contrib", dashboard_metrics, "credit", reactive(RVs$total_bt), color_palette)

    output$citation_text <-
      renderUI({
        date <- format(Sys.Date(), "%d %B, %Y")
        date_mla <- format(Sys.Date(), "%B %d %Y")
        citation <- dplyr::case_when(
          input$citationStyle == "APA" ~ HTML("BIH QUEST Center for Responsible Research. (n. d.).
                          <i>Charité Dashboard on Responsible Research.</i>
                          Retrieved", paste0(date, ","), "from https://quest-dashboard.charite.de/"),
          input$citationStyle == "MLA" ~ HTML("BIH QUEST Center for Responsible Research. <i>Charité Dashboard on Responsible Research.</i>
                                              https://quest-dashboard.charite.de/. Accessed  ", paste0(date_mla, ".")),
          input$citationStyle == "Chicago" ~ HTML("BIH QUEST Center for Responsible Research. n. d.
                                                  “Charité Dashboard on Responsible Research.” Accessed ", paste0(date,"."),
                                                  "https://quest-dashboard.charite.de/.")
        )
        h5(citation, style = "margin-left:0cm; margin-top:8mm")

      }) |>
      bindEvent(input$citationStyle)

    output$OA <-
      renderUI({
        box_value <- get_current_OA(dashboard_metrics)
        box_text <- paste0("of publications were open access in ", dashboard_metrics$year |> max())
        alignment <- "right"

        metricBoxOutput(title = "Open Access",
                        value = box_value,
                        value_text = box_text,
                        plot = OAOutput("plot_OA", height = "300px"),
                        info_id = "infoOA",
                        info_title = "Open Access",
                        info_text = open_access_tooltip,
                        info_alignment = alignment)
      })  |>
      bindEvent(reactive(RVs$total_os))

    output$OD <-
      renderUI({
        box_value <- get_current_OD(dashboard_metrics)
        box_text <- paste0("of screened publications mentioned sharing data openly in ",
                           dashboard_metrics$year |>  max())
        alignment <- "left"
        metricBoxOutput(title = "Any Open Data",
                        value = box_value,
                        value_text = box_text,
                        plot = ODOutput("plot_OD", height = "300px"),
                        info_id = "infoOD",
                        info_title = "Open Data",
                        info_text = open_data_tooltip,
                        info_alignment = alignment)
      })  |>
      bindEvent(reactive(RVs$total_os))

    output$OC <-
      renderUI({
        box_value <- get_current_OC(dashboard_metrics)
        box_text <- paste0("of screened publications mentioned sharing code openly in ",
                           dashboard_metrics$year |>  max())
        alignment <- "left"
        metricBoxOutput(title = "Any Open Code",
                        value = box_value,
                        value_text = box_text,
                        plot = ODOutput("plot_OC", height = "300px"),
                        info_id = "infoOC",
                        info_title = "Open Code",
                        info_text = open_code_tooltip,
                        info_alignment = alignment)
      })  |>
      bindEvent(reactive(RVs$total_os))

    output$DAS <-
      renderUI({
        box_value <- get_current_DAS(dashboard_metrics)
        box_text <- paste0("of screened publications included a Data (DAS) or Code Availability Statement (CAS) in ",
                           dashboard_metrics$year |>  max())
        alignment <- "left"
        metricBoxOutput(title = "Any Data (DAS) or Code Availability Statement (CAS)",
                        value = box_value,
                        value_text = box_text,
                        plot = ODOutput("plot_DAS", height = "300px"),
                        info_id = "infoDAS",
                        info_title = "Data or Code Availability Statements",
                        info_text = das_tooltip,
                        info_alignment = alignment)
      })  |>
      bindEvent(reactive(RVs$total_os))


    output$preprints <-
      renderUI({
        box_value <- get_current_val(dashboard_metrics_aggregate, n_preprints)
        box_text <- paste0("preprints published in ", dashboard_metrics_aggregate$year |> max())

        metricBoxOutput(title = "Preprints",
                        value = box_value,
                        value_text = box_text,
                        plot = preprintsOutput("plot_preprints", height = "300px"),
                        info_id = "infoPreprints",
                        info_title = "Preprints",
                        info_text = preprints_tooltip,
                        info_alignment = "left")
      }) |>
      bindEvent(reactive(RVs$total_os))

    output$sumres <-
      renderUI({
        box_value <- get_current_sumres(sumres_data)
        box_text <- paste0("of due trials registered on the EU Clinical Trials Register reported results (as of: ",
                           sumres_data$retrieval_date |> max(), ")")

        metricBoxOutput(title = "Summary Results Reporting",
                        value = box_value,
                        value_text = box_text,
                        plot = sumresOutput("plot_sumres", height = "300px"),
                        info_id = "infoSumres",
                        info_title = "Summary Results",
                        info_text = summary_results_tooltip,
                        info_alignment = "left")
      })

    output$prospreg <-
      renderUI({
        box_value <- get_current_ctgov(dashboard_metrics_aggregate, perc_prosp_reg)
        box_text <- paste0("of clinical trials registered on ClinicalTrials.gov were prospectively registered in ",
                           dashboard_metrics_aggregate$year |> max())
        metricBoxOutput(title = "Prospective Registration",
                        value = box_value,
                        value_text = box_text,
                        plot = ctgovOutput("plot_prosp_reg", height = "300px"),
                        info_id = "infoProspReg",
                        info_title = "Prospective Registration",
                        info_text = prospective_registration_tooltip,
                        info_alignment = "left")
      })

    output$vis_problem <-
      renderUI({
        box_value <- get_current_vis(dashboard_metrics, perc_bar)
        box_text <- paste0("of publications from ", dashboard_metrics$year |> max(),
                           " used bar graphs for continuous data")

        metricBoxOutput(title = "Problematic graph types",
                        value = box_value,
                        value_text = box_text,
                        plot = visOutput("plot_barzooka_problem", height = "300px"),
                        info_id = "infoVisProblem",
                        info_title = "Problematic graph types",
                        info_text = vis_problem_tooltip)
      })

    output$vis_inform <-
      renderUI({
        box_value <- get_current_vis(dashboard_metrics, perc_informative)
        box_text <- paste0("of publications from ", dashboard_metrics$year |> max(), " used more informative graph types")

        metricBoxOutput(title = "More informative graph types for continuous data",
                        value = box_value,
                        value_text = box_text,
                        plot = visOutput("plot_barzooka_inform", height = "300px"),
                        info_id = "infoVisInform",
                        info_title = "More informative graph types",
                        info_text = vis_inform_tooltip,
                        info_alignment = "left")
      })

    output$orcid_pubs <- renderUI({
      box_value <- get_current_orcids_from_pubs(dashboard_metrics)
      box_text <- paste0("of publications with a Charité correspondence author", " included at least one ORCID in ",
                         dashboard_metrics$year |> max())

      metricBoxOutput(title = "ORCIDs in Publications",
                      value = box_value,
                      value_text = box_text,
                      plot = orcidOutput('plot_orcid_pubs', height = "300px"),
                      info_id = "infoOrcidPubs",
                      info_title = "ORCIDpubs",
                      info_text = orcid_pubs_tooltip,
                      info_alignment = "right")
    })

    output$authorship <- renderUI({
      box_value <- get_current_contribot(dashboard_metrics, perc_has_contrib)
      box_text <- paste0("of screened publications had authorship statements in ",
                         dashboard_metrics$year |> max())
      metricBoxOutput(title = "Authorship Statements",
                      value = box_value,
                      value_text = box_text,
                      plot = contribotOutput("plot_contrib", height = "300px"),
                      info_id = "infoAuthorship",
                      info_title = "Authorship",
                      info_text = authorship_tooltip,
                      info_alignment = "left")
    }) |>
      bindEvent(reactive(RVs$total_bt))

    # dynamically determine column width of Open Science metrics at program start
    # four columns if resolution large enough, otherwise two columns
    output$OpenScience_metrics <- renderUI({
      req(input$width)
      if(input$width < 1400) {
        col_width <- 6
        alignment <- "left"
      } else {
        col_width <- 4
        alignment <- "right"
      }

      wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
                h2(strong("Open Science"), align = "left"),
                fluidRow(
                  column(2, checkboxInput("checkbox_total_OS", strong("Show absolute numbers"), value = FALSE)),
                  column(8, h5(strong("Double-click or select rectangular area inside any panel to zoom in")))
                ),
                fluidRow(
                  column(col_width, uiOutput("OA") |>
                           shinycssloaders::withSpinner(color = "#007265")),
                  column(col_width, uiOutput("preprints") |>
                           shinycssloaders::withSpinner(color = "#007265"))
                ),
                fluidRow(column(col_width, uiOutput("DAS") |>
                                  shinycssloaders::withSpinner(color = "#007265")),
                         column(col_width, uiOutput("OD") |>
                                  shinycssloaders::withSpinner(color = "#007265")),
                         column(col_width, uiOutput("OC") |>
                                  shinycssloaders::withSpinner(color = "#007265"))
                )
      )
    })



    output$CT_metrics <- renderUI({
      req(input$width)
      if(input$width < 1400) {
        col_width <- 6
        alignment <- "left"
      } else {
        col_width <- 4
        alignment <- "right"
      }

      fluidRow(
        column(12,
               wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
                         h2(strong("Clinical Trials"), align = "left"),
                         fluidRow(
                           column(2, checkboxInput("checkbox_total_CT", strong("Show absolute numbers"), value = FALSE)),
                           column(8, h5(strong("Double-click or select rectangular area inside any panel to zoom in")))
                         ),
                         fluidRow(
                           column(col_width, uiOutput("sumres") |>
                                    shinycssloaders::withSpinner(color = "#007265")),
                           column(col_width, metricBoxOutput(title = "Timely publication of results",
                                                             value = paste(round(intovalue_dataset$percentage_published_2_years |> last() * 100, 0), "%"),
                                                             value_text = paste0("of trials registered on CT.gov or DRKS that ended in ",
                                                                                 intovalue_dataset$completion_year |> last(),
                                                                                 " published results
                                                                 within 2 years"),
                                                             plot = plotlyOutput('plot_intovalue', height = "300px"),
                                                             info_id = "infoIntoValue",
                                                             info_title = "Timely publication of results",
                                                             info_text = intovalue_tooltip,
                                                             info_alignment = alignment)),
                           column(col_width, uiOutput("prospreg") |>
                                    shinycssloaders::withSpinner(color = "#007265")))))
      )
    })

    output$Broader_transparency_metrics <- renderUI({
      req(input$width)
      if(input$width < 1400) {
        col_width <- 6
        alignment <- "left"
      } else {
        col_width <- 4
        alignment <- "right"
      }
      wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
                h2(strong("Broader Transparency"), align = "left"),
                fluidRow(
                  column(2, checkboxInput("checkbox_total_BT", strong("Show absolute numbers"), value = FALSE)),
                  column(8, h5(strong("Double-click or select rectangular area inside any panel to zoom in")))
                ),
                fluidRow(
                  column(
                    col_width, uiOutput("orcid_pubs") |>
                      shinycssloaders::withSpinner(color = "#007265")
                  ),
                  column(
                    col_width, uiOutput("authorship") |>
                      shinycssloaders::withSpinner(color = "#007265")
                  )
                )
      )
    })

    output$Visualizations_metrics <- renderUI({

      #always show two tabs in one row for the visualization metrics
      col_width <- 6
      alignment <- "left"

      wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
                h2(strong("Visualizations"),
                   align = "left"),
                fluidRow(
                  column(2, checkboxInput("checkbox_total_Vis", strong("Show absolute numbers"), value = FALSE)),
                  column(8, h5(strong("Double-click or select rectangular area inside any panel to zoom in")))
                ),
                fluidRow(
                  column(col_width, uiOutput("vis_problem") |>
                           shinycssloaders::withSpinner(color = "#007265")),
                  column(col_width, uiOutput("vis_inform") |>
                           shinycssloaders::withSpinner(color = "#007265"))
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

                column(col_width, metricBoxOutput(style_resp = style_resp,
                                             title = "FAIR scores by principles",
                                             value = glue::glue("{n} %", n = fair_dataset %>% filter(repository_type == "field-specific repository") %>% pull(fuji_percent) %>% mean(na.rm = TRUE) %>% round(0)),
                                             value_text = "is the average FAIR score of datasets from 2022 in disciplinary repositories",
                                             plot = plotlyOutput('plot_fair_principle', height = "300px"),
                                             info_id = "infoFAIRprinciples",
                                             info_title = "FAIR scores by principles",
                                             info_text = fair_principles_tooltip,
                                             info_alignment = "bottom")),

                column(col_width, metricBoxOutput(style_resp = style_resp,
                                             title = "Dataset licenses",
                                             value = glue::glue("{n} %", n = round(nrow(fair_dataset[fair_dataset$repository_type == "general-purpose repository" & fair_dataset$license_fuji != "no license", ])/nrow(fair_dataset[fair_dataset$repository_type == "general-purpose repository", ])*100, 0)),
                                             value_text = "of datasets in general-purpose repositories specified a standard, machine readable license under which data can be reused",
                                             plot = plotlyOutput('plot_fair_license', height = "300px"),
                                             info_id = "infoFAIRlicenses",
                                             info_title = "Dataset licenses",
                                             info_text = fair_licenses_tooltip,
                                             info_alignment = "bottom")),
                column(col_width, metricBoxOutput(style_resp = style_resp,
                                             title = "FAIR scores by identifiers",
                                             value = glue::glue("{n} %", n = round(nrow(fair_dataset[fair_dataset$guid_scheme_fuji != "url", ])/nrow(fair_dataset)*100, 0)),
                                             value_text = "of 2022 datasets have a persistent identifier (e.g., DOI, Handle) associated with a higher FAIR score",
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
  # IntoValue
  #---------------------------------

  output$plot_intovalue <- renderPlotly({
    if(input$checkbox_total_CT) {
      return(plot_intovalue_total(intovalue_dataset, color_palette))
    } else {
      return(plot_intovalue_perc(intovalue_dataset, color_palette))
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

}

show_dashboard()
