library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggvis)
library(ggplot2)
library(shinythemes)
library(shinyBS)

#----------------------------------------------------------------------------------------------------------------------
# load data & functions
#----------------------------------------------------------------------------------------------------------------------

source("app_functions_OA.R")
source("app_functions_oddpub.R")
source("ui_elements.R")
source("methods_descriptions.R", encoding = "UTF-8")

dashboard_metrics <- read_csv("data/dashboard_metrics.csv") %>%
  rename(year = e_pub_year)

dashboard_metrics_aggregate <- read_csv("data/dashboard_metrics_aggregate.csv")


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

ui <- navbarPage(
  "Charité Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
  tabPanel("Start page",
           #overall_design_options,
           wellPanel(
             br(),
             fluidRow(
               column(8,
                      h1(style = "margin-left:0cm", strong("Charité Dashboard"), align = "left"),
                      h4(style = "margin-left:0cm",
                         "This dashboard gives an overview over several metrics of open and responsible
                        research at the Charité. For more detailed information on the methods used to
                        calculate those metrics or for ressources to improve your own research practices
                        click one of the following buttons."),
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
                      #h4(style = "margin-left:10cm", HTML(paste0(a(href = 'https://osf.io/fh426/', "Learn more")))),
                      br())
             )
           ),

           wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
                     h2(strong("Open Science"), align = "left"),
                     p(paste0("These metrics show Open Science practices at Charité. The proportion of Charité
                publications that are published as Open Access articles are mesured as well as
                the proportion of publications that share their research data or analysis code.
                Additionally, we count articles published on preprint servers like bioRxiv.
                The data are shown for the year ", show_year, ".")),
                     fluidRow(
                       column(3, metric_box("Open Access", paste(round((OA_data %>% filter(year == show_year))[["OA_perc"]] *100, 0), "%"),
                                            "Open Access articles")),
                       column(3, metric_box("Open Data", paste(round((oddpub_data %>%  filter(year == show_year))[["open_data_perc"]] *100, 0), "%"),
                                            "of publications have Open Data")),
                       column(3, metric_box("Open Code", paste(round((oddpub_data %>%  filter(year == show_year))[["open_code_perc"]] *100, 0), "%"),
                                            "of publications have Open Code")),
                       column(3, metric_box("Preprints", metrics_show_year$preprints,
                                            "preprints published"))
                     )
           ),

           wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
                     h2(strong("Clinical trials"), align = "left"),
                     p("These metrics look at clinical trials that are registered on ClinicalTrials.gov
                with Charité as the sponsor or with a priniciple investigator from Charité. We look
                both at the timely reporting of summary results (within 12 or 24 month) on
                ClinicalTrials.gov as well as prospective registration of the trials."),
                     fluidRow(
                       column(3, metric_box("Summary Results", paste(round(dashboard_metrics_aggregate[[13,"perc_sum_res_12"]] *100, 0), "%"),
                                            "of trials completed in 2018 posted summary results on CT.gov within 12 month")),
                       column(3, metric_box("Summary Results", paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_24"]] *100, 0), "%"),
                                            "of trials completed in 2017 posted summary results on CT.gov within 24 month")),
                       column(3, metric_box("Prospective registration", paste(round(metrics_show_year$perc_prosp_reg *100, 0), "%"),
                                            "of clinical trials started in 2019 are prospectively registered on CT.gov"))
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
  methods_panels,
  tabPanel("Educational resources", value = "tabRessources",
           h1("Educational resources"),
           h4("If you want to improve your own research practices related to the topics
             of Open Science, clinical trials or visualizations, you can
             have a look at the following resources."),
           br(),
           h2("Open Science"),
           p(HTML('- <a href="https://www.bihealth.org/de/forschung/quest-center/mission-ansaetze/open-science/quest-toolbox/">
                  The QUEST Toolbox </a>')),
           h2("Clinical trials"),
           p("???"),
           h2("Visualizations"),
           p(HTML('- <a href="http://statistika.mfub.bg.ac.rs/interactive-dotplot/">
                  Interactive dotplot tool </a>')),
           p(HTML('- <a href="https://doi.org/10.1371/journal.pbio.1002128">
                  Beyond Bar and Line Graphs: Time for a New Data Presentation Paradigm </a>')),
           p(HTML('- <a href="https://doi.org/10.1161/CIRCULATIONAHA.118.037777">
                  Reveal, Don’t Conceal - Transforming Data Visualization to Improve Transparency</a>'))

  )
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
  })

  observeEvent(input$buttonResources, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabRessources")
  })



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
      geom_bar(stat="identity", fill = color_palette[2], color = "black", size = 0.8) +
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


