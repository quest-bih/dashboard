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

dashboard_metrics_aggregate <- read_csv("data/dashboard_metrics_aggregate.csv") %>%
  mutate(perc_prosp_reg = perc_prosp_reg * 100) %>%
  mutate(perc_sum_res_12 = perc_sum_res_12 * 100) %>%
  mutate(perc_sum_res_24 = perc_sum_res_24 * 100)


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
                      #h4(style = "margin-left:10cm", HTML(paste0(a(href = 'https://osf.io/fh426/', "Learn more")))),
                      br())
             )
           ),

           wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
                     h2(strong("Open Science"), align = "left"),
                     h4(paste0("These metrics show Open Science practices at Charité. The percentage of Charité
                original research publications that are published as Open Access articles are mesured as well as
                the percentage of publications that state that they share their research data or analysis code.
                Additionally, we count articles published on preprint servers like bioRxiv.
                The data are shown for the year ", show_year, ".")),
                     br(),
                     fluidRow(
                       column(3, metric_box("Open Access",
                                            paste(round((OA_data %>% filter(year == show_year))[["OA_perc"]], 0), "%"),
                                            "Open Access articles")),
                       column(3, metric_box("Any Open Data",
                                            paste(round((oddpub_data %>%  filter(year == show_year))[["open_data_perc"]], 0), "%"),
                                            "of publications mention sharing of research data")),
                       column(3, metric_box("Any Open Code",
                                            paste(round((oddpub_data %>%  filter(year == show_year))[["open_code_perc"]], 0), "%"),
                                            "of publications mention sharing of code")),
                       column(3, metric_box("Preprints",
                                            metrics_show_year$preprints,
                                            "preprints published"))
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
                       column(3, metric_box("Summary Results: 12 months",
                                            paste(round(dashboard_metrics_aggregate[[13,"perc_sum_res_12"]], 0), "%"),
                                            "of trials completed in 2018 posted summary results on CT.gov within 12 months")),
                       column(3, metric_box("Summary Results: 24 months",
                                            paste(round(dashboard_metrics_aggregate[[12,"perc_sum_res_24"]], 0), "%"),
                                            "of trials completed in 2017 posted summary results on CT.gov within 24 months")),
                       column(3, metric_box("Prospective registration",
                                            paste(round(metrics_show_year$perc_prosp_reg, 0), "%"),
                                            "of clinical trials started in 2019 are prospectively registered on CT.gov"))
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
                       column(3, metric_box("Bar graphs for continuous data",
                                            filter(barzooka_data, year == show_year)$has_bar %>% round(0),
                                            "out of 1000 publications use bar graphs for continuous data")),
                       column(3, metric_box("Pie charts",
                                            filter(barzooka_data, year == show_year)$has_pie %>% round(0),
                                            "out of 1000 publications use pie charts")),
                       column(3, metric_box("More informative graph types",
                                            filter(barzooka_data, year == show_year)$has_informative %>% round(0),
                                            "out of 1000 publications use more informative graph types"))
                     )
           ),

           wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
                     h2(strong("Development over time"), align = "left"),
                     h2(strong("Open Science"), align = "left"),
                     fluidRow(
                       column(4,
                              h4(strong("Open Access"), align = "center"),
                              plotOutput('plot_OA')
                       ),
                       column(4,
                              h4(strong("Open Data"), align = "center"),
                              plotOutput('plot_oddpub_data')
                       ),
                       column(4,
                              h4(strong("Open Code"), align = "center"),
                              plotOutput('plot_oddpub_code')
                       )
                     ),
                     fluidRow(
                       column(4,
                              h4(strong("Preprints"), align = "center"),
                              plotOutput('plot_preprints')
                       )
                     ),
                     h2(strong("Clincal trials"), align = "left"),
                     fluidRow(
                       column(4,
                              h4(strong(HTML("Clinical trials - <br> Timely reporting")), align = "center"),
                              plotOutput('plot_CTgov_1')
                       ),
                       column(4,
                              h4(strong(HTML("Clinical trials - <br> Prospective registration")), align = "center"),
                              plotOutput('plot_CTgov_2')
                              #ggvisOutput('plot_prospective')
                       )
                     ),
                     h2(strong("Vizualisations"), align = "left"),
                     fluidRow(
                       column(6,
                              h4(strong("Problematic graphs"), align = "center"),
                              plotOutput('plot_barzooka_problem')
                       ),
                       column(6,
                              h4(strong("More informative graphs"), align = "center"),
                              plotOutput('plot_barzooka_inform')
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

  ),
  tabPanel("About",

           h3("Contributors"),
           br(),
           h4("ODDPub - Open Data & Code detection"),
           helpText('Riedel, Nico (Conceptualization, Methodology, Technical Implementation, Validation);
                     Bobrov, Evgeny (Conceptualization, Methodology, Validation); Kip, Miriam (Conceptualization, Methodology)'),
           br(),
           h4("Barzooka - Vizualization type detection"),
           helpText('Riedel, Nico (Conceptualization, Methodology, technical implementation);
                     Weissgerber, Tracey (Conceptualization, Methodology); Schultz, Robert (Validation)'),
           br(),
           h4("Clinical trial metrics"),
           helpText('Riedel, Nico (Conceptualization, Methodology, technical implementation);
                     Strech, Dainel; Wieschowski, Susanne; Grabitz, Peter; Franzen, Delwen; Salholz-Hillel, Maia'),
           br(),
           h4("Shiny app"),
           helpText('Riedel, Nico (Conceptualization, Technical Implementation); Weissgerber, Tracey (Conceptualization);
                     Dirnagl, Ulrich (Conceptualization); Bobrov, Evgeny (Conceptualization); Strech, Daniel (Conceptualization);
                     Franzen, Delwen (Conceptualization); Salholz-Hillel, Maia (Conceptualization)'),
           br(),
           h3('Contact address'),
           helpText('QUEST Center for Transforming Biomedical Research,'),
           helpText('Berlin Institute of Health (BIH), Berlin, Germany'),
           helpText('Anna-Louisa-Karsch-Str. 2'),
           helpText('10178 Berlin '),
           helpText('quest@bihealth.de'),
           helpText(HTML('<a href="https://www.bihealth.org/quest-center/">
                  https://www.bihealth.org/quest-center/ </a>'))
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
    #updateCollapse(session, "methodsPanels_OpenScience", open = "Preprints")
  })

  observeEvent(input$buttonResources, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabRessources")
  })



  color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#810050", "#000000")
  background_color <- "#ecf0f1"


  OA_plot_data <- dashboard_metrics %>%
    make_OA_plot_data()

  output$plot_OA <- renderPlot({
    ggplot(OA_plot_data, aes(x=year, y=perc, fill = category)) +
      geom_bar(stat="identity", color = "black", width = 0.8, size = 0.8) +
      scale_fill_manual(values=color_palette[c(3,6,7)]) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage Open Access") +
      ylim(0, 100) +
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
    rename(`Open Code` = open_code_perc) #%>%
    #gather(`Open Data`, `Open Code`, key="category", value="perc")

  output$plot_oddpub_data <- renderPlot({
    ggplot(oddpub_plot_data, aes(x=year, y=`Open Data`)) +
      geom_bar(stat="identity", position=position_dodge(),
               color = "black", fill = color_palette[3], size = 0.8) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage of publications") +
      ylim(0, 100) +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))

  }, height = 300, type = "cairo")


  output$plot_oddpub_code <- renderPlot({
    ggplot(oddpub_plot_data, aes(x=year, y=`Open Code`)) +
      geom_bar(stat="identity", position=position_dodge(),
               color = "black", fill = color_palette[3], size = 0.8) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage of publications") +
      ylim(0, 100) +
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
      geom_bar(stat="identity", fill = color_palette[3], color = "black", size = 0.8) +
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
    rename(`12 months` = perc_sum_res_12) %>%
    rename(`24 months` = perc_sum_res_24) %>%
    gather(`12 months`, `24 months`, key="category", value="perc")

  output$plot_CTgov_1 <- renderPlot({
    ggplot(CTgov_plot_data_1, aes(x=year, y=perc, fill=category)) +
      geom_bar(stat="identity", position=position_dodge(), color = "black", size = 0.6) +
      scale_fill_manual(values = color_palette[2:4]) +
      theme_minimal() +
      xlab("Year") +
      ylab("Percentage of trials") +
      ylim(0, 100) +
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
      ylim(0, 100) +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))

  }, height = 300, type = "cairo")


  vis_prospective <- reactive({
    CTgov_plot_data_2 %>%
      ggvis(x=~year, y=~perc) %>%
      layer_bars(fill := "#879C9D", stroke := "#3C5D70", fillOpacity := 0.5, fillOpacity.hover := 0.8,
                 width = 0.8) %>%
      hide_legend("fill") %>%
      add_axis("x", title = "Year",
               properties = axis_props(
                 labels = list(angle = -90, align = "right", baseline = "middle", fontSize = 15))) %>%
      add_axis("y", title = "Percentage of trials", title_offset = 60,
               format = "%",
               properties = axis_props(
                 labels = list(fontSize = 14),
                 title = list(fontSize = 16))) %>%
      add_tooltip(function(data){
        as.character(data$perc)
      }, "hover") %>%
      scale_numeric("y", domain = c(0, 1))
  })
  vis_prospective %>% bind_shiny("plot_prospective")



  barzooka_plot_data <- barzooka_data %>%
    rename(`bar graph` = has_bar) %>%
    rename(`pie chart` = has_pie) %>%
    rename(`bar graph with dots` = has_bardot) %>%
    rename(`box plot` = has_box) %>%
    rename(`dot plot` = has_dot) %>%
    rename(`histogram` = has_hist) %>%
    rename(`violin plot` = has_violin) %>%
    rename(`any informative` = has_informative)


  barzooka_plot_data_problem <- barzooka_plot_data %>%
    select(year, `bar graph`, `pie chart`) %>%
    gather(category, value, -year)

  output$plot_barzooka_problem <- renderPlot({
    ggplot(barzooka_plot_data_problem, aes(x=year, y=value, color = category)) +
      geom_line(aes(color=category), size=1.2) +
      geom_point(size=3) +
      scale_color_manual(values = color_palette[2:4]) +
      theme_minimal() +
      xlab("Year") +
      ylab("Graph types per 1000 publications") +
      ylim(0, 250) +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))
  }, height = 300, type = "cairo")


  barzooka_plot_data_inform <- barzooka_plot_data %>%
    select(-total, -`bar graph`, -`pie chart`) %>%
    gather(category, value, -year)

  output$plot_barzooka_inform <- renderPlot({
    ggplot(barzooka_plot_data_inform, aes(x=year, y=value, color = category)) +
      geom_line(aes(color=category), size=1.2) +
      geom_point(size=3) +
      scale_color_manual(values = color_palette) +
      theme_minimal() +
      xlab("Year") +
      ylab("Graph types per 1000 publications") +
      ylim(0, 250) +
      theme(axis.text=element_text(size=14, face = "bold"),
            axis.title=element_text(size=16, face = "bold"),
            legend.title=element_text(size=14, face = "bold"),
            legend.text=element_text(size=12, face = "bold"),
            panel.grid=element_blank(),
            plot.background = element_rect(fill = background_color, colour = background_color))
  }, height = 300, type = "cairo")

}

shinyApp(ui, server)


