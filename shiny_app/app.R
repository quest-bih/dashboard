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

ui <- navbarPage(
  "Charité Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
  tabPanel("Start page",
           #overall_design_options,
           wellPanel(
             br(),
             fluidRow(
               column(6,
                      h1(style = "margin-left:0cm", strong("Charité Dashboard"), align = "left"),
                      h4(style = "margin-left:0cm",
                         "This dashboard gives an overview over several metrics of open and responsible
                        research at the Charité. For more detailed information on the methods used to
                        calculate those metrics or for ressources to improve your own research practices
                        click one of the following buttons."),
                      br()),
               column(6,
                      hr(),
                      br(),
                      actionButton(style = "color: white; background-color: #aa1c7d;",
                                   'buttonMethods',
                                   'See methods',
                                   onclick ="window.open('https://www.bihealth.org/quest-center/', '_blank')"),
                      actionButton(style = "color: white; background-color: #aa1c7d;",
                                   'buttonResources',
                                   'See resources',
                                   onclick ="window.open('https://www.bihealth.org/quest-center/', '_blank')"),
                      #h4(style = "margin-left:10cm", HTML(paste0(a(href = 'https://osf.io/fh426/', "Learn more")))),
                      br())
             )
           ),

           wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
                     h2(strong("Open Science"), align = "left"),
                     p("These metrics show Open Science practices at Charité. The proportion of Charité
                publications that are published as Open Access articles are mesured as well as
                the proportion of publications that share their research data or analysis code.
                Additionally, we count articles published on preprint servers like bioRxiv."),
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
                     p("These metrics look at clinical trials that are registered on ClinicalTrials.gov
                with Charité as the sponsor or with a priniciple investigator from Charité. We look
                both at the timely reporting of summary results (within 12 or 24 month) on
                ClinicalTrials.gov as well as prospective registration of the trials."),
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
           h1("Detailed Methods"),
           h4("You can extend the panels to view the detailed methods for
              the individual metrics. The code for the actual implementation
              of the methods can be found on Github (Link)"),
           h2("Publication search"),
           bsCollapse(id = "methodsPanels_PublicationSearch",
                      bsCollapsePanel(strong("Publication Search"),
                        p("Many of the assessed metrics are publication-based metrics. To assess those metrics
                        on the institutional level, we first need to identify the publications that
                        can be assigned to the Charité. For this, we search the publication databases
                        Pubmed and Embase for biomedical publications with at least one of the
                        authors affiliated to the Charité or the Berlin Insitute of Health.
                        Subsequently, the search results from both databases were merged and deduplicated.
                        After a final filtering step for the publication year (2015 - 2019) and for
                        research articles, we obtained a list of institutional publications that
                        was used to calculate the publication-based metrics."),
                        style = "default")),
           h2("Open Science"),
           bsCollapse(id = "methodsPanels_OpenScience",
                      methods_panel("Open Access",
                        "The Open Access metric measures the degree of openness of the
                        publications published by the Charité researchers. Open Access
                        publications are freely and (usually) immediately available
                        to all other researchers in the world and thus help to distribute
                        research results transparently, openly and fast.",
                        "Using the obtained list of institutional publications, we query the
                        unpaywall database via its API (https://unpaywall.org/products/api)
                        to obtain information on the Open Access (OA) status of the publications.
                        Unpaywall is today the most comprehensive database of Open Access
                        information on research articles. It can be queried using publication
                        DOIs. There are different Open Access statuses a publication can have,
                        which are color-coded. Gold OA denotes publication in a pure OA journal.
                        Green OA denotes a freely available repository version. Hybrid OA denotes
                        an OA publication in a journal with offers both a subscription based model
                        as well as an Open Access option. Bronze OA denotes a publication which is
                        freely available on the journal page, but without a clear open license.
                        Those can be articles in a non-OA journal which have been made available
                        voluntarily by the journal but which might lose its OA status again.
                        Thus we only consider the OA categories gold, green and hybrid here.
                        As one publication can have several OA versions (e.g. a gold version
                        in an OA journal as well as a green version in a repository), we define
                        a hierarchy of the OA categories and for each publication only assign
                        the OA catagory with the highest priority. We use a hierarchy of
                        gold - hybrid - green (journal version before repository version),
                        as also implemented in the unpaywall database itself.
                        After querying the unpaywall API for all publication DOIs, we group
                        the results by OA status and publication year.",
                        "The unpaywall only stores information for publications that have
                        a DOI assigend by crossref. Articles without crossref DOI have to
                        be excluded from the OA analysis. However, in the most recent years
                        this is the case for a tiny minority (<1%) of the publications."),
                      methods_panel("Open Data and Open Code",
                        "The Open Data and Open Code metric measure how many publications
                        share their raw research data or analysis code with the publication.
                        Openly shared data and code makes research more transparent,
                        as research findings can be reproduced. Additionally, shared datasets
                        can be reused and combined by other scientists to answer new research
                        questions. Note however, that data sharing is not possible for all
                        studies, as there is either no dataset to share or as the data
                        cannot be shared, e.g. due to privacy concerns for patient data.",
                        "To identify publications that share research data or analysis code,
                        we use the text-mining algorithm ODDPub
                        (Code: https://github.com/quest-bih/oddpub,
                        preprint: https://doi.org/10.1101/2020.05.11.088021),
                        which was developed by QUEST. ODDPub searches the publication full-text
                        for statements indicating sharing of raw data or analysis code. A
                        text-mining approach is necessary, as there is not yet a standardized
                        way of sharing and reporting Open Data, and no database offers
                        comprehensive information on shared datasets or code.
                        To assess data and code sharing for the Charité publications,
                        we first downloaded the full-texts of the publications that were
                        accessible to us. Then we screened those full-texts with ODDPub and
                        summarized the results for each publication year.",
                        "Several limitations apply:
                        Only full texts for Open Access publications or publications in journals
                        that are subscribed by the Charité could be retrieved
                        (~90% of all detected publications).ODDPub only finds ~75% of all Open Data
                        publications and finds false positive cases (no manual check of the results
                        is done). Open Data is not relevant for all publications, so we would not
                        expect 100% of the publications to contain Open Data, even not in an ideal case.
                        We considered all publications that had at least one author affiliated
                        with the Charité – in some cases where those were only middle authors
                        with minor contributions to the project, they might have little impact
                        on the decision if data were made available."),
                      methods_panel("Preprints",
                        "This metric measures how many preprints with authors from the Charité
                        have been published on a range of preprint servers. Preprints allow
                        rapid and transparent communication of preliminary research results
                        before publication in a peer-reviewed journal.",
                        "To identify preprints published by Charité authors we used the dimensions
                        database (www.dimensions.ai), which indexes many preprint servers, including
                        arXiv and bioRxiv. We searched for articles of the type 'preprint' and
                        with authors assigned to the Charité via its Grid ID. The number of preprints
                        found by this search are then summarized by year.",
                        "Not all relevant preprint servers are currently indexed by dimensions,
                        including some of the bigger preprint servers like OSF preprints or medRxiv.
                        Thus we likely underestimate the number of published preprints.")),

           hr(),
           h2("Clinical trials"),
           bsCollapse(id = "methodsPanels_ClinicalTrials",
                      methods_panel("Timely reporting",
                        "This metric measures how often clinical trials registered at ClinicalTrials.gov
                        share their results in the form of summary results within 12 or 24 month.
                        Clinical trials are expensive and have often many contributing patients.
                        A fast dissemination of the trial results is crucial to make the evidence gained
                        in those trials available. The World Health organization recommends publishing
                        clinical trial results within one year after the end of a study.",
                        "We identified clinical trials associated to the Charité by searching the
                        AACT database (which aggregates the data from ClinicalTrials.gov) for trials
                        mentioning the Charité as either sponsor, responsible party or
                        with a priniciple investigator from Charité. We additionally filtered
                        for interventional trials and for the study status as either
                        completed, terminated, Suspended or unknown (which means that the estimated study
                        completion data has already passed, but the registry entry has not been updated
                        for more than 2 years). For the identified trials we calculated the time to
                        summary results reporting by comparing the completion date to the date
                        of results submission (if any were submitted), which are both included
                        in the AACT dataset. We then grouped the results by the completion year
                        of the studies.",
                        "While ClinicalTrials.gov is the largest trial registry, it is not the only
                        available registry. There are other registries like the EU Clinical Trials Register
                        or the German Clinical Trials Registry, which are not considered here.
                        Additionally, the completion dates given in the CT.gov registry can also
                        be planned completion dates (if the record is not updated after study
                        completion or if the study runs for longer), which can lead to inaccurate
                        measurement of time to reporting."),
                      methods_panel("Prospective registration",
                        "This metric measures if the clinical trials are registered before the
                        start date of the study, according to the information given on ClinicalTrials.gov.
                        The idea of prospective registration of studies is to make the trail specifications,
                        including primary and secondary outcomes, publically available before study start.
                        Prospective registration adds transparency, helps protect against outcome switching.",
                        "We used the same methods as for the timely reporting metric to identify Charité
                        trials. To assess if a study has been prospecively registered, we compare
                        the date the study was first submitted to the registry with the
                        start date given in the registry. As some of the earlier dates in the database
                        only stated the month but not the exact day and to account for other possible delays
                        we chose a conservative estimate of prospective registration and allow for a delay
                        between start and registration date of up to 60 days.",
                        "Like in the case of the summary results metric, we only focussed on the
                        ClinicalTrials.gov while there are other available registries as well.
                        Also, we rely on the information on ClinicalTrials.gov being accurate.")),
           hr(),
           h2("Visualizations"),
           bsCollapse(id = "methodsPanels_visualizations",
                      methods_panel("Graph types",
                        "This metric measures how often different graph types appear in the publications.
                        Especially bar graphs for continuous data are common in the biomedical literature
                        but are considered a suboptimal practice, as they conceal the underlying data
                        points and since many different data distributions can lead to the same bar graph.
                        Different alternative graph types like dot plots, violin plots, box plots or
                        histograms can be used instead. The best choice of graph type depends among other
                        things on the size of the dataset.",
                        "To detect graph types from publications we used the Barzooka algorithm (unpublished),
                        a deep neural network that was trained to detect graph types on publication pages and
                        that was developed by QUEST. Barzooka screens each page of the publication PDFs
                        and reports if one or several graph types were detected. The results were then
                        combined for each publication. For each publication we then report if a certain is
                        present (at least once) or not present. The number of publications with each
                        graph types are then summarized for each publication year.",
                        "Barzooka detects the graph types with different accuracies and in particular
                        for the categories of bar graphs for continuous data and appropriate bar graphs
                        (for count data) there can be errornous assignments.")),

  ),
  tabPanel("Educational tools",

  )
)

#----------------------------------------------------------------------------------------------------------------------
# server
#----------------------------------------------------------------------------------------------------------------------

server <- function(input, output, session)
{

  #output$genericPlot <- renderPlot(plot(rnorm(100)))
  # observeEvent(input$p1Button, ({
  #   updateCollapse(session, "methodsPanels", open = "Panel Open Access")
  # }))
  # observeEvent(input$styleSelect, ({
  #   updateCollapse(session, "methodsPanels", style = list("Panel Open Access" = input$styleSelect))
  # }))


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


