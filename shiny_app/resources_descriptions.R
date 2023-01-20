resources_panel <- tabPanel("Educational resources", value = "tabRessources",
         h1("Educational resources"),
         h4("Want to improve your own research practices? See the following resources to get started."),
         br(),
      wellPanel(
         h2("Open Science"),
         h4("Use the QUEST Toolbox to find tools for making your science more open and reproducible,
              at any phase of the research process (planning a project, executing a project, publishing your research):"),
         h4(HTML('<a href="https://www.bihealth.org/de/translation/innovationstreiber/quest-center/services/service/die-quest-toolbox-fuer-die-eigene-forschung/">
                  https://www.bihealth.org/de/translation/innovationstreiber/quest-center/services/service/die-quest-toolbox-fuer-die-eigene-forschung/ </a>'))
      ),
      wellPanel(
         h2("Visualizations"),
         h4("Wondering why you shouldn’t use bar graphs for continuous data, what types of graphs to use instead,
             and where to find free graphing tools & resources that will help you to replace bar graphs with more informative figures?
             See the following resources:"),
         br(),
         h4(tags$b("A fast, visual overview")),
         h4(HTML('<a href="https://twitter.com/T_Weissgerber/status/1192694904603992064">
                  https://twitter.com/T_Weissgerber/status/1192694904603992064 </a>')),
         br(),
         h4(tags$b('Papers')),
         h4(HTML(' 1. 2015 PLoS Biology paper: <a href="http://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1002128">
                    http://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1002128 </a>')),
         h4(HTML(' 2. More recent data (see Table 3 to find a free visualization resource that meets your needs):
                    <a href="https://www.ahajournals.org/doi/10.1161/CIRCULATIONAHA.118.037777">
                    https://www.ahajournals.org/doi/10.1161/CIRCULATIONAHA.118.037777 </a>')),
         br(),
         h4(tags$b("Webinar")),
         h4(HTML('<a href="https://elifesciences.org/inside-elife/5114d8e9/webinar-report-transforming-data-visualisation-to-improve-transparency-and-reproducibility">
                    https://elifesciences.org/inside-elife/5114d8e9/webinar-report-transforming-data-visualisation-to-improve-transparency-and-reproducibility </a>'))
      ),
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
