resources_panel <- tabPanel("Educational resources", value = "tabRessources",
                            h1("Educational resources"),
                            h4("Want to improve your own research practices? See the following resources to get started."),
                            br(),
                            wellPanel(
                               h2("Open Science"),
                               h4("Learn strategies to make your research more transparent, robust & reproducible with the
         Reproducibiliteach video series. Strategies cover all phases of
         the research process (planning a project, executing a project, publishing your research):"),
                               h4(HTML('<a href="https://www.youtube.com/@reproducibiliteach">
                  https://www.youtube.com/@reproducibiliteach </a>'))
                            ),
                            wellPanel(
                               h2("Visualizations"),
                               h4("Wondering why you shouldn’t use bar graphs for continuous data, what types of graphs to use instead,
             and where to find free graphing tools & resources that will help you to replace bar graphs with more informative figures?
             See the following resources:"),
                               br(),
                               h4(tags$b("A fast, visual overview")),
                               h4(HTML('<a href="https://osf.io/bsa46/files/yd3th">
                  https://osf.io/bsa46/files/yd3th </a>')),
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
                    https://elifesciences.org/inside-elife/5114d8e9/webinar-report-transforming-data-visualisation-to-improve-transparency-and-reproducibility </a>')),
                               br(),
                               h4(tags$b("Video Series")),
                               h4(HTML('<a href="https://www.youtube.com/watch?v=tT8SecE1-S0&list=PLWb8IFSVeQ62NbG-u4vQlh4srFcC2KH5g">
                    https://www.youtube.com/watch?v=tT8SecE1-S0&list=PLWb8IFSVeQ62NbG-u4vQlh4srFcC2KH5g </a>'))
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
