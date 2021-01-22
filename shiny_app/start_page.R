start_page <- tabPanel(
    "Start page",
    value = "tabStart",
    wellPanel(
        br(),
        fluidRow(
            column(
                8,
                h1(style = "margin-left:0cm", strong("Responsible Metrics Dashboard"), align = "left"),
                h4(style = "margin-left:0cm",
                   "This dashboard is a proof-of-principle overview of several metrics of open and responsible research for several German University Medical Centres (UMC's). For more detailed information on the methods used to calculate those metrics, the dataset underlying the metrics, or resources to improve your own research practices, click one of the following buttons."),
                h4(style = "margin-left:0cm",
                   "This dashboard is a pilot that is still under development, and should not be used to compare UMC's or inform policy. More metrics may be added in the future."),
                br()
            ),
            column(
                4,
                hr(),
                br(),
                br(),
                actionButton(
                    style = "color: white; background-color: #aa1c7d;",
                    'buttonAllUMCs',
                    'See all UMC\'s'
                ),
                actionButton(
                    style = "color: white; background-color: #aa1c7d;",
                    'buttonMethods',
                    'See methods'
                ),
                actionButton(
                    style = "color: white; background-color: #aa1c7d;",
                    'buttonDatasets',
                    'See data sets'
                ),
                br()
            )
        ),
        fluidRow(
            column(
                4,
                br(),
                br(),
                selectInput(
                    "selectUMC",
                    strong("Choose UMC"),
                    choices = list(

                        ## This needs to be updated manually
                        ## From here:
                        ## rm_data$city %>% unique()
                        
                        "Show average only" = "all",
                        "Berlin" = "berlin",
                        "Bochum" = "bochum",
                        "Cologne" = "cologne",
                        "Duisberg-Essen" = "duisburg-essen",
                        "Erlangen" = "erlangen",
                        "Hamburg" = "hamburg",
                        "Hannover" = "hannover",
                        "Leipzig" = "leipzig",
                        "Magdeburg" = "magdeburg",
                        "Oldenburg" = "oldenburg",
                        "Rostock" = "rostock",
                        "Witten" = "witten",
                        "Wurzburg" = "wurzburg"
                    ),
                    selected = NA
                )
            )
        )
    ),
    uiOutput("robustness_metrics")
)

randomization_tooltip <- strwrap("This metric shows ...")
