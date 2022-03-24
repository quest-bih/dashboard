fair_panel <-
  tabPanel("FAIR data", value = "tabFAIR",
           wellPanel(
             br(),
             fluidRow(
               column(8,
                      h1(style = "margin-left:0cm", strong("Data Reusability (FAIR data)"), align = "left"),
                      h4(style = "margin-left:0cm",
                         HTML("Good data management is a key factor in generating, reproducing, and reusing scientific knowledge. The <a href = 'https://www.go-fair.org/fair-principles/'>FAIR principles</a> provide guidance to increase the findability, accessability, interoperability, and reusability of research data objects. As a result of the increase in volume, complexity, and creation speed of data the FAIR principles emphasize the machine-actionability of data management.")),
                      br()),
               column(4,
                      hr(),
                      br(),
                      br(),
                      actionButton(inputId = 'buttonMethodsFAIR',
                                   label = 'See methods',
                                   style = "color: white; background-color: #aa1c7d;"),
                      actionButton(inputId = 'buttonDatasetFAIR',
                                   label = 'See dataset',
                                   style = "color: white; background-color: #aa1c7d;"),
                      br()),
             )
           ),

                         # h1("Data Reusability (FAIR data)"),
                         #
                         # bsCollapsePanel("Methods — Click to expand",
                         #                 HTML(
                         #                   "In order for a research data object to be reused, it — and the hosting data repository —
                         #          must meet certain quality criteria that ensure it is <a href = 'https://www.go-fair.org/fair-principles/'>Findable, Accessible, Interoperable, and Reusable (FAIR)</a>.
                         #          For this purpose the data must be stored together with detailed metadata that must conform to a defined standard.
                         #          In addition, datasets are easier to reuse if the corresponding metadata is machine-readable and uses a standardized vocabulary.
                         #          "),
                         #
                         #                 br(),
                         #                 br(),
                         #
                         #                   HTML("Based on the text-mining algorithm <a href = 'https://doi.org/10.5334/dsj-2020-042'>ODDPub</a>
                         #          we manually screened the detected data statements and extracted one research dataset ID per mentioned repository.
                         #          We then used the extracted dataset IDs to query the automated screening tool <a href='https://www.f-uji.net'>F-UJI</a>.
                         #          F-UJI assesses the FAIRness of research data objects based on <a href = 'https://zenodo.org/record/4081213#.YhdU_C8w1pQ'>metrics</a>
                         #          developed by the <a href = 'https://www.fairsfair.eu'>FAIRsFAIR</a> project.
                         #               The results were then aggregated and enriched with data from <a href = 'https://www.re3data.org'>re3data</a>."),
                         #
                         #                 br(),
                         #                 br(),
                         #
                         #                   "F-UJI does not provide suitable automatic tests for all FAIR principles. FAIR principles A1.1, A1.2 and I2 are not assessed. The quality of metadata and compliance with FAIR principles depends mainly
                         #          on the repository providers and can therefore only be influenced by the creators of the datasets to a limited extent.",
                         #                 style = "default"),

                        # plotlyOutput('plot_fair_treemap', height = "300px"),

           uiOutput("DataReusability_2_metrics"),
           uiOutput("DataReusability_1_metrics"),
           uiOutput("DataReusability_metrics"),

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


                         # br(),
                         # bsCollapse(id = "datasetPanels_PublicationDataset",
                         #            bsCollapsePanel("Dataset — click to expand",
                         #                            DT::dataTableOutput("data_table_FAIR"),
                         #                            style = "default"))
           # shiny.router

           )
