fair_panel <-
  tabPanel("FAIR data", value = "tabFAIR",
           wellPanel(
             br(),
             fluidRow(
               column(8,
                      h1(style = "margin-left:0cm", strong("Data Reusability (FAIR data)"), align = "left"),
                      h4(style = "margin-left:0cm",
                         HTML("Good data management is a key factor in generating, reproducing, and reusing scientific knowledge. The <a href = 'https://www.go-fair.org/fair-principles/'>FAIR principles</a> provide guidance to increase the findability, accessability, interoperability, and reusability of research data objects. As a result of the increase in volume, complexity, and creation speed of data the FAIR principles emphasize the machine-actionability of data reuse")),
                      h4(style = "margin-left:0cm",
                         HTML("The FAIR data metrics in this dashboard indicate how well research data objects shared by Charité researchers and the repositories used to deposit them conform with the FAIR principles.")),
                      h4(style = "margin-left:0cm",
                         HTML("It is important that the FAIR metrics are not to be understood as evaluations, but rather as assistance. This is true at the repository level and even more so at the publication level. Individual researchers have limited influence on the FAIRness of research data objects, which is primarily determined by the data repositories.")),
                      checkboxInput("checkbox_colorblind", "Click for colorblind-friendly charts", value = FALSE),
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
           )
