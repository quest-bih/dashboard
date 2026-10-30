fair_panel <-
  tabPanel("FAIR data", value = "tabFAIR",
           wellPanel(
             br(),
             fluidRow(
               column(8,
                      h1(style = "margin-left:0cm", strong("Data Reusability (FAIR data)"), align = "left"),
                      h4(style = "margin-left:0cm",
                         HTML("Good data management is a key factor in generating, reproducing, and reusing scientific knowledge. The <a href = 'https://www.go-fair.org/fair-principles/'>FAIR principles</a>
                              provide guidance to increase the findability, accessibility, interoperability, and reusability of research data objects. As a result of the increase in volume, complexity,
                              and creation speed of data, the FAIR principles emphasize the machine-actionability of data reuse.")),
                      h4(style = "margin-left:0cm",
                         HTML("The FAIR data metrics in this dashboard indicate how well research data objects shared by Charité’s researchers and the repositories used to deposit them conform with the FAIR principles.")),
                      h4(style = "margin-left:0cm",
                         HTML("FAIR metrics are not to be understood as evaluations, but rather as assistance. This is particularly true at the repository level, as individual
                              researchers have limited influence on the FAIRness of research data objects.")),
                      checkboxInput("checkbox_colorblind", "Click for colorblind-friendly charts", value = FALSE)
               ),
               column(4,
                      hr(),
                      br(),
                      br(),
                      actionButton(inputId = 'buttonMethodsFAIR',
                                   label = 'See methods',
                                   style = "margin-left:20mm; color: white; background-color: #aa1c7d;"),
                      actionButton(inputId = 'buttonDatasetFAIR',
                                   label = 'See dataset',
                                   style = "color: white; background-color: #aa1c7d;"),
                      br(),
                      br(),
                      h4(style = "margin-left:18mm", strong("Latest Update: October 2026"))
               ),
             )
           ),

           uiOutput("DataReusability_timeline"),
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
