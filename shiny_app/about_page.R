about_page <- tabPanel("About", value = "tabAbout",
                       h3("Contributors"),
                       br(),
                       h4("Publication search"),
                       helpText(HTML('The publication list was kindly provided by the
                                     <a href="https://medbib-charite.github.io/oa-dashboard/#about">Charité Medical Library</a>
                                     team: Jenny Delasalle (Concept); Ursula Flitner (Concept, Data collection);
                                     Manuela Gregor (Data collection); Katja Maly (Data collection); Anja Siebert (Data collection);
                                     Jan Taubitz (Concept, Technical implementation)
')),
                       br(),
                       h4("Open access data"),
                       helpText(HTML('The open access data were also provided by the
                                     <a href="https://medbib-charite.github.io/oa-dashboard/#about">Charité Medical Library</a>
                                     team.')),
                       br(),
                       h4("ODDPub - Open Data & Code detection"),
                       helpText('Nico Riedel (Conceptualization, Methodology, Technical Implementation, Validation);
                                Evgeny Bobrov (Conceptualization, Methodology, Validation); Miriam Kip (Conceptualization, Methodology)'),
                       br(),
                       h4("Barzooka - Visualization type detection"),
                       helpText('Nico Riedel (Conceptualization, Methodology, technical implementation);
                                Tracey Weissgerber (Conceptualization, Methodology); Robert Schultz (Validation)'),
                       br(),
                       h4("Clinical trial metrics"),
                       helpText('Nico Riedel (Conceptualization, Methodology, technical implementation);
                                Daniel Strech (Conceptualization, Methodology); Susanne Wieschowski (Conceptualization, Methodology);
                                Peter Grabitz (Conceptualization, Methodology); Delwen Franzen (Conceptualization, Methodology);
                                Maia Salholz-Hillel (Conceptualization, Methodology)'),
                       br(),
                       h4("Shiny app"),
                       helpText('Nico Riedel (Conceptualization, Technical Implementation); Tracey Weissgerber (Conceptualization);
                                Ulrich Dirnagl (Conceptualization); Evgeny Bobrov (Conceptualization); Daniel Strech (Conceptualization);
                                Delwen Franzen (Conceptualization); Maia Salholz-Hillel (Conceptualization)'),
                       br(),
                       h3('Acknowledgment'),
                       helpText('We greatfully acknowledge funding by the Wellcome Trust (Charite-BIH Translational Partnership)'),
                       br(),
                       h3('Contact address'),
                       helpText('QUEST Center for Responsible Research '),
                       helpText('Berlin Institute of Health at Charité – Universitätsmedizin Berlin'),
                       helpText('Anna-Louisa-Karsch-Str. 2'),
                       helpText('10178 Berlin, Germany'),
                       helpText('quest@bihealth.de'),
                       helpText(HTML('<a href="https://www.bihealth.org/quest-center/">
                                      https://www.bihealth.org/quest-center/ </a>')),
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
