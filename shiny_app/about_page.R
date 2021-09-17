about_page <- tabPanel("About", value = "tabAbout",
                       h3("Contributors"),
                       br(),
                       h4("Publication search"),
                       helpText(HTML('The publication list was kindly provided by the
                                     <a href="https://bibliothek.charite.de">Charité medical library</a>
                                     team.')),
                       br(),
                       h4("Open Access data"),
                       helpText(HTML('The open access data were also provided by the
                                     <a href="https://bibliothek.charite.de">Charité medical library</a>
                                     team.')),
                       br(),
                       h4("ODDPub - Open Data & Code detection"),
                       helpText('Riedel, Nico (Conceptualization, Methodology, Technical Implementation, Validation);
                                Bobrov, Evgeny (Conceptualization, Methodology, Validation); Kip, Miriam (Conceptualization, Methodology)'),
                       br(),
                       h4("Barzooka - Visualization type detection"),
                       helpText('Riedel, Nico (Conceptualization, Methodology, technical implementation);
                                Weissgerber, Tracey (Conceptualization, Methodology); Schultz, Robert (Validation)'),
                       br(),
                       h4("Clinical trial metrics"),
                       helpText('Riedel, Nico (Conceptualization, Methodology, technical implementation);
                                Strech, Daniel (Conceptualization, Methodology); Wieschowski, Susanne (Conceptualization, Methodology);
                                Grabitz, Peter (Conceptualization, Methodology); Franzen, Delwen (Conceptualization, Methodology);
                                Salholz-Hillel, Maia (Conceptualization, Methodology)'),
                       br(),
                       h4("Shiny app"),
                       helpText('Riedel, Nico (Conceptualization, Technical Implementation); Weissgerber, Tracey (Conceptualization);
                                Dirnagl, Ulrich (Conceptualization); Bobrov, Evgeny (Conceptualization); Strech, Daniel (Conceptualization);
                                Franzen, Delwen (Conceptualization); Salholz-Hillel, Maia (Conceptualization)'),
                       br(),
                       h3('Contact address'),
                       helpText('QUEST Center for Transforming Biomedical Research'),
                       helpText('Berlin Institute of Health at Charité – Universitätsmedizin Berlin'),
                       helpText('Anna-Louisa-Karsch-Str. 2'),
                       helpText('10178 Berlin, Germany'),
                       helpText('quest@bihealth.de'),
                       helpText(HTML('<a href="https://www.bihealth.org/quest-center/">
                                      https://www.bihealth.org/quest-center/ </a>'))
)
