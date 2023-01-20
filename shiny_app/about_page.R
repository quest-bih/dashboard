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
                       helpText(HTML('Nico Riedel (Conceptualization, Methodology, Technical Implementation, Validation);
                                Evgeny Bobrov (Conceptualization, Methodology, Validation); Miriam Kip (Conceptualization, Methodology); Vladislav Nachev (Methodology, Technical Impelementation)')),
                       br(),
                       h4("Barzooka - Visualization type detection"),
                       helpText(HTML('Nico Riedel (Conceptualization, Methodology, Technical Implementation); Tracey Weissgerber (Conceptualization, Methodology); Robert Schulz (Validation); Vladislav Nachev (Technical Impelementation)')),
                       br(),
                       h4("Clinical trial metrics"),
                       helpText(HTML('Nico Riedel (Conceptualization, Methodology, Technical Implementation);
                                     Daniel Strech (Conceptualization, Methodology); Susanne Wieschowski (Conceptualization, Methodology);
                                     Peter Grabitz (Conceptualization, Methodology); Delwen Franzen (Conceptualization, Methodology);
                                     Maia Salholz-Hillel (Conceptualization, Methodology); Benjamin Gregory Carlisle (Conceptualization, Methodology); Vladislav Nachev (Methodology, Technical Impelementation)')),
                       br(),
                       h4("Data Reusability - FAIR data"),
                       helpText(HTML('Jan Taubitz (Conceptualization, Methodology, Technical Implementation);
                                Evgeny Bobrov (Conceptualization, Methodology, Validation), Anastasiia Iarkaeva (Validation)')),
                       br(),
                       h4("Berlin Science Survey (BSS)"),
                       helpText(HTML('The <a href="https://www.berlinsciencesurvey.de/en/index.html">Berlin Science Survey (BSS)</a> was conducted and the data of the Charité subsample were provided by the <a href="https://www.rmz.hu-berlin.de/en">Robert K. Merton Center for Science Studies</a>.
                                     The analysis and visualization of the Charité subsample was carried out by Jan Taubitz (Conceptualization, Methodology, Technical Implementation) and
                                     Evgeny Bobrov (Conceptualization, Methodology).')),
                       br(),
                       h4("Shiny app"),
                       helpText(HTML('Nico Riedel (Conceptualization, Technical Implementation); Tracey Weissgerber (Conceptualization);
                                Ulrich Dirnagl (Conceptualization); Evgeny Bobrov (Conceptualization); Daniel Strech (Conceptualization);
                                Delwen Franzen (Conceptualization); Maia Salholz-Hillel (Conceptualization);
                                     Jan Taubitz (Conceptualization, Technical Implementation); Vladislav Nachev (Methodology, Technical Impelementation)')),
                       br(),
                       h3('Acknowledgment'),
                       helpText('We gratefully acknowledge funding by the Wellcome Trust (Charite-BIH Translational Partnership) and the Berlin University Alliance (Objective 3: Advancing Research Quality and Value).'),
                       br(),
                       h3('Contact address'),
                       helpText('QUEST Center for Responsible Research '),
                       helpText('Berlin Institute of Health at Charité – Universitätsmedizin Berlin'),
                       helpText('Anna-Louisa-Karsch-Str. 2'),
                       helpText('10178 Berlin, Germany'),
                       helpText('quest@bih-charite.de'),
                       helpText(HTML('<a href="https://www.bihealth.org/de/translation/innovationstreiber/quest-center">
                                      https://www.bihealth.org/de/translation/innovationstreiber/quest-center</a>')),
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
