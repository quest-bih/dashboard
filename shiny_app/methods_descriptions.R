methods_panel <- tabPanel("Detailed Methods",  value = "tabMethods",


         h1("Detailed Methods"),

         h4(HTML('You can extend the panels to view the detailed methods for
              the individual metrics. The code for the actual implementation
              of the methods can be found on
                 <a href="https://github.com/quest-bih/dashboard">Github</a>.')),


         h2("Publication search"),
         bsCollapse(id = "methodsPanels_PublicationSearch",
                    bsCollapsePanel("Publication Search",
                                    p("Many of the assessed metrics are publication-based metrics.
                        To assess those metrics on the institutional level, we first need to
                        identify the publications that can be assigned to the Charité.
                        The Charité Medical Library provided this list of publications, which
                        was created by merging, deduplicating and improving data resulting from
                        searches in two separate publication databases. Web of Science and Embase
                        were both searched for biomedical publications with at least one author
                        affiliated to the Charité or the Berlin Institute of Health."),
                                    value = "methodsPanels_PublicationSearch",
                                    style = "default")),


         h2("Open Science"),
         bsCollapse(id = "methodsPanels_OpenScience",
                    methods_panel("Open Access",

                                  "The open access metric measures the degree of openness
                        of the publications by Charité researchers. Open access publications
                        are available to everyone worldwide for free, helping to distribute
                        research results quickly and transparently.",

                                  HTML('The Charité Medical Library first created a list
                        of journal article publications by Charité researchers, then queried
                        the Unpaywall database via its API to obtain information on the
                        Open Access (OA) status of those publications. Unpaywall is today
                        the most comprehensive database of open access information on
                        research articles. It has been queried using Digital Object
                        Identifiers (DOIs) for each of the publications. There are different
                        OA statuses a publication can have, which are color-coded. Gold OA
                        denotes publication in a pure OA journal. Green OA denotes a freely
                        available repository version. Hybrid OA denotes an OA publication
                        in a paywalled journal where the author(s) have opted to pay for
                        their article to be open access. Bronze OA denotes a publication
                        which is freely available on the publisher website, but without a
                        clear open license enabling re-use: this includes articles in a
                        paywalled journal which have been made free to read but access
                        might be withdrawn at any time. Thus we only consider the categories
                        gold, green and hybrid to be true open access here. As one
                        publication can have several OA versions (e.g. a gold version in an
                        OA journal as well as a green version in a repository), a hierarchy
                        is used so that each publication is assigned the OA category with the
                        highest level of openness. The standard hierarchy used here is
                        gold - hybrid - green (journal version before repository version,
                        excepting bronze). We group the results from Unpaywall by OA status
                        and publication year. One important point for OA status is that it
                        may change over time: the OA percentage is not a fixed number.
                        Repository versions (green OA) are often made available after a
                        delay, such that the OA percentage for a given year typically rises
                        retrospectively. Thus the point in time of data retrieval is
                        important for understanding the OA percentage. The current OA status
                        data were retrieved in September 2022.'),

                                  "Unpaywall only stores information for publications that
                        have a DOI assigned by Crossref. Articles without a Crossref DOI
                        have to be excluded from the OA analysis. However, in the most
                        recent years DOIs are missing for only a tiny minority (<1%) of
                        the publications."),


                    methods_panel("Open Data and Open Code",

                                  HTML('The Open Data and Open Code metrics measure how many publications
                        share their raw research data or analysis code with the publication.
                        Openly shared data and code makes research more transparent,
                        as research findings can be reproduced. Additionally, shared datasets
                        can be reused and combined by other scientists to answer new research
                        questions. The definition of Open Data used here is a low barrier definition.
                        Only a part of the raw data underlying a study have to be freely available
                        and no further quality criteria, like the FAIR criteria, are checked -
                        for further details on the Open Data definition used see
                        <a href="https://www.bihealth.org/en/research/quest-center/mission-approaches/incentives/including-open-data/open-data-criteria/">
                        here</a>. This definition is also used for the distribution of performance-oriented
                        funding for publications with Open Data at the Charité.
                        Here, we distinguish several subcategories for Open Data & Open Code.
                        For Open Data we distinguish 1) disciplinary repositories, that are typically
                        made for a specific type of dataset with a standardized format and which is used in
                        a specific community, and 2) general-purpose repositories or
                        other websites, that can contain various types of datasets.
                        For Open Code we distinguish code shared 1) via GitHub, which is the most common
                        way of code sharing, and 2) via other websites or repositories.
                        Note also that data sharing is not possible for all
                        studies, as there is either no dataset to share or as the data
                        cannot be shared, e.g. due to privacy concerns for patient data.
                        Data sharing under restrictions is only available from the year 2020 onwards.'),

                                  HTML('To identify publications that share research data or analysis code,
                        we use the text-mining algorithm ODDPub
                        (Code: <a href="https://github.com/quest-bih/oddpub">
                        https://github.com/quest-bih/oddpub</a>,
                        publication: <a href="https://doi.org/10.5334/dsj-2020-042">
                        https://doi.org/10.5334/dsj-2020-042</a>),
                        which was developed by QUEST. ODDPub searches the publication full-text
                        for statements indicating sharing of raw data or analysis code.
                        It does however not check the shared data itself.
                        A text-mining approach is necessary, as there is not yet a standardized
                        way of sharing and reporting Open Data, and no database offers
                        comprehensive information on shared datasets or code.
                        To assess data and code sharing for the Charité publications,
                        we first downloaded the full-texts of the publications that were
                        accessible to us using the unpaywall and crossref APIs.
                        Then we screened those full-texts with ODDPub and
                        summarized the results for each publication year.
                        Finally, we performed a manual check of the publications detected
                        by ODDPub as potentially open data or open code, in order to exclude
                        false positive detections. We calculated the percentages of
                        Open Data & Code relative to the publications with available full texts,
                        which could indeed be screened.'),

                                  "Several limitations apply:
                        Only full texts for open access publications or publications in journals
                        that are subscribed by the Charité could be retrieved
                        (~85% of all detected publications).
                        Open Data is not relevant for all publications, so we would not
                        expect 100% of the publications to contain Open Data, even not in an ideal case.
                        We considered all publications that had at least one author affiliated
                        with the Charité – in some cases where those were only middle authors
                        with minor contributions to the project, they might have little impact
                        on the decision if data were made available."),

                    # ODDPub only finds ~75% of all Open Data
                    # publications and finds false positive cases (no manual check of the results
                    # is done).
                    # ODDPub also does not verify that the indicated dataset
                    # is indeed available and if the dataset fulfills our definition
                    # of Open Data.

                    methods_panel("Preprints",

                                  "This metric measures how many preprints with authors from the Charité
                        have been published on a range of preprint servers. Preprints allow
                        rapid and transparent communication of preliminary research results
                        before publication in a peer-reviewed journal.",

                                  HTML('To identify preprints published by Charité authors we used the
                        <a href="www.dimensions.ai">dimensions</a> database,
                        which indexes many preprint servers, including arXiv and bioRxiv.
                        We searched for articles of the type \'preprint\' and
                        with authors assigned to the Charité via its Grid ID. The number of preprints
                        found by this search are then summarized by year.
                        The current preprint data were retrieved on: 30.09.2022.'),

                                  "Not all relevant preprint servers are currently indexed by dimensions,
                        including some of the bigger preprint servers like OSF preprints or medRxiv.
                        Thus we likely underestimate the number of published preprints.")),
         hr(),
         h2("Clinical trials"),
         bsCollapse(id = "methodsPanels_ClinicalTrials",
                    methods_panel("Summary results reporting",

                                  "This metric measures how many clinical trials registered in the
                        EU Clinical Trials Register that are due to report their results have already
                        done so. A trial is due to report its results 12 month after trial completion.
                        Clinical trials are expensive and have often many contributing patients.
                        A fast dissemination of the trial results is crucial to make the evidence gained
                        in those trials available. The World Health organization recommends publishing
                        clinical trial results within one year after the end of a study.",

                                  HTML('The data were retrieved from the
                        <a href="eu.trialstracker.net">EU Trials Tracker</a> by the EBM DataLab.'),

                                  "While the EU Clinical Trials Register is one of the most important
                        European trial registries, it is not the only
                        available registry. There are other registries like ClinicalTrials.gov.
                        or the German Clinical Trials Registry, which are not considered here.
                        Additionally, the EU Trials Tracker does not measure for how long the
                        trials have been due."),

                    methods_panel("Timely publication of results",

                                  "This metric measures how many clinical trials registered on ClinicalTrials.gov
                        or on the German Clinical Trials Register (DRKS)
                        reported their results either as a journal publication or as summary
                        results on the trials registry within 2 or 5 years after completion. Trials
                        completed between 2009 and 2013 were considered.
                        A fast dissemination of the trial results is crucial to make the evidence gained
                        in those trials available. The World Health organization recommends publishing
                        clinical trial results within one year after the end of a study.",

                                  HTML('The registries ClinicalTrials.gov and DRKS.de were searched for studies with Charité
                        as the responsible party/sponsor or with a principle investigator from
                        Charité. A manual search for published results was done, searching the
                        registry, PubMed and Google. When calculating the time to publication, we only
                        considered trials where we could track the full timeframe since completion.
                        As not all trials could be tracked for 5 years since completion at
                        the time when this study was carried out, we have less trials where we can
                        report the publications 5 years after completion. The results were previously
                        published as part of the <a href="https://s-quest.bihealth.org/intovalue/">IntoValue study</a>.
                        Detailed methods can be found under
                        <a href="https://doi.org/10.1101/467746">https://doi.org/10.1101/467746</a>.'),
                                  "Some detected publications might be missed in the manual search
                        procedure as we only searched a limited number of scientific databases and did not
                        contact the responsible parties. Furthermore, we did not include observational clinical
                        studies in our sample. Additionally, we might overestimate the time to publication
                        for some studies as we stopped the manual search after the first detected publication."),



                    methods_panel("Prospective registration",

                                  "This metric measures if the clinical trials are registered before the
                        start date of the study, according to the information given on ClinicalTrials.gov.
                        The idea of prospective registration of studies is to make the trail specifications,
                        including primary and secondary outcomes, publicly available before study start.
                        Prospective registration adds transparency, helps protect against outcome switching.",

                                  "We used the same methods as for the timely reporting metric to identify Charité
                        trials. To assess if a study has been prospectively registered, we compare
                        the date the study was first submitted to the registry with the
                        start date given in the registry. As some of the earlier dates in the database
                        only stated the month but not the exact day and to account for other possible delays
                        we chose a conservative estimate of prospective registration and counted
                        a registration as prospective registration if the study was registered
                        in the month of the study start or earlier.",

                                  "Like in the case of the summary results metric, we only focused on the
                        ClinicalTrials.gov while there are other available registries as well.
                        Also, we rely on the information on ClinicalTrials.gov being accurate.")),
         hr(),
         h2("Persistent Identifiers"),

         bsCollapse(id = "methodsPanels_persistent_ids",

         methods_panel("ORCID",

                       "This metric measures how many researchers currently affiliated with the
                        Charité have an ORCID. The ORCID makes each researcher uniquely identifiable
                        despite name variants or name changes and uniquely associates
                        publications or other types of research output with them.
                        Many publishers now request ORCIDs when manuscripts are submitted.",

                       HTML('To identify Charité researchers with ORCIDs, we query the
                        <a href="https://members.orcid.org/api">ORCID API</a> using the following
                        search query: "current-institution-affiliation-name:
                        (Charité OR Charite OR (Universitätsmedizin AND Berlin)
                        OR (Berlin AND Institute AND of AND Health))". That way, we only identify
                        researchers that have listed the Charité as their current institution,
                        but not researchers that have listed it as a past institution.'),

                       "The method relies on the information entered by the ORCID
                        users being up to date. Some users might not enter correct or up to
                        date information or keeping their account information private,
                        which could then not be found. Also no historic data are available
                        for this metric.")),


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

                                  "To detect graph types from publications we used the Barzooka algorithm
                        (unpublished), a deep neural network that was trained to detect graph types on
                        publication pages and that was developed by QUEST. We again only used the publications
                        for which we could retrieve the full texts (see methods for Open Data & Code).
                        Barzooka screens each page of the available publication PDFs
                        and reports if one or several graph types were detected. The results were then
                        combined for each publication. For each publication we then report if a certain is
                        present (at least once) or not present. The number of publications with each
                        graph types are then summarized for each publication year. We again reported
                        the percentages relative to all publications that could indeed be screened.",

                                  "Barzooka detects the graph types with different accuracies and in particular
                        for the categories of bar graphs for continuous data and appropriate bar graphs
                        (for count data) there can be erroneous assignments.")),

         hr(),
         h2("Data Reusability"),

         bsCollapse(id = "methodsPanels_FAIR",


                    methods_panel("FAIR data",

                                  HTML("In order for a research data object to be reused, it — and the hosting data repository —
                                  must meet certain quality criteria that ensure it is <a href = 'https://www.go-fair.org/fair-principles/'>Findable, Accessible, Interoperable, and Reusable (FAIR)</a>.
                                  For this purpose the data must be stored together with detailed metadata that must conform to a defined standard.
                                  In addition, datasets are easier to reuse if the corresponding metadata is machine-readable and uses a standardized vocabulary.
                                  "),

                                  HTML("Based on the results of the text-mining algorithm <a href = 'https://doi.org/10.5334/dsj-2020-042'>ODDPub</a>
                                   — which identifies publications mentioning the generation and sharing of research data objects —
                                  we manually screened the detected data statements and extracted one research dataset ID per mentioned repository.
                                  We then used the extracted dataset IDs to query the automated screening tool <a href='https://www.f-uji.net'>F-UJI</a>.
                                  F-UJI assesses the FAIRness of research data objects based on <a href = 'https://zenodo.org/record/4081213#.YhdU_C8w1pQ'>metrics</a>
                                  developed by the <a href = 'https://www.fairsfair.eu'>FAIRsFAIR</a> project.
                                       The results were then aggregated and enriched with data from <a href = 'https://www.re3data.org'>re3data</a> to include further information about the data repositories in the analysis."),

                                  "F-UJI does not provide suitable automatic tests for all FAIR principles. FAIR principles A1.1 and A1.2 are not assessed. The quality of metadata and compliance with FAIR principles depends mainly
                                  on the repository providers and can therefore only be influenced by the creators of the datasets to a limited extent.
                                  Furthermore, F-UJI analyzes only the metadata of a research data object. The reusability and quality of the research data itself is not assessed")),
         hr(),
         h2("Berlin Science Survey (BSS)"),
         bsCollapse(id = "methodsPanels_BSS",
                    bsCollapsePanel("Charité Subsample of Berlin Science Survey",
                                    style = "default",
                                    value = "methodsPanels_BSS",
                                    HTML('<p><a href = "https://www.berlinsciencesurvey.de/en/documentation">Documentation of the survey questionnaires and a methodology report</a> has been published by the Berlin Science Survey.</p>
                                         <p>From all survey responses of the Berlin Science Survey, a subsample was generated with responses of those survey participants who answered Charité to the question "At which Berlin research institution are you currently mainly employed?".</p>
                                         <p>The data were imported in Stata file format (*.dta) and filtered and aggregated for each analysis.
                                         For the combined chart in the first analysis "Prioritizing scientific goals in the field of tension between external expectations and self-ascribed importance" the mean values of the four-item scale responses were calculated. The values for the three questions were then plotted on one axis.
                                         For the third and fourth analyses, "Research environment" and "Importance of the expansion of open science", the answer options "I cannot judge" and "don\'t know", respectively, were removed.</p>')
                                    )),
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


#------------------------------------------------------------------------
# Tooltip info descriptions for all metrics
#------------------------------------------------------------------------


open_access_tooltip <- strwrap("The open access metric shows the percentage of Charité original
                             research publications that are published as open access (OA) articles.
                             Gold OA denotes publication in a pure OA journal.
                             Green OA denotes a freely available repository version. Hybrid OA denotes
                             an OA publication in a journal with offers both a subscription based model
                             as well as an open access option. Bronze OA denotes a publication which is
                             freely available on the journal page, but without a clear open license.
                             Closed articles are not freely available. For some articles no open access
                             information was available.
                             - Click for methods details.") %>%
  paste(collapse = " ")

open_data_tooltip <- strwrap("The Open Data metric measures the the percentage of screened publications
                             that state that they shared their research data.
                             Openly shared data makes research more transparent,
                             as research findings can be reproduced. Additionally, shared datasets
                             can be reused and combined by other scientists to answer new research
                             questions.
                             - Click for methods details.") %>%
  paste(collapse = " ")

open_code_tooltip <- strwrap("The Open Code metric measures the the percentage of screened publications
                             that state that they shared their analysis code.
                             Like openly shared data, Open Code makes research more transparent,
                             as research findings can be reproduced.
                             - Click for methods details.") %>%
  paste(collapse = " ")

preprints_tooltip <- strwrap("This metric measures how many preprints with authors from the Charité
                             have been published on a range of preprint servers. Preprints allow
                             rapid and transparent communication of preliminary research results
                             before publication in a peer-reviewed journal.
                             - Click for methods details.") %>%
  paste(collapse = " ")

orcid_tooltip <- strwrap("This metric measures how many researchers currently affiliated with the
                          Charité have an ORCID. The ORCID makes each researcher uniquely identifiable
                          despite name variants or name changes and uniquely associates
                          publications or other types of research output with him/her.
                          Many publishers now request ORCIDs when manuscripts are submitted.
                          - Click for methods details.") %>%
  paste(collapse = " ")


summary_results_tooltip <- strwrap("This metric measures how many clinical trials registered in the
                        EU Clinical Trials Register that are due to report their results have already
                        done so. A trial is due to report its results 12 month after trial completion.
                        The data were retrieved from the EU Trials Tracker by the EBM DataLab
                        (eu.trialstracker.net).
                        Clinical trials are expensive and have often many contributing patients.
                        A fast dissemination of the trial results is crucial to make the evidence gained
                        in those trials available. The World Health organization recommends publishing
                        clinical trial results within one year after the end of a study.
                                       - Click for methods details.") %>%
  paste(collapse = " ")

intovalue_tooltip <- strwrap("This metric measures how many clinical trials registered on CT.gov or
                        on the German Clinical Trials Register (DRKS)
                        reported their results either as a journal publication or as summary
                        results on the trials registry within 2 or 5 years after completion. Trials
                        completed between 2009 and 2013 were considered. The results were previously
                        published as part of the IntoValue study (https://s-quest.bihealth.org/intovalue/).
                        As not all trials could be followed up for at least 5 years at the time the studies
                        were conducted, not all years have numbers for results publication within 5 years.
                        Clinical trials are expensive and have often many contributing patients.
                        A fast dissemination of the trial results is crucial to make the evidence gained
                        in those trials available. The World Health organization recommends publishing
                        clinical trial results within one year after the end of a study.
                                       - Click for methods details.") %>%
  paste(collapse = " ")

prospective_registration_tooltip <- strwrap("This metric measures if the clinical trials are registered before the
                        start date of the study, according to the information given on ClinicalTrials.gov.
                        The idea of prospective registration of studies is to make the trail specifications,
                        including primary and secondary outcomes, publicly available before study start.
                        Prospective registration adds transparency, helps protect against outcome switching.
                                       - Click for methods details.") %>%
  paste(collapse = " ")



vis_problem_tooltip <- strwrap("Bar graphs for continuous data are common but are considered a suboptimal practice,
                                as they conceal the underlying data points
                                and many different data distributions can lead to the same bar graph.
                                Bar graphs of continuous data should be replaced with more informative graphs
                                that provide information about the data distribution and sample size,
                                such as dot plots, box plots, violin plots or histograms. Pie charts are suboptimal,
                                as humans have difficulty assessing proportions based on differences in area and angle.
                                This is especially difficult when a pie chart includes many slices, or groups with similar proportions.
                                - Click for more info.") %>%
  paste(collapse = " ")


vis_inform_tooltip <- strwrap("Bar graphs for continuous data are common but are considered a suboptimal practice,
                                as they conceal the underlying data points
                                and many different data distributions can lead to the same bar graph.
                                Bar graphs of continuous data should be replaced with more informative graphs
                                that provide information about the data distribution and sample size,
                                such as dot plots, box plots, violin plots or histograms.
                                - Click for more info.") %>%
  paste(collapse = " ")

fair_fuji_tooltip <- strwrap("F-UJI uses 16 metrics to assess 12 (out of 15) FAIR principles.
As FAIR principles are abstract and generically defined F-UJI specifies one or more metrics for each assessed FAIR principle.
Since each metric can be tested in various means depending on data contexts and current best practises F-UJI additionally uses one or more practical tests to evaluate datasets against a particular metric.
                             - Click for more info.") %>%
  paste(collapse = " ")

fair_repositories_tooltip <- strwrap("This metric measures the average FAIR score (in percent) by data repositories
                                for research data published in 2021.
                                Data repositories have a major impact on the FAIR score of research data because
                                they provide the infrastructure that ensures Findability, Accessibility, Interoperability,
                                and Reusability of digital assets.
                                The FAIR score is based on practical tests executed by an automated FAIR data assessment tool
                                against specific metrics derived from the FAIR principles.
                                - Click for more info.") %>%
  paste(collapse = " ")

fair_principles_tooltip <- strwrap("This metric measures the average FAIR score (in percent)
                                by the four main principles Findability, Accessibility, Interoperability,
                                and Reusability for research data published in 2021.
                                The FAIR score is based on practical tests executed by the automated FAIR data assessment tool
                                F-UJI against specific metrics derived from the FAIR principles.
                                - Click for more info.") %>%
  paste(collapse = " ")

fair_licenses_tooltip <-
  strwrap(
    "This metric measures if a standard, machine readable license is specified under which research data published in 2021 can be reused. It is derived from the FAIR principle R1.1 — (Meta)data are released with a clear and accessible data usage license. - Click for more info."
  ) %>%
  paste(collapse = " ")

fair_identifiers_tooltip <- strwrap("This metric measures the average FAIR score of the 2021 datasets by the unique identifiers. It is derived from the FAIR principle F1 — (Meta)data are assigned a globally unique and persistent identifier.
                                - Click for more info.") %>%
  paste(collapse = " ")
