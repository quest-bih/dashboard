methods_panel <- tabPanel("Detailed Methods",  value = "tabMethods",


         h1("Detailed Methods"),

         h4(HTML('You can extend the panels to view the detailed methods for
              the individual metrics. The code for the actual implementation
              of the methods can be found on
                 <a href="https://github.com/quest-bih/dashboard">Github</a>')),


         h2("Publication search"),
         bsCollapse(id = "methodsPanels_PublicationSearch",
                    bsCollapsePanel(strong("Publication Search"),
                                    p("Many of the assessed metrics are publication-based metrics.
                        To assess those metrics on the institutional level, we first need to
                        identify the publications that can be assigned to the Charité.
                        For this, we use a list of publications kindly provided by the Charité Medical Library.
                        They obtained this publication list by searching the publication databases
                        Web of Science and Embase for biomedical publications with at least one of the
                        authors affiliated to the Charité or the Berlin Institute of Health.
                        Subsequently, the search results from both databases were merged and deduplicated."),
                                    value = "methodsPanels_PublicationSearch",
                                    style = "default")),


         h2("Open Science"),
         bsCollapse(id = "methodsPanels_OpenScience",


                    methods_panel("Open Access",

                                  "The Open Access metric measures the degree of openness of the
                        publications published by the Charité researchers. Open Access
                        publications are freely and (usually) immediately available
                        to all other researchers in the world and thus help to distribute
                        research results transparently, openly and fast.",

                                  HTML('Using the list of institutional publications, the
                        Charité Medical Library queried the unpaywall database via its
                        <a href="https://unpaywall.org/products/api">API</a>
                        to obtain information on the Open Access (OA) status of the publications.
                        Unpaywall is today the most comprehensive database of Open Access
                        information on research articles. It can be queried using publication
                        DOIs. There are different Open Access statuses a publication can have,
                        which are color-coded. Gold OA denotes publication in a pure OA journal.
                        Green OA denotes a freely available repository version. Hybrid OA denotes
                        an OA publication in a journal with offers both a subscription based model
                        as well as an Open Access option. Bronze OA denotes a publication which is
                        freely available on the journal page, but without a clear open license.
                        Those can be articles in a non-OA journal which have been made available
                        voluntarily by the journal but which might lose its OA status again.
                        Thus we only consider the OA categories gold, green and hybrid here.
                        As one publication can have several OA versions (e.g. a gold version
                        in an OA journal as well as a green version in a repository), Unpaywall defines
                        a hierarchy of the OA categories and for each publication only assigns
                        the OA category with the highest priority. The standard hierarchy used here is
                        gold - hybrid - green (journal version before repository version).
                        We group the results from unpaywall by OA status and publication year.
                        One important point that has to be considered with OA data is, that
                        the OA percentage is not a fixed number but is changing over time.
                        This comes from the repository versions that are often made available
                        with a delay, such that the OA percentage for a given year is typically
                        rising retrospectively. Thus the point in time of data retrieval is important
                        for the OA percentage. The current OA data were retrieved on: 16.09.2021.'),

                                  "Unpaywall only stores information for publications that have
                        a DOI assigned by crossref. Articles without crossref DOI have to
                        be excluded from the OA analysis. However, in the most recent years
                        this is the case for a tiny minority (<1%) of the publications."),


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
                        For Open Data we distinguish 1) field-specific repositories, that are typically
                        made for a specific type of dataset with a standardized format and which is used in
                        a specific community, 2) general-purpose repositories or
                        other websites, that can contain various types of datasets, and 3) supplemental data.
                        For Open Code we distinguish code shared 1) via GitHub, which is the most common
                        way of code sharing, 2) via other websites or repositories, or 3) via the supplement.
                        Note also that data sharing is not possible for all
                        studies, as there is either no dataset to share or as the data
                        cannot be shared, e.g. due to privacy concerns for patient data.
                        Data sharing under restrictions is currently not considered,
                        but we are planning to do so in the future.'),

                                  HTML('To identify publications that share research data or analysis code,
                        we use the text-mining algorithm ODDPub
                        (Code: <a href="https://github.com/quest-bih/oddpub">
                        https://github.com/quest-bih/oddpub</a>,
                        preprint: <a href="https://doi.org/10.1101/2020.05.11.088021">
                        https://doi.org/10.1101/2020.05.11.088021</a>),
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
                        We calculated the percentages of Open Data & Code relative to the publications
                        with available full texts, which could indeed be screened.'),

                                  "Several limitations apply:
                        Only full texts for Open Access publications or publications in journals
                        that are subscribed by the Charité could be retrieved
                        (~85% of all detected publications). ODDPub only finds ~75% of all Open Data
                        publications and finds false positive cases (no manual check of the results
                        is done). ODDPub also does not verify that the indicated dataset
                        is indeed available and if the dataset fulfills our definition
                        of Open Data. Open Data is not relevant for all publications, so we would not
                        expect 100% of the publications to contain Open Data, even not in an ideal case.
                        We considered all publications that had at least one author affiliated
                        with the Charité – in some cases where those were only middle authors
                        with minor contributions to the project, they might have little impact
                        on the decision if data were made available."),


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
                        The current preprint data were retrieved on: 14.09.2020.'),

                                  "Not all relevant preprint servers are currently indexed by dimensions,
                        including some of the bigger preprint servers like OSF preprints or medRxiv.
                        Thus we likely underestimate the number of published preprints."),

                    methods_panel("ORCID",

                                  "This metric measures how many researchers currently affiliated with the
                        Charité have an ORCID. The ORCID makes each researcher uniquely identifiable
                        despite name variants or name changes and uniquely associates
                        publications or other types of research output with him/her.
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
                        (for count data) there can be erroneous assignments."))
)


#------------------------------------------------------------------------
# Tooltip info descriptions for all metrics
#------------------------------------------------------------------------


open_access_tooltip <- strwrap("The Open Access metric shows the percentage of Charité original
                             research publications that are published as Open Access (OA) articles.
                             Gold OA denotes publication in a pure OA journal.
                             Green OA denotes a freely available repository version. Hybrid OA denotes
                             an OA publication in a journal with offers both a subscription based model
                             as well as an Open Access option. Bronze OA denotes a publication which is
                             freely available on the journal page, but without a clear open license.
                             Closed articles are not freely available. For some articles no Open Access
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
