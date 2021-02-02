methods_page <- tabPanel(
    "Methods", value = "tabMethods",
    h1("Methods"),
    
    h4(HTML('This dashboard displays a proof-of-principle dataset for responsible metrics at German University
    Medical Centers (UMCs). Please note that the data presented in this dashboard is still under development
    and should not be used &#8211 solely or in part &#8211 to compare UMCs or inform policy decisions.
    You can find more information on our methods for individual metrics by extending the panels below.')),
    
    h2("Publication search"),
    bsCollapse(id = "methodsPanels_PublicationSearch",
               bsCollapsePanel(strong("Publication Search"),
                               p(HTML("Many of the assessed metrics are publication-based metrics. To assess those
                               metrics on the institutional level, we first had to identify publications
                               that can be assigned to one of the UMCs in Germany. We searched
                               Web of Science for publications published in 2018 with at least one author
                               at each UMC. We used the organisation-enhanced index of the Web of Science
                               Core Collection to disambiguate author affiliations. As a proxy for the publication
                               output of UMCs, we identified biomedical publications at the aforementioned
                               institutions using a combination of biomedical journal-level subject categories
                               in Web of Science and article-level categories in Dimensions. The results were
                               filtered for the following document types: &#39Article&#39 and &#39Review&#39.
                               We included publications in all languages. Web of Science searches and extractions
                               were performed between 14/08/2020 and 22/09/2020. The Dimensions query was
                               performed on 22/09/2020.
                               <br>
                               <br>To evaluate the specificity of our approach, we performed a manual check of
                               a sample of publications per UMC. A
                               <a href=https://osf.io/a248e/>detailed protocol</a> of our specificity checks
                               is openly available in OSF. Briefly, 50 publications per UMC were manually checked
                               as to whether any author is affiliated to the medical faculty of the university of
                               question. Based on these results, we generated a proof-of-principle dataset with
                               publications from UMCs with a specificity equal to or higher than 85 (n=13 UMCs). For
                               each UMC, we selected a random sample of 500 articles. Reviews were exluded from
                               the development of this proof-of-principle dataset as most metrics are based on
                                      articles.")),
                               value = "methodsPanels_PublicationSearch",
                               style = "default")),
    
    
    h2("Open Science"),
    bsCollapse(id = "methodsPanels_OpenScience",
               
               
               methods_panel("Open Access",
                             
                             "A lot of valuable research, much of which is publicly funded, is hidden
                             behind paywalls. Open Access (OA) makes research articles available online,
                             free of charge and most copyright barriers. The free, public availability of
                             research articles accelerates and broadens the dissemination of research discoveries.
                             OA also enables greater visibility of research and makes it easier to build
                             on existing knowledge. Research funders are increasingly encouraging OA
                             to maximise the value and impact of research discoveries.",
                             
                             HTML('Using the obtained list of institutional publications, we queried the
                        Unpaywall database via its <a href="https://unpaywall.org/products/api">API</a>
                        to obtain information on the OA status of the publications. Unpaywall is today the
                        most comprehensive database of OA information on research articles. It can be queried
                        using publication DOIs. Publications can have different OA statuses which are
                        color-coded. Gold OA denotes a publication in an OA journal. Green OA denotes a
                        freely available repository version. Hybrid OA denotes an OA publication in a journal
                        which offers both a subscription based model as well as an OA option. Bronze OA denotes
                        a publication which is freely available on the journal page, but without a clear open
                        license. These can be articles in a non-OA journal which have been made available
                        voluntarily by the journal, but which might at some stage lose their OA status again.
                        Thus, we only consider the OA categories gold, green and hybrid for this dashboard.
                        As one publication can have several OA versions (e.g. a gold version in an OA journal
                        as well as a green version in a repository), we define a hierarchy for the OA categories
                        and for each publication only assign the OA category with the highest priority. We use
                        a hierarchy of gold - hybrid - green (journal version before repository version), as
                        also implemented in the Unpaywall database itself.
                        After querying the Unpaywall API for all publication DOIs, we group
                        the results by OA status.
                        <br>
                        <br>One important point that has to be considered with OA data is that
                        the OA percentage is not a fixed number, but changes over time. This is due to the fact
                        that repository versions are often made available with a delay, such that the OA
                        percentage for a given year typically rises retrospectively. Thus, the point in time
                        at which the OA status is retrieved is important for the OA percentage. The current
                        OA data was retrieved on: 26/01/2021.'),
                             
                             "Unpaywall only stores information for publications which have a DOI assigned by
                        Crossref. Articles without a Crossref DOI have to be excluded from the OA analysis."),
               
               
               methods_panel("Open Data and Open Code",
                             
                             HTML('The Open Data and Open Code metrics measure how many publications
                        share their raw research data or analysis code along with the publication.
                        Openly shared data and code makes research more transparent,
                        as research findings can be reproduced. Additionally, shared datasets
                        can be reused and combined by other scientists to answer new research
                        questions. The definition of Open Data used here is a low barrier definition.
                        Only a part of the raw data underlying a study has to be freely available
                        and no further quality criteria such as the FAIR criteria are checked -
                        for further details on the Open Data definition used see
                        <a href="https://www.bihealth.org/en/research/quest-center/mission-approaches/incentives/including-open-data/open-data-criteria/">
                        here</a>. Note also that data sharing is not possible for all studies, for example if
                        there is no dataset to be shared or if the data cannot be shared, e.g. due to privacy
                        concerns for patient data. Data sharing under restrictions is currently not considered,
                        but we are planning to do so in the future.'),
                             
                             HTML('To identify publications which share research data or analysis code,
                        we use the text-mining algorithm ODDPub
                        (Code: <a href="https://github.com/quest-bih/oddpub">
                        https://github.com/quest-bih/oddpub</a>,
                        publication: <a href="https://datascience.codata.org/article/10.5334/dsj-2020-042/">
                        https://datascience.codata.org/article/10.5334/dsj-2020-042/</a>),
                        developed by QUEST. ODDPub searches the publication full-text
                        for statements indicating sharing of raw data or analysis code.
                        It does however not check the shared data itself.
                        A text-mining approach is necessary, as a standardized
                        way of sharing and reporting Open Data does not yet exist, and no database offers
                        sufficiently comprehensive information on shared datasets or code.
                        To assess data and code sharing for UMC publications, we first downloaded the
                        full-texts of the publications that were accessible to us using the Unpaywall
                        and Crossref APIs. We screened those full-texts with ODDPub and calculated the
                        percentages of Open Data & Code relative to the publications in English and
                                  available as full text.'),
                             
                             "Several limitations apply:
                        Only full texts for Open Access publications or publications in journals to which
                        the Charite has a subscription could be retrieved (~78% of all detected publications).
                        ODDPub only finds ~75% of all Open Data publications and finds false positive cases
                        (no manual check of the results is done). ODDPub also does not verify that the
                        indicated dataset is indeed available and whether the dataset fulfills our definition
                        of Open Data. Open Data is not relevant for all publications, so we would not
                        expect 100% of the publications to contain Open Data, not even in an ideal case.
                        We considered all publications which had at least one author affiliated to one of the
                        included UMCs. Depending on an author's contribution to a project, he/she/they may have
                        differing influence on the decision whether to make data or code available alongside
                        a publication."),
               
               
               methods_panel("Potential Green Open Access (OA)",
                             
                             "This metric measures how many publications currently hidden behind a paywall
                             could be made openly accessible in a repository based on journal self-archiving
                             policies.In most cases, journal self-archiving policies allow researchers to
                             make the accepted version (and in some cases the published version) of their
                             publication openly accessible in a repository 6 to 12 months after publication.",
                             
                             HTML('In a first step, we filtered our dataset for publications which are currently
                             not OA (this includes bronze Open Access). Then, we queried the
                             <a href="https://shareyourpaper.org/permissions/about#api">
                             Shareyourpaper.org permissions API</a> (Open Access button) to identify
                                  publications which can be made openly accessible in an institutional
                                  or generalist repository based on article-level self-archiving permissions.'),
                             
                             "The method relies on the Shareyourpaper.org (Open Access Button) permissions
                             database being up to date. The date at which a publication can be made openly
                             accessible via self-archiving depends on the publication date and the length of
                             the embargo (if any). Therefore, the number of potential green OA research articles
                             will change over time. The Shareyourpaper permissions API was queried on
                             [enter date]")),
    
    hr(),
    h2("Clinical trials"),
    bsCollapse(id = "methodsPanels_ClinicalTrials",
               methods_panel("Reporting of Trial Registration Number (TRN)",
                             
                             HTML("Reporting of clinical trial registration numbers in related publications
                             facilitates transparent linkage between registration and publication and enhances
                             the value of the individual parts towards more responsible biomedical research
                             and evidence-based medicine. The <a 
                             href=https://www.sciencedirect.com/science/article/pii/S0140673607618352?via%3Dihub>
                             Consolidated Standards of Reporting Trials (CONSORT)</a>
                             as well as the <a href=http://www.icmje.org/recommendations/>ICMJE Recommendations
                             for the Conduct, Reporting, Editing, and Publication of Scholarly Work in Medical
                             Journals</a> call for reporting <i>&#39trial registration number and name of the
                             trial register&#39</i> in both the full-text and abstract."),
                             
                             HTML('We developed an <a href="https://github.com/maia-sh/ctregistries">open source R
                                  package</a> for the detection and classification of clinical trial registration
                                  numbers. Our regular-expression-based algorithm searches text strings for
                                  matches to TRN patterns for all PubMed-indexed and ICTRP-network registries.
                                  In a first step, we filtered the publication dataset for PubMed-classified
                                  human clinical trials. Then, we used the aforementioned package to detect
                                  and classify trial registration numbers in the PubMed secondary identifier
                                  metadata and abstract.'),
                             
                             HTML("We identified human clinical trials based on the following search term in PubMed:
                             <code>&#39clinical trial&#39[pt] NOT (animals [mh] NOT humans [mh])</code>. However,
                             we have not tested (1) the sensitivity of this PubMed search term (i.e., what
                             proportion of true clinical trial publications are detected?); (2) the specificity
                             of this search term (i.e, what proportion of detected publications are not true
                             clinical trials publications?). Furthermore, our algorithm does not
                                  distinguish true TRNs that do not resolve to a registration. Finally, the
                                  algorithm does not determine whether the TRN is reported as a registration
                                  for the publication&#39s study (i.e., clinical trial result) or is otherwise
                                  mentioned (i.e., in a review, reference to other clinical trials, etc.)")),
               
               methods_panel("Summary results reporting",
                             
                             "This metric measures how many clinical trials registered in the
                        EU Clinical Trials Register that are due to report their results have already
                        done so. A trial is due to report its results 12 month after trial completion.
                        Clinical trials are expensive and have often many contributing patients.
                        A fast dissemination of the trial results is crucial to make the evidence gained
                        in those trials available. The World Health organization recommends publishing
                        clinical trial results within one year after the end of a study.",
                             
                             HTML('The data were retrieved for all UMCs included in this proof-of-principle
                             dataset from the
                        <a href="eu.trialstracker.net">EU Trials Tracker</a> by the EBM DataLab.'),
                             
                             "While the EU Clinical Trials Register is one of the most important
                        European trial registries, it is not the only available registry. There are other
                        registries such as ClinicalTrials.gov. or the German Clinical Trials Registry,
                        which are not considered here. Additionally, the EU Trials Tracker does not
                        measure for how long the trials have been due. Finally, we only considered the
                             latest data available in the EU Trials Tracker. We plan to include historic
                             data in the future."),
               
               methods_panel("Timely publication of results",
                             
                             "This metric measures how many clinical trials registered on ClinicalTrials.gov
                        reported their results either as a journal publication or as summary
                        results on the trials registry within 2 or 5 years after completion. Trials
                        completed between 2009 and 2013 were considered.
                        A fast dissemination of the trial results is crucial to make the evidence gained
                        in those trials available. The World Health organization recommends publishing
                        clinical trial results within one year after the end of a study.",
                             
                             HTML('The registry ClinicalTrials.gov was searched for studies with one of the UMCs
                             as the responsible party/sponsor or with a principle investigator from one of the
                             UMCs. A manual search for published results was done, searching the
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
                             
                             "We used the same methods as for the timely reporting metric to identify trials
                             from UMCs. To assess if a study has been prospectively registered, we compare
                        the date the study was first submitted to the registry with the
                        start date given in the registry. As some of the earlier dates in the database
                        only stated the month but not the exact day and to account for other possible delays
                        we chose a conservative estimate of prospective registration and allow for a delay
                        between start and registration date of up to 60 days.",
                             
                             "Like in the case of the summary results metric, we only focused on the
                        ClinicalTrials.gov while there are other available registries as well.
                        Also, we rely on the information on ClinicalTrials.gov being accurate.")),
    
    hr(),
    h2("Robustness"),
    bsCollapse(id = "methodsPanels_Robustness",
               methods_panel("Robustness of animal studies",
                             
                             HTML("The robustness metrics assess whether animal studies in our publication set
                        adhere to a core set of reporting standards for animal studies as described by
                        <a href=https://www.nature.com/articles/nature11556>Landis et al. (2012)</a>.
                        Specifically, we focus on the following research parameters: reporting of blinding,
                        randomization, and sample size estimation. We also display whether an Institutional
                                  Animal Care and Use Committee (IACUC) statement is reported."),
                             
                             HTML('In a first step, we filtered the publication dataset for animal studies based
                        on a previously published <a href=https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3104815/
                        >PubMed search query</a>. We focused on animal studies as these The parameters in question were extracted with
                        <a href="https://www.sciencedirect.com/science/article/pii/S2589004220308907">SciScore</a>,
                        an automated tool which evaluates research articles based on their adherence to rigour
                        and reproducibility criteria.'),
                             
                             HTML("There are several limitations: We identified animal studies based on a
                        previously published PubMed search filter which has been shown to retrieve more records
                        than the regular search method in PubMed (<i>Limit: Animals</i>). However, we did not
                        test its sensitivity and specificity in the context of this proof-of-principle dataset.
                        Moreover, this PubMed search filter does not distinguish between publications in which
                        animals are mentioned, and publications in which animals are the main research subject.
                        It also does not identify animal studies in our publication set not indexed in PubMed.
                        Finally, it is important to note that randomization, blinding, and sample size
                        estimation in most cases do not apply to early-stage exploratory research
                        (hypothesis-generating experiments). At this stage, we do not have a way of distinguishing
                                  these studies from confirmatory, hypothesis-testing experiments.")))
)


## Tooltips for Open Science metrics

openaccess_tooltip <- strwrap("This metric ...")

opendata_tooltip <- strwrap("This metric ...")

opencode_tooltip <- strwrap("This metric ...")

## Tooltips for Clinical Trials metrics

trn_tooltip <- strwrap("This metric ...")

## Tooltips for Robustness metrics

randomization_tooltip <- strwrap("This metric shows ...")

blinding_tooltip <- strwrap("This other metric shows ...")

power_tooltip <- strwrap("This other metric shows ...")

iacuc_tooltip <- strwrap("This other metric shows ...")
