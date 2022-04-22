# Charité Dashboard on Responsible Research

The Charité Dashboard on Responsible Research provides an up-to-date overview of the status quo (as well as the progress) of various metrics of trustworthy and useful research at Charité and BIH. Examples of metrics shown are open sharing of data and analysis code in Charité publications and publication rates of clinical trials.

The dashboard can be accessed via the following link: https://quest-dashboard.charite.de/.

This code repository includes the code to calculate the underlying metrics as well as the Shiny app to present the results.


## Description

The dashboard project is structured in two main blocks
 - The calculation and combination of metrics results for the different metrics using different input datasets.
 - The visualization of those combined results in the shiny app.

The files for the calculation and gathering of the results of the metrics can be found in the subfolders
 - ‘main’ (Publication list, PDF download, results combination)
 - ‘metrics’ (with subfolders containing scripts for the calculations of each metric)
 - ‘results’ (results of metrics calculations for the individual metrics)

The files for the shiny app can be found in the ‘shiny_app’ folder and are structured as follows:
 - The main app file that includes the main structure (UI, server) of the is ‘app.R’
 - Due to the size of the app, several parts and functions used by the app are outsourced in different files in the ‘shiny_app’ folder and loaded by the main file ‘app.R’ (e.g. app_functions_OA.R or  plots.R for additional functions or ‘methods_descriptions.R’ or ‘impressum.R’ for longer text sections appearing in the Shiny app 
 - The combined datasets that are visualized by the Shiny app are stored in the ‘data’ subfolder
 
The following metrics are currently shown in the dashboard:
 - Open Access
 - Open Data & Open Code
 - Preprints
 - ORCIDs
 - Clinical trials - Summary results
 - Clinical trials - Timely publication
 - Clinical trials – Prospective registration
 - Vizualizations - Bar graphs for continuous data and more informative alternatives
 - FAIR data


## License

The dashboard code is available under the MIT license. See the [LICENSE](https://github.com/quest-bih/dashboard/blob/master/LICENSE) file for more info.
