library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggvis)
library(ggplot2)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(DT)
library(R.utils)

## Load data

rm_data <- read_csv(
    "data/2021-01-20_pp-dataset.csv",
    col_types="ccdddcccccdcccdllllllcddccccDlccccccccccccccccccccdddddddddddddddddddddddddd"
    ## Need to specify column types here because read_csv
    ## only looks at the first few rows to determine type
    ## automatically, and if they're all empty, assumes
    ## that they're logical. This is a problem when it
    ## gets right to the end of the TRN columns and finds
    ## an NCT number there and kicks back a warning.

    ## NOTE: IF WE EVER ADD MORE COLUMNS, THE COLUMN TYPE
    ## SPECIFICATION WILL NEED TO BE UPDATED MANUALLY
)

## Load functions

source("ui_elements.R")
source("plots.R")

## Load pages

source("start_page.R")
source("all_umcs.R")
source("methods_page.R")
source("datasets_page.R")
source("about_rm.R")

## Define UI

ui <- tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
        "Responsible Metrics Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
        start_page,
        all_umcs_page,
        methods_page,
        datasets_page,
        about_rm_page,
        tags$head
        (
            tags$script
            ('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        '
            )
        )
    )
)

## Define server function

server <- function (input, output, session) {

    ## Define button actions

    observeEvent(
        input$buttonAllUMCs, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabAllUMCs"
            )
        }
    )
    
    observeEvent(
        input$buttonMethods, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabMethods"
            )
        }
    )

    observeEvent(
        input$buttonDatasets, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabDatasets"
            )
        }
    )

    ## Dynamically determine column width of for displayed metrics
    ## at program start; four columns if resolution large enough,
    ## otherwise two columns.

    output$robustness_metrics <- renderUI({

        req(input$width)

        if (input$width < 1400) {
            col_width <- 6
            alignment <- "left"
        } else {
            col_width <- 6
            alignment <- "left"
        }

        wellPanel(
            style = "padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Robustness"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Randomization",
                        value = 3,
                        value_text = "of them",
                        plot = plotlyOutput('plot_randomization', height="300px"),
                        info_id = "infoRandomization",
                        info_title = "Randomization",
                        info_text = randomization_tooltip
                    )
                )
            )
        )        
        
    })

    color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                     "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
                     "#DCE3E5")

    ## Robustness plot

    output$plot_randomization <- renderPlotly({
        return (plot_randomization(rm_data, input$selectUMC, color_palette))
    })
    
}

## Create Shiny object

shinyApp(ui, server)
