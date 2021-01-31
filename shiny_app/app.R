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
    "data/2021-01-31_pop_with_oa_trn_sciscore.csv",
    col_types="ccdddcccccdcccdllllllcddccccDlccccccccccccccccccccdddddddddddddddddddddddd"
    ## Need to specify column types here because read_csv
    ## only looks at the first few rows to determine type
    ## automatically, and if they're all empty, assumes
    ## that they're logical. This is a problem when it
    ## gets right to the end of the TRN columns and finds
    ## an NCT number there and kicks back a warning.

    ## NOTE: IF WE EVER ADD MORE COLUMNS, THE COLUMN TYPE
    ## SPECIFICATION WILL NEED TO BE UPDATED MANUALLY
)

rm_data %>% spec()

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
            col_width <- 3
            alignment <- "right"
        }

        all_numer_rando <- rm_data %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            select(randomization) %>%
            sum(na.rm=TRUE)

        all_numer_blinded <- rm_data %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            select(blinding) %>%
            sum(na.rm=TRUE)

        all_denom_animal_sciscore <- rm_data %>%
            filter(
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        all_percent_randomized <- paste0(round(100*all_numer_rando/all_denom_animal_sciscore), "%")
        all_percent_blinded <- paste0(round(100*all_numer_blinded/all_denom_animal_sciscore), "%")

        wellPanel(
            style = "padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Robustness"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Randomization",
                        value = all_percent_randomized,
                        value_text = "of animal studies report randomization",
                        plot = plotlyOutput('plot_randomization', height="300px"),
                        info_id = "infoRandomization",
                        info_title = "Randomization",
                        info_text = randomization_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Blinding",
                        value = all_percent_blinded,
                        value_text = "of animal studies report blinding",
                        plot = plotlyOutput('plot_blinding', height="300px"),
                        info_id = "infoBlinding",
                        info_title = "Blinding",
                        info_text = blinding_tooltip
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

    ## Blinding plot
    output$plot_blinding <- renderPlotly({
        return(plot_blinding(rm_data, input$selectUMC, color_palette))
    })
    
}

## Create Shiny object

shinyApp(ui, server)
