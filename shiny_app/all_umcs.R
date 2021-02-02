allumc_openaccess_tooltip <- strwrap("This metric ...")

## Calculate the overall "value" to be displayed
all_numer_oa <- rm_data %>%
    filter(
        color == "gold" | color == "green" | color == "hybrid"
        
    ) %>%
    nrow()

all_denom_oa <- rm_data %>%
    filter(
        ! is.na(color)
        
    ) %>%
    nrow()

## Define the page layout
all_umcs_page <- tabPanel(
    "All UMC's", value = "tabAllUMCs",
    wellPanel(
        br(),
        fluidRow(
            column(
                12,
                h1(
                    style = "margin-left: 0",
                    strong("Responsible Metrics Dashboard: All UMC's"),
                    align = "left"
                ),
                h4(
                    style = "margin-left: 0",
                    "This dashboard provides an overview of the relative performance of several German University Medical Centres (UMC's) on several metrics of open and responsible research. For more detailed information on the methods used to calculate those metrics, the dataset underlying the metrics, or resources to improve your own research practices, click one of the following buttons."
                ),
                h4(style = "margin-left:0cm",
                   "This dashboard is a pilot that is still under development, and should not be used to compare UMC's or inform policy. More metrics may be added in the future."),
                br()
            )
        )
    ),
    wellPanel(
        style="padding-top: 0px; padding-bottom: 0px;",
        h2(strong("Open Science"), align = "left"),
        fluidRow(
            column(
                12,
                metric_box(
                    title = "Open Access",
                    value = paste0(round(100*all_numer_oa/all_denom_oa), "%"),
                    value_text = "of publications were Open Access",
                    plot = plotlyOutput('plot_allumc_openaccess', height="300px"),
                    info_id = "infoALLUMCOpenAccess",
                    info_title = "Open Access (All UMC's)",
                    info_text = allumc_openaccess_tooltip
                )
            )
        )
    )
)
