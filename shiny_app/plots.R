## Open Science plots

## Open Access

plot_opensci_oa <- function (dataset, umc, color_palette) {

    ## Calculate the numerators and the denominator for the
    ## "all" bars

    plot_data <- dataset %>%
        filter( ! is.na (color) )

    all_denom <- plot_data %>%
        nrow()

    all_gold <- plot_data %>%
        filter( color == "gold") %>%
        nrow()

    all_green <- plot_data %>%
        filter( color == "green") %>%
        nrow()

    all_hybrid <- plot_data %>%
        filter( color == "hybrid") %>%
        nrow()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_gold <- plot_data %>%
            filter(
                color == "gold",
                city == umc
            ) %>%
            nrow()

        umc_green <- plot_data %>%
            filter(
                color == "green",
                city == umc
            ) %>%
            nrow()

        umc_hybrid <- plot_data %>%
            filter(
                color == "hybrid",
                city == umc
            ) %>%
            nrow()     

        plot_data <- tribble(
            ~x_label, ~colour, ~percentage,
            "All", "Gold", round(100*all_gold/all_denom),
            "All", "Green", round(100*all_green/all_denom),
            "All", "Hybrid", round(100*all_hybrid/all_denom),
            capitalize(umc), "Gold", round(100*umc_gold/umc_denom),
            capitalize(umc), "Green", round(100*umc_green/umc_denom),
            capitalize(umc), "Hybrid", round(100*umc_hybrid/umc_denom)
        )
        
    } else {

        plot_data <- tribble(
            ~x_label, ~colour, ~percentage,
            "All", "Gold", round(100*all_gold/all_denom),
            "All", "Green", round(100*all_green/all_denom),
            "All", "Hybrid", round(100*all_hybrid/all_denom)
        )

    }
    
    plot_ly(
        plot_data,
        x = ~x_label,
        color = ~colour,
        y = ~percentage,
        type = 'bar',
        colors = c(
            "#F1BA50",
            "#007265",
            "#634587"
        ),
        marker = list(
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Open Access</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
}

## Open Data

plot_opensci_od <- function (dataset, umc, color_palette) {

    ## Remove non-analyzable and non-English data points
    plot_data <- dataset %>%
        filter(
            ! is.na(is_open_data),
            language == "English"
        )

    all_denom <- plot_data %>%
        nrow()

    all_numer <- plot_data$is_open_data %>%
        sum()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer <- plot_data %>%
            filter(city == umc, is_open_data == TRUE) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom),
            capitalize(umc), round(100*umc_numer/umc_denom)
        )
        
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )

    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Open Data</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Open Code

plot_opensci_oc <- function (dataset, umc, color_palette) {

    ## Remove non-analyzable and non-English data points
    plot_data <- dataset %>%
        filter(
            ! is.na(is_open_code),
            language == "English"
        )

    all_denom <- plot_data %>%
        nrow()

    all_numer <- plot_data$is_open_code %>%
        sum()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer <- plot_data %>%
            filter(city == umc, is_open_code == TRUE) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom),
            capitalize(umc), round(100*umc_numer/umc_denom)
        )
        
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )

    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Open Code</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Clinical Trials plots

## TRN

plot_clinicaltrials_trn <- function (dataset, umc, color_palette) {

    plot_data <- dataset %>%
        filter(is_human_ct == 1)

    all_denom <- plot_data %>%
        nrow()
    
    all_numer_abs <- plot_data %>%
        filter( ! is.na (abs_trn_1) ) %>%
        nrow()

    all_numer_si <- plot_data %>%
        filter( ! is.na (si_trn_1) ) %>%
        nrow()

    if ( umc != "all" ) {
        ## If the selected UMC is not "all," calculate
        ## the percentage

        umc_denom <- plot_data %>%
            filter(city == umc) %>%
            nrow()

        umc_numer_abs <- plot_data %>%
            filter(
                ! is.na(abs_trn_1),
                city == umc
            ) %>%
            nrow()

        umc_numer_si <- plot_data %>%
            filter(
                ! is.na(si_trn_1),
                city == umc
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~colour, ~percentage,
            "All", "In abstract", round(100*all_numer_abs/all_denom),
            "All", "Secondary information", round(100*all_numer_si/all_denom),
            capitalize(umc), "In abstract", round(100*umc_numer_abs/umc_denom),
            capitalize(umc), "Secondary information", round(100*umc_numer_si/umc_denom),
        )
        
    } else {

        plot_data <- tribble(
            ~x_label, ~colour, ~percentage,
            "All", "In abstract", round(100*all_numer_abs/all_denom),
            "All", "Secondary information", round(100*all_numer_si/all_denom)
        )
        
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        color = ~colour,
        y = ~percentage,
        type = 'bar',
        colors = c(
            "#F1BA50",
            "#007265",
            "#634587"
        ),
        marker = list(
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>TRN reporting</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Robustness plots

## Randomisation
plot_randomization <- function (dataset, umc, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        select(randomization) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        nrow()

    if ( umc != "all" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            )

        umc_numer <- umc_numerator$randomization %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            capitalize(umc), round(100*umc_numer/umc_denom),
            "All", round(100*all_numer/all_denom)
        )
        
        ## message(umc)
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
        ## message("umc not set")
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage Randomized</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Blinding
plot_blinding <- function (dataset, umc, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        select(blinding) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        nrow()

    if ( umc != "all" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            )

        umc_numer <- umc_numerator$blinding %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            capitalize(umc), round(100*umc_numer/umc_denom),
            "All", round(100*all_numer/all_denom)
        )
        
        ## message(umc)
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
        ## message("umc not set")
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage Blinded</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )

}

## Power calc
plot_power <- function (dataset, umc, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        select(power) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        nrow()

    if ( umc != "all" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            )

        umc_numer <- umc_numerator$power %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            capitalize(umc), round(100*umc_numer/umc_denom),
            "All", round(100*all_numer/all_denom)
        )
        
        ## message(umc)
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
        ## message("umc not set")
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage Reporting a Power Calculation</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )

}

## Power calc
plot_iacuc <- function (dataset, umc, color_palette) {

    ## Calculate the numerator and denominator for the
    ## "all" bar

    all_numer <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        select(iacuc) %>%
        sum(na.rm=TRUE)

    all_denom <- dataset %>%
        filter(
            is_animal == 1,
            ! is.na(sciscore),
            type == "Article"
        ) %>%
        nrow()

    if ( umc != "all" ) {

        ## If the selected UMC is not "all," calculate
        ## the percentage 
        
        umc_numerator <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            )

        umc_numer <- umc_numerator$iacuc %>%
            sum(na.rm=TRUE)

        umc_denom <- dataset %>%
            filter(
                city == umc,
                is_animal == 1,
                ! is.na(sciscore),
                type == "Article"
            ) %>%
            nrow()

        plot_data <- tribble(
            ~x_label, ~percentage,
            capitalize(umc), round(100*umc_numer/umc_denom),
            "All", round(100*all_numer/all_denom)
        )
        
        ## message(umc)
    } else {

        plot_data <- tribble(
            ~x_label, ~percentage,
            "All", round(100*all_numer/all_denom)
        )
        
        ## message("umc not set")
    }

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage Reporting an IACUC statement</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )

}
