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
