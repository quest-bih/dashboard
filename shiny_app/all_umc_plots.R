## Open Science plots

## Open Access

plot_allumc_openaccess <- function (dataset, color_palette) {

    dataset <- dataset %>%
        filter( ! is.na (color) )

    plot_data <- tribble (
        ~x_label, ~colour, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_gold <- dataset %>%
            filter(
                color == "gold",
                city == umc
            ) %>%
            nrow()

        umc_green <- dataset %>%
            filter(
                color == "green",
                city == umc
            ) %>%
            nrow()

        umc_hybrid <- dataset %>%
            filter(
                color == "hybrid",
                city == umc
            ) %>%
            nrow()

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~colour, ~percentage,
                    capitalize(umc), "Gold", round(100*umc_gold/umc_denom),
                    capitalize(umc), "Green", round(100*umc_green/umc_denom),
                    capitalize(umc), "Hybrid", round(100*umc_hybrid/umc_denom)
                )
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
