metricBoxOutput <- function(style_resp = "padding-top: 0px;
                            padding-bottom: 0px;
                            background-color:#DCE3E5",
                            title,
                            value,
                            value_text,
                            plot,
                            info_id,
                            info_title,
                            info_text,
                            info_alignment = "right") {

  wellPanel(style = style_resp, #"height: 500px; overflow: scroll; padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5", #padding-bottom: 0px;
            fluidRow(
              column(8, align="left", h4(strong(title))),
              column(4, align="right", h4(actionButton(inputId = info_id, label = "", icon = icon("circle-info"),
                                                      class = "btn-primary", style="padding:1px")),
                     bsPopover(info_id, info_title, info_text,
                               info_alignment, options = list(container = "body")),
                     tags$style(".popover{width: 300px;}"))
            ),
            h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", value),
            h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", value_text),
            plot)
}
get_current_val(tib <- dashboard_metrics_aggregate, n_preprints)

get_current_val <- function(tib, col) {
  if ("year" %in% names(tib)) {
    total <- tib |>
      filter(year == max(year)) |>
      pull({{ col }})
  }

  if (length(total) > 1) {
    total <- sum(total)
  }
  if (total > 1) {
    return(total)
  } else {
    return(
      (total * 100) |>
        round() |>
        paste("%")
    )
  }

  total
}
