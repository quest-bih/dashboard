metricBoxOutput <- function(style_resp = "padding-top: 0px;
                            padding-bottom: 0px;",
                            title,
                            value,
                            value_text,
                            plot,
                            info_id,
                            info_title,
                            info_text,
                            info_alignment = "right",
                            bg_color = "#DCE3E5",
                            font_color = "#B0101C") {

  if (!is.null(bg_color)) {
    style_resp <- paste0(style_resp, "background-color: ", bg_color, ";")
  }
  if (!is.null(font_color)) {
    font_color <- paste0("color: ", font_color, ";")
  }


  wellPanel(style = style_resp, #"height: 500px; overflow: scroll; padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5", #padding-bottom: 0px;
                top = 0,
                height = 500,
                left = 0,
            fluidRow(
              column(8, align="left", h4(strong(title))),
              column(4, align="right", h4(actionButton(inputId = info_id, label = "", icon = icon("circle-info"),
                                                      class = "btn-primary", style="padding:1px")),
                     bsPopover(info_id, info_title, info_text,
                               info_alignment, options = list(container = "body")),
                     tags$style(".popover{width: 300px;}"))
            ),
            h1(style = paste0(font_color, "text-align:left;font-size:40px;"), value),
            h4(style = paste0(font_color, "text-align:left;font-size:18px;"), value_text),
            plot)
}


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
