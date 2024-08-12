#----------------------------------------------------------------------
# FAIR assessment data loading & preprocessing functions
#----------------------------------------------------------------------

# Data for FAIR license bar chart

make_fair_license_plot_data <- function(data_table)
  {

  fair_license_plot_data <- data_table %>%
    mutate(license_fuji = factor(
      license_fuji,
      levels = c(
        "Public Domain",
        "CC0",
        "CC BY",
        "CC BY-SA",
        "CC BY-NC",
        "CC-BY-NC-SA",
        "CC BY-ND",
        "CC BY-NC-ND",
        "other license",
        "no license"
      )
    )) %>%
    group_by(license_fuji, repository_type) %>%
    count() %>%
    ungroup() %>%
   # complete(license_fuji, repository_type, fill = list(n = 0)) %>%
    group_by(repository_type) %>%
    mutate(perc = round(n / sum(n), 3)) %>%
    ungroup() %>%
    mutate(
      rep_type_2 = case_when(
        repository_type == "field-specific repository" ~ paste0("Disciplinary\nrepository\nn = ", sum(n[repository_type == "field-specific repository"])),
        repository_type == "general-purpose repository" ~ paste0("General-purpose\nrepository\nn = ", sum(n[repository_type == "general-purpose repository"]))
      )
    )

  return(fair_license_plot_data)
}


# Data for FAIR principle violin chart

make_fair_principle_plot_data <- function(data_table)
{
  fair_principle_plot_data <- data_table %>%
    select(
      best_identifier,
      repository_re3data,
      repository_type,
      fuji_percent_f,
      fuji_percent_a,
      fuji_percent_i,
      fuji_percent_r
    ) %>%
    pivot_longer(cols = starts_with("fuji_percent")) %>%
    mutate(
      name = case_when(
        name == "fuji_percent_f" ~ "F",
        name == "fuji_percent_a" ~ "A",
        name == "fuji_percent_i" ~ "I",
        name == "fuji_percent_r" ~ "R"
      )
    ) %>%
    mutate(name = factor(name, levels = c("F", "A", "I", "R"))) %>%
    mutate(value = value / 100) %>%
    mutate(repository_type = case_when(repository_type == "field-specific repository" ~ "Disciplinary\nrepositories",
                                       repository_type == "general-purpose repository" ~ "General-purpose\nrepositories",
                                       TRUE ~ repository_type)) %>%
    drop_na(value)

   return(fair_principle_plot_data)
}

# Data for FAIR repository treemap chart

make_fair_treemap_plot_data <- function(data_table)
{
  # Function to combine round() and mean()
  rounded_mean <- compose(
    partial(round, digits = 1),
    partial(mean, na.rm = TRUE)
  )

  fair_treemap_plot_data_head_1 <- data_table %>%
    summarise(n = n(),
              fair_score = rounded_mean(fuji_percent),
              f_score = rounded_mean(fuji_percent_f),
              a_score = rounded_mean(fuji_percent_a),
              i_score = rounded_mean(fuji_percent_i),
              r_score = rounded_mean(fuji_percent_r)
    ) %>%
    mutate(repository = "All repositories", .before = n) %>%
    mutate(repository_re3data = "All repositories", .before = n)

  fair_treemap_plot_data_head_2 <- data_table %>%
    group_by(repository_type) %>%
    summarise(n = n(),
              fair_score = rounded_mean(fuji_percent),
              f_score = rounded_mean(fuji_percent_f),
              a_score = rounded_mean(fuji_percent_a),
              i_score = rounded_mean(fuji_percent_i),
              r_score = rounded_mean(fuji_percent_r)
    ) %>%
    rename(repository = repository_type) %>%
    mutate(repository_re3data = repository) %>%
    mutate(repository_type = "All repositories")

  fair_treemap_plot_data <- data_table %>%
    group_by(repository, repository_re3data, repository_type) %>%
    summarise(n = n(),
              fair_score = rounded_mean(fuji_percent),
              f_score = rounded_mean(fuji_percent_f),
              a_score = rounded_mean(fuji_percent_a),
              i_score = rounded_mean(fuji_percent_i),
              r_score = rounded_mean(fuji_percent_r)
    ) %>%
    ungroup()

  fair_treemap_plot_data <-
    bind_rows(fair_treemap_plot_data_head_1,
              fair_treemap_plot_data_head_2,
              fair_treemap_plot_data) %>%
    mutate(across(
      where(is.character),
      ~ case_when(
        str_detect(., "field-specific repository") ~ "Disciplinary repositories",
        str_detect(., "general-purpose repository") ~ "General-purpose rep.",
        TRUE ~ as.character(.)
      )
    ))

  return(fair_treemap_plot_data)
}

# Data for FAIR repository sunburst chart

make_fair_sunburst_plot_data <- function(data_table)
{
  # Function to combine round() and mean()
  rounded_mean <- compose(
    partial(round, digits = 1),
    partial(mean, na.rm = TRUE)
  )

  fair_sunburst_plot_data_head_1 <- data_table %>%
    summarise(n = n(), fair_score = rounded_mean(fuji_percent)) %>%
    mutate(guid_scheme_fuji = "All repositories", .before = n)

  fair_sunburst_plot_data_head_2 <- data_table %>%
    group_by(repository_type) %>%
    summarise(n = n(), fair_score = rounded_mean(fuji_percent)) %>%
    mutate(guid_scheme_fuji = repository_type) %>%
    mutate(repository_type = "All repositories")

  fair_sunburst_plot_data <- data_table %>%
    group_by(guid_scheme_fuji, repository_type) %>%
    summarise(n = n(), fair_score = rounded_mean(fuji_percent)) %>%
    ungroup() %>%
    complete(guid_scheme_fuji, repository_type, fill = list(n =0, fair_score = 0))

  fair_sunburst_plot_data <-
    bind_rows(fair_sunburst_plot_data_head_1,
              fair_sunburst_plot_data_head_2,
              fair_sunburst_plot_data) %>%
    mutate(across(
      where(is.character),
      ~ case_when(
        str_detect(., "field-specific repository") ~ "Disciplinary repositories",
        str_detect(., "general-purpose repository") ~ "General-purpose rep.",
        TRUE ~ as.character(.)
      )
    )) %>%
    mutate(repository_type = case_when(str_detect(repository_type, "Disciplinary repositories") ~ "All repositories - Disciplinary repositories",
                                       str_detect(repository_type, "General-purpose rep.") ~ "All repositories - General-purpose rep.",
                                       TRUE ~ as.character(repository_type))) %>%
    mutate(ids = paste0(repository_type, " - ", guid_scheme_fuji)) %>%
    mutate(ids = str_remove(ids, "NA - ")) %>%
    drop_na(guid_scheme_fuji)

  return(fair_sunburst_plot_data)
}

# For test, remove later
#test <- make_fair_sunburst_plot_data(fair_dataset)

#------------------------------------------------------------------------
# FAIR assessment
#------------------------------------------------------------------------

plot_fair_license_perc <- function(plot_data, color_palette)
{

  hovertext <-
    c(
      "<b>{license_fuji}</b>
          prop.: {perc*100}%
          n: {n}"
    )

  pal_lic <- colorRampPalette(c("#FFFFCC", "#F1BA50", "#F16A50"))
  pal_license <- c(rev(pal_lic(6)), "#879C9D")

  plot_data %>%
    plot_ly(
      x = ~ perc,
      y = ~ rep_type_2,
      color = ~ license_fuji,
      colors = pal_license,
      marker = list(line = list(color = "#000000",
                                width = 1)),
      text = ~ license_fuji,
      textposition = 'inside',
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "auto", size = 14), #color = #ffffff
      hoverinfo = "text",
      hovertext = ~ str_glue(hovertext)
    ) %>%
    add_bars() %>%
    layout(
      margin = list(l = 4, b = 10, pad = 0),
      barmode = "stack",
      xaxis = list(
        title = FALSE,
        side = "top",
        tickformat = ",.0%"
      ),
      yaxis = list(title = FALSE, side = "right"),
      uniformtext = list(minsize = 8, mode = "hide"),
      legend = list(
        orientation = 'h',
        traceorder = "normal",
        x = 0, #1
        xanchor = "left", # right
        font = list(size = 10)
      ),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9]
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

plot_fair_license_total <- function(plot_data, color_palette)
{

  hovertext <-
    c(
      "<b>{license_fuji}</b>
          prop.: {perc*100}%
          n: {n}"
    )

  pal_lic <- colorRampPalette(c("#FFFFCC", "#F1BA50", "#F16A50"))
  pal_license <- c(rev(pal_lic(6)), "#879C9D")

  plot_data %>%
    plot_ly(
      x = ~ n,
      y = ~ rep_type_2,
      color = ~ license_fuji,
      colors = pal_license,
      marker = list(line = list(color = "#000000",
                                width = 1)),
      text = ~ license_fuji,
      textposition = 'inside',
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "auto", size = 14),
      hoverinfo = "text",
      hovertext = ~ str_glue(hovertext)
    ) %>%
    add_bars() %>%
    layout(
      margin = list(l = 4, b = 10, pad = 0),
      barmode = "stack",
      xaxis = list(
        title = FALSE,
        side = "top"
      ),
      yaxis = list(title = FALSE, side = "right"),
      uniformtext = list(minsize = 12, mode = "hide"),
      legend = list(
        orientation = 'h',
        traceorder = "normal",
        x = 1,
        xanchor = "right",
        font = list(size = 10)
      ),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9]
    ) %>%
    plotly::config(displayModeBar = FALSE)
}


plot_fair_principle_perc <- function(plot_data, color_palette)
{
  plot_data %>%
    mutate(jitter = value + round(runif(nrow(.),-0.02, 0.02), 3)) %>%
    ggplot(aes(
      x = name,
      y = jitter,
      fill = repository_type,
      group = name,
      text = paste0(
        "FAIR Principle: ",
        name,
        "<br>FAIR Score: ",
        round(value, 2) * 100,
        "%",
        "<br>Repository: ",
        repository_re3data
      )
    )) +
    geom_violin(trim = TRUE,
                scale = "width",
                na.rm = TRUE) +
    geom_quasirandom(
      width = 0.45, # 0.45
      bandwidth = 0.2, # 0.2
      varwidth = TRUE,
      size = 0.1, #0.4
      alpha = 0.4, # 0.4
      shape = 21, #21
      method = "quasirandom",
      na.rm = TRUE,
      color = "#000000"
    ) +
    # stat_summary(
    #   fun = median,
    #   geom = "errorbar",
    #   width = 0.3,
    #   size = 0.25,
    #   color = "#000000",
    #   na.rm = TRUE
    # ) +
    facet_wrap(~ repository_type) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank()
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c(color_palette[2], color_palette[3]))

  ggplotly(tooltip = "text") %>% #list("AB" = "name", "XY" = "value")
    layout(yaxis = list(
      title = list(text = "FAIR score", font = list(size = 12))),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9]) %>%
    plotly::config(displayModeBar = FALSE)
}

# For test, remove later
# plot_data <- test
# plot_fair_principle_perc(test, c("grey", "yellow"))
# ggplotly()


# Function for treemap plot for FAIR score by repository

plot_fair_treemap <- function(plot_data, color_palette, fair_perc, color_seq)
{
  # Prepare hovertext for str_glue()
  hovertext <-
    c(
      "<b>{repository_re3data}</b><span style='font-family:courier'>
          n = {n}
          FAIR = {fair_score}%
          F = {f_score}%
          A = {a_score}%
          I = {i_score}%
          R = {r_score}%</span>"
    )

  plot_data %>% plot_ly(
    labels = ~ repository,
    parents = ~ repository_type,
    values = ~ n,
    type = "treemap",
    branchvalues = "total",
    textinfo = "label",
    hoverinfo = "text",
    hovertext = ~ str_glue(hovertext),
    marker = list(
      colorscale = list(c(0, 0.35, 1), color_seq), # c("#AA493A", "#F1BA50", "#007265")
      cmin = 0,
      cmid = 0.35,
      cmax = 1,
      colors = ~ eval(as.symbol(fair_perc)) / 100, #fair_score
      showscale = TRUE,
      line = list(color = color_palette[9], width = 0.5),
      pad = list(b = 5, l = 5,r = 5,t = 25),
      colorbar = list(title = "FAIR<br>Score",
                      tickformat = ".0%",
                      tickfont = list(size = 10),
                      outlinecolor = color_palette[9])
    ),
    pathbar = list(visible = FALSE),
    outsidetextfont=list(size=20),
    insidetextfont=list(size=25)
  ) %>%
    layout(paper_bgcolor = color_palette[9],
           margin=list(l=0, r=0, b=10, t=0)) %>%
    plotly::config(displayModeBar = FALSE) #  plot_bgcolor = pal_bg
}

# Function for sunburst plot for FAIR score by guid

plot_fair_sunburst <- function(plot_data, color_palette, color_seq)
{
  # Prepare hovertext for str_glue()
  hovertext <-
    c(
      "<b>{guid_scheme_fuji}</b>
          n = {n}
          FAIR = {fair_score}%"
    )

  plot_data %>% plot_ly(
    ids = ~ ids,
    labels = ~ guid_scheme_fuji,
    parents = ~ repository_type,
    values = ~ n,
    type = "sunburst",
    branchvalues = "total",
    hoverinfo = "text",
    hovertext = ~ str_glue(hovertext),
    marker = list(
      colorscale = list(c(0, 0.35, 1), color_seq), # c("#AA493A", "#F1BA50", "#007265")
      cmin = 0,
      cmid = 0.35,
      cmax = 1,
      colors = ~ fair_score / 100,
      showscale = TRUE,
      # line = list(color = color_palette[9], width = 2),
      # pad = list(b = 5, l = 5,r = 5,t = 25),
      colorbar = list(title = "FAIR<br>Score",
                      tickformat = ".0%",
                      tickfont = list(size = 10),
                      outlinecolor = color_palette[9]))) %>%
    layout(paper_bgcolor = color_palette[9],
           margin=list(l=0, r=0, b=5, t=0)) %>%
    plotly::config(displayModeBar = FALSE)

}


# Function for sunburst plot for FAIR assessment according to F-UJI

plot_fair_principle_sunburst <- function(plot_data, color_palette, select_repository, color_seq)
{

  if (select_repository == "all repositories") {
    plot_data <- plot_data
  } else if (select_repository == "field-specific repositories") {
    plot_data <- plot_data %>% filter(repository_type == "field-specific repository")
  } else if (select_repository == "general-purpose repositories") {
    plot_data <- plot_data %>% filter(repository_type == "general-purpose repository")
  } else {
    plot_data <- plot_data %>% filter(repository_re3data %in% select_repository)
  }


  plot_data <- plot_data %>%
    #filter(repository_type == "field-specific repository") %>%
    #filter(repository_re3data == "figshare") %>%
    select(starts_with("fuji_percent"), starts_with("FsF")) %>%
    summarise(across(where(is.numeric), ~ mean(.)), n = n()) %>%
    pivot_longer(cols = c(starts_with("fuji_percent"), starts_with("FsF"))) %>%
    mutate(name = factor(
      name,
      levels = c(
        "fuji_percent",
        "fuji_percent_f",
        "fuji_percent_a",
        "fuji_percent_i",
        "fuji_percent_r",
        "fuji_percent_f1",
        "fuji_percent_f2",
        "fuji_percent_f3",
        "fuji_percent_f4",
        "fuji_percent_a1",
        "fuji_percent_i1",
        "fuji_percent_i2",
        "fuji_percent_i3",
        "fuji_percent_r1",
        "fuji_percent_r1_1",
        "fuji_percent_r1_2",
        "fuji_percent_r1_3",
        "FsF-F1-01D", "FsF-F1-02D", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
        "FsF-A1-01M", "FsF-A1-02M", "FsF-A1-03D",
        "FsF-I1-01M", "FsF-I2-01M", "FsF-I3-01M",
        "FsF-R1-01MD", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M", "FsF-R1.3-02D"
      ),
      labels =
        c("FAIR", "F", "A", "I", "R",
          "F1", "F2", "F3", "F4",
          "A1",
          "I1", "I2", "I3",
          "R1", "R1.1", "R1.2", "R1.3",
          "FsF-F1-01D", "FsF-F1-02D", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
          "FsF-A1-01M", "FsF-A1-02M", "FsF-A1-03D",
          "FsF-I1-01M", "FsF-I2-01M", "FsF-I3-01M",
          "FsF-R1-01MD", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M", "FsF-R1.3-02D")
    )) %>%
    arrange(name) %>%
    mutate(parent = name) %>%
    mutate(parent = str_replace(parent, "^FAIR$", NA_character_)) %>%
    mutate(parent = str_replace(parent, "^[FAIR]$", "FAIR")) %>%
    mutate(parent = str_replace_all(parent, "[0-9.]", "")) %>%
    mutate(parent = case_when(str_detect(name, "^FsF.*") ~ as.character(str_extract(name, "(?<=-)(.*)(?=-)")),
                              TRUE ~ parent)) %>%
    mutate(score = c(24,
                     7, 3, 4, 10,
                     2, 2, 1, 2,
                     3,
                     2, 1, 1,
                     4, 2, 2, 2,
                     1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 4, 2, 2, 1, 1)) %>%
    mutate(name = factor(name, levels = rev(levels(name)))) %>%
    # mutate(name = if(select_chart == "sunburst"){factor(name, levels = rev(levels(name)))}else{factor(name, levels = levels(name))}) %>%
    arrange(name) %>%
    mutate(principle = case_when(name == "FAIR" ~ "FAIR",
                                 name == "F" ~ "Findability",
                                 name == "A" ~ "Accessibility",
                                 name == "I" ~ "Interoperability",
                                 name == "R" ~ "Reusability",
                                 name == "F1" ~ "F1 — (Meta)data are assigned a globally unique and persistent identifier",
                                 name == "F2" ~ "F2 — Data are described with rich metadata (defined by R1 below)",
                                 name == "F3" ~ "F3 — Metadata clearly and explicitly include the identifier of the data they describe",
                                 name == "F4" ~ "F4 — (Meta)data are registered or indexed in a searchable resource",
                                 name == "A1" ~ "A1 — (Meta)data are retrievable by their identifier using a standardised communications protocol",
                                 name == "I1" ~ "I1 — (Meta)data use a formal, accessible, shared, and broadly applicable language for knowledge representation",
                                 name == "I3" ~ "I3 — (Meta)data include qualified references to other (meta)data",
                                 name == "R1" ~ "R1 — Meta(data) are richly described with a plurality of accurate and relevant attributes",
                                 name == "R1.1" ~ "R1.1 — (Meta)data are released with a clear and accessible data usage license",
                                 name == "R1.2" ~ "R1.2 — (Meta)data are associated with detailed provenance",
                                 name == "R1.3" ~ "R1.3 — (Meta)data meet domain-relevant community standards",
                                 name == "FsF-F1-01D" ~ "FsF-F1-01D — Data is assigned a globally unique identifier.",
                                 name == "FsF-F1-02D" ~ "FsF-F1-02D — Data is assigned a persistent identifier.",
                                 name == "FsF-F2-01M" ~ "FsF-F2-01M — Metadata includes descriptive core elements (creator, title, data identifier, publisher, publication date, summary and keywords) to support data findability.",
                                 name == "FsF-F3-01M" ~ "FsF-F3-01M — Metadata includes the identifier of the data it describes.",
                                 name == "FsF-F4-01M" ~ "FsF-F4-01M — Metadata is offered in such a way that it can be retrieved programmatically.",
                                 name == "FsF-A1-01M" ~ "FsF-A1-01M — Metadata contains access level and access conditions of the data.",
                                 name == "FsF-A1-02M" ~ "FsF-A1-02M — Metadata is accessible through a standardized communication protocol.",
                                 name == "FsF-A1-03D" ~ "FsF-A1-03D — Data is accessible through a standardized communication protocol.",
                                 name == "FsF-I1-01M" ~ "FsF-I1-01M — Metadata is represented using a formal knowledge representation language.",
                                 name == "FsF-I2-01M" ~ "FsF-I2-01M — Metadata uses semantic resources",
                                 name == "FsF-I3-01M" ~ "FsF-I3-01M — Metadata includes links between the data and its related entities.",
                                 name == "FsF-R1-01MD" ~ "FsF-R1-01MD — Metadata specifies the content of the data.",
                                 name == "FsF-R1.1-01M" ~ "FsF-R1.1-01M — Metadata includes license information under which data can be reused.",
                                 name == "FsF-R1.2-01M" ~ "FsF-R1.2-01M — Metadata includes provenance information about data creation or generation.",
                                 name == "FsF-R1.3-01M" ~ "FsF-R1.3-01M — Metadata follows a standard recommended by the target research community of the data.",
                                 name == "FsF-R1.3-02D" ~ "FsF-R1.3-02D — Data is available in a file format recommended by the target research community."
    )) %>%
    mutate(principle = str_wrap(principle, width = 50))

  hovertext <-
    c(
      "<b>{principle}</b>
    Score = {round(value, 1)}%
    Weight = {round(score/0.24, 1)}%"
    )

  caption <- glue::glue("n = {n}
                      Click on chart to see corresponding metrics
                      Size of wedges correspond to the weight of the FAIR assessment sections",
                        n = first(plot_data$n))


  plot_data %>% plot_ly(
    labels = ~ name,
    parents = ~ parent,
    values = ~ score,
    #text = ~ paste0(round(value, 0),"%"),
    type = "sunburst",
    maxdepth = 3,
    branchvalues = "total",
    rotation = 90,
    sort = FALSE,
    hoverinfo = "text",
    hovertext = ~ str_glue(hovertext),
    textinfo = "label",
    insidetextfont = list(size = 14),
    marker = list(
      colorscale = list(c(0, 0.35, 1), color_seq), # viridis c("#FDE725FF", "#21908CFF", "#440154FF") # red green c("#AA493A", "#F1BA50", "#007265")
      cmin = 0,
      cmid = 0.35,
      cmax = 1,
      colors = ~ value / 100,
      showscale = TRUE,
      # line = list(width = 1.5),
      # pad = list(b = 5, l = 5,r = 5,t = 25),
      colorbar = list(title = "FAIR<br>Score",
                      tickformat = ".0%",
                      tickfont = list(size = 10),
                      outlinecolor = color_palette[9])
    )
  ) %>%
    layout(paper_bgcolor = color_palette[9],
           margin=list(l=0, r=0, b=35, t=0),
           annotations =
             list(x = 0, y = -0.08, text = caption,
                  align = "left",
                  showarrow = F, xref='paper', yref='paper',
                  xanchor='auto', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=11))) %>%
    plotly::config(displayModeBar = FALSE)
}
