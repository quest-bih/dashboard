#----------------------------------------------------------------------------------------------------------------------
# Berlin Science Survey panel
#----------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------
# Import BSS data
#----------------------------------------------------------------------------------------------------------------------

load("data/bss_data.Rdata")
# load("shiny_app/data/bss_data.Rdata") # For testing purposes

#----------------------------------------------------------------------------------------------------------------------
# Data preparation for functions
#----------------------------------------------------------------------------------------------------------------------

# Research characteristics

traits <- bss_stata |>
  select(id, starts_with("v6")) |>
  mutate(across(starts_with("v6"), as.numeric)) |>
  mutate(across(starts_with("v6"), ~ case_when(. >= 3 ~ TRUE,
                                               TRUE ~ FALSE))) |>
  mutate(all = TRUE) |>
  pivot_longer(cols = -id) |>
  filter(value == TRUE) |>
  select(-value)

# Change columns into factors
bss_labeled <- as_factor(bss_stata, only_labelled = TRUE)

# Data preparation for dataset publication ----
bss_labeled_dataset <- bss_labeled |>
  select(
    status_group = a2,
    starts_with("v6"),
    matches("^(f1|f2|f3)"),
    matches("^(o1a|o1c_[124]|o5_[1235])"),
    matches("^(b1)"),
    matches("^o2|a2$")
  ) |>
  mutate(
    status_group = as.character(status_group),
    status_group = case_when(
      status_group == "Juniorprofessor:in" ~ "Professor:in",
      TRUE ~ status_group
    )
  ) |>
  mutate(status_group = factor(
    status_group,
    levels = c(
      "Professor:in",
      "Promovierte:r Wissenschaftler:in",
      "Nicht-promovierte:r Wissenschaftler:in"
    ),
    labels = c("Professor",
               "Scientist with a PhD",
               "Scientist without a PhD")
  )) |>
  mutate(across(v6_1:v6_9, ~ factor(
    .,
    levels = c("überhaupt nicht",
               "teilweise",
               "überwiegend",
               "voll und ganz"),
    labels = c("not at all",
               "partly",
               "mostly",
               "fully")
  ))) |>
  rename(
    research_theoretical_conceptual = v6_1,
    research_empirical = v6_2,
    research_team = v6_3,
    research_specialized = v6_4,
    research_experimental = v6_5,
    research_agenda = v6_6,
    research_basic = v6_7,
    research_competition = v6_8,
    research_infrastructure = v6_9
  ) |>
  mutate(across(f1_1:f1_13, ~ factor(
    .,
    levels = c("sollte überhaupt kein Ziel sein",
               "sollte ein untergeordnetes Ziel sein",
               "sollte ein übergeordnetes Ziel sein",
               "sollte höchstes Ziel sein"),
    labels = c("should not be a goal at all",
               "should be a subordinate goal",
               "should be an overriding goal",
               "should be a highest goal")
  ))) |>
  rename(
    goal_originality = f1_1,
    goal_accuracy = f1_2,
    goal_impact= f1_4,
    goal_publ_output = f1_5,
    goal_teaching = f1_7,
    goal_open_science = f1_11,
    goal_cooperation = f1_13
  ) |>
  mutate(across(f2_1:f2_13, ~ factor(
    .,
    levels = c("gar kein Erwartungsdruck",
               "geringer Erwartungsdruck",
               "hoher Erwartungsdruck",
               "sehr hoher Erwartungsdruck"),
    labels = c("no pressure at all",
               "low pressure",
               "high pressure",
               "very high pressure")
  ))) |>
  rename(
    pressure_originality = f2_1,
    pressure_accuracy = f2_2,
    pressure_impact= f2_4,
    pressure_publ_output = f2_5,
    pressure_teaching = f2_7,
    pressure_open_science = f2_11,
    pressure_cooperation = f2_13
  ) |>
  mutate(across(f3_1:f3_13, ~ factor(
    .,
    levels = c("keine Priorität",
               "geringe Priorität",
               "hohe Priorität",
               "höchste Priorität"),
    labels = c("no priority",
               "low priority",
               "high priority",
               "very high priority")
  ))) |>
  rename(
    priority_originality = f3_1,
    priority_accuracy = f3_2,
    priority_impact= f3_4,
    priority_publ_output = f3_5,
    priority_teaching = f3_7,
    priority_open_science = f3_11,
    priority_cooperation = f3_13
  ) |>
  mutate(across(o1c_1:o1c_4, ~ factor(
    .,
    levels = c("nie",
               "selten",
               "gelegentlich",
               "oft",
               "sehr oft",
               "immer",
               "trifft auf meine Forschungspraxis nicht zu"),
    labels = c("never",
               "rarely",
               "occasionally",
               "often",
               "very often",
               "always",
               "does not apply to my research practice")
  ))) |>
  rename(practice_oa = o1a,
         practice_data_sharing = o1c_1,
         practice_open_peer_review = o1c_2,
         practice_code_sharing = o1c_4) |>
  mutate(across(o5_1:o5_5, ~ factor(
    .,
    levels = c("überhaupt keine  Schwierigkeiten",
               "geringe Schwierigkeiten",
               "große Schwierigkeiten",
               "sehr große Schwierigkeiten",
               "kann ich nicht einschätzen"),
    labels = c("no difficulties at all",
               "minor difficulties",
               "great difficulties",
               "very great difficulties",
               "I cannot estimate")
  ))) |>
  rename(difficulties_oa = o5_1,
         difficulties_data_sharing = o5_2,
         difficulties_open_peer_review = o5_3,
         difficulties_code_sharing = o5_5) |>
  mutate(across(b1_1:b1_11, ~ factor(
    .,
    levels = c("sehr schlecht",
               "eher schlecht",
               "eher gut",
               "sehr gut",
               "kann ich nicht beurteilen"),
    labels = c("very bad",
               "somewhat bad",
               "somewhat good",
               "very good",
               "I cannot judge")
  ))) |>
  rename(environment_innovation = b1_1,
         environment_cooperation = b1_2,
         environment_quality = b1_3,
         environment_open_science = b1_4,
         environment_internationality = b1_5,
         environment_autonomy = b1_6,
         environment_young_talent = b1_7,
         environment_knowledge_exchange = b1_8,
         environment_diversity = b1_9,
         environment_human_ressources = b1_10,
         environment_inst_framework = b1_11) |>
  mutate(o2 = factor(o2,
                     levels = c("überhaupt nicht wichtig",
                                "kaum wichtig",
                                "etwas wichtig",
                                "ziemlich wichtig",
                                "sehr wichtig",
                                "weiß nicht"),
                     labels = c("not at all important",
                                "not very important",
                                "somewhat important",
                                "fairly important",
                                "very important",
                                "don't know"))) |>
  rename(expansion_open_science = o2)


# Dataframe with attributes from column names
label <- bss_labeled |> map_chr(~attributes(.)$label) |>
  data.frame() |>
  rename(label = 1) |>
  rownames_to_column(var = "question_id")

#----------------------------------------------------------------------------------------------------------------------
# Functions
#----------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------
# Functions for data aggregations
#----------------------------------------------------------------------------------------------------------------------

# Goals and prios
data_prep_prio <- function(x, y) {
  data <- bss_stata |>
    filter(a2 %in% x) |>
    filter(id %in% traits$id[traits$name %in% y]) |>
    select(matches("^(f1|f2|f3)")) |>
    summarise(across(everything(), ~ mean(., na.rm = TRUE)), n = n()) |>
    pivot_longer(cols = -n, names_to = "question_id") |>
    left_join(label, by = "question_id") |>
    separate(label, into = c("label", "goals"), sep = "]") |>
    mutate(label = str_remove(label, "^\\[")) |>
    mutate(goals = factor(goals,
                          levels = c(
                            " Erwartungsdruck",
                            " Priorisierung",
                            " Wichtigkeit Ziele"
                          ),
                          labels = c(
                            "Pressure of expectations",
                            "Prioritization",
                            "Importance"
                          ))) |>
    relocate(c(value, n), .after = last_col()) |>
    mutate(label = factor(
      label,
      levels = c(
        "Open Science",
        "gute Lehre",
        "Kooperationen",
        "Societal Impact",
        "Publikationsoutput",
        "Originalität der Forschungsergebnisse",
        "Methodische Strenge (Akkuratheit)"
      ),
      labels = str_wrap(c(
        "Open Science",
        "Good teaching",
        "Cooperation with other disciplines (interdisciplinarity)",
        "Social usability of research results/societal impact",
        "Publication output",
        "Originality of research results",
        "Methodical rigour (accuracy)"
      ), 33)
    )) |>
    group_by(label) |>
    mutate(min = min(value, na.rm = TRUE),
           max = max(value, na.rm = TRUE)) |>
    ungroup()
}

# Goals and prios for likert chart
data_prep_prio_likert <- function(x, y) {
  data <- bss_stata |>
    filter(a2 %in% x) |>
    filter(id %in% traits$id[traits$name %in% y]) |>
    select(matches("^(f1|f2|f3)")) |>
    mutate(across(everything(), ~ factor(., levels = c(4, 3, 2, 1),
                                         labels = c("highest", "high", "low", "no")))) |>
    mutate(n = n()) |>
    pivot_longer(cols = -n, names_to = "question_id") |>
    drop_na() |>
    count(n, question_id, value, name = "nn") |>
    mutate(perc = nn/n) |>
    left_join(label, by = "question_id") |>
    separate(label, into = c("label", "goals"), sep = "]") |>
    mutate(label = str_remove(label, "^\\[")) |>
    mutate(label = factor(
      label,
      levels = c(
        "Open Science",
        "gute Lehre",
        "Kooperationen",
        "Societal Impact",
        "Publikationsoutput",
        "Originalität der Forschungsergebnisse",
        "Methodische Strenge (Akkuratheit)"
      ),
      labels = str_wrap(c(
        "Open Science",
        "Good teaching",
        "Cooperation with other disciplines (interdisciplinarity)",
        "Social usability of research results/societal impact",
        "Publication output",
        "Originality of research results",
        "Methodical rigour (accuracy)"
      ), 33)
    ))
}

# OS practices and difficulties
data_prep_practices <- function(x, y) {
  data <- bss_stata |>
    filter(a2 %in% x) |>
    filter(id %in% traits$id[traits$name %in% y]) |>
    # Select os practices and difficulties questions
    select(matches("^(o1a|o1c_[124]|o5_[1235])")) |>
    # Change "trifft auf meine Forschungspraxis nicht zu" into NA
    mutate(across(starts_with("o1c_"), ~ ifelse(. == 7, NA, .))) |>
    # Change "kann ich nicht einschätzen" into NA
    mutate(across(starts_with("o5_"), ~ ifelse(. == 5, NA, .))) |>
    summarise(across(everything(), ~ mean(., na.rm = TRUE)), n = n()) |>
    # Normalize columns
    mutate(o1a = o1a/100*5) |>
    mutate(across(matches("^(o1c_|o5_)"), ~ . - 1)) |>
    mutate(across(matches("^o5_"), ~ 3 - .))

  data <- data |>
    pivot_longer(
      cols = !n
    ) |>
    mutate(label = case_when(str_detect(name, "^o1") ~ "practices_applied",
                             TRUE ~ "difficulties_implementing")) |>
    mutate(os_practice = rep(c("Open Access", "Data Sharing", "Code & Material Sharing", "Open Peer Review"), 2)) |>
    mutate(os_practice = factor(os_practice, levels = rev(c("Open Access", "Data Sharing", "Open Peer Review", "Code & Material Sharing")))) |>
    select(-name) |>
    pivot_wider(names_from = label) |>
    mutate(difficulties_normalized = difficulties_implementing/3*5)
}

# Practices and difficulties for likert chart
data_prep_practices_likert <- function(x, y) {
  data_practices <- bss_stata |>
    filter(a2 %in% x) |>
    filter(id %in% traits$id[traits$name %in% y]) |>
    select(matches("^(o1a|o1c_[124])")) |>
    mutate(o1a = case_when(o1a >= 50 ~ "practiced",
                           TRUE ~ "other")) |>
    mutate(across(starts_with("o1c_"), ~ ifelse(. %in% c(4, 5, 6), "practiced", "other"))) |>
    pivot_longer(cols = everything()) |>
    count(name, value) |>
    group_by(name) |>
    mutate(nn = sum(n)) |>
    ungroup() |>
    mutate(perc = n/nn) |>
    mutate(name = factor(name,
                         levels = c("o1a", "o1c_1", "o1c_4", "o1c_2"),
                         labels = c("Open Access",
                                    "Data Sharing",
                                    "Open Peer Review",
                                    "Code & Material Sharing"
                                    ))) |>
    filter(value == "practiced")

  data_difficulties <- bss_stata |>
    filter(a2 %in% x) |>
    filter(id %in% traits$id[traits$name %in% y]) |>
    select(matches("^(o5_[1235])")) |>
    mutate(across(starts_with("o5_"), ~ ifelse(. %in% c(3, 4), "difficulties", "other"))) |>
    pivot_longer(cols = everything()) |>
    count(name, value) |>
    group_by(name) |>
    mutate(nn = sum(n)) |>
    ungroup() |>
    mutate(perc = n/nn) |>
    mutate(name = factor(name,
                         levels = c("o5_1", "o5_2", "o5_5", "o5_3"),
                         labels = c("Open Access",
                                    "Data Sharing",
                                    "Open Peer Review",
                                    "Code & Material Sharing"
                         ))) |>
    filter(value == "difficulties")

data <- bind_rows(data_practices, data_difficulties)
}

# Research environment
data_prep_environment <- function(x, y) {
  data <- bss_stata |>
    filter(a2 %in% x) |>
    filter(id %in% traits$id[traits$name %in% y]) |>
    select(matches("^(b1)")) |>
    filter(if_any(everything(), ~ . != 5)) |>
    mutate(n = n()) |>
    pivot_longer(cols = -n, names_to = "question_id") |>
    filter(value != 5) |>
    drop_na() |>
    count(n, question_id, value, name = "nn") |>
    group_by(question_id) |>
    mutate(perc = nn/sum(nn)) |>
    ungroup() |>
    left_join(label, by = "question_id") |>
    separate(label, into = c("label", "category"), sep = "]") |>
    select(-category) |>
    mutate(label = str_remove(label, "^\\[")) |>
    mutate(value = factor(value, levels = c(4, 3, 2, 1),
                          labels = c("very good", "somewhat good", "somewhat bad", "very bad"))) |>
    # group_by(question_id) |>
    # mutate(order = nn[value == "very good"] + nn[value == "somewhat good"] ) |>
    # ungroup() |>
    mutate(label = factor(label,
                          levels = c(
                            "Forschungsqualität",
                            "Internationalität",
                            "Innovationspotenzial",
                            "Kooperationfähigkeit",

                            "Forschungsautonomie",
                            "Diversität",
                            "Umsetzung von Open Science",
                            "Wissenstransfer",

                            "Nachwuchsförderung",
                            "Verfügbarkeit personeller Ressourcen/Bewerbungslage",
                            "materielle und institutionelle Rahmenbedingungen"
                          ),
                          labels = str_wrap(c(
                            "Research quality",
                            "Internationality",
                            "Innovation potential",
                            "Ability to cooperate",

                            "Research autonomy",
                            "Diversity",
                            "<b>Implementation of open science</b>",
                            "Knowledge exchange",

                            "Promotion of young talent",
                            "Availability of human resources/applicant situation",
                            "Material and institutional framework"
                          ), 30)))
}

# Importance of expansion of open science practices

data_plot_importance <- function() {
data <- bss_stata |>
 # filter(a2 %in% x) |>
#  filter(id %in% traits$id[traits$name %in% y]) |>
  select(matches("^o2|a2$")) |>
  filter(o2 != 6) |>
  drop_na(o2) |>
  mutate(a2 = as.numeric(a2)) |>
  mutate(a2 = case_when(a2 == 2 ~ 1,
                        TRUE ~ a2)) |>
  mutate(n = n()) |>
  count(n, a2, o2, name = "nn") |>
  complete(a2, o2, fill = list(nn = 0)) |>
  fill(n, .direction = "updown") |>
  group_by(a2) |>
  mutate(perc = nn/sum(nn)) |>
  ungroup()

data_all <- bss_stata |>
  select(matches("^o2$")) |>
  filter(o2 != 6) |>
  drop_na(o2) |>
  mutate(n = n()) |>
  count(n, o2, name = "nn") |>
  complete(o2, fill = list(nn = 0)) |>
  fill(n, .direction = "updown") |>
  mutate(perc = nn/sum(nn)) |>
  mutate(a2 = 0)


data <- data |>
  bind_rows(data_all) |>
  mutate(o2 = factor(
    o2,
    levels = c(1:5),
    labels = c(
      "not at all important",
      "not very important",
      "somewhat important",
      "fairly important",
      "very important" #, "don't know"
    )
  )) |>
  group_by(a2) |>
  mutate(ngroup = sum(nn)) |>
  ungroup() |>
  mutate(a2 = factor(
    a2,
    levels = c(0, 1, 3, 4),
    labels = paste(c(
      "All status groups",
      "Professors",
      "Scientists with a PhD",
      "Scientists without a PhD"
    ))
  ))

 # col_lik_7 <- c("#852557", "#B07E9F", "#83829E", "#56869C", "#21527B", "#767676")

data |>
  plot_ly(
    x = ~ perc,
    y = ~ a2,
    color = ~ o2,
    text = ~ paste0(round(perc * 100, 0), "%"),
    textposition = "inside",
    insidetextanchor = "middle",
    textangle = 0,
    textfont = list(color = "white", size = 11)
  ) |>
  add_bars(
    data = data |> filter(o2 == "very important"),
    marker = list(color = "#21527B")
  ) |>
  add_bars(
    data = data |> filter(o2 == "fairly important"),
    marker = list(color = "#56869C")
  ) |>
  add_bars(
    data = data |> filter(o2 == "somewhat important"),
    marker = list(color = "#83829E"),
    visible = "legendonly") |>
  add_bars(
    data = data |> filter(o2 == "not very important"),
    marker = list(color = "#B07E9F"),
    visible = "legendonly"
  ) |>
  add_bars(
    data = data |> filter(o2 == "not at all important"),
    marker = list(color = "#852557"),
    visible = "legendonly"
  ) |>
  # add_bars(
  #   data = data |> filter(o2 == "don't know"),
  #   marker = list(color = "#767676"),
  #   visible = "legendonly"
  # ) |>
  add_annotations(
    x = 1,
    y = 0,
    xref = "paper",
    yref = "paper",
    text = ~ glue::glue(
      '<a href="https://www.berlinsciencesurvey.de/en/index.html">Berlin Science Survey 2022</a>, Charité Subsample, n={n}
      Answer option "don\'t know" was removed',
      n = max(n)
    ),
    showarrow = FALSE,
    xanchor = "right",
    yanchor = "auto",
    xshift = 0,
    yshift = -55,
    font = list(size = 11)
  ) |>
  layout(
    barmode = "stack",
    xaxis = list(
      title = FALSE,
      tickformat = ".0%",
      zeroline = FALSE,
      dtick = 0.25,
      tick0 = 0,
      tickmode = "linear",
      gridcolor = "#A0A0A0",
      range = c(-0.01, 1.15)
    ),
    yaxis = list(title = FALSE,
                 autorange = "reversed"),
    legend = list(traceorder = "normal"),
    #   legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", traceorder = "normal"),
    margin = list(b = 65),
    uniformtext = list(minsize = 10, mode = "hide"),
    paper_bgcolor = "#DCE3E5",
    plot_bgcolor = "#DCE3E5",
    shapes = list(
      type = "line",
      x0 = -0.05,
      x1 = 1,
      xref = "paper",
      y0 = 0.5,
      y1 = 0.5,
      line = list(color = "black", dash = "dot")
    )
  ) |>
  config(displayModeBar = FALSE)

}




#----------------------------------------------------------------------------------------------------------------------
# Functions for plotly elements
#----------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------
# Plots for prios and goals
#----------------------------------------------------------------------------------------------------------------------

# Plot prios and goals as combined chart
plot_prio <- function(data) {

# data <- data_prep_prio(1:4, "v6_1")
  # Prepare standard data
  data_standard <- bss_stata |>
    select(matches("^(f1|f2|f3)")) |>
    summarise(across(everything(), ~ mean(., na.rm = TRUE)), n = n()) |>
    pivot_longer(cols = -n, names_to = "question_id") |>
    left_join(label, by = "question_id") |>
    separate(label, into = c("label", "goals"), sep = "]") |>
    mutate(label = str_remove(label, "^\\[")) |>
    mutate(goals = factor(goals,
                          levels = c(
                            " Erwartungsdruck",
                            " Priorisierung",
                            " Wichtigkeit Ziele"
                          ),
                          labels = c(
                            "Pressure of expectations",
                            "Prioritization",
                            "Importance"
                          ))) |>
    relocate(c(value, n), .after = last_col()) |>
    mutate(label = factor(
      label,
      levels = c(
        "Open Science",
        "gute Lehre",
        "Kooperationen",
        "Societal Impact",
        "Publikationsoutput",
        "Originalität der Forschungsergebnisse",
        "Methodische Strenge (Akkuratheit)"
      ),
      labels = str_wrap(c(
        "Open Science",
        "Good teaching",
        "Cooperation with other disciplines (interdisciplinarity)",
        "Social usability of research results/societal impact",
        "Publication output",
        "Originality of research results",
        "Methodical rigour (accuracy)"
      ), 33)
    )) |>
    group_by(label) |>
    mutate(min = min(value, na.rm = TRUE),
           max = max(value, na.rm = TRUE)) |>
    ungroup()


  fig <- plot_ly(colors = c("#440154FF", "#21908CFF", "#440154FF"))

  fig <- fig |>
    add_markers(
      data = data_standard,
      x = ~ value,
      y = ~ label,
      symbol = ~ as.factor(goals),
      color = ~ as.factor(goals),
      # colors = "grey",
      marker = list(
        size = 17,
        opacity = 0.2,
        symbols = c("circle", "triangle-up", "star")
      ),
      showlegend = FALSE,
      hoverinfo = "skip"
    )

  fig <- fig |>
  # plot_ly(colors = c("#440154FF", "#21908CFF", "#440154FF"), symbols = c("circle", "triangle-up", "star")) |>
   add_segments(
     data = data,
     x = ~ min,
     xend = ~ max,
     y = ~ label,
     yend = ~ label,
     line = list(color = "grey", width = 3),
     opacity = 0.3,
     showlegend = FALSE,
     hoverinfo = "skip"
   ) |>
    add_markers(
      data = data,
      x = ~ value,
      y = ~ label,
      symbol = ~ as.factor(goals),
      color = ~ as.factor(goals),
      marker = list(size = 15, opacity = 0.8,
                    symbols = c("circle", "triangle-up", "star")),
      hoverinfo = "text",
      hovertext = ~ str_glue(c("{goals} of {label}
                             {round(value, 1)} (mean value of the four-item scale)"))) |>
    add_annotations(
      x = 1,
      y = 0,
      xref = "paper",
      yref = "paper",
      text = ~ glue::glue("<a href='https://www.berlinsciencesurvey.de/en/index.html'>Berlin Science Survey 2022</a>, Charité Subsample, n={n},
                          faded icons show entire Charité Subsample",
                          n = max(n)),
      showarrow = F,
      xanchor = "right",
      yanchor = "auto",
      xshift = 0,
      yshift = -85,
      font = list(size = 12),
      bordercolor = "#63666A", #aa1c7d
      borderpad = 2,
      borderwidth = 2
    ) |>
    layout(
      xaxis = list(
        title = FALSE,
       # ticktext = sprintf("<b>%s</b>", c("no\npressure/priority/importance", "low", "high", "highest\npressure/priority/importance")),
        ticktext = list("no\npressure/priority/importance", "low", "high", "highest\npressure/priority/importance"),
        tickvals = list("1", "2", "3", "4"),
        range = c(0.5, 4.5),
        showgrid = FALSE
      ),
      yaxis = list(title = FALSE,
                   showgrid = FALSE),
      legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center"),
      margin = list(t = 10, b = 95),
      paper_bgcolor = "#DCE3E5",
      plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)
  fig
}

# Plot prios and goals as likert chart
plot_prio_likert <- function(data) {

  col_lik <- rev(c("#852557", "#B07E9F", "#7DAABE", "#21527B"))

  plot_1 <- data |>
    filter(str_detect(question_id, "^f1_.*")) |>
    plot_ly(
      x = ~ perc,
      y = ~ label,
      color = ~ value,
      colors = col_lik,
      legendgroup = ~ value,
      text = ~ paste0(round(perc * 100, 0), "%"),
      textposition = "inside",
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "white", size = 11)
    ) |>
    add_bars() |>
    add_annotations(
      x = 1,
      y = 0,
      xref = "paper",
      yref = "paper",
      text = ~ glue::glue("<a href='https://www.berlinsciencesurvey.de/en/index.html'>Berlin Science Survey 2022</a>, Charité Subsample, n={n}",
                          n = max(n)),
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "auto",
      xshift = 0,
      yshift = -50,
      font = list(size = 12),
      bordercolor = "#63666A",
      borderpad = 2,
      borderwidth = 2
    ) |>
    layout(barmode = "stack",
           xaxis = list(title = FALSE,
                        tickformat = ".0%",
                        zeroline = FALSE,
                        dtick = 0.5,
                        tick0 = 0,
                        tickmode = "linear",
                        gridcolor = "#A0A0A0",
                        range = c(-0.01, 1.05)))

  plot_2 <- data |>
    filter(str_detect(question_id, "^f2_.*")) |>
    plot_ly(
      x = ~ perc,
      y = ~ label,
      color = ~ value,
      colors = col_lik,
      legendgroup = ~ value,
      showlegend = FALSE,
      text = ~ paste0(round(perc * 100, 0), "%"),
      textposition = "inside",
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "white", size = 11)
    ) |>
    add_bars() |>
    layout(barmode = "stack",
           xaxis = list(title = FALSE,
                        tickformat = ".0%",
                        zeroline = FALSE,
                        dtick = 0.5,
                        tick0 = 0,
                        tickmode = "linear",
                        gridcolor = "#A0A0A0",
                        range = c(-0.01, 1.05)))

  plot_3 <- data |>
    filter(str_detect(question_id, "^f3_.*")) |>
    plot_ly(
      x = ~ perc,
      y = ~ label,
      color = ~ value,
      colors = col_lik,
      legendgroup = ~ value,
      showlegend = FALSE,
      text = ~ paste0(round(perc * 100, 0), "%"),
      textposition = "inside",
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "white", size = 11)
    ) |>
    add_bars() |>
    layout(barmode = "stack",
           xaxis = list(title = FALSE,
                        tickformat = ".0%",
                        zeroline = FALSE,
                        dtick = 0.5,
                        tick0 = 0,
                        tickmode = "linear",
                        gridcolor = "#A0A0A0",
                        range = c(-0.01, 1.05)))

  subplot(plot_2, plot_3, plot_1, shareY = TRUE, shareX = TRUE) |>
    layout(yaxis = list(title = FALSE),
           legend = list(orientation = "h", x = 0.5, y = 1.2, xanchor = "center"),
           annotations = list(
             list(x = 0.1 , y = 1.07, text = "Pressure", showarrow = F, xref='paper', yref='paper'),
             list(x = 0.5 , y = 1.07, text = "Prioritization", showarrow = F, xref='paper', yref='paper'),
             list(x = 0.87 , y = 1.07, text = "Importance", showarrow = F, xref='paper', yref='paper')),
           margin = list(t = 30, b = 60),
           uniformtext = list(minsize = 10, mode = "hide"),
           autosize = TRUE,
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)
}

#----------------------------------------------------------------------------------------------------------------------
# Plots for practices and difficulties
#----------------------------------------------------------------------------------------------------------------------

# Plot practices and difficulties as combined chart
plot_practices <- function(data) {
  ax <- list(
    tickfont = list(color = "#440154FF"),
    overlaying = "x",
    side = "top",
    title = FALSE,
    ticktext = list("very great difficulties", "great", "minor", "no  difficulties"),
    tickvals = list("0", "1", "2", "3"),
    range = c(-0.3, 3),
    zeroline = FALSE,
    anchor = "free",
    position = -0.1,
    showgrid = FALSE)

  data |>
    plot_ly() |>
    add_segments(
      x = ~ practices_applied,
      xend = ~ difficulties_normalized,
      y = ~ os_practice,
      yend = ~ os_practice,
      line = list(color = "grey", width = 3),
      opacity = 0.3,
      showlegend = FALSE,
      hoverinfo = "skip") |>
    add_markers(
      x = ~ practices_applied,
      y = ~ os_practice,
      marker = list(size = 15, opacity = 0.8, symbol = "triangle-up", color = "#21908CFF"),
      name = "os practice applied",
    ) |>
    add_markers(
      x = ~ difficulties_implementing,
      y = ~ os_practice,
      xaxis = "x2",
      marker = list(size = 15, opacity = 0.8, symbol = "circle", color = "#440154FF"),
      name = "difficulties in implementing",
    ) |>
    add_annotations(
      x = 1,
      y = 0,
      xref = "paper",
      yref = "paper",
      text = ~ glue::glue("<a href='https://www.berlinsciencesurvey.de/en/index.html'>Berlin Science Survey 2022</a>, Charité Subsample, n={n}",
                          n = max(n)),
      showarrow = F,
      xanchor = "right",
      yanchor = "auto",
      xshift = 0,
      yshift = -40,
      font = list(size = 11)
    ) |>
    layout(
      xaxis = list(
        title = FALSE,
        tickfont = list(color = "#21908CFF"),
        ticktext = list("never applied", "rarely", "occasionally", "often", "very often", "always applied"),
        tickvals = list("0", "1", "2", "3", "4", "5"),
        range = c(-0.5, 5),
        zeroline = FALSE,
        showgrid = FALSE
      ),
      xaxis2 = ax,
      yaxis = list(title = FALSE,
                   range = c(-1, 3.5),
                   showgrid = FALSE),
      margin = list(t = 50, r = 50, b = 50),
      legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center"),
      autosize = TRUE,
      paper_bgcolor = "#DCE3E5",
      plot_bgcolor = "#DCE3E5"
    ) |>
    config(displayModeBar = FALSE)

}

# Plot practices and difficulties as bar chart
plot_practices_likert <- function(data) {
  plot_1 <- data |>
    filter(value == "practiced") |>
    plot_ly(
      x = ~ perc,
      y = ~ name,
      marker = list(color = "#21527B"),
      #   legendgroup = ~ value,
      text = ~ paste0(round(perc * 100, 0), "%"),
      textposition = "inside",
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "white", size = 11),
      showlegend = FALSE
    ) |>
    add_bars() |>
    layout(
      xaxis = list(
        title = FALSE,
        tickformat = ".0%",
        zeroline = FALSE,
        dtick = 0.25,
        tick0 = 0,
        range = c(-0.01, 0.9)
      )
    )

  plot_2 <- data |>
    filter(value == "difficulties") |>
    plot_ly(
      x = ~ perc,
      y = ~ name,
      marker = list(color = "#852557"),
      #   legendgroup = ~ value,
      text = ~ paste0(round(perc * 100, 0), "%"),
      textposition = "inside",
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "white", size = 11),
      showlegend = FALSE) |>
    add_bars() |>
    add_annotations(
      x = 1,
      y = 0,
      xref = "paper",
      yref = "paper",
      text = ~ glue::glue("<a href='https://www.berlinsciencesurvey.de/en/index.html'>Berlin Science Survey 2022</a>, Charité Subsample, n={n}",
                          n = max(nn)),
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "auto",
      xshift = 0,
      yshift = -40,
      font = list(size = 11)
    ) |>
    layout(xaxis = list(title = FALSE,
                        tickformat = ".0%",
                        zeroline = FALSE,
                        dtick = 0.25,
                        tick0 = 0,
                        range = c(-0.01,0.9)),
           margin = list(b = 50))

  subplot(plot_1, plot_2, shareY = TRUE) |>
    layout(yaxis = list(autorange = "reversed", title = FALSE),
         #  legend = list(orientation = "h", x = 0.5, xanchor = "center"),
           annotations = list(
             list(x = 0 , y = 1.07, text = "often, very often or always practiced", showarrow = F, xref='paper', yref='paper', xanchor = "left",
                  font = list(size = 13
                  )),
             list(x = 0.52 , y = 1.07, text = "great or very great implementation difficulties", showarrow = F, xref='paper', yref='paper', xanchor = "left",
                  font = list(size = 13
                  ))),
           margin = list(t = 30),
           uniformtext = list(minsize = 10, mode = "hide"),
           autosize = TRUE,
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)

}

#----------------------------------------------------------------------------------------------------------------------
# Plots for research environment
#----------------------------------------------------------------------------------------------------------------------

# Plot research environment as likert chart

plot_environment <- function(data) {

  #col_lik <- rev(c("#852557", "#B07E9F", "#7DAABE", "#21527B"))

plot_ly(x = ~ perc,
            y = ~ label,
            color = ~ value,
            text = ~ paste0(round(perc * 100, 0), "%"),
            textposition = "inside",
            insidetextanchor = "middle",
            textangle = 0,
            textfont = list(color = "white", size = 11)) |>
    add_bars(data = data |> filter(value == "very good"),
             marker = list(color = "#21527B")
             #colors = c(col_lik, "#767676"),
             ) |>
  add_bars(data = data |> filter(value == "somewhat good"),
           marker = list(color = "#7DAABE")
           ) |>
  add_bars(data = data |> filter(value == "somewhat bad"),
           marker = list(color = "#B07E9F"),
           visible = "legendonly"
  ) |>
  add_bars(data = data |> filter(value == "very bad"),
           marker = list(color = "#852557"),
           visible = "legendonly"
  ) |>
  # add_bars(data = data |> filter(value == "I cannot judge"),
  #          marker = list(color = "#767676"),
  #          visible = "legendonly"
  # ) |>
  add_annotations(
    x = 1,
    y = 0,
    xref = "paper",
    yref = "paper",
    text = ~ glue::glue('<a href="https://www.berlinsciencesurvey.de/en/index.html">Berlin Science Survey 2022</a>, Charité Subsample, n={n}
                        Answer option "I cannot judge" was removed',
                        n = max(n)),
    showarrow = FALSE,
    xanchor = "right",
    yanchor = "auto",
    xshift = 0,
    yshift = -55,
    font = list(size = 11)
  ) |>
    layout(barmode = "stack",
           xaxis = list(title = FALSE,
                        tickformat = ".0%",
                        zeroline = FALSE,
                        dtick = 0.25,
                        tick0 = 0,
                        tickmode = "linear",
                        gridcolor = "#A0A0A0",
                        range = c(-0.01, 1.05)),
           yaxis = list(title = FALSE,
                        autorange = "reversed"),
           legend = list(traceorder = "normal"), #orientation = "h", x = 0.5, y = 1.1, xanchor = "center",
           margin = list(b = 65),
           uniformtext = list(minsize = 10, mode = "hide"),
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)

}

#----------------------------------------------------------------------------------------------------------------------
# Shiny modules
#----------------------------------------------------------------------------------------------------------------------

# UI module for balancing scientific goals

moduleUI_status_research_prio <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
    h2(strong("Prioritizing scientific goals in the field of tension between external expectations and self-ascribed importance"), align = "left"),
    )),
    fluidRow(column(
      4,
      selectInput(
        ns("module_status"),
        "Status groups",
        choices = list(
          "All" = "all",
          "Professors" = "prof",
          "Scientists with a PhD" = "postdoc",
          "Scientists without a PhD" = "praedoc"
        ),
        selected = FALSE,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    ),
    column(
      4,
      selectInput(
        ns("module_research"),
        "Research characteristics",
        choices = list(
          "All" = "all",
          "Research characteristics" = list(
            "Theoretical/conceptual" = "theory",
            "Empirical" = "empiric",
            "Takes place in teams" = "teams",
            "Experimental/hypothesis testing" = "experiment",
            "Follows a long-term research agenda" = "agenda",
            "Basic research" = "basic",
            "Characterized by competition with other research groups working on the same topic" = "competition",
            "Relies on technical infrastructure" = "infra"
          )
        ),
        selected = FALSE,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ))
    ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        )))),
        # fluidRow(column(12, align = "left", h4(strong(
        #   "Regarding these goals, how much do you feel a pressure of expectations in your scientific work?"
        # )))),
        # fluidRow(column(12, align = "left", h4(strong(
        #   "How do you prioritize these goals in your own work?"
        # )))),
        h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", textOutput(ns("module_number_prio"))),
        h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", textOutput(ns("module_text"))),
        plotlyOutput(ns("module_plot"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/text_BSS_prio.md")
    )),
    fluidRow(
      (column(6,
              checkboxInput(ns("checkbox"), label = "Show likert scale chart", value = FALSE)
      ))
    )
  )
}


# UI module for implementations and difficulties

moduleUI_status_research_practices <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    h2(strong("Open science practices and implementation difficulties"), align = "left"),
    fluidRow(column(
      4,
      selectInput(
        ns("module_status"),
        "Status groups",
        choices = list(
          "All" = "all",
          "Professors" = "prof",
          "Scientists with a PhD" = "postdoc",
          "Scientists without a PhD" = "praedoc"
        ),
        selected = FALSE,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    ),
    column(
      4,
      selectInput(
        ns("module_research"),
        "Research characteristics",
        choices = list(
          "All" = "all",
          "Research characteristics" = list(
            "Theoretical/conceptual" = "theory",
            "Empirical" = "empiric",
            "Takes place in teams" = "teams",
            "Experimental/hypothesis testing" = "experiment",
            "Follows a long-term research agenda" = "agenda",
            "Basic research" = "basic",
            "Characterized by competition with other research groups working on the same topic" = "competition",
            "Relies on technical infrastructure" = "infra"
          )
        ),
        selected = FALSE,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    )),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        fluidRow(
          column(12, align = "left", h4(strong(
          "How often have you applied the following open science practices in your research? – Do you see difficulties implementing the following open science practices in your current research practice?"
        )))
        ),
        h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", textOutput(ns("module_number_practices"))),
        h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", textOutput(ns("module_text_practices"))),
        plotlyOutput(ns("module_plot_practices"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/text_BSS_practices.md")
    )),
    # fluidRow(
    #   (column(6,
    #           checkboxInput(ns("checkbox"), label = "Show combined chart", value = FALSE)
    #   ))
    # )

  )
}

# UI module for implementations and difficulties

moduleUI_status_research_environment <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    h2(strong("Research environment"), align = "left"),
    fluidRow(column(
      4,
      selectInput(
        ns("module_status"),
        "Status groups",
        choices = list(
          "All" = "all",
          "Professors" = "prof",
          "Scientists with a PhD" = "postdoc",
          "Scientists without a PhD" = "praedoc"
        ),
        selected = FALSE,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    ),
    column(
      4,
      selectInput(
        ns("module_research"),
        "Research characteristics",
        choices = list(
          "All" = "all",
          "Research characteristics" = list(
            "Theoretical/conceptual" = "theory",
            "Empirical" = "empiric",
            "Takes place in teams" = "teams",
            "Experimental/hypothesis testing" = "experiment",
            "Follows a long-term research agenda" = "agenda",
            "Basic research" = "basic",
            "Characterized by competition with other research groups working on the same topic" = "competition",
            "Relies on technical infrastructure" = "infra"
          )
        ),
        selected = FALSE,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    )),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        fluidRow(column(12, align = "left", h4(strong(
          "How would you rate the Berlin research environment with regard to the following aspects?"
        )))),
        h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", textOutput(ns("module_number_environment"))),
        h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", textOutput(ns("module_text_environment"))),
        plotlyOutput(ns("module_plot_environment"), height = "500px")
      )
    ),
    column(
      4, includeMarkdown("texts/text_BSS_environment.md")
    ))
  )
}


moduleUI_status_research_importance <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    h2(strong("Importance of the expansion of open science"), align = "left"),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        fluidRow(column(12, align = "left", h4(strong(
          "How important is the expansion of open science practices for science as a whole?"
        )))),
        h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "86%"),
        h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", "of Charité scientists consider the expansion of open science practices fairly important or very important"),
        data_plot_importance()
      )
    ),
    column(
      4, includeMarkdown("texts/text_BSS_importance.md")
    ))
  )
}
# Test plot
# moduleServer_plot <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       output$module_plot <- renderPlotly({
#
#         x = c("A", "B")
#         y = c(1, 2)
#
#         plot_ly(
#           x = ~ x,
#           y = ~ y
#         )
#
#       })
#     }
#   )
# }

# Server module
moduleServer_plot <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      # Function for dropdown selection of status and research characteristics
      status_research <- function() {
        if (input$module_status == "all") {
          x <<- 1:4
        } else if (input$module_status == "prof") {
          x <<- 1:2
        } else if (input$module_status == "postdoc") {
          x <<- 3
        } else {
          x <<- 4
        }

        if (input$module_research == "all") {
          y <<- "all"
          research <<- "Alle"
        } else if (input$module_research == "theory") {
          y <<- "v6_1"
          research <- "Theoretisch"
        } else if (input$module_research == "empiric") {
          y <<- "v6_2"
          research <<- "Empirisch"
        } else if (input$module_research == "teams") {
          y <<- "v6_3"
          research <<- "Teams"
        } else if (input$module_research == "experiment") {
          y <<- "v6_5"
          research <<- "Experimentell"
        } else if (input$module_research == "agenda") {
          y <<- "v6_6"
          research <<- "langfristige Forschungsagenda"
        } else if (input$module_research == "basic") {
          y <<- "v6_7"
          research <<- "Grundlagenforschung"
        } else if (input$module_research == "competition") {
          y <<- "v6_8"
          research <<- "geprägt von Wettbewerb"
        } else if (input$module_research == "infra") {
          y <<- "v6_9"
          research <<- "angewiesen auf technische Infrastrukturen"
        } else {
          x <<- paste0("v6_", c(1:9))
          research <<- "Alle"
        }
      }

      # Outputs

      output$module_text <- renderText({
        if (input$module_status == "all") {
          return("of Charité scientists attribute high or highest importance to open science")
        } else if (input$module_status == "prof") {
          return("of Charité professors give high or highest priority to open science")
        } else if (input$module_status == "postdoc") {
          return("of Charité scientists with a PhD give high or highest priority to open science")
        } else {
          return("of Charité scientists without a PhdD give high or highest priority to open science")
        }
      })

      output$module_number_prio <- renderText({
        if (input$module_status == "all") {
          return("42 %")
        } else if (input$module_status == "prof") {
          return("41 %")
        } else if (input$module_status == "postdoc") {
          return("37 %")
        } else {
          return("51 %")
        }

      })

      output$module_plot <- renderPlotly({

        status_research()

        if (input$checkbox == FALSE) {

          data <- data_prep_prio(x, y)
          # Draw chart
          plot_prio(data)

        } else {

          data <- data_prep_prio_likert(x, y)

          plot_prio_likert(data)

        }

      })

      output$module_number_practices <- renderText({
        if (input$module_status == "all") {
          return("25 %")
        } else if (input$module_status == "prof") {
          return("25 %")
        } else if (input$module_status == "postdoc") {
          return("27 %")
        } else {
          return("21 %")
        }

      })

      output$module_text_practices <- renderText({
        if (input$module_status == "all") {
          return("of Charité scientists routinely share their data")
        } else if (input$module_status == "prof") {
          return("of Charité professors routinely share their data")
        } else if (input$module_status == "postdoc") {
          return("of Charité scientists with a PhD routinely share their data")
        } else {
          return("of Charité scientists without a PhD routinely share their data")
        }

      })

      output$module_plot_practices <- renderPlotly({

        status_research()

        data <- data_prep_practices_likert(x, y)
        plot_practices_likert(data)

        # Combined chart is removed because it is too confusing
        # if (input$checkbox == FALSE) {
        #
        #   data <- data_prep_practices_likert(x, y)
        #
        #   plot_practices_likert(data)
        #
        # } else {
        #
        #   data <- data_prep_practices(x, y)
        #   # Draw chart
        #   plot_practices(data)
        #
        # }

      })


      output$module_text_environment <- renderText({
        if (input$module_status == "all") {
          return("of Charité scientists rate the implementation of open science as somewhat good or very good")
        } else if (input$module_status == "prof") {
          return("of Charité professors rate the implementation of open science as somewhat good or very good")
        } else if (input$module_status == "postdoc") {
          return("of Charité scientists with a PhD rate the implementation of open science as somewhat good or very good")
        } else {
          return("of Charité scientists without a PhD rate the implementation of open science as somewhat good or very good")
        }

      })


      output$module_number_environment <- renderText({
        if (input$module_status == "all") {
          return("65 %")
        } else if (input$module_status == "prof") {
          return("77 %")
        } else if (input$module_status == "postdoc") {
          return("66 %")
        } else {
          return("58 %")
        }

      })

      output$module_plot_environment <- renderPlotly({

        status_research()

        data <- data_prep_environment(x, y)
        # Draw chart
        plot_environment(data)

      })
    }
  )
}


#----------------------------------------------------------------------------------------------------------------------
# Shiny UI ----
#----------------------------------------------------------------------------------------------------------------------

bss_panel <-
  tabPanel("Berlin Science Survey", value = "tabBSS",
           wellPanel(
             br(),
             fluidRow(
               column(8,
                      h1(style = "margin-left:0cm", strong("Berlin Science Survey (Charité Subsample)"), align = "left"),
                      h4(style = "margin-left:0cm",
                         HTML("What are the views of Charité researchers on open science? How do they implement open science in their own work? Which difficulties do they face? How important do they think open science is as a goal? And do they prioritize open science practices themselves? These are questions, which the <a href='https://www.berlinsciencesurvey.de/en/index.html'>Berlin Science Survey (BSS)</a> set out to answer for the Berlin metropolitan area.")),
                      h4(style = "margin-left:0cm",
                         HTML("The BSS is a long-term online survey of scientists, designed as a trend study to be repeated every two years. It is conducted by the <a href='https://www.rmz.hu-berlin.de/en'>Robert K. Merton Center for Science Studies</a> and funded by the Berlin University Alliance. The BSS is a multi-topic survey, also including aspects of research integrity, scientific collaboration, and knowledge transfer.")),
                      h4(style = "margin-left:0cm",
                         HTML("<b>This dashboard presents only survey results relating to open science, and only responses from Charité researchers, which constitute a subsample of the overall BSS participants.</b>"))
               ),
               column(4,
                      hr(),
                      br(),
                      br(),
                      actionButton(inputId = 'buttonMethodsBSS',
                                   label = 'See methods',
                                   style = "color: white; background-color: #aa1c7d;"),
                      actionButton(inputId = 'buttonDatasetBSS',
                                   label = 'See dataset',
                                   style = "color: white; background-color: #aa1c7d;")
               ),
             )
           ),

           moduleUI_status_research_prio("prio"),
           moduleUI_status_research_practices("practices"),
           moduleUI_status_research_environment("environment"),
           moduleUI_status_research_importance("importance"),

           br(),
           br(),
           br(),
           hr(),
           bsCollapsePanel(strong("Impressum"),
                           impressum_text,
                           style = "default"),
           bsCollapsePanel(strong("Datenschutz"),
                           datenschutz_text,
                           style = "default")
  )

#----------------------------------------------------------------------------------------------------------------------
# End ----
#----------------------------------------------------------------------------------------------------------------------

