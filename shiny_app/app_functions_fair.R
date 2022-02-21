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

# make_fair_principle_plot_data(plot_data)

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

# plot_data <- read_csv("shiny_app/data/fair_assessment.csv")
# make_fair_treemap_plot_data(plot_data)
# paste0(plot_data$fuji_percent %>% mean(na.rm = TRUE) %>% round(0), "%")
