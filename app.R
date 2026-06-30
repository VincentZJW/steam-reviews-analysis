# Steam Reviews Monitor - 本地交互式 Dashboard

required_packages <- c(
  "shiny", "bslib", "dplyr", "readr", "stringr", "lubridate", "ggplot2",
  "plotly", "DT", "scales", "tidyr", "slider", "httr2", "jsonlite",
  "tibble", "htmltools", "ggwordcloud", "stringi"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop("请先安装缺失 R 包: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(DT)
})

source("R/steam_api.R")
source("R/fetch_reviews.R")
source("R/clean_reviews.R")
source("R/summarize_reviews.R")
source("R/text_analysis.R")
source("R/game_search.R")
source("R/alert_rules.R")
source("R/dashboard_helpers.R")

cache_dir <- "data/cache"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

empty_reviews_result <- function(message = "Search and load a Steam game to begin.") {
  list(
    reviews = empty_dashboard_reviews(),
    cached = NA,
    cache_file = NA_character_,
    fetched_at = NA,
    message = message,
    warning = NULL,
    language_status = tibble::tibble(),
    requested_languages = character(),
    language_choice = NA_character_,
    error = NULL
  )
}

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "darkly",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter"),
  primary = "#66C0F4",
  secondary = "#A4B1CD",
  success = "#5CC38A",
  danger = "#FF6B6B",
  warning = "#F2C94C",
  bg = "#0A0F16",
  fg = "#E8EEF8"
)

app_css <- "
:root {
  --steam-panel: #111927;
  --steam-panel-2: #162132;
  --steam-border: rgba(164, 177, 205, 0.18);
  --steam-muted: #95A3B8;
  --steam-blue: #66C0F4;
  --steam-green: #5CC38A;
  --steam-red: #FF6B6B;
}

body { letter-spacing: 0; }

.sidebar {
  background: linear-gradient(180deg, #0D1520 0%, #101827 100%);
  border-right: 1px solid var(--steam-border);
}

.app-header {
  display: grid;
  grid-template-columns: minmax(260px, 1fr) repeat(3, minmax(150px, 220px));
  gap: 12px;
  align-items: stretch;
  margin-bottom: 14px;
}

.header-main,
.header-pill,
.metric-tile,
.summary-panel {
  background: linear-gradient(180deg, var(--steam-panel) 0%, var(--steam-panel-2) 100%);
  border: 1px solid var(--steam-border);
  border-radius: 8px;
  box-shadow: 0 18px 40px rgba(0, 0, 0, 0.20);
}

.header-main { padding: 18px 20px; }
.header-title { margin: 0; font-size: 1.45rem; font-weight: 750; }
.header-subtitle { margin-top: 6px; font-size: 0.92rem; color: var(--steam-muted); }
.header-pill { padding: 14px 16px; }
.header-label,
.metric-label,
.source-note {
  color: var(--steam-muted);
}

.header-label {
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
}

.header-value { margin-top: 8px; font-weight: 700; font-size: 1rem; }

.metric-grid {
  display: grid;
  grid-template-columns: repeat(4, minmax(150px, 1fr));
  gap: 12px;
}

.overview-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(260px, 1fr));
  gap: 12px;
  margin-top: 14px;
}

.metric-tile { padding: 16px; min-height: 104px; }
.metric-value { font-size: 1.8rem; font-weight: 780; line-height: 1.1; margin-top: 10px; }
.metric-accent-blue { color: var(--steam-blue); }
.metric-accent-green { color: var(--steam-green); }
.metric-accent-red { color: var(--steam-red); }

.summary-panel {
  margin-top: 14px;
  padding: 16px 18px;
  color: #D8E2F2;
}

.card { border-radius: 8px; border-color: var(--steam-border); }
.form-label { color: #D8E2F2; font-weight: 650; }
.source-note { font-size: 0.82rem; line-height: 1.45; }
.summary-panel pre {
  color: #D8E2F2;
  background: transparent;
  border: 0;
  padding: 0;
  margin: 8px 0 0;
  white-space: pre-wrap;
}
.dataTables_wrapper { color: #E8EEF8; }
table.dataTable tbody td { vertical-align: top; }
.review-cell { max-width: 680px; white-space: normal; line-height: 1.35; }

.trend-controls {
  display: grid;
  grid-template-columns: repeat(5, minmax(150px, 1fr));
  gap: 12px;
  align-items: end;
}

@media (max-width: 1000px) {
  .app-header, .trend-controls, .overview-grid { grid-template-columns: 1fr; }
  .metric-grid { grid-template-columns: repeat(2, minmax(140px, 1fr)); }
}

@media (max-width: 620px) {
  .metric-grid { grid-template-columns: 1fr; }
}
"

empty_plotly <- function(message = "No data available under current filters.") {
  plotly::plot_ly(type = "scatter", mode = "markers") |>
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        list(
          text = message,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(color = "#95A3B8", size = 14)
        )
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}

empty_ggplot <- function(message = "No keyword result for the current game.") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, color = "#95A3B8", size = 4) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#111927", color = NA))
}

filter_reviews_for_language <- function(reviews, language_filter) {
  if (is.null(reviews) || nrow(reviews) == 0 || identical(language_filter, "Both")) {
    return(reviews)
  }

  label <- dashboard_language_label(dashboard_language_options(language_filter))
  reviews |>
    dplyr::filter(language %in% label)
}

fallback_keyword_counts <- function(reviews,
                                    language,
                                    top_n = 20,
                                    sentiment_scope = c("negative", "all", "positive")) {
  sentiment_scope <- match.arg(sentiment_scope)
  empty <- tibble::tibble(keyword = character(), n = integer(), review_share = numeric(), scope = character())

  if (is.null(reviews) || nrow(reviews) == 0) {
    return(empty)
  }

  scoped <- reviews |>
    dplyr::filter(.data$language == .env$language) |>
    filter_reviews_by_sentiment(sentiment_scope)

  if (nrow(scoped) == 0) {
    return(empty)
  }

  if (identical(language, "English")) {
    tokens <- unlist(tokenize_english_reviews(scoped$review_clean), use.names = FALSE)
    stopwords <- english_stopwords()
    tokens <- tokens[nchar(tokens) >= 3 & !tokens %in% stopwords]
  } else {
    tokens <- unlist(segment_chinese_reviews(scoped$review_clean), use.names = FALSE)
    stopwords <- chinese_stopwords()
    tokens <- tokens[nchar(tokens) >= 2 & !tokens %in% stopwords]
  }

  tibble::tibble(keyword = tokens) |>
    dplyr::filter(!is.na(keyword), keyword != "") |>
    dplyr::count(keyword, sort = TRUE) |>
    dplyr::mutate(
      review_share = n / max(1, nrow(scoped)),
      scope = paste0(sentiment_scope, "_reviews")
    ) |>
    dplyr::slice_head(n = top_n)
}

semantic_keywords_with_fallback <- function(reviews,
                                            language,
                                            top_n = 20,
                                            sentiment_scope = c("negative", "all", "positive")) {
  sentiment_scope <- match.arg(sentiment_scope)

  data <- safe_semantic_keywords(
    reviews,
    language,
    top_n = top_n,
    sentiment_scope = sentiment_scope
  )

  if (nrow(data) >= 5 || (identical(sentiment_scope, "positive") && nrow(data) > 0)) {
    return(data)
  }

  fallback <- fallback_keyword_counts(
    reviews,
    language = language,
    top_n = top_n,
    sentiment_scope = sentiment_scope
  )

  if (nrow(fallback) > nrow(data)) {
    return(fallback)
  }

  data
}

keyword_language_label <- function(selection) {
  if (identical(selection, "简体中文")) {
    return("Chinese")
  }

  if (identical(selection, "Both")) {
    return("Both")
  }

  "English"
}

keyword_language_set <- function(selection) {
  label <- keyword_language_label(selection)

  if (identical(label, "Both")) {
    return(c("English", "Chinese"))
  }

  label
}

trend_x_col <- function(granularity) {
  switch(
    granularity,
    "Daily" = "review_date",
    "Weekly" = "review_week",
    "Monthly" = "review_month_date",
    "review_date"
  )
}

plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#111927", color = NA),
      panel.background = ggplot2::element_rect(fill = "#111927", color = NA),
      panel.grid.major = ggplot2::element_line(color = "rgba(164,177,205,0.16)"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(color = "#E8EEF8", face = "bold"),
      axis.title = ggplot2::element_text(color = "#A4B1CD"),
      axis.text = ggplot2::element_text(color = "#A4B1CD"),
      legend.position = "top",
      legend.title = ggplot2::element_text(color = "#A4B1CD"),
      legend.text = ggplot2::element_text(color = "#D8E2F2")
    )
}

make_main_trend_plot <- function(summary,
                                 granularity,
                                 metric,
                                 language_filter,
                                 show_rolling,
                                 min_daily_reviews) {
  summary <- filter_reviews_for_language(summary, language_filter)

  if (is.null(summary) || nrow(summary) == 0) {
    return(empty_plotly("Load a game or adjust filters to view trends."))
  }

  x_col <- trend_x_col(granularity)
  x_label <- switch(granularity, "Daily" = "Date", "Weekly" = "Week", "Monthly" = "Month")

  if (identical(metric, "Review Volume")) {
    plot_data <- summary
    y_col <- "total_reviews"

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[[x_col]],
        y = .data[[y_col]],
        color = language,
        text = paste0(
          x_label, ": ", .data[[x_col]],
          "<br>Language: ", language,
          "<br>Total reviews: ", scales::comma(total_reviews),
          "<br>Negative rate: ", scales::percent(negative_rate, accuracy = 0.1)
        )
      )
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 1.8, alpha = 0.8) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::labs(title = paste(granularity, "Review Volume"), x = NULL, y = "Reviews", color = "Language") +
      ggplot2::scale_color_manual(values = c("Chinese" = "#66C0F4", "English" = "#F2994A")) +
      plot_theme()

    return(plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
  }

  if (identical(metric, "Negative Review Rate")) {
    plot_data <- summary

    if (identical(granularity, "Daily")) {
      plot_data <- plot_data |>
        dplyr::filter(total_reviews >= min_daily_reviews)
    }

    y_col <- dplyr::case_when(
      show_rolling && identical(granularity, "Daily") && "negative_rate_7d" %in% names(plot_data) ~ "negative_rate_7d",
      show_rolling && identical(granularity, "Weekly") && "negative_rate_4w" %in% names(plot_data) ~ "negative_rate_4w",
      TRUE ~ "negative_rate"
    )

    plot_data <- plot_data |>
      dplyr::filter(!is.na(.data[[y_col]]))

    if (nrow(plot_data) == 0) {
      return(empty_plotly("No negative-rate trend after current filters."))
    }

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[[x_col]],
        y = .data[[y_col]],
        color = language,
        text = paste0(
          x_label, ": ", .data[[x_col]],
          "<br>Language: ", language,
          "<br>Total reviews: ", scales::comma(total_reviews),
          "<br>Negative reviews: ", scales::comma(negative_reviews),
          "<br>Negative rate: ", scales::percent(.data[[y_col]], accuracy = 0.1)
        )
      )
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 1.8, alpha = 0.8) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(title = paste(granularity, "Negative Review Rate"), x = NULL, y = "Negative Rate", color = "Language") +
      ggplot2::scale_color_manual(values = c("Chinese" = "#66C0F4", "English" = "#F2994A")) +
      plot_theme()

    return(plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
  }

  sentiment_data <- summary |>
    dplyr::select(language, dplyr::all_of(x_col), positive_reviews, negative_reviews) |>
    tidyr::pivot_longer(
      c(positive_reviews, negative_reviews),
      names_to = "sentiment",
      values_to = "reviews"
    ) |>
    dplyr::mutate(
      sentiment = dplyr::recode(
        sentiment,
        positive_reviews = "Positive",
        negative_reviews = "Negative"
      ),
      series = paste(language, sentiment)
    )

  p <- ggplot2::ggplot(
    sentiment_data,
    ggplot2::aes(
      x = .data[[x_col]],
      y = reviews,
      color = series,
      text = paste0(
        x_label, ": ", .data[[x_col]],
        "<br>Series: ", series,
        "<br>Reviews: ", scales::comma(reviews)
      )
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.6, alpha = 0.8) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = paste(granularity, "Positive vs Negative Reviews"), x = NULL, y = "Reviews", color = NULL) +
    plot_theme()

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

ui <- bslib::page_sidebar(
  title = "Steam Reviews Monitor",
  theme = theme,
  fillable = TRUE,
  tags$head(tags$style(HTML(app_css))),
  sidebar = bslib::sidebar(
    width = 320,
    open = TRUE,
    tags$div(class = "source-note", "Steam Store Reviews API · cache separated by appid/language/type/pages"),
    textInput("game_query", "Game search", value = "", placeholder = "e.g. Overwatch 2, Dota 2"),
    actionButton("search_game", "Search games", class = "btn-primary w-100"),
    uiOutput("search_status"),
    uiOutput("game_choice_ui"),
    actionButton("load_game_btn", "Load selected game", class = "btn-success w-100"),
    hr(),
    radioButtons("fetch_language", "Fetch language", choices = c("Both", "English", "简体中文"), selected = "Both"),
    radioButtons("fetch_review_type", "Fetch review type", choices = c("All", "Positive", "Negative"), selected = "All"),
    selectInput("purchase_type", "Purchase type", choices = c("All" = "all", "Steam purchases" = "steam", "Non-Steam purchases" = "non_steam_purchase"), selected = "all"),
    sliderInput("fetch_pages", "Fetch pages per language", min = 1, max = 100, value = 20, step = 1),
    selectInput("date_preset", "Date range preset", choices = c("From 2024" = "from_2024", "Last 1 year" = "last_year", "Last 6 months" = "last_6_months", "Custom" = "custom"), selected = "from_2024"),
    dateRangeInput("fetch_date_range", "Review date range", start = as.Date("2024-01-01"), end = Sys.Date(), max = Sys.Date()),
    checkboxInput("use_cache", "Use cache", value = TRUE),
    checkboxInput("force_refresh", "Force live refresh", value = FALSE),
    numericInput("cache_hours", "Cache valid hours", value = 6, min = 1, max = 72, step = 1),
    hr(),
    numericInput("alert_min_reviews", "Alert min daily reviews", value = 10, min = 1, max = 1000, step = 1),
    numericInput("alert_window", "Alert rolling window", value = 7, min = 3, max = 30, step = 1),
    selectInput("alert_metric", "Alert metric", choices = c("Negative Rate", "Negative Review Count", "Review Volume Spike"), selected = "Negative Rate"),
    selectInput("alert_k", "Threshold multiplier", choices = c("1.0" = 1, "1.5" = 1.5, "2.0" = 2), selected = 1.5)
  ),
  tags$div(
    class = "app-header",
    tags$div(
      class = "header-main",
      tags$h1(class = "header-title", "Steam Reviews Monitor"),
      tags$div(class = "header-subtitle", "Live review signals bound to the currently selected Steam appid.")
    ),
    tags$div(class = "header-pill", tags$div(class = "header-label", "Selected Game"), tags$div(class = "header-value", textOutput("header_game", inline = TRUE))),
    tags$div(class = "header-pill", tags$div(class = "header-label", "App ID"), tags$div(class = "header-value", textOutput("header_appid", inline = TRUE))),
    tags$div(class = "header-pill", tags$div(class = "header-label", "Updated"), tags$div(class = "header-value", textOutput("header_updated", inline = TRUE)))
  ),
  bslib::navset_card_tab(
    full_screen = TRUE,
    bslib::nav_panel(
      "Overview",
      tags$div(class = "metric-grid", uiOutput("overview_cards")),
      tags$div(class = "summary-panel", htmlOutput("overview_summary")),
      tags$div(
        class = "overview-grid",
        bslib::card(bslib::card_header("Summary"), DTOutput("overview_summary_table")),
        bslib::card(bslib::card_header("Sentiment Distribution"), plotlyOutput("overview_sentiment_plot", height = 300)),
        bslib::card(bslib::card_header("Language Distribution"), plotlyOutput("overview_language_plot", height = 300)),
        bslib::card(bslib::card_header("Monthly Preview"), plotlyOutput("overview_monthly_plot", height = 300))
      ),
      tags$div(
        class = "summary-panel",
        tags$strong("Language Debug Summary"),
        verbatimTextOutput("language_debug_summary")
      ),
      tags$div(
        class = "summary-panel",
        tags$strong("Data Source Debug"),
        verbatimTextOutput("data_source_debug")
      )
    ),
    bslib::nav_panel(
      "Reviews",
      bslib::card(
        bslib::card_header("Review Details"),
        DTOutput("reviews_table")
      )
    ),
    bslib::nav_panel(
      "Trends",
      bslib::card(
        bslib::card_header("Trend Controls"),
        tags$div(
          class = "trend-controls",
          selectInput("trend_granularity", "Time granularity", c("Daily", "Weekly", "Monthly"), selected = "Daily"),
          selectInput("trend_metric", "Metric", c("Review Volume", "Negative Review Rate", "Positive vs Negative Reviews"), selected = "Review Volume"),
          selectInput("trend_language", "Language", c("Both", "English", "简体中文"), selected = "Both"),
          checkboxInput("show_rolling", "Show rolling average", value = TRUE),
          numericInput("min_daily_reviews", "Minimum daily reviews", value = 10, min = 1, max = 1000, step = 1)
        )
      ),
      bslib::card(
        bslib::card_header("Trend"),
        plotlyOutput("main_trend_plot", height = "600px")
      )
    ),
    bslib::nav_panel(
      "Keywords & Word Cloud",
      bslib::card(
        bslib::card_header("Keyword Controls"),
        tags$div(
          class = "trend-controls",
          selectInput("keyword_subset", "Review subset", c("Negative", "All", "Positive"), selected = "Negative"),
          numericInput("keyword_top_n", "Top N", value = 20, min = 5, max = 50, step = 5),
          selectInput("keyword_language", "Keyword language", c("English", "简体中文", "Both"), selected = "English")
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Semantic Keywords"), plotOutput("keywords_bar", height = 460)),
        bslib::card(bslib::card_header("Word Cloud"), plotOutput("keywords_wordcloud", height = 460))
      ),
      tags$div(
        class = "summary-panel",
        tags$strong("Keyword Debug Summary"),
        verbatimTextOutput("keyword_debug_summary")
      )
    ),
    bslib::nav_panel(
      "Alerts",
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(bslib::card_header("Negative Rate Alerts"), plotlyOutput("alerts_plot", height = 440)),
        bslib::card(bslib::card_header("Alert Days"), DTOutput("alerts_table"))
      ),
      bslib::card(
        bslib::card_header("Negative Reviews During Alert Days"),
        DTOutput("alert_reviews_table")
      )
    )
  )
)

server <- function(input, output, session) {
  search_results <- reactiveVal(tibble::tibble(appid = integer(), name = character(), match_score = numeric()))
  search_message <- reactiveVal(NULL)
  current_reviews_result <- reactiveVal(empty_reviews_result())

  observeEvent(input$date_preset, {
    end_date <- Sys.Date()
    start_date <- switch(
      input$date_preset,
      "last_year" = end_date - 365,
      "last_6_months" = end_date - 183,
      "custom" = input$fetch_date_range[1] %||% as.Date("2024-01-01"),
      as.Date("2024-01-01")
    )

    if (!identical(input$date_preset, "custom")) {
      updateDateRangeInput(session, "fetch_date_range", start = start_date, end = end_date, max = end_date)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fetch_language, {
    choices <- switch(
      input$fetch_language %||% "Both",
      "English" = "English",
      "简体中文" = "简体中文",
      c("English", "简体中文", "Both")
    )

    selected <- input$keyword_language %||% choices[[1]]

    if (!selected %in% choices) {
      selected <- choices[[1]]
    }

    updateSelectInput(session, "keyword_language", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  observeEvent(input$search_game, {
    query <- stringr::str_squish(input$game_query %||% "")

    if (query == "") {
      search_message(list(type = "warning", text = "Please enter a game name."))
      search_results(tibble::tibble(appid = integer(), name = character(), match_score = numeric()))
      return()
    }

    withProgress(message = "Searching Steam Store", value = 0.4, {
      results <- tryCatch(
        search_steam_games(query, cache_dir = cache_dir, max_results = 20, use_cache = TRUE),
        error = function(e) {
          search_message(list(type = "danger", text = conditionMessage(e)))
          tibble::tibble(appid = integer(), name = character(), match_score = numeric())
        }
      )

      if (nrow(results) == 0) {
        search_message(list(type = "warning", text = "No matching Steam games found."))
      } else {
        search_message(list(type = "success", text = paste0(nrow(results), " candidate games found. Select one to load data.")))
      }

      search_results(results)
    })
  })

  output$search_status <- renderUI({
    msg <- search_message()

    if (is.null(msg)) {
      return(NULL)
    }

    class <- switch(
      msg$type,
      success = "alert alert-success",
      warning = "alert alert-warning",
      danger = "alert alert-danger",
      "alert alert-info"
    )

    tags$div(class = class, role = "alert", msg$text)
  })

  output$game_choice_ui <- renderUI({
    results <- search_results()

    if (nrow(results) == 0) {
      return(NULL)
    }

    choices <- stats::setNames(as.character(results$appid), paste0(results$name, " (", results$appid, ")"))
    selectInput("game_choice", "Game candidates", choices = choices, selected = choices[[1]])
  })

  selected_game <- reactive({
    results <- search_results()

    if (nrow(results) == 0 || is.null(input$game_choice) || input$game_choice == "") {
      return(list(appid = NA_integer_, name = NA_character_))
    }

    selected_appid <- suppressWarnings(as.integer(input$game_choice))
    selected <- results |>
      dplyr::filter(appid == selected_appid) |>
      dplyr::slice_head(n = 1)

    if (nrow(selected) == 0) {
      return(list(appid = NA_integer_, name = NA_character_))
    }

    list(appid = selected$appid[[1]], name = selected$name[[1]])
  })

  selected_appid <- reactive({
    selected_game()$appid
  })

  observeEvent(input$game_choice, {
    current_reviews_result(empty_reviews_result("Click Load selected game to fetch reviews for the selected appid."))
  }, ignoreInit = TRUE)

  observeEvent(input$load_game_btn, {
    if (is.na(selected_appid())) {
      showNotification("Please search and select a Steam game first.", type = "warning")
      return()
    }

    game <- selected_game()
    review_type <- dashboard_review_type(input$fetch_review_type %||% "All")
    date_range <- dashboard_date_range(input$fetch_date_range[1], input$fetch_date_range[2])

    result <- withProgress(message = paste("Loading reviews for", game$name), value = 0.2, {
      fetch_reviews_for_dashboard(
        appid = game$appid,
        game_name = game$name,
        language_choice = input$fetch_language %||% "Both",
        review_type = review_type,
        purchase_type = input$purchase_type %||% "all",
        max_pages = input$fetch_pages %||% 1,
        start_date = date_range$start_date,
        end_date = date_range$end_date,
        cache_dir = cache_dir,
        cache_hours = input$cache_hours %||% 6,
        use_cache = isTRUE(input$use_cache) && !isTRUE(input$force_refresh),
        force_refresh = isTRUE(input$force_refresh)
      )
    })

    current_reviews_result(result)
  })

  observeEvent(current_reviews_result(), {
    result <- current_reviews_result()

    if (!is.null(result$error)) {
      showNotification(result$message, type = "warning", duration = 8)
    } else if (!is.null(result$warning)) {
      showNotification(result$warning, type = "warning", duration = 8)
    } else if (!is.na(result$cached)) {
      showNotification(result$message, type = "message", duration = 4)
    }

    if (!is.null(result$reviews) && nrow(result$reviews) > 0) {
      message("Current reviews by language:")
      print(dplyr::count(result$reviews, language, name = "n"))
    }
  }, ignoreInit = TRUE)

  current_reviews <- reactive({
    result <- current_reviews_result()
    reviews <- result$reviews %||% empty_dashboard_reviews()
    appid <- selected_appid()

    if (!is.na(appid) && nrow(reviews) > 0 && "app_id" %in% names(reviews)) {
      reviews <- reviews |>
        dplyr::filter(app_id == appid)
    }

    reviews
  })

  filtered_reviews <- reactive({
    date_range <- dashboard_date_range(input$fetch_date_range[1], input$fetch_date_range[2])

    filter_dashboard_reviews(
      current_reviews(),
      language_filter = input$fetch_language %||% "Both",
      sentiment_filter = input$fetch_review_type %||% "All",
      date_range = c(date_range$start_date, date_range$end_date)
    )
  })

  current_daily_summary <- reactive({
    reviews <- filtered_reviews()

    if (nrow(reviews) == 0) {
      return(tibble::tibble())
    }

    summarize_reviews_daily(reviews, group_cols = "language")
  })

  current_weekly_summary <- reactive({
    reviews <- filtered_reviews()

    if (nrow(reviews) == 0) {
      return(tibble::tibble())
    }

    summarize_reviews_weekly(reviews, group_cols = "language")
  })

  current_monthly_summary <- reactive({
    summarize_reviews_monthly(filtered_reviews(), group_cols = "language")
  })

  current_trend_summary <- reactive({
    switch(
      input$trend_granularity %||% "Daily",
      "Daily" = current_daily_summary(),
      "Weekly" = current_weekly_summary(),
      "Monthly" = current_monthly_summary(),
      current_daily_summary()
    )
  })

  alert_daily_summary <- reactive({
    reviews <- filtered_reviews()

    if (nrow(reviews) == 0) {
      return(tibble::tibble())
    }

    summarize_reviews_daily(reviews, group_cols = character(), complete_dates = TRUE)
  })

  alert_data <- reactive({
    summary <- alert_daily_summary()

    if (nrow(summary) == 0) {
      return(tibble::tibble())
    }

    detect_review_alerts(
      summary,
      min_reviews = input$alert_min_reviews %||% 5,
      k = as.numeric(input$alert_k %||% 1.5),
      window = input$alert_window %||% 7,
      metric = input$alert_metric %||% "Negative Rate"
    )
  })

  keyword_reviews <- reactive({
    filter_reviews_for_keywords(
      filtered_reviews(),
      subset = input$keyword_subset %||% "Negative",
      language = input$keyword_language %||% "English"
    )
  })

  current_keyword_data <- reactive({
    appid <- selected_appid()
    subset_label <- input$keyword_subset %||% "Negative"
    language_selection <- input$keyword_language %||% "English"
    languages <- keyword_language_set(language_selection)
    reviews <- keyword_reviews()
    scope <- switch(
      subset_label,
      "All" = "all",
      "Positive" = "positive",
      "Negative" = "negative",
      "negative"
    )

    data <- lapply(languages, function(language) {
      semantic_keywords_with_fallback(
        reviews,
        language = language,
        top_n = input$keyword_top_n %||% 20,
        sentiment_scope = scope
      ) |>
        dplyr::mutate(language = language)
    }) |>
      dplyr::bind_rows() |>
      dplyr::arrange(language, dplyr::desc(n))

    message(
      "Current keyword preview for appid=", appid,
      ", subset=", subset_label,
      ", language=", language_selection,
      ", input_rows=", nrow(reviews),
      ":"
    )
    print(utils::head(data, 8))

    data
  })

  output$header_game <- renderText({
    game <- selected_game()
    ifelse(is.na(game$name), "Not selected", game$name)
  })

  output$header_appid <- renderText({
    appid <- selected_appid()
    ifelse(is.na(appid), "N/A", as.character(appid))
  })

  output$header_updated <- renderText({
    fetched_at <- current_reviews_result()$fetched_at

    if (length(fetched_at) == 0 || is.na(fetched_at)) {
      "N/A"
    } else {
      format(as.POSIXct(fetched_at), "%Y-%m-%d %H:%M")
    }
  })

  output$overview_cards <- renderUI({
    metrics <- build_overview_metrics(filtered_reviews())
    result <- current_reviews_result()

    data_source <- dplyr::case_when(
      !is.null(result$error) && !isTRUE(result$cached) ~ "API Failed",
      !is.null(result$warning) && identical(result$cached, FALSE) ~ "Live API Warning",
      !is.null(result$error) && isTRUE(result$cached) ~ "Cache Fallback",
      isTRUE(result$cached) ~ "Cache",
      identical(result$cached, FALSE) ~ "Live API",
      TRUE ~ "No data"
    )

    tagList(
      tags$div(class = "metric-tile", tags$div(class = "metric-label", "Total Reviews"), tags$div(class = "metric-value metric-accent-blue", format_kpi_number(metrics$total_reviews))),
      tags$div(class = "metric-tile", tags$div(class = "metric-label", "Positive Reviews"), tags$div(class = "metric-value metric-accent-green", format_kpi_number(metrics$positive_reviews))),
      tags$div(class = "metric-tile", tags$div(class = "metric-label", "Negative Reviews"), tags$div(class = "metric-value metric-accent-red", format_kpi_number(metrics$negative_reviews))),
      tags$div(class = "metric-tile", tags$div(class = "metric-label", "Negative Rate"), tags$div(class = "metric-value metric-accent-red", format_kpi_percent(metrics$negative_rate))),
      tags$div(class = "metric-tile", tags$div(class = "metric-label", "English Reviews"), tags$div(class = "metric-value", format_kpi_number(metrics$english_reviews))),
      tags$div(class = "metric-tile", tags$div(class = "metric-label", "Chinese Reviews"), tags$div(class = "metric-value", format_kpi_number(metrics$chinese_reviews))),
      tags$div(class = "metric-tile", tags$div(class = "metric-label", "Latest Review Date"), tags$div(class = "metric-value", ifelse(is.na(metrics$latest_review_date), "N/A", as.character(metrics$latest_review_date)))),
      tags$div(class = "metric-tile", tags$div(class = "metric-label", "Data Source"), tags$div(class = "metric-value", data_source))
    )
  })

  output$overview_summary <- renderUI({
    reviews <- filtered_reviews()
    result <- current_reviews_result()

    if (nrow(reviews) == 0) {
      if (!is.null(result$error)) {
        return(HTML(paste0(
          "<strong>Steam API request failed.</strong> ",
          htmltools::htmlEscape(result$message),
          "<br><code>",
          htmltools::htmlEscape(stringr::str_trunc(result$error, width = 240)),
          "</code>"
        )))
      }

      return(HTML("Search a Steam game, choose a candidate, then load reviews. All KPI cards, trends, tables, keywords, and alerts will be recalculated from the selected appid."))
    }

    game <- selected_game()
    cache_file <- result$cache_file
    cache_note <- ifelse(is.na(cache_file), "No cache file", basename(cache_file))
    status_note <- if (!is.null(result$error)) {
      paste0(" <strong>API issue:</strong> ", htmltools::htmlEscape(stringr::str_trunc(result$error, width = 180)), ".")
    } else if (!is.null(result$warning)) {
      paste0(" <strong>Warning:</strong> ", htmltools::htmlEscape(result$warning), ".")
    } else {
      ""
    }

    HTML(paste0(
      "<strong>", htmltools::htmlEscape(game$name), "</strong>",
      " currently has <strong>", scales::comma(nrow(reviews)), "</strong> reviews in the active dataset. ",
      "The dashboard is using <strong>", ifelse(isTRUE(result$cached), "cache", "live API data"), "</strong>. ",
      "Cache snapshot: <code>", htmltools::htmlEscape(cache_note), "</code>.",
      status_note
    ))
  })

  output$overview_summary_table <- DT::renderDT({
    reviews <- filtered_reviews()
    metrics <- build_overview_metrics(reviews)
    game <- selected_game()
    result <- current_reviews_result()
    date_range <- dashboard_date_range(input$fetch_date_range[1], input$fetch_date_range[2])

    data_source <- dplyr::case_when(
      !is.null(result$error) && !isTRUE(result$cached) ~ "API Failed",
      !is.null(result$warning) && identical(result$cached, FALSE) ~ "Live API Warning",
      !is.null(result$error) && isTRUE(result$cached) ~ "Cache Fallback",
      isTRUE(result$cached) ~ "Cache",
      identical(result$cached, FALSE) ~ "Live API",
      TRUE ~ "No data"
    )

    table_data <- tibble::tibble(
      Metric = c(
        "Selected Game", "App ID", "Date Range", "Total Reviews",
        "Positive Reviews", "Negative Reviews", "Negative Rate",
        "English Reviews", "Chinese Reviews", "Data Source"
      ),
      Value = c(
        ifelse(is.na(game$name), "Not selected", game$name),
        ifelse(is.na(game$appid), "N/A", as.character(game$appid)),
        paste(date_range$start_date, "to", date_range$end_date),
        format_kpi_number(metrics$total_reviews),
        format_kpi_number(metrics$positive_reviews),
        format_kpi_number(metrics$negative_reviews),
        format_kpi_percent(metrics$negative_rate),
        format_kpi_number(metrics$english_reviews),
        format_kpi_number(metrics$chinese_reviews),
        data_source
      )
    )

    DT::datatable(
      table_data,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE)
    )
  })

  output$overview_sentiment_plot <- renderPlotly({
    plot_sentiment_distribution(filtered_reviews())
  })

  output$overview_language_plot <- renderPlotly({
    plot_language_distribution(filtered_reviews())
  })

  output$overview_monthly_plot <- renderPlotly({
    reviews <- filtered_reviews()

    if (nrow(reviews) == 0) {
      return(empty_plotly("No reviews loaded for monthly preview."))
    }

    data <- summarize_reviews_monthly(reviews, group_cols = character())

    if (nrow(data) == 0) {
      return(empty_plotly("No monthly summary available."))
    }

    plot_monthly_summary_preview(data)
  })

  output$language_debug_summary <- renderText({
    result <- current_reviews_result()

    format_counts <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return("none")
      }

      counts <- df |>
        dplyr::count(language, name = "n") |>
        dplyr::arrange(language)

      paste0(counts$language, ": ", counts$n, collapse = "\n")
    }

    status <- result$language_status
    status_text <- if (is.null(status) || nrow(status) == 0) {
      "none"
    } else {
      paste0(
        status$language,
        ": ",
        status$n,
        " (",
        status$status,
        ", ",
        status$source,
        ifelse(is.na(status$error), "", paste0(", ", stringr::str_trunc(status$error, width = 90))),
        ")",
        collapse = "\n"
      )
    }

    paste0(
      "Fetch language: ", input$fetch_language %||% "Both", "\n",
      "Fetch review type: ", input$fetch_review_type %||% "All", "\n",
      "Loaded language key: ", result$language_choice %||% "N/A", "\n",
      "Loaded raw counts:\n", format_counts(current_reviews()), "\n\n",
      "Active counts after sidebar filters:\n", format_counts(filtered_reviews()), "\n\n",
      "Fetch/cache status:\n", status_text
    )
  })

  output$data_source_debug <- renderText({
    game <- selected_game()
    reviews <- current_reviews()
    keywords <- current_keyword_data()

    format_counts <- function(df, col) {
      if (is.null(df) || nrow(df) == 0 || !col %in% names(df)) {
        return("none")
      }

      counts <- df |>
        dplyr::count(.data[[col]], name = "n") |>
        dplyr::arrange(dplyr::desc(n))

      paste0(counts[[col]], ": ", counts$n, collapse = "\n")
    }

    date_range <- if (nrow(reviews) == 0 || all(is.na(reviews$review_date))) {
      "none"
    } else {
      paste(min(reviews$review_date, na.rm = TRUE), "to", max(reviews$review_date, na.rm = TRUE))
    }

    keyword_preview <- if (is.null(keywords) || nrow(keywords) == 0) {
      "none"
    } else {
      preview <- utils::head(keywords, 8)
      paste0(preview$keyword, " (", preview$n, ")", collapse = "\n")
    }

    paste0(
      "selected_game: ", ifelse(is.na(game$name), "N/A", game$name), "\n",
      "selected_appid: ", ifelse(is.na(game$appid), "N/A", game$appid), "\n",
      "nrow(current_reviews): ", nrow(reviews), "\n",
      "app_id counts:\n", format_counts(reviews, "app_id"), "\n\n",
      "language counts:\n", format_counts(reviews, "language"), "\n\n",
      "review_date range: ", date_range, "\n\n",
      "keyword preview:\n", keyword_preview
    )
  })

  output$keyword_debug_summary <- renderText({
    game <- selected_game()
    reviews <- keyword_reviews()
    keywords <- current_keyword_data()

    format_counts <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return("none")
      }

      counts <- df |>
        dplyr::count(language, voted_up, name = "n") |>
        dplyr::arrange(language, dplyr::desc(voted_up))

      paste0(
        counts$language,
        " | voted_up=",
        counts$voted_up,
        ": ",
        counts$n,
        collapse = "\n"
      )
    }

    date_range <- if (nrow(reviews) == 0 || all(is.na(reviews$review_date))) {
      "none"
    } else {
      paste(min(reviews$review_date, na.rm = TRUE), "to", max(reviews$review_date, na.rm = TRUE))
    }

    sample_reviews <- if (nrow(reviews) == 0 || !"review" %in% names(reviews)) {
      "none"
    } else {
      reviews |>
        dplyr::slice_head(n = 5) |>
        dplyr::pull(review) |>
        stringr::str_squish() |>
        stringr::str_trunc(width = 180) |>
        paste(collapse = "\n---\n")
    }

    keyword_preview <- if (is.null(keywords) || nrow(keywords) == 0) {
      "none"
    } else {
      preview <- utils::head(keywords, 10)
      language_text <- if ("language" %in% names(preview)) paste0(" [", preview$language, "]") else ""
      paste0(preview$keyword, language_text, " (", preview$n, ")", collapse = "\n")
    }

    paste0(
      "selected_game: ", ifelse(is.na(game$name), "N/A", game$name), "\n",
      "selected_appid: ", ifelse(is.na(game$appid), "N/A", game$appid), "\n",
      "keyword_review_subset: ", input$keyword_subset %||% "Negative", "\n",
      "keyword_language: ", input$keyword_language %||% "English", "\n",
      "nrow(keyword_reviews): ", nrow(reviews), "\n",
      "keyword_reviews count(language, voted_up):\n", format_counts(reviews), "\n\n",
      "keyword_reviews date range: ", date_range, "\n\n",
      "head(keyword_reviews$review, 5):\n", sample_reviews, "\n\n",
      "head(current_keyword_data, 10):\n", keyword_preview
    )
  })

  output$reviews_table <- DT::renderDT({
    reviews <- filtered_reviews()

    if (nrow(reviews) == 0) {
      return(DT::datatable(tibble::tibble(Message = "No reviews loaded for the current game."), rownames = FALSE))
    }

    table_data <- reviews |>
      dplyr::mutate(
        sentiment = dplyr::if_else(voted_up, "Positive", "Negative"),
        sentiment = factor(sentiment, levels = c("Positive", "Negative")),
        language = factor(language, levels = c("Chinese", "English")),
        review = stringr::str_trunc(review, width = 600)
      ) |>
      dplyr::select(review_date, language, sentiment, review, author.steamid, author.num_games_owned, author.num_reviews) |>
      dplyr::arrange(dplyr::desc(review_date))

    DT::datatable(
      table_data,
      rownames = FALSE,
      escape = TRUE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(width = "48%", targets = 3, className = "review-cell"))
      )
    )
  })

  output$main_trend_plot <- renderPlotly({
    plot_trend_summary(
      summary_data = current_trend_summary(),
      granularity = input$trend_granularity %||% "Daily",
      metric = input$trend_metric %||% "Review Volume",
      language_filter = input$trend_language %||% "Both",
      show_rolling = isTRUE(input$show_rolling),
      min_daily_reviews = input$min_daily_reviews %||% 10
    )
  })

  output$keywords_bar <- renderPlot({
    data <- current_keyword_data()
    language <- keyword_language_label(input$keyword_language %||% "English")

    if (nrow(data) < 5) {
      return(empty_ggplot("Not enough keyword candidates under current filters."))
    }

    plot_semantic_keywords(
      data,
      title = paste("Top", language, "Semantic Keywords"),
      language = language,
      top_n = input$keyword_top_n %||% 20
    )
  })

  output$keywords_wordcloud <- renderPlot({
    data <- current_keyword_data()
    language <- keyword_language_label(input$keyword_language %||% "English")

    if (nrow(data) < 5) {
      return(empty_ggplot("Not enough keyword candidates under current filters."))
    }

    plot_wordcloud_from_keywords(
      data,
      title = paste(language, "Semantic Keywords"),
      language = language,
      max_terms = 45
    )
  })

  output$alerts_plot <- renderPlotly({
    plot_alerts(alert_data(), metric = input$alert_metric %||% "Negative Rate")
  })

  output$alerts_table <- DT::renderDT({
    alerts <- alert_data()

    if (nrow(alerts) == 0) {
      return(DT::datatable(tibble::tibble(Message = "No alert data under current filters."), rownames = FALSE))
    }

    data <- alerts |>
      dplyr::filter(alert_flag) |>
      dplyr::transmute(
        review_date,
        total_reviews,
        negative_reviews,
        negative_rate = scales::percent(negative_rate, accuracy = 0.1),
        alert_value,
        threshold = alert_threshold
      )

    if (nrow(data) == 0) {
      closest <- alerts |>
        dplyr::filter(!is.na(alert_threshold), total_reviews >= (input$alert_min_reviews %||% 5)) |>
        dplyr::arrange(dplyr::desc(distance_to_threshold)) |>
        dplyr::slice_head(n = 5) |>
        dplyr::transmute(
          review_date,
          total_reviews,
          negative_reviews,
          negative_rate = scales::percent(negative_rate, accuracy = 0.1),
          alert_value,
          threshold = alert_threshold,
          distance_to_threshold
        )

      data <- if (nrow(closest) == 0) {
        tibble::tibble(
          Message = paste0(
            "No alert days detected. Settings: min_reviews=",
            input$alert_min_reviews %||% 5,
            ", window=",
            input$alert_window %||% 7,
            ", k=",
            input$alert_k %||% 1.5,
            "."
          )
        )
      } else {
        closest
      }
    }

    DT::datatable(data, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$alert_reviews_table <- DT::renderDT({
    alert_days <- alert_data() |>
      dplyr::filter(alert_flag)

    reviews <- filter_alert_reviews(filtered_reviews(), alert_days) |>
      dplyr::mutate(review = stringr::str_trunc(review, width = 700)) |>
      dplyr::select(review_date, language, review, author.steamid, author.num_reviews) |>
      dplyr::arrange(dplyr::desc(review_date))

    if (nrow(reviews) == 0) {
      reviews <- tibble::tibble(Message = "No negative reviews linked to alert days.")
    }

    DT::datatable(reviews, rownames = FALSE, escape = TRUE, filter = "top", options = list(pageLength = 8, scrollX = TRUE))
  })
}

shinyApp(ui, server)
