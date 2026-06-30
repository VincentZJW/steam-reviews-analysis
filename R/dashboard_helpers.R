# Dashboard 数据获取、缓存、筛选和指标辅助函数

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

dashboard_language_options <- function(selection) {
  selection <- selection %||% "Both"

  map_one <- function(value) {
    switch(
      as.character(value),
      "English" = "english",
      "english" = "english",
      "简体中文" = "schinese",
      "Chinese" = "schinese",
      "chinese" = "schinese",
      "schinese" = "schinese",
      "Both" = c("schinese", "english"),
      "both" = c("schinese", "english"),
      c("schinese", "english")
    )
  }

  mapped <- unlist(lapply(as.character(selection), map_one), use.names = FALSE)
  mapped <- unique(mapped)

  if (all(c("schinese", "english") %in% mapped)) {
    return(c("schinese", "english"))
  }

  mapped
}

dashboard_language_label <- function(language) {
  dplyr::recode(
    language,
    english = "English",
    schinese = "Chinese",
    Chinese = "Chinese",
    English = "English",
    .default = language
  )
}

dashboard_review_type <- function(selection) {
  switch(
    selection,
    "Positive" = "positive",
    "Negative" = "negative",
    "All" = "all",
    "all"
  )
}

dashboard_date_range <- function(start_date = as.Date("2024-01-01"),
                                 end_date = Sys.Date()) {
  start_date <- as.Date(start_date %||% as.Date("2024-01-01"))
  end_date <- as.Date(end_date %||% Sys.Date())

  if (is.na(start_date)) {
    start_date <- as.Date("2024-01-01")
  }

  if (is.na(end_date)) {
    end_date <- Sys.Date()
  }

  if (start_date > end_date) {
    tmp <- start_date
    start_date <- end_date
    end_date <- tmp
  }

  list(start_date = start_date, end_date = end_date)
}

date_cache_key <- function(date) {
  format(as.Date(date), "%Y-%m-%d")
}

empty_dashboard_reviews <- function() {
  tibble::tibble(
    recommendationid = character(),
    language = character(),
    voted_up = logical(),
    is_negative = logical(),
    review = character(),
    review_clean = character(),
    created_at = as.POSIXct(character(), tz = "UTC"),
    updated_at = as.POSIXct(character(), tz = "UTC"),
    review_date = as.Date(character()),
    review_year = integer(),
    review_month = character(),
    author.steamid = character(),
    author.num_games_owned = numeric(),
    author.num_reviews = numeric(),
    app_id = integer(),
    game_name = character()
  )
}

cache_language_key <- function(languages) {
  languages <- dashboard_language_options(languages)

  if (setequal(languages, c("schinese", "english"))) {
    return("both")
  }

  languages[[1]]
}

legacy_cache_language_keys <- function(languages) {
  key <- cache_language_key(languages)

  if (identical(key, "both")) {
    return(c("both", "english-schinese", "schinese-english"))
  }

  key
}

reviews_cache_pattern <- function(appid,
                                  languages,
                                  review_type,
                                  max_pages,
                                  purchase_type = "all",
                                  start_date = as.Date("2024-01-01"),
                                  end_date = Sys.Date()) {
  language_keys <- stringr::str_replace_all(legacy_cache_language_keys(languages), "[^A-Za-z0-9-]", "-")
  language_pattern <- paste(language_keys, collapse = "|")
  purchase_key <- stringr::str_replace_all(purchase_type, "[^A-Za-z0-9-]", "-")
  dates <- dashboard_date_range(start_date, end_date)

  paste0(
    "^reviews_appid_",
    appid,
    "_lang_(",
    language_pattern,
    ")",
    "_type_",
    review_type,
    "(_purchase_",
    purchase_key,
    ")?",
    "_start_",
    date_cache_key(dates$start_date),
    "_end_",
    date_cache_key(dates$end_date),
    "_pages_",
    max_pages,
    "_.*\\.rds$"
  )
}

latest_reviews_cache_file <- function(cache_dir,
                                      appid,
                                      languages,
                                      review_type,
                                      max_pages,
                                      purchase_type = "all",
                                      start_date = as.Date("2024-01-01"),
                                      end_date = Sys.Date()) {
  pattern <- reviews_cache_pattern(appid, languages, review_type, max_pages, purchase_type, start_date, end_date)
  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    return(NA_character_)
  }

  files[which.max(file.info(files)$mtime)]
}

latest_compatible_reviews_cache_file <- function(cache_dir,
                                                 appid,
                                                 languages,
                                                 review_type,
                                                 purchase_type = "all",
                                                 start_date = as.Date("2024-01-01"),
                                                 end_date = Sys.Date()) {
  language_keys <- stringr::str_replace_all(legacy_cache_language_keys(languages), "[^A-Za-z0-9-]", "-")
  language_pattern <- paste(language_keys, collapse = "|")
  purchase_key <- stringr::str_replace_all(purchase_type, "[^A-Za-z0-9-]", "-")
  dates <- dashboard_date_range(start_date, end_date)
  pattern <- paste0(
    "^reviews_appid_",
    appid,
    "_lang_(",
    language_pattern,
    ")",
    "_type_",
    review_type,
    "(_purchase_",
    purchase_key,
    ")?_start_",
    date_cache_key(dates$start_date),
    "_end_",
    date_cache_key(dates$end_date),
    "_pages_[0-9]+_.*\\.rds$"
  )
  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    return(NA_character_)
  }

  files[which.max(file.info(files)$mtime)]
}

latest_appid_reviews_cache_file <- function(cache_dir, appid) {
  pattern <- paste0("^reviews_appid_", appid, "_.*\\.rds$")
  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    return(NA_character_)
  }

  files[which.max(file.info(files)$mtime)]
}

reviews_cache_is_fresh <- function(path, cache_hours = 6) {
  if (is.na(path) || !file.exists(path)) {
    return(FALSE)
  }

  age_hours <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "hours"))
  is.finite(age_hours) && age_hours <= cache_hours
}

read_reviews_cache <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(NULL)
  }

  readRDS(path)
}

filter_cached_reviews_to_languages <- function(reviews, languages) {
  if (is.null(reviews) || nrow(reviews) == 0 || !"language" %in% names(reviews)) {
    return(empty_dashboard_reviews())
  }

  requested_labels <- unique(c(dashboard_language_label(languages), languages))

  reviews |>
    dplyr::filter(language %in% requested_labels)
}

filter_reviews_to_date_range <- function(reviews,
                                         start_date = as.Date("2024-01-01"),
                                         end_date = Sys.Date()) {
  if (is.null(reviews) || nrow(reviews) == 0 || !"review_date" %in% names(reviews)) {
    return(empty_dashboard_reviews())
  }

  dates <- dashboard_date_range(start_date, end_date)

  reviews |>
    dplyr::filter(
      review_date >= dates$start_date,
      review_date <= dates$end_date
    )
}

language_status_from_reviews <- function(reviews,
                                         languages,
                                         source = "cache",
                                         errors = list()) {
  requested <- dashboard_language_options(languages)

  tibble::tibble(
    language_api = requested,
    language = dashboard_language_label(requested)
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      n = if (is.null(reviews) || nrow(reviews) == 0) {
        0L
      } else {
        sum(reviews$language == language, na.rm = TRUE)
      },
      status = dplyr::case_when(
        language_api %in% names(errors) ~ "failed",
        n > 0 ~ "fetched",
        TRUE ~ "empty"
      ),
      source = source,
      error = if (language_api %in% names(errors)) errors[[language_api]] else NA_character_
    ) |>
    dplyr::ungroup()
}

language_status_message <- function(status, total_reviews) {
  if (is.null(status) || nrow(status) == 0) {
    return(paste0("Combined ", total_reviews, " total reviews."))
  }

  details <- paste0(
    status$language,
    ": ",
    status$n,
    " (",
    status$status,
    ")",
    collapse = "; "
  )

  paste0("Combined ", total_reviews, " total reviews. ", details, ".")
}

language_status_warning <- function(status) {
  if (is.null(status) || nrow(status) == 0) {
    return(NULL)
  }

  issues <- status |>
    dplyr::filter(.data$status %in% c("failed", "empty"))

  if (nrow(issues) == 0) {
    return(NULL)
  }

  paste0(
    paste0(issues$language, " reviews ", issues$status, collapse = "; "),
    "."
  )
}

write_reviews_cache <- function(cache_dir,
                                appid,
                                game_name,
                                languages,
                                review_type,
                                purchase_type,
                                start_date,
                                end_date,
                                max_pages,
                                reviews) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  language_key <- stringr::str_replace_all(cache_language_key(languages), "[^A-Za-z0-9-]", "-")
  dates <- dashboard_date_range(start_date, end_date)
  path <- file.path(
    cache_dir,
    paste0(
      "reviews_appid_",
      appid,
      "_lang_",
      language_key,
      "_type_",
      review_type,
      "_purchase_",
      stringr::str_replace_all(purchase_type, "[^A-Za-z0-9-]", "-"),
      "_start_",
      date_cache_key(dates$start_date),
      "_end_",
      date_cache_key(dates$end_date),
      "_pages_",
      max_pages,
      "_",
      timestamp,
      ".rds"
    )
  )

  cache_object <- list(
    reviews = reviews,
    metadata = list(
      appid = appid,
      game_name = game_name,
      language_choice = cache_language_key(languages),
      languages = languages,
      review_type = review_type,
      purchase_type = purchase_type,
      start_date = dates$start_date,
      end_date = dates$end_date,
      max_pages = max_pages,
      fetched_at = Sys.time(),
      source = "Steam Store Reviews API"
    )
  )

  saveRDS(cache_object, path)
  path
}

prepare_dashboard_reviews <- function(raw_reviews, appid, game_name = NA_character_) {
  if (is.null(raw_reviews) || nrow(raw_reviews) == 0) {
    return(empty_dashboard_reviews())
  }

  flattened <- jsonlite::flatten(raw_reviews)

  required_fields <- c(
    "recommendationid",
    "language",
    "review",
    "voted_up",
    "created_at",
    "updated_at",
    "author.steamid",
    "author.num_games_owned",
    "author.num_reviews"
  )

  for (field in required_fields) {
    if (!field %in% names(flattened)) {
      flattened[[field]] <- NA
    }
  }

  if (!"app_id" %in% names(flattened)) {
    flattened$app_id <- appid
  }

  clean_reviews(flattened) |>
    dplyr::mutate(
      app_id = as.integer(appid),
      game_name = game_name,
      language = dashboard_language_label(language)
    )
}

fetch_reviews_for_dashboard <- function(appid,
                                        game_name = NA_character_,
                                        language_choice = NULL,
                                        languages = NULL,
                                        review_type = "all",
                                        purchase_type = "all",
                                        max_pages = 1,
                                        start_date = as.Date("2024-01-01"),
                                        end_date = Sys.Date(),
                                        cache_dir = "data/cache",
                                        cache_hours = 6,
                                        use_cache = TRUE,
                                        force_refresh = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(language_choice)) {
    language_choice <- languages %||% "Both"
  }

  languages <- dashboard_language_options(language_choice)
  language_key <- cache_language_key(languages)
  dates <- dashboard_date_range(start_date, end_date)
  day_range <- max(1L, as.integer(Sys.Date() - dates$start_date) + 1L)

  cache_file <- latest_reviews_cache_file(
    cache_dir = cache_dir,
    appid = appid,
    languages = languages,
    review_type = review_type,
    max_pages = max_pages,
    purchase_type = purchase_type,
    start_date = dates$start_date,
    end_date = dates$end_date
  )

  if (use_cache && !force_refresh && !is.na(cache_file) && file.exists(cache_file)) {
    cached <- read_reviews_cache(cache_file)
    cached_reviews <- cached$reviews |>
      filter_cached_reviews_to_languages(languages) |>
      filter_reviews_to_date_range(dates$start_date, dates$end_date)
    status <- language_status_from_reviews(cached_reviews, languages, source = "cache")
    warning <- language_status_warning(status)
    freshness <- if (reviews_cache_is_fresh(cache_file, cache_hours)) "fresh" else "stale"

    return(
      list(
        reviews = cached_reviews,
        cached = TRUE,
        cache_file = cache_file,
        fetched_at = cached$metadata$fetched_at,
        message = paste("Loaded", freshness, "cache.", language_status_message(status, nrow(cached_reviews))),
        warning = warning,
        error = NULL,
        language_status = status,
        requested_languages = languages,
        language_choice = language_key,
        start_date = dates$start_date,
        end_date = dates$end_date,
        day_range = day_range
      )
    )
  }

  fetched <- tryCatch(
    {
      language_results <- lapply(
        languages,
        function(lang) {
          label <- dashboard_language_label(lang)
          message("Fetching ", label, " reviews...")

          tryCatch(
            {
              reviews <- fetch_recent_reviews(
                appid = appid,
                max_pages = max_pages,
                sleep_sec = 0.25,
                fail_on_error = TRUE,
                filter = "all",
                language = lang,
                review_type = review_type,
                purchase_type = purchase_type,
                day_range = day_range,
                timeout_sec = 6,
                retry_max_tries = 1
              )

              n_reviews <- nrow(reviews)
              message("Fetched ", n_reviews, " ", label, " reviews.")

              list(
                language_api = lang,
                language = label,
                reviews = reviews,
                n = n_reviews,
                status = if (n_reviews > 0) "fetched" else "empty",
                error = NA_character_
              )
            },
            error = function(e) {
              err <- conditionMessage(e)
              message("Failed ", label, " reviews: ", err)

              list(
                language_api = lang,
                language = label,
                reviews = tibble::tibble(),
                n = 0L,
                status = "failed",
                error = err
              )
            }
          )
        }
      )

      raw_reviews <- dplyr::bind_rows(lapply(language_results, `[[`, "reviews"))
      status <- tibble::tibble(
        language_api = vapply(language_results, `[[`, character(1), "language_api"),
        language = vapply(language_results, `[[`, character(1), "language"),
        n = vapply(language_results, `[[`, integer(1), "n"),
        status = vapply(language_results, `[[`, character(1), "status"),
        source = "live_api",
        error = vapply(language_results, `[[`, character(1), "error")
      )

      if (nrow(raw_reviews) == 0 && all(status$status == "failed")) {
        stop(
          paste0(
            "Steam API failed for all requested languages: ",
            paste(status$language_api, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      reviews <- prepare_dashboard_reviews(raw_reviews, appid = appid, game_name = game_name) |>
        filter_reviews_to_date_range(dates$start_date, dates$end_date)
      new_cache_file <- write_reviews_cache(
        cache_dir = cache_dir,
        appid = appid,
        game_name = game_name,
        languages = languages,
        review_type = review_type,
        purchase_type = purchase_type,
        start_date = dates$start_date,
        end_date = dates$end_date,
        max_pages = max_pages,
        reviews = reviews
      )

      warning <- language_status_warning(status)
      msg <- paste("Fetched from Steam API.", language_status_message(status, nrow(reviews)))
      message("Combined ", nrow(reviews), " total reviews.")

      list(
        reviews = reviews,
        cached = FALSE,
        cache_file = new_cache_file,
        fetched_at = Sys.time(),
        message = msg,
        warning = warning,
        error = NULL,
        language_status = status,
        requested_languages = languages,
        language_choice = language_key,
        start_date = dates$start_date,
        end_date = dates$end_date,
        day_range = day_range
      )
    },
    error = function(e) {
      stale <- read_reviews_cache(cache_file)

      if (!is.null(stale)) {
        stale_reviews <- stale$reviews |>
          filter_cached_reviews_to_languages(languages) |>
          filter_reviews_to_date_range(dates$start_date, dates$end_date)
        status <- language_status_from_reviews(stale_reviews, languages, source = "cache")

        return(
          list(
            reviews = stale_reviews,
            cached = TRUE,
            cache_file = cache_file,
            fetched_at = stale$metadata$fetched_at,
            message = paste("Steam API failed. Loaded exact cache instead.", language_status_message(status, nrow(stale_reviews))),
            warning = language_status_warning(status),
            error = conditionMessage(e),
            language_status = status,
            requested_languages = languages,
            language_choice = language_key,
            start_date = dates$start_date,
            end_date = dates$end_date,
            day_range = day_range
          )
        )
      }

      status <- tibble::tibble(
        language_api = languages,
        language = dashboard_language_label(languages),
        n = 0L,
        status = "failed",
        source = "live_api",
        error = conditionMessage(e)
      )

      list(
        reviews = empty_dashboard_reviews(),
        cached = FALSE,
        cache_file = NA_character_,
        fetched_at = NA,
        message = "Steam API failed and no exact matching cache is available.",
        warning = NULL,
        error = conditionMessage(e),
        language_status = status,
        requested_languages = languages,
        language_choice = language_key,
        start_date = dates$start_date,
        end_date = dates$end_date,
        day_range = day_range
      )
    }
  )

  fetched
}

filter_dashboard_reviews <- function(reviews,
                                     language_filter = "Both",
                                     sentiment_filter = "All",
                                     date_range = NULL) {
  if (is.null(reviews) || nrow(reviews) == 0) {
    return(empty_dashboard_reviews())
  }

  out <- reviews

  if (!identical(language_filter, "Both")) {
    label <- if (identical(language_filter, "简体中文")) "Chinese" else language_filter
    out <- out |>
      dplyr::filter(language == label)
  }

  if (identical(sentiment_filter, "Positive")) {
    out <- out |>
      dplyr::filter(voted_up)
  } else if (identical(sentiment_filter, "Negative")) {
    out <- out |>
      dplyr::filter(is_negative)
  }

  if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
    out <- out |>
      dplyr::filter(review_date >= date_range[1], review_date <= date_range[2])
  }

  out
}

filter_reviews_for_keywords <- function(reviews,
                                        subset = "Negative",
                                        language = "English") {
  if (is.null(reviews) || nrow(reviews) == 0) {
    return(empty_dashboard_reviews())
  }

  out <- reviews

  if (identical(subset, "Positive")) {
    out <- out |>
      dplyr::filter(voted_up %in% TRUE)
  } else if (identical(subset, "Negative")) {
    out <- out |>
      dplyr::filter(is_negative %in% TRUE)
  }

  if (!identical(language, "Both")) {
    label <- dashboard_language_label(dashboard_language_options(language))
    out <- out |>
      dplyr::filter(language %in% label)
  }

  out
}

build_overview_metrics <- function(reviews) {
  if (is.null(reviews) || nrow(reviews) == 0) {
    return(
      tibble::tibble(
        total_reviews = 0,
        positive_reviews = 0,
        negative_reviews = 0,
        negative_rate = NA_real_,
        english_reviews = 0,
        chinese_reviews = 0,
        latest_review_date = as.Date(NA)
      )
    )
  }

  tibble::tibble(
    total_reviews = nrow(reviews),
    positive_reviews = sum(reviews$voted_up, na.rm = TRUE),
    negative_reviews = sum(reviews$is_negative, na.rm = TRUE),
    negative_rate = mean(reviews$is_negative, na.rm = TRUE),
    english_reviews = sum(reviews$language == "English", na.rm = TRUE),
    chinese_reviews = sum(reviews$language == "Chinese", na.rm = TRUE),
    latest_review_date = max(reviews$review_date, na.rm = TRUE)
  )
}

dashboard_empty_plotly <- function(message = "No data available under current filters.") {
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

dashboard_plot_theme <- function() {
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

plot_sentiment_distribution <- function(review_data) {
  if (is.null(review_data) || nrow(review_data) == 0) {
    return(dashboard_empty_plotly("No reviews loaded for sentiment distribution."))
  }

  data <- review_data |>
    dplyr::mutate(sentiment = dplyr::if_else(voted_up, "Positive", "Negative")) |>
    dplyr::count(sentiment, name = "reviews") |>
    dplyr::mutate(sentiment = factor(sentiment, levels = c("Positive", "Negative")))

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = sentiment,
      y = reviews,
      fill = sentiment,
      text = paste0(sentiment, ": ", scales::comma(reviews))
    )
  ) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::scale_fill_manual(values = c("Positive" = "#5CC38A", "Negative" = "#FF6B6B")) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = NULL, y = "Reviews", title = "Positive vs Negative") +
    dashboard_plot_theme() +
    ggplot2::theme(legend.position = "none")

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

plot_language_distribution <- function(review_data) {
  if (is.null(review_data) || nrow(review_data) == 0) {
    return(dashboard_empty_plotly("No reviews loaded for language distribution."))
  }

  data <- review_data |>
    dplyr::count(language, name = "reviews")

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = language,
      y = reviews,
      fill = language,
      text = paste0(language, ": ", scales::comma(reviews))
    )
  ) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::scale_fill_manual(values = c("Chinese" = "#66C0F4", "English" = "#F2994A")) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = NULL, y = "Reviews", title = "Language Split") +
    dashboard_plot_theme() +
    ggplot2::theme(legend.position = "none")

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

plot_monthly_summary_preview <- function(monthly_summary) {
  if (is.null(monthly_summary) || nrow(monthly_summary) == 0) {
    return(dashboard_empty_plotly("No monthly summary available."))
  }

  plotly::plot_ly(monthly_summary, x = ~review_month_date) |>
    plotly::add_bars(
      y = ~total_reviews,
      name = "Reviews",
      marker = list(color = "#66C0F4"),
      text = ~paste0(
        "Month: ", review_month_date,
        "<br>Reviews: ", scales::comma(total_reviews),
        "<br>Negative rate: ", scales::percent(negative_rate, accuracy = 0.1)
      ),
      hoverinfo = "text"
    ) |>
    plotly::add_lines(
      y = ~negative_rate,
      name = "Negative rate",
      yaxis = "y2",
      line = list(color = "#FF6B6B", width = 2),
      text = ~paste0(
        "Month: ", review_month_date,
        "<br>Negative rate: ", scales::percent(negative_rate, accuracy = 0.1)
      ),
      hoverinfo = "text"
    ) |>
    plotly::layout(
      title = "Monthly Reviews and Negative Rate",
      xaxis = list(title = ""),
      yaxis = list(title = "Reviews"),
      yaxis2 = list(title = "Negative Rate", overlaying = "y", side = "right", tickformat = ".0%"),
      legend = list(orientation = "h", x = 0, y = 1.12),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}

plot_trend_summary <- function(summary_data,
                               granularity = "Daily",
                               metric = "Review Volume",
                               language_filter = "Both",
                               show_rolling = TRUE,
                               min_daily_reviews = 10) {
  if (!identical(language_filter, "Both") && !is.null(summary_data) && nrow(summary_data) > 0) {
    label <- dashboard_language_label(dashboard_language_options(language_filter))
    summary_data <- summary_data |>
      dplyr::filter(language %in% label)
  }

  if (is.null(summary_data) || nrow(summary_data) == 0) {
    return(dashboard_empty_plotly("Load a game or adjust filters to view trends."))
  }

  if (!"language" %in% names(summary_data)) {
    summary_data <- summary_data |>
      dplyr::mutate(language = "All")
  }

  x_col <- switch(
    granularity,
    "Daily" = "review_date",
    "Weekly" = "review_week",
    "Monthly" = "review_month_date",
    "review_date"
  )
  x_label <- switch(granularity, "Daily" = "Date", "Weekly" = "Week", "Monthly" = "Month")
  language_colors <- c("Chinese" = "#66C0F4", "English" = "#F2994A", "All" = "#B8C0CC")

  summary_data <- summary_data |>
    dplyr::arrange(language, .data[[x_col]]) |>
    dplyr::mutate(
      .trend_period = .data[[x_col]],
      .base_tooltip = paste0(
        x_label, ": ", .trend_period,
        "<br>Language: ", language,
        "<br>Total reviews: ", scales::comma(total_reviews),
        "<br>Positive reviews: ", scales::comma(positive_reviews),
        "<br>Negative reviews: ", scales::comma(negative_reviews),
        "<br>Negative rate: ", scales::percent(negative_rate, accuracy = 0.1)
      )
    )

  if (identical(metric, "Review Volume")) {
    plot_data <- summary_data

    if (identical(granularity, "Daily") && "rolling_total_reviews_7d" %in% names(plot_data)) {
      plot_data <- plot_data |>
        dplyr::mutate(
          rolling_total_reviews_7d_avg = dplyr::if_else(
            !is.na(rolling_total_reviews_7d),
            rolling_total_reviews_7d / 7,
            NA_real_
          ),
          .rolling_tooltip = paste0(
            .base_tooltip,
            "<br>7-day rolling avg reviews: ",
            scales::comma(round(rolling_total_reviews_7d_avg, 1))
          )
        )
    }

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .trend_period,
        y = total_reviews,
        color = language,
        group = language,
        text = .base_tooltip
      )
    ) +
      ggplot2::geom_line(linewidth = 1.15, alpha = 0.95)

    if (identical(granularity, "Daily")) {
      p <- p +
        ggplot2::geom_point(size = 1.1, alpha = 0.35)
    }

    if (
      identical(granularity, "Daily") &&
        isTRUE(show_rolling) &&
        "rolling_total_reviews_7d_avg" %in% names(plot_data)
    ) {
      rolling_data <- plot_data |>
        dplyr::filter(!is.na(rolling_total_reviews_7d_avg))

      if (nrow(rolling_data) > 0) {
        p <- p +
          ggplot2::geom_line(
            data = rolling_data,
            ggplot2::aes(
              y = rolling_total_reviews_7d_avg,
              group = language
            ),
            linewidth = 1.15,
            linetype = "longdash",
            alpha = 0.95
          )
      }
    }

    p <- p +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::labs(title = paste(granularity, "Review Volume"), x = NULL, y = "Reviews", color = "Language") +
      ggplot2::scale_color_manual(values = language_colors) +
      dashboard_plot_theme()

    return(plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
  }

  if (identical(metric, "Negative Review Rate")) {
    plot_data <- summary_data

    if (identical(granularity, "Daily")) {
      plot_data <- plot_data |>
        dplyr::filter(total_reviews >= min_daily_reviews)
    }

    plot_data <- plot_data |>
      dplyr::filter(!is.na(negative_rate))

    if (nrow(plot_data) == 0) {
      return(dashboard_empty_plotly("No negative-rate trend after current filters."))
    }

    if (identical(granularity, "Daily") && "negative_rate_7d" %in% names(plot_data)) {
      plot_data <- plot_data |>
        dplyr::mutate(
          .rolling_tooltip = paste0(
            .base_tooltip,
            "<br>7-day rolling negative rate: ",
            scales::percent(negative_rate_7d, accuracy = 0.1)
          )
        )
    }

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .trend_period,
        y = negative_rate,
        color = language,
        group = language,
        text = .base_tooltip
      )
    ) +
      ggplot2::geom_line(linewidth = 1.15, alpha = 0.95)

    if (identical(granularity, "Daily")) {
      p <- p +
        ggplot2::geom_point(size = 1.1, alpha = 0.35)
    }

    if (
      identical(granularity, "Daily") &&
        isTRUE(show_rolling) &&
        "negative_rate_7d" %in% names(plot_data)
    ) {
      rolling_data <- plot_data |>
        dplyr::filter(!is.na(negative_rate_7d))

      if (nrow(rolling_data) > 0) {
        p <- p +
          ggplot2::geom_line(
            data = rolling_data,
            ggplot2::aes(
              y = negative_rate_7d,
              group = language
            ),
            linewidth = 1.2,
            linetype = "longdash",
            alpha = 0.95
          )
      }
    }

    p <- p +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(title = paste(granularity, "Negative Review Rate"), x = NULL, y = "Negative Rate", color = "Language") +
      ggplot2::scale_color_manual(values = language_colors) +
      dashboard_plot_theme()

    return(plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
  }

  sentiment_data <- summary_data |>
    dplyr::mutate(
      positive_reviews_total = positive_reviews,
      negative_reviews_total = negative_reviews
    ) |>
    dplyr::select(
      language,
      .trend_period,
      total_reviews,
      positive_reviews,
      negative_reviews,
      positive_reviews_total,
      negative_reviews_total,
      negative_rate
    ) |>
    tidyr::pivot_longer(
      c(positive_reviews, negative_reviews),
      names_to = "sentiment",
      values_to = "reviews"
    ) |>
    dplyr::mutate(
      sentiment = dplyr::recode(sentiment, positive_reviews = "Positive", negative_reviews = "Negative"),
      series = paste(language, sentiment),
      .sentiment_tooltip = paste0(
        x_label, ": ", .trend_period,
        "<br>Language: ", language,
        "<br>Sentiment: ", sentiment,
        "<br>Total reviews: ", scales::comma(total_reviews),
        "<br>Positive reviews: ", scales::comma(positive_reviews_total),
        "<br>Negative reviews: ", scales::comma(negative_reviews_total),
        "<br>Negative rate: ", scales::percent(negative_rate, accuracy = 0.1)
      )
    )

  p <- ggplot2::ggplot(
    sentiment_data,
    ggplot2::aes(
      x = .trend_period,
      y = reviews,
      color = series,
      group = series,
      text = .sentiment_tooltip
    )
  ) +
    ggplot2::geom_line(linewidth = 1.1, alpha = 0.95)

  if (identical(granularity, "Daily")) {
    p <- p +
      ggplot2::geom_point(size = 1, alpha = 0.3)
  }

  p <- p +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = paste(granularity, "Positive vs Negative Reviews"), x = NULL, y = "Reviews", color = NULL) +
    ggplot2::scale_color_manual(
      values = c(
        "Chinese Positive" = "#6FCF97",
        "Chinese Negative" = "#EB5757",
        "English Positive" = "#56CCF2",
        "English Negative" = "#F2994A",
        "All Positive" = "#6FCF97",
        "All Negative" = "#EB5757"
      )
    ) +
    dashboard_plot_theme()

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

plot_semantic_keywords <- function(keyword_data,
                                   title = "Semantic Keywords",
                                   language = "English",
                                   top_n = 20) {
  if (is.null(keyword_data) || nrow(keyword_data) < 5) {
    return(NULL)
  }

  if (identical(language, "Both") && "language" %in% names(keyword_data)) {
    plot_data <- keyword_data |>
      dplyr::group_by(language) |>
      dplyr::slice_head(n = top_n) |>
      dplyr::ungroup() |>
      dplyr::mutate(.plot_term = stats::reorder(keyword, n))

    return(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = .plot_term, y = n, fill = language)) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(~language, scales = "free_y") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_fill_manual(values = c("Chinese" = "#66C0F4", "English" = "#F2994A")) +
        ggplot2::labs(title = title, x = "Keyword / Keyphrase", y = "Frequency", fill = "Language") +
        dashboard_plot_theme() +
        ggplot2::theme(strip.text = ggplot2::element_text(color = "#E8EEF8", face = "bold"))
    )
  }

  fill <- if (identical(language, "Chinese")) "#66C0F4" else "#F2994A"
  family <- if (identical(language, "Chinese")) "Arial Unicode MS" else ""

  plot_keyword_bars(
    keyword_data,
    term_col = "keyword",
    title = title,
    fill = fill,
    base_family = family,
    top_n = top_n
  ) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#111927", color = NA))
}

plot_wordcloud_from_keywords <- function(keyword_data,
                                         title = "Word Cloud",
                                         language = "English",
                                         max_terms = 45) {
  if (is.null(keyword_data) || nrow(keyword_data) < 5) {
    return(NULL)
  }

  if (identical(language, "Both") && "language" %in% names(keyword_data)) {
    plot_data <- keyword_data |>
      dplyr::group_by(language) |>
      dplyr::slice_head(n = max_terms) |>
      dplyr::ungroup()

    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(label = keyword, size = n, color = language)
      ) +
        ggwordcloud::geom_text_wordcloud_area(
          fontface = "bold",
          seed = 2027,
          eccentricity = 0.7,
          grid_size = 4,
          rm_outside = TRUE
        ) +
        ggplot2::facet_wrap(~language) +
        ggplot2::scale_size_area(max_size = 18) +
        ggplot2::scale_color_manual(values = c("Chinese" = "#66C0F4", "English" = "#F2994A")) +
        ggplot2::labs(title = title) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "#111927", color = NA),
          panel.background = ggplot2::element_rect(fill = "#111927", color = NA),
          plot.title = ggplot2::element_text(color = "#E8EEF8", face = "bold", hjust = 0.5),
          strip.text = ggplot2::element_text(color = "#E8EEF8", face = "bold"),
          legend.position = "none"
        )
    )
  }

  family <- if (identical(language, "Chinese")) "Arial Unicode MS" else ""
  palette <- if (identical(language, "Chinese")) {
    c("#66C0F4", "#5CC38A", "#F2994A", "#A4B1CD")
  } else {
    c("#F2994A", "#66C0F4", "#5CC38A", "#A4B1CD")
  }

  plot_keyword_wordcloud(
    keyword_data,
    term_col = "keyword",
    title = title,
    max_terms = max_terms,
    palette = palette,
    base_family = family,
    seed = 2027
  ) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#111927", color = NA))
}

plot_alerts <- function(alert_data, metric = "Negative Rate") {
  if (is.null(alert_data) || nrow(alert_data) == 0) {
    return(dashboard_empty_plotly("No alert data under current filters."))
  }

  percent_metric <- identical(metric, "Negative Rate")

  p <- ggplot2::ggplot(
    alert_data,
    ggplot2::aes(
      x = review_date,
      y = alert_value,
      text = paste0(
        "Date: ", review_date,
        "<br>Reviews: ", total_reviews,
        "<br>Negative reviews: ", negative_reviews,
        "<br>Negative Rate: ", scales::percent(negative_rate, accuracy = 0.1),
        "<br>", metric, ": ", if (percent_metric) scales::percent(alert_value, accuracy = 0.1) else scales::comma(alert_value),
        "<br>Threshold: ", if (percent_metric) scales::percent(alert_threshold, accuracy = 0.1) else scales::comma(alert_threshold)
      )
    )
  ) +
    ggplot2::geom_line(color = "#66C0F4", linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = rolling_mean), color = "#5CC38A", linewidth = 0.75) +
    ggplot2::geom_line(ggplot2::aes(y = alert_threshold), color = "#F2C94C", linewidth = 0.75, linetype = "dashed") +
    ggplot2::geom_point(data = alert_data |> dplyr::filter(alert_flag), color = "#FF6B6B", size = 2.4) +
    ggplot2::labs(x = NULL, y = metric, title = paste(metric, "vs Alert Threshold")) +
    dashboard_plot_theme()

  if (percent_metric) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  } else {
    p <- p + ggplot2::scale_y_continuous(labels = scales::comma)
  }

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

summarize_reviews_monthly <- function(df, group_cols = character()) {
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  group_cols <- intersect(group_cols, names(df))
  summary_cols <- c(group_cols, "review_month_date")

  df |>
    dplyr::mutate(review_month_date = as.Date(paste0(format(review_date, "%Y-%m"), "-01"))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) |>
    dplyr::summarise(
      total_reviews = dplyr::n(),
      negative_reviews = sum(is_negative, na.rm = TRUE),
      positive_reviews = sum(voted_up, na.rm = TRUE),
      negative_rate = negative_reviews / total_reviews,
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(summary_cols)))
}

safe_semantic_keywords <- function(reviews, language, top_n = 20, sentiment_scope = "negative") {
  empty_keywords <- tibble::tibble(
    keyword = character(),
    n = integer(),
    review_share = numeric(),
    scope = character()
  )

  if (is.null(reviews) || nrow(reviews) == 0) {
    return(empty_keywords)
  }

  subset <- reviews |>
    dplyr::filter(.data$language == language)

  if (nrow(subset) == 0) {
    return(empty_keywords)
  }

  tryCatch(
    {
      if (identical(language, "English")) {
        extract_english_semantic_keywords(
          subset,
          text_col = "review_clean",
          top_n = top_n,
          sentiment_scope = sentiment_scope,
          min_count = 1
        )
      } else {
        extract_chinese_semantic_keywords(
          subset,
          text_col = "review_clean",
          top_n = top_n,
          sentiment_scope = sentiment_scope,
          min_count = 1
        )
      }
    },
    error = function(e) {
      empty_keywords
    }
  )
}

format_kpi_number <- function(x) {
  scales::comma(x, accuracy = 1)
}

format_kpi_percent <- function(x) {
  ifelse(is.na(x), "N/A", scales::percent(x, accuracy = 0.1))
}
