# 评论汇总和指标构建函数

check_required_columns <- function(df, required_cols, data_name = "data") {
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      data_name,
      " 缺少必要字段: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

add_daily_rolling_metrics <- function(df, group_cols = character(), window = 7) {
  arrange_cols <- c(group_cols, "review_date")

  out <- df |>
    dplyr::arrange(dplyr::across(dplyr::all_of(arrange_cols)))

  if (length(group_cols) > 0) {
    out <- out |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  }

  out |>
    dplyr::mutate(
      rolling_negative_reviews_7d = slider::slide_dbl(
        negative_reviews,
        sum,
        .before = window - 1,
        .complete = TRUE,
        na.rm = TRUE
      ),
      rolling_total_reviews_7d = slider::slide_dbl(
        total_reviews,
        sum,
        .before = window - 1,
        .complete = TRUE,
        na.rm = TRUE
      ),
      negative_rate_7d = dplyr::if_else(
        rolling_total_reviews_7d > 0,
        rolling_negative_reviews_7d / rolling_total_reviews_7d,
        NA_real_
      )
    ) |>
    dplyr::ungroup()
}

add_weekly_rolling_metrics <- function(df, group_cols = character(), window = 4) {
  arrange_cols <- c(group_cols, "review_week")

  out <- df |>
    dplyr::arrange(dplyr::across(dplyr::all_of(arrange_cols)))

  if (length(group_cols) > 0) {
    out <- out |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  }

  out |>
    dplyr::mutate(
      rolling_negative_reviews_4w = slider::slide_dbl(
        negative_reviews,
        sum,
        .before = window - 1,
        .complete = TRUE,
        na.rm = TRUE
      ),
      rolling_total_reviews_4w = slider::slide_dbl(
        total_reviews,
        sum,
        .before = window - 1,
        .complete = TRUE,
        na.rm = TRUE
      ),
      negative_rate_4w = dplyr::if_else(
        rolling_total_reviews_4w > 0,
        rolling_negative_reviews_4w / rolling_total_reviews_4w,
        NA_real_
      )
    ) |>
    dplyr::ungroup()
}

complete_daily_dates <- function(df, group_cols = character()) {
  fill_values <- list(
    total_reviews = 0,
    negative_reviews = 0,
    positive_reviews = 0
  )

  if (length(group_cols) == 0) {
    return(
      df |>
        tidyr::complete(
          review_date = seq(min(review_date), max(review_date), by = "day"),
          fill = fill_values
        )
    )
  }

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    tidyr::complete(
      review_date = seq(min(review_date), max(review_date), by = "day"),
      fill = fill_values
    ) |>
    dplyr::ungroup()
}

# 生成每日评论量、正负评数量、差评率和 7 日滚动差评率
summarize_reviews_daily <- function(df,
                                    group_cols = character(),
                                    complete_dates = TRUE,
                                    rolling_window = 7) {
  check_required_columns(
    df,
    c("review_date", "is_negative", "voted_up"),
    "reviews data"
  )

  group_cols <- intersect(group_cols, names(df))
  summary_cols <- c(group_cols, "review_date")

  daily_summary <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) |>
    dplyr::summarise(
      total_reviews = dplyr::n(),
      negative_reviews = sum(is_negative, na.rm = TRUE),
      positive_reviews = sum(voted_up, na.rm = TRUE),
      .groups = "drop"
    )

  if (complete_dates) {
    daily_summary <- complete_daily_dates(daily_summary, group_cols)
  }

  daily_summary |>
    dplyr::mutate(
      negative_rate = dplyr::if_else(
        total_reviews > 0,
        negative_reviews / total_reviews,
        NA_real_
      )
    ) |>
    add_daily_rolling_metrics(group_cols = group_cols, window = rolling_window)
}

# 生成每周评论量、正负评数量、差评率和 4 周滚动差评率
summarize_reviews_weekly <- function(df,
                                     group_cols = character(),
                                     rolling_window = 4) {
  check_required_columns(
    df,
    c("review_date", "is_negative", "voted_up"),
    "reviews data"
  )

  group_cols <- intersect(group_cols, names(df))
  summary_cols <- c(group_cols, "review_week")

  df |>
    dplyr::mutate(review_week = as.Date(cut(review_date, breaks = "week"))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) |>
    dplyr::summarise(
      total_reviews = dplyr::n(),
      negative_reviews = sum(is_negative, na.rm = TRUE),
      positive_reviews = sum(voted_up, na.rm = TRUE),
      negative_rate = negative_reviews / total_reviews,
      .groups = "drop"
    ) |>
    add_weekly_rolling_metrics(group_cols = group_cols, window = rolling_window)
}

# 生成整体评论数量和差评率，可按语言等字段分组
summarize_reviews_overall <- function(df, group_cols = character()) {
  check_required_columns(
    df,
    c("review_date", "is_negative", "voted_up"),
    "reviews data"
  )

  group_cols <- intersect(group_cols, names(df))

  out <- df

  if (length(group_cols) > 0) {
    out <- out |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  }

  out |>
    dplyr::summarise(
      total_reviews = dplyr::n(),
      negative_reviews = sum(is_negative, na.rm = TRUE),
      positive_reviews = sum(voted_up, na.rm = TRUE),
      negative_rate = mean(is_negative, na.rm = TRUE),
      first_review_date = min(review_date, na.rm = TRUE),
      latest_review_date = max(review_date, na.rm = TRUE),
      .groups = "drop"
    )
}

# 统计语言分布，供 README、报告和语言分布图复用
summarize_language_distribution <- function(df, language_col = "language") {
  check_required_columns(df, language_col, "reviews data")

  df |>
    dplyr::count(language = .data[[language_col]], name = "reviews") |>
    dplyr::mutate(share = reviews / sum(reviews)) |>
    dplyr::arrange(dplyr::desc(reviews))
}
