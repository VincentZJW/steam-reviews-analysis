# 差评异常波动检测函数

detect_review_alerts <- function(daily_summary,
                                 min_reviews = 10,
                                 k = 2,
                                 window = 7,
                                 metric = "Negative Rate") {
  required_cols <- c("review_date", "total_reviews", "negative_reviews", "negative_rate")
  missing_cols <- setdiff(required_cols, names(daily_summary))

  if (length(missing_cols) > 0) {
    stop(
      "daily_summary 缺少必要字段: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(daily_summary) == 0) {
    return(
      daily_summary |>
        dplyr::mutate(
          alert_metric = character(),
          alert_value = numeric(),
          rolling_mean = numeric(),
          rolling_sd = numeric(),
          alert_threshold = numeric(),
          distance_to_threshold = numeric(),
          neg_rate_mean = numeric(),
          neg_rate_sd = numeric(),
          neg_rate_threshold = numeric(),
          alert_flag = logical()
        )
    )
  }

  value_col <- switch(
    metric,
    "Negative Review Count" = "negative_reviews",
    "Review Volume Spike" = "total_reviews",
    "negative_reviews" = "negative_reviews",
    "total_reviews" = "total_reviews",
    "Negative Rate" = "negative_rate",
    "negative_rate" = "negative_rate",
    "negative_rate"
  )

  out <- daily_summary |>
    dplyr::arrange(review_date) |>
    dplyr::mutate(
      alert_metric = metric,
      alert_value = .data[[value_col]],
      rolling_mean = slider::slide_dbl(
        alert_value,
        mean,
        .before = window - 1,
        .complete = TRUE,
        na.rm = TRUE
      ),
      rolling_sd = slider::slide_dbl(
        alert_value,
        sd,
        .before = window - 1,
        .complete = TRUE,
        na.rm = TRUE
      ),
      alert_threshold = rolling_mean + k * rolling_sd,
      distance_to_threshold = alert_value - alert_threshold,
      neg_rate_mean = slider::slide_dbl(
        negative_rate,
        mean,
        .before = window - 1,
        .complete = TRUE,
        na.rm = TRUE
      ),
      neg_rate_sd = slider::slide_dbl(
        negative_rate,
        sd,
        .before = window - 1,
        .complete = TRUE,
        na.rm = TRUE
      ),
      neg_rate_threshold = neg_rate_mean + k * neg_rate_sd,
      alert_flag = total_reviews >= min_reviews &
        !is.na(alert_threshold) &
        alert_value > alert_threshold
    )

  out
}

filter_alert_reviews <- function(reviews, alert_days) {
  if (nrow(reviews) == 0 || nrow(alert_days) == 0) {
    return(reviews[0, , drop = FALSE])
  }

  alert_dates <- alert_days |>
    dplyr::filter(alert_flag) |>
    dplyr::pull(review_date)

  reviews |>
    dplyr::filter(review_date %in% alert_dates, is_negative)
}
