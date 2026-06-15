# Load the data
daily_summary <- readr::read_csv(
  "data/clean/reviews_chn_daily_summary.csv",
  col_types = readr::cols(
    review_date = readr::col_date(),
    total_reviews = readr::col_double(),
    negative_reviews = readr::col_double(),
    positive_reviews = readr::col_double(),
    negative_rate = readr::col_double()
  )
)

daily_alerts <- daily_summary |>
  dplyr::arrange(review_date) |>
  dplyr::mutate(
    neg_rate_mean_7 = slider::slide_dbl(
      negative_rate,
      mean,
      .before = 6,
      .complete = TRUE,
      na.rm = TRUE
    ),
    neg_rate_sd_7 = slider::slide_dbl(
      negative_rate,
      sd,
      .before = 6,
      .complete = TRUE,
      na.rm = TRUE
    ),
    review_vol_mean_7 = slider::slide_dbl(
      total_reviews,
      mean,
      .before = 6,
      .complete = TRUE,
      na.rm = TRUE
    ),
    review_vol_sd_7 = slider::slide_dbl(
      total_reviews,
      sd,
      .before = 6,
      .complete = TRUE,
      na.rm = TRUE
    ),
    neg_rate_threshold = neg_rate_mean_7 + 2 * neg_rate_sd_7,
    review_vol_threshold = review_vol_mean_7 + 2 * review_vol_sd_7,
    alert_flag = total_reviews >= 10 &
      !is.na(neg_rate_threshold) &
      negative_rate > neg_rate_threshold
  )

alert_days <- daily_alerts |>
  dplyr::filter(alert_flag) |>
  dplyr::select(
    review_date,
    total_reviews,
    negative_reviews,
    positive_reviews,
    negative_rate,
    neg_rate_mean_7,
    neg_rate_sd_7,
    neg_rate_threshold,
    review_vol_mean_7,
    review_vol_sd_7,
    review_vol_threshold,
    alert_flag
  )