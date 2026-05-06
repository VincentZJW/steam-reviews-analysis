# Load data
source("R/summarize_reviews.R")

reviews_chn_clean <- readr::read_csv("data/clean/reviews_chn_clean.csv") |>
dplyr::mutate(
    recommendationid = as.character(recommendationid),
    author.steamid = as.character(author.steamid),
    review_year = as.integer(review_year)
  )

overall_summary <- summarize_reviews_overall(reviews_chn_clean)
daily_summary <- summarize_reviews_daily(reviews_chn_clean)

# Save the summary to CSV file
# readr::write_csv(daily_summary, "data/clean/reviews_chn_daily_summary.csv")

p_volume <- ggplot2::ggplot(daily_summary, ggplot2::aes(x = review_date, y = total_reviews)) +
  ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::geom_line(ggplot2::aes(y = total_reviews), linewidth = 1) +
  ggplot2::labs(
    title = "Daily Review Volume",
    x = "Date",
    y = "Number of Reviews"
  ) +
  ggplot2::theme_minimal()

plotly::ggplotly(p_volume)

# Filter the records that have more than 10 reviews
daily_summary_filtered <- daily_summary |>
  dplyr::filter(total_reviews >= 10)

# Plot the time trend
p_negative_rate <- ggplot2::ggplot(daily_summary_filtered, 
  ggplot2::aes(x = review_date, y = negative_rate)) +
  ggplot2::geom_line(ggplot2::aes(y = negative_rate), linewidth = 1) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    title = "Daily Negative Review Rate",
    x = "Date",
    y = "Negative Rate"
  ) +
  ggplot2::theme_minimal()

plotly::ggplotly(p_negative_rate)

# Weekly intergrated
weekly_summary <- reviews_chn_clean |>
  dplyr::mutate(
    review_week = as.Date(cut(review_date, breaks = "week"))
  ) |>
  dplyr::group_by(review_week) |>
  dplyr::summarise(
    total_reviews = dplyr::n(),
    negative_reviews = sum(is_negative, na.rm = TRUE),
    positive_reviews = sum(voted_up, na.rm = TRUE),
    negative_rate = negative_reviews / total_reviews,
    .groups = "drop"
  ) |>
  dplyr::arrange(review_week)

# Save to csv file
readr::write_csv(weekly_summary, "data/clean/reviews_chn_weekly_summary.csv")

# Weekly negative reviews plot
p_weekly <- ggplot2::ggplot(weekly_summary, ggplot2::aes(x = review_week, y = negative_rate)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    title = "Weekly Negative Review Rate",
    x = "Week",
    y = "Negative Rate"
  ) +
  ggplot2::theme_minimal()

plotly::ggplotly(p_weekly)
