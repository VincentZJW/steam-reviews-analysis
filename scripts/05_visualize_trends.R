# Generate trend and distribution figures from cleaned Steam reviews.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(slider)
  library(tidyr)
})

if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("Package 'ragg' is required to save PNG figures.")
}

figures_dir <- "outputs/figures"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

save_figure <- function(plot, filename, width = 9, height = 5.5) {
  ggplot2::ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot,
    device = ragg::agg_png,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

read_clean_reviews <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      recommendationid = readr::col_character(),
      author.steamid = readr::col_character(),
      .default = readr::col_guess()
    )
  )
}

# Read cleaned Chinese and English review data.
reviews_chn <- read_clean_reviews("data/clean/reviews_chn_clean.csv")
reviews_eng <- read_clean_reviews("data/clean/reviews_eng_clean.csv")

reviews_all <- dplyr::bind_rows(reviews_chn, reviews_eng) |>
  dplyr::mutate(
    language = dplyr::recode(
      language,
      schinese = "Chinese",
      english = "English",
      .default = language
    ),
    sentiment = dplyr::if_else(voted_up, "Positive", "Negative")
  )

# Build daily review metrics by language.
daily_by_language <- reviews_all |>
  dplyr::group_by(language, review_date) |>
  dplyr::summarise(
    total_reviews = dplyr::n(),
    negative_reviews = sum(is_negative, na.rm = TRUE),
    positive_reviews = sum(voted_up, na.rm = TRUE),
    negative_rate = negative_reviews / total_reviews,
    .groups = "drop"
  ) |>
  tidyr::complete(
    language,
    review_date = seq(min(review_date), max(review_date), by = "day"),
    fill = list(total_reviews = 0, negative_reviews = 0, positive_reviews = 0)
  ) |>
  dplyr::arrange(language, review_date) |>
  dplyr::group_by(language) |>
  dplyr::mutate(
    negative_rate = dplyr::if_else(total_reviews > 0, negative_reviews / total_reviews, NA_real_),
    rolling_negative_reviews = slider::slide_dbl(negative_reviews, sum, .before = 6, .complete = TRUE),
    rolling_total_reviews = slider::slide_dbl(total_reviews, sum, .before = 6, .complete = TRUE),
    negative_rate_7d = dplyr::if_else(
      rolling_total_reviews > 0,
      rolling_negative_reviews / rolling_total_reviews,
      NA_real_
    )
  ) |>
  dplyr::ungroup()

# Plot daily review volume by language.
p_daily_volume <- ggplot2::ggplot(
  daily_by_language,
  ggplot2::aes(x = review_date, y = total_reviews, color = language)
) +
  ggplot2::geom_line(linewidth = 0.8, alpha = 0.9) +
  ggplot2::scale_color_manual(values = c("Chinese" = "#2C7FB8", "English" = "#D95F0E")) +
  ggplot2::labs(
    title = "Daily Review Volume by Language",
    x = "Review Date",
    y = "Number of Reviews",
    color = "Language"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "top")

save_figure(p_daily_volume, "daily_review_volume_by_language.png")

# Plot daily negative review rate by language.
p_daily_negative_rate <- daily_by_language |>
  dplyr::filter(total_reviews >= 10) |>
  ggplot2::ggplot(ggplot2::aes(x = review_date, y = negative_rate, color = language)) +
  ggplot2::geom_line(linewidth = 0.8, alpha = 0.9) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_color_manual(values = c("Chinese" = "#2C7FB8", "English" = "#D95F0E")) +
  ggplot2::labs(
    title = "Daily Negative Review Rate by Language",
    subtitle = "Only days with at least 10 reviews are shown",
    x = "Review Date",
    y = "Negative Review Rate",
    color = "Language"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "top")

save_figure(p_daily_negative_rate, "daily_negative_rate_by_language.png")

# Plot 7-day rolling negative review rate by language.
p_rolling_negative_rate <- daily_by_language |>
  dplyr::filter(!is.na(negative_rate_7d)) |>
  ggplot2::ggplot(ggplot2::aes(x = review_date, y = negative_rate_7d, color = language)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_color_manual(values = c("Chinese" = "#2C7FB8", "English" = "#D95F0E")) +
  ggplot2::labs(
    title = "7-Day Rolling Negative Review Rate",
    x = "Review Date",
    y = "7-Day Negative Review Rate",
    color = "Language"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "top")

save_figure(p_rolling_negative_rate, "rolling_7d_negative_rate_by_language.png")

# Plot overall positive and negative review share by language.
sentiment_share <- reviews_all |>
  dplyr::count(language, sentiment) |>
  dplyr::group_by(language) |>
  dplyr::mutate(share = n / sum(n)) |>
  dplyr::ungroup()

p_sentiment_share <- ggplot2::ggplot(
  sentiment_share,
  ggplot2::aes(x = language, y = share, fill = sentiment)
) +
  ggplot2::geom_col(width = 0.65) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_fill_manual(values = c("Positive" = "#2CA25F", "Negative" = "#DE2D26")) +
  ggplot2::labs(
    title = "Positive and Negative Review Share",
    x = "Language",
    y = "Share of Reviews",
    fill = "Sentiment"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "top")

save_figure(p_sentiment_share, "sentiment_share_by_language.png")

# Plot language distribution in the cleaned dataset.
language_distribution <- reviews_all |>
  dplyr::count(language) |>
  dplyr::mutate(share = n / sum(n))

p_language_distribution <- ggplot2::ggplot(
  language_distribution,
  ggplot2::aes(x = reorder(language, n), y = n, fill = language)
) +
  ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(scales::comma(n), " (", scales::percent(share, accuracy = 0.1), ")")),
    vjust = -0.35,
    size = 3.5
  ) +
  ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.12))) +
  ggplot2::scale_fill_manual(values = c("Chinese" = "#2C7FB8", "English" = "#D95F0E")) +
  ggplot2::labs(
    title = "Review Language Distribution",
    x = "Language",
    y = "Number of Reviews"
  ) +
  ggplot2::theme_minimal(base_size = 12)

save_figure(p_language_distribution, "language_distribution.png", width = 7, height = 5)

message("Saved trend figures to: ", normalizePath(figures_dir))
