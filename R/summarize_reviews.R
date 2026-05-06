# Function to summarize daily reviews
summarize_reviews_daily <- function(df) {
  df |>
    dplyr::group_by(review_date) |>
    dplyr::summarise(
      total_reviews = dplyr::n(),
      negative_reviews = sum(is_negative, na.rm = TRUE),
      positive_reviews = sum(voted_up, na.rm = TRUE),
      negative_rate = negative_reviews / total_reviews,
      .groups = "drop"
    ) |>
    dplyr::arrange(review_date)
}

# Function to summarize all reviews
summarize_reviews_overall <- function(df) {
  df |>
    dplyr::summarise(
      total_reviews = dplyr::n(),
      negative_reviews = sum(is_negative, na.rm = TRUE),
      positive_reviews = sum(voted_up, na.rm = TRUE),
      negative_rate = mean(is_negative, na.rm = TRUE)
    )
}