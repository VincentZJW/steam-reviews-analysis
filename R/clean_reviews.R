clean_reviews <- function(df) {
  df |>
    dplyr::mutate(
      recommendationid = as.character(recommendationid),
      author.steamid = as.character(author.steamid)
    ) |>
    dplyr::distinct(recommendationid, .keep_all = TRUE) |>
    dplyr::mutate(
      review = stringr::str_replace_all(review, "[\\r\\n]+", " "),
      review = stringr::str_squish(review),

      created_at = lubridate::ymd_hms(created_at, tz = "UTC"),
      updated_at = lubridate::ymd_hms(updated_at, tz = "UTC"),

      review_date = as.Date(created_at),
      review_year = as.integer(lubridate::year(created_at)),
      review_month = format(created_at, "%Y-%m"),

      is_negative = !voted_up,

      review_clean = review |>
        stringr::str_to_lower() |>
        stringr::str_replace_all("https?://\\S+|www\\.\\S+", " ") |>
        stringr::str_replace_all("[[:punct:]]+", " ") |>
        stringr::str_replace_all("[[:digit:]]+", " ") |>
        stringr::str_squish()
    ) |>
    dplyr::filter(!is.na(review), review != "") |>
    dplyr::select(
      recommendationid,
      language,
      voted_up,
      is_negative,
      review,
      review_clean,
      created_at,
      updated_at,
      review_date,
      review_year,
      review_month,
      author.steamid,
      author.num_games_owned,
      author.num_reviews
    )
}