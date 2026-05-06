source("R/steam_api.R")
source("R/fetch_reviews.R")

appid <- 2246340

# Get Chinese reviews
reviews_chn <- fetch_recent_reviews(
  appid = appid,
  max_pages = 50,
  filter = "recent",
  language = "schinese",
  review_type = "all",
  purchase_type = "steam"
)

reviews_chn_data <- reviews_chn |>
  jsonlite::flatten() |>
  dplyr::select(recommendationid, language, review, voted_up, created_at, updated_at,
  author.steamid, author.num_games_owned, author.num_reviews)

# Save to csv file
readr::write_csv(reviews_chn_data, "data/raw/reviews_chn.csv")

# Get English reviews
reviews_eng <- fetch_recent_reviews(
  appid = appid,
  max_pages = 50,
  filter = "recent",
  language = "english",
  review_type = "all",
  purchase_type = "steam"
)

reviews_eng_data <- reviews_eng |>
  jsonlite::flatten() |>
  dplyr::select(recommendationid, language, review, voted_up, created_at, updated_at,
  author.steamid, author.num_games_owned, author.num_reviews)

# Save to csv file
readr::write_csv(reviews_eng_data, "data/raw/reviews_eng.csv")
