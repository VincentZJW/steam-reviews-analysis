source("R/steam_api.R")
source("R/fetch_reviews.R")
source("R/io.R")

appid <- 570

reviews <- fetch_recent_reviews(
  appid = appid,
  max_pages = 5,
  filter = "recent",
  language = "chinese",
  review_type = "all",
  purchase_type = "steam"
)

save_raw_reviews(reviews, appid = appid)

dplyr::glimpse(reviews)
head(reviews)