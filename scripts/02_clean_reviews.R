source("R/clean_reviews.R")

reviews_chn_raw <- readr::read_csv("data/raw/reviews_chn.csv", show_col_types = FALSE)

reviews_chn_clean <- clean_reviews(reviews_chn_raw)

# Save to csv file
readr::write_csv(reviews_chn_clean, "data/clean/reviews_chn_clean.csv")