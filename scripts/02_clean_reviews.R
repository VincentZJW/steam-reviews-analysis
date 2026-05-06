source("R/clean_reviews.R")

# Clean CHN data
reviews_chn_raw <- readr::read_csv("data/raw/reviews_chn.csv", show_col_types = FALSE)

reviews_chn_clean <- clean_reviews(reviews_chn_raw)

# Save to csv file
readr::write_csv(reviews_chn_clean, "data/clean/reviews_chn_clean.csv")

# Clean ENG data
reviews_eng_raw <- readr::read_csv("data/raw/reviews_eng.csv", show_col_types = FALSE)

reviews_eng_clean <- clean_reviews(reviews_eng_raw)

# Save to csv file
readr::write_csv(reviews_eng_clean, "data/clean/reviews_eng_clean.csv")