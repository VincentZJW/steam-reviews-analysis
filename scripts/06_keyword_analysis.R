# Generate semantic keyword/keyphrase outputs for Steam reviews.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(stringr)
  library(stringi)
})

if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("Package 'ragg' is required to save PNG figures.")
}

source("R/text_analysis.R")

clean_dir <- "data/clean"
figures_dir <- "outputs/figures"
tables_dir <- "outputs/tables"

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

top_n <- 20
wordcloud_n <- 45

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

save_keyword_figure <- function(plot, filename, width = 8.5, height = 6) {
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

# Read the cleaned review datasets produced by the cleaning pipeline.
reviews_eng <- read_clean_reviews(file.path(clean_dir, "reviews_eng_clean.csv"))
reviews_chn <- read_clean_reviews(file.path(clean_dir, "reviews_chn_clean.csv"))

# Main outputs focus on negative reviews because they best explain player pain points.
english_semantic_keywords <- extract_english_semantic_keywords(
  reviews_eng,
  text_col = "review_clean",
  sentiment_scope = "negative",
  top_n = top_n,
  max_ngram = 4,
  min_count = 2
)

chinese_semantic_keywords <- extract_chinese_semantic_keywords(
  reviews_chn,
  text_col = "review_clean",
  sentiment_scope = "negative",
  top_n = top_n,
  max_ngram = 5,
  min_count = 2
)

# Keep all-review versions for later comparison, but do not use them as the main charts.
english_all_semantic_keywords <- extract_english_semantic_keywords(
  reviews_eng,
  text_col = "review_clean",
  sentiment_scope = "all",
  top_n = top_n,
  max_ngram = 4,
  min_count = 2
)

chinese_all_semantic_keywords <- extract_chinese_semantic_keywords(
  reviews_chn,
  text_col = "review_clean",
  sentiment_scope = "all",
  top_n = top_n,
  max_ngram = 5,
  min_count = 2
)

# Save semantic keyword tables. The primary files are negative-review focused.
readr::write_csv(
  english_semantic_keywords,
  file.path(tables_dir, "english_semantic_keywords.csv")
)
readr::write_csv(
  chinese_semantic_keywords,
  file.path(tables_dir, "chinese_semantic_keywords.csv")
)
readr::write_csv(
  english_semantic_keywords,
  file.path(tables_dir, "english_negative_semantic_keywords.csv")
)
readr::write_csv(
  chinese_semantic_keywords,
  file.path(tables_dir, "chinese_negative_semantic_keywords.csv")
)
readr::write_csv(
  english_all_semantic_keywords,
  file.path(tables_dir, "english_all_semantic_keywords.csv")
)
readr::write_csv(
  chinese_all_semantic_keywords,
  file.path(tables_dir, "chinese_all_semantic_keywords.csv")
)

# Save frequency bar charts for the semantic keyword/keyphrase outputs.
save_keyword_figure(
  plot_keyword_bars(
    english_semantic_keywords,
    term_col = "keyword",
    title = "Top English Semantic Keywords in Negative Reviews",
    fill = "#D95F0E",
    top_n = top_n
  ),
  "top_english_semantic_keywords.png"
)

save_keyword_figure(
  plot_keyword_bars(
    chinese_semantic_keywords,
    term_col = "keyword",
    title = "Top Chinese Semantic Keywords in Negative Reviews",
    fill = "#2C7FB8",
    base_family = "Arial Unicode MS",
    top_n = top_n
  ),
  "top_chinese_semantic_keywords.png"
)

# Save word clouds based only on the final quality-filtered semantic outputs.
save_keyword_figure(
  plot_keyword_wordcloud(
    english_semantic_keywords,
    term_col = "keyword",
    title = "English Semantic Keywords Word Cloud",
    max_terms = wordcloud_n,
    palette = c("#D95F0E", "#756BB1", "#3182BD", "#31A354"),
    seed = 2026
  ),
  "english_semantic_keywords_wordcloud.png",
  width = 9,
  height = 6
)

save_keyword_figure(
  plot_keyword_wordcloud(
    chinese_semantic_keywords,
    term_col = "keyword",
    title = "Chinese Semantic Keywords Word Cloud",
    max_terms = wordcloud_n,
    palette = c("#2C7FB8", "#31A354", "#756BB1", "#D95F0E"),
    base_family = "Arial Unicode MS",
    seed = 2027
  ),
  "chinese_semantic_keywords_wordcloud.png",
  width = 9,
  height = 6
)

message("Saved semantic keyword tables to: ", normalizePath(tables_dir))
message("Saved semantic keyword figures to: ", normalizePath(figures_dir))
