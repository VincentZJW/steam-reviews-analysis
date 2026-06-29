# 基于清洗后的 Steam 评论数据构建指标表

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/summarize_reviews.R")

clean_dir <- "data/clean"
tables_dir <- "outputs/tables"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

read_clean_reviews <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      recommendationid = readr::col_character(),
      author.steamid = readr::col_character(),
      created_at = readr::col_datetime(),
      updated_at = readr::col_datetime(),
      review_date = readr::col_date(),
      .default = readr::col_guess()
    )
  )
}

standardize_review_language <- function(df, language_label) {
  df |>
    dplyr::mutate(language = language_label)
}

# 读取 scripts/02_clean_reviews.R 生成的清洗数据
reviews_chn <- read_clean_reviews(file.path(clean_dir, "reviews_chn_clean.csv")) |>
  standardize_review_language("Chinese")

reviews_eng <- read_clean_reviews(file.path(clean_dir, "reviews_eng_clean.csv")) |>
  standardize_review_language("English")

reviews_all <- dplyr::bind_rows(reviews_chn, reviews_eng)

# 生成单语言和合并语言维度的每日指标
reviews_chn_daily_summary <- summarize_reviews_daily(reviews_chn)
reviews_eng_daily_summary <- summarize_reviews_daily(reviews_eng)
reviews_daily_by_language <- summarize_reviews_daily(
  reviews_all,
  group_cols = "language"
)

# 生成单语言和合并语言维度的每周指标
reviews_chn_weekly_summary <- summarize_reviews_weekly(reviews_chn)
reviews_eng_weekly_summary <- summarize_reviews_weekly(reviews_eng)
reviews_weekly_by_language <- summarize_reviews_weekly(
  reviews_all,
  group_cols = "language"
)

# 生成适合 README 和报告使用的整体汇总表
overall_all <- summarize_reviews_overall(reviews_all) |>
  dplyr::mutate(language = "All", .before = 1)

overall_by_language <- summarize_reviews_overall(
  reviews_all,
  group_cols = "language"
)

reviews_overall_summary <- dplyr::bind_rows(
  overall_all,
  overall_by_language
)

language_distribution <- summarize_language_distribution(reviews_all)

# 保存核心指标表
readr::write_csv(
  reviews_chn_daily_summary,
  file.path(clean_dir, "reviews_chn_daily_summary.csv")
)
readr::write_csv(
  reviews_eng_daily_summary,
  file.path(clean_dir, "reviews_eng_daily_summary.csv")
)
readr::write_csv(
  reviews_daily_by_language,
  file.path(clean_dir, "reviews_daily_by_language.csv")
)
readr::write_csv(
  reviews_chn_weekly_summary,
  file.path(clean_dir, "reviews_chn_weekly_summary.csv")
)
readr::write_csv(
  reviews_eng_weekly_summary,
  file.path(clean_dir, "reviews_eng_weekly_summary.csv")
)
readr::write_csv(
  reviews_weekly_by_language,
  file.path(clean_dir, "reviews_weekly_by_language.csv")
)

# 保存报告用表格
readr::write_csv(
  reviews_overall_summary,
  file.path(tables_dir, "reviews_overall_summary.csv")
)
readr::write_csv(
  language_distribution,
  file.path(tables_dir, "language_distribution.csv")
)

message("Saved daily and weekly metrics to: ", normalizePath(clean_dir))
message("Saved reporting tables to: ", normalizePath(tables_dir))
