save_raw_reviews <- function(df, appid, prefix = "reviews_raw") {
  dir.create("data/raw/reviews", recursive = TRUE, showWarnings = FALSE)

  file_name <- sprintf(
    "data/raw/reviews/%s_appid_%s_%s.csv",
    prefix,
    appid,
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  readr::write_csv(df, file_name)
  message("Saved to: ", file_name)

  invisible(file_name)
}