reviews_page <- function(appid,
                         cursor = "*",
                         filter = "recent",
                         language = "all",
                         review_type = "all",
                         num_per_page = 100,
                         purchase_type = "all",
                         day_range = NULL,
                         filter_offtopic_activity = 1,
                         timeout_sec = 12,
                         retry_max_tries = 2) {
  url <- sprintf("https://store.steampowered.com/appreviews/%s", appid)

  req <- httr2::request(url) |>
    httr2::req_url_query(
      json = 1,
      filter = filter,
      language = language,
      review_type = review_type,
      purchase_type = purchase_type,
      num_per_page = num_per_page,
      cursor = cursor,
      day_range = day_range,
      filter_offtopic_activity = filter_offtopic_activity
    ) |>
    httr2::req_user_agent("steam-reviews-monitor/1.0 (personal project)") |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(
      max_tries = retry_max_tries,
      retry_on_failure = TRUE
    )

  resp <- httr2::req_perform(req)
  txt  <- httr2::resp_body_string(resp)
  dat  <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)

  list(
    cursor = dat$cursor,
    reviews = dat$reviews
  )
}
