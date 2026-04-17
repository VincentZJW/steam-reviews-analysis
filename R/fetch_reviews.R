fetch_recent_reviews <- function(appid,
                          max_pages = 5,
                          sleep_sec = 1,
                          ...) {
  cursor <- "*"
  out <- list()

  for (i in seq_len(max_pages)) {
    message("Fetching page ", i, " ...")

    page <- reviews_page(appid, cursor = cursor, ...)

    if (is.null(page$reviews) || nrow(page$reviews) == 0) {
      break
    }

    out[[i]] <- page$reviews
    cursor <- page$cursor

    Sys.sleep(sleep_sec)
  }

  if (length(out) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(out) |>
    dplyr::mutate(
      app_id = appid,
      created_at = lubridate::as_datetime(timestamp_created),
      updated_at = lubridate::as_datetime(timestamp_updated)
    )
}