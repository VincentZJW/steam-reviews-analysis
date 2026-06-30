fetch_recent_reviews <- function(appid,
                          max_pages = 5,
                          sleep_sec = 1,
                          fail_on_error = FALSE,
                          ...) {
  cursor <- "*"
  out <- list()

  for (i in seq_len(max_pages)) {
    message("Fetching page ", i, " ...")

    page <- tryCatch(
      reviews_page(appid, cursor = cursor, ...),
      error = function(e) {
        if (fail_on_error) {
          stop(e)
        }

        warning(
          "Steam reviews request failed on page ",
          i,
          " for appid ",
          appid,
          ": ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )

    if (is.null(page)) {
      break
    }

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
