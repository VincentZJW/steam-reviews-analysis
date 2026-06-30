# Steam 游戏搜索函数

steam_app_list_url <- function() {
  "https://api.steampowered.com/ISteamApps/GetAppList/v0002/"
}

steam_store_search_url <- function() {
  "https://store.steampowered.com/api/storesearch/"
}

steam_app_list_cache_file <- function(cache_dir = "data/cache") {
  file.path(cache_dir, "steam_app_list.rds")
}

steam_store_search_cache_file <- function(query, cache_dir = "data/cache") {
  safe_query <- query |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "-") |>
    stringr::str_replace_all("(^-|-$)", "")

  file.path(cache_dir, paste0("steam_store_search_", safe_query, ".rds"))
}

is_cache_fresh <- function(path, cache_hours = 24) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  age_hours <- as.numeric(
    difftime(Sys.time(), file.info(path)$mtime, units = "hours")
  )

  is.finite(age_hours) && age_hours <= cache_hours
}

get_steam_app_list <- function(cache_dir = "data/cache",
                               cache_hours = 24,
                               use_cache = TRUE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- steam_app_list_cache_file(cache_dir)

  if (use_cache && is_cache_fresh(cache_file, cache_hours)) {
    return(readRDS(cache_file))
  }

  app_list <- tryCatch(
    {
      resp <- httr2::request(steam_app_list_url()) |>
        httr2::req_url_query(format = "json") |>
        httr2::req_user_agent("steam-reviews-monitor/1.0 (personal project)") |>
        httr2::req_timeout(30) |>
        httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
        httr2::req_perform()

      body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      apps <- tibble::as_tibble(body$applist$apps)

      apps |>
        dplyr::transmute(
          appid = as.integer(appid),
          name = stringr::str_squish(as.character(name))
        ) |>
        dplyr::filter(!is.na(appid), !is.na(name), name != "") |>
        dplyr::distinct(appid, .keep_all = TRUE) |>
        dplyr::arrange(name)
    },
    error = function(e) {
      if (file.exists(cache_file)) {
        warning(
          "Steam app list 请求失败，已使用旧缓存: ",
          conditionMessage(e),
          call. = FALSE
        )
        return(readRDS(cache_file))
      }

      stop("Steam app list 请求失败，且本地没有可用缓存: ", conditionMessage(e), call. = FALSE)
    }
  )

  saveRDS(app_list, cache_file)
  app_list
}

search_steam_games <- function(query,
                               app_list = NULL,
                               cache_dir = "data/cache",
                               max_results = 20,
                               use_cache = TRUE) {
  query <- stringr::str_squish(query %||% "")

  if (query == "") {
    return(tibble::tibble(appid = integer(), name = character(), match_score = numeric()))
  }

  if (is.null(app_list)) {
    return(
      search_steam_store_games(
        query = query,
        cache_dir = cache_dir,
        max_results = max_results,
        use_cache = use_cache
      )
    )
  }

  query_lower <- stringr::str_to_lower(query)
  app_names_lower <- stringr::str_to_lower(app_list$name)

  exact_match <- app_names_lower == query_lower
  starts_match <- stringr::str_starts(app_names_lower, stringr::fixed(query_lower))
  contains_match <- stringr::str_detect(app_names_lower, stringr::fixed(query_lower))

  edit_distance <- utils::adist(query_lower, app_names_lower, ignore.case = TRUE)[1, ]
  normalized_distance <- edit_distance / pmax(nchar(app_names_lower), nchar(query_lower), 1)

  app_list |>
    dplyr::mutate(
      match_score = dplyr::case_when(
        exact_match ~ 100,
        starts_match ~ 85,
        contains_match ~ 70,
        normalized_distance <= 0.2 ~ 55,
        normalized_distance <= 0.35 ~ 40,
        TRUE ~ 0
      ),
      distance = normalized_distance
    ) |>
    dplyr::filter(match_score > 0) |>
    dplyr::arrange(dplyr::desc(match_score), distance, name) |>
    dplyr::slice_head(n = max_results) |>
    dplyr::select(appid, name, match_score)
}

search_steam_store_games <- function(query,
                                     cache_dir = "data/cache",
                                     max_results = 20,
                                     use_cache = TRUE,
                                     cache_hours = 24,
                                     country = "US",
                                     language = "english") {
  query <- stringr::str_squish(query %||% "")

  if (query == "") {
    return(tibble::tibble(appid = integer(), name = character(), match_score = numeric()))
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- steam_store_search_cache_file(query, cache_dir)

  if (use_cache && is_cache_fresh(cache_file, cache_hours)) {
    return(readRDS(cache_file))
  }

  results <- tryCatch(
    {
      resp <- httr2::request(steam_store_search_url()) |>
        httr2::req_url_query(
          term = query,
          l = language,
          cc = country
        ) |>
        httr2::req_user_agent("steam-reviews-monitor/1.0 (personal project)") |>
        httr2::req_timeout(20) |>
        httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
        httr2::req_perform()

      body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      if (is.null(body$items) || length(body$items) == 0) {
        return(tibble::tibble(appid = integer(), name = character(), match_score = numeric()))
      }

      items <- tibble::as_tibble(body$items)
      query_lower <- stringr::str_to_lower(query)
      name_lower <- stringr::str_to_lower(items$name)

      items |>
        dplyr::transmute(
          appid = as.integer(id),
          name = stringr::str_squish(as.character(name)),
          match_score = dplyr::case_when(
            name_lower == query_lower ~ 100,
            stringr::str_starts(name_lower, stringr::fixed(query_lower)) ~ 85,
            stringr::str_detect(name_lower, stringr::fixed(query_lower)) ~ 70,
            TRUE ~ 50
          )
        ) |>
        dplyr::filter(!is.na(appid), !is.na(name), name != "") |>
        dplyr::distinct(appid, .keep_all = TRUE) |>
        dplyr::arrange(dplyr::desc(match_score), name) |>
        dplyr::slice_head(n = max_results)
    },
    error = function(e) {
      if (file.exists(cache_file)) {
        warning(
          "Steam Store 搜索请求失败，已使用旧缓存: ",
          conditionMessage(e),
          call. = FALSE
        )
        return(readRDS(cache_file))
      }

      stop("Steam Store 搜索请求失败，且本地没有可用缓存: ", conditionMessage(e), call. = FALSE)
    }
  )

  saveRDS(results, cache_file)
  results
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
