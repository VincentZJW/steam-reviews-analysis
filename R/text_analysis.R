# Text analysis helpers for Steam review keyword extraction.

english_title_stopwords <- function() {
  c(
    "monster", "monsters", "hunter", "hunters", "wilds", "world",
    "capcom", "mhwilds", "rise", "hunt"
  )
}

english_generic_game_stopwords <- function() {
  c(
    "game", "games", "play", "playing", "played", "player", "players",
    "gameplay", "steam", "review", "reviews", "weapon", "weapons", "story",
    "quest", "quests", "mission", "missions"
  )
}

english_low_information_stopwords <- function() {
  c(
    "now", "its", "it", "some", "after", "much", "than", "really", "still",
    "even", "also", "just", "very", "quite", "pretty", "actually", "maybe",
    "thing", "things", "something", "anything", "everything", "good", "great",
    "better", "best", "fun", "new", "time", "times", "hour", "hours", "lot",
    "lots", "feel", "feels", "felt", "love", "keep", "don", "dont", "doesn",
    "didn", "isn", "wasn", "cant", "way", "say", "says", "said", "run",
    "runs", "series", "back", "first", "people", "think", "want", "wants",
    "recommend", "recommended", "experience", "keeps", "wise", "stable",
    "system", "systems", "update", "updates", "updated", "patch", "patches",
    "improvement", "improvements", "recent", "expansion", "content", "year",
    "years", "finally", "graphics", "high", "far", "dlc", "fight", "fights",
    "easy", "end", "bit", "hunting", "amazing", "big", "rank", "though",
    "base", "overall", "fixed", "solid"
  )
}

english_standard_stopwords <- function() {
  c(
    "a", "about", "above", "across", "again", "against", "all", "almost",
    "alone", "along", "already", "although", "always", "am", "among", "an",
    "and", "another", "any", "anyone", "are", "around", "as", "at", "away",
    "be", "because", "been", "before", "being", "between", "both", "but",
    "by", "can", "cannot", "could", "did", "do", "does", "doing", "done",
    "down", "during", "each", "either", "enough", "ever", "every", "few",
    "for", "from", "further", "get", "gets", "getting", "got", "had", "has",
    "have", "having", "he", "her", "here", "hers", "him", "his", "how", "i",
    "if", "in", "into", "is", "isn", "itself", "let", "like", "made",
    "make", "makes", "many", "may", "me", "might", "mine", "more", "most",
    "my", "near", "need", "needs", "never", "no", "nor", "not", "of", "off",
    "on", "once", "one", "only", "or", "other", "our", "ours",
    "out", "over", "own", "rather", "same", "see", "seem", "seems", "she",
    "should", "since", "so", "than", "that", "the", "their", "them", "then",
    "there", "these", "they", "this", "those", "through", "to", "too",
    "under", "until", "up", "us", "use", "used", "using", "was", "we",
    "well", "were", "what", "when", "where", "which", "while", "who",
    "whom", "why", "will", "with", "within", "without", "would", "you",
    "your", "yours"
  )
}

english_stopwords <- function(extra_stopwords = character()) {
  unique(tolower(c(
    english_standard_stopwords(),
    english_title_stopwords(),
    english_generic_game_stopwords(),
    english_low_information_stopwords(),
    extra_stopwords
  )))
}

english_domain_stopwords <- function(extra_stopwords = character()) {
  unique(tolower(c(
    english_title_stopwords(),
    english_generic_game_stopwords(),
    english_low_information_stopwords(),
    extra_stopwords
  )))
}

english_issue_terms <- function() {
  c(
    "bad", "broken", "bug", "compilation", "compile", "connection", "crash",
    "disconnect", "drop", "error", "fps", "frame", "freeze", "issue", "lag",
    "loading", "optimization", "performance", "poor", "problem", "refund",
    "server", "shader", "slow", "stutter", "unplayable"
  )
}

english_positive_terms <- function() {
  c(
    "recommend", "recommended", "worth", "enjoy", "enjoyed", "enjoyable",
    "fun", "charming", "cute", "beautiful", "great", "good", "excellent",
    "amazing", "polished", "smooth", "cozy", "relaxing", "music", "soundtrack",
    "story", "visuals", "art", "friends", "coop", "cooperative"
  )
}

normalize_english_tokens <- function(tokens) {
  tokens <- tolower(tokens)
  tokens[tokens %in% c("crashes", "crashed", "crashing")] <- "crash"
  tokens[tokens %in% c("stutters", "stuttered", "stuttering")] <- "stutter"
  tokens[tokens %in% c("lags", "lagged", "lagging")] <- "lag"
  tokens[tokens %in% c("frames")] <- "frame"
  tokens[tokens %in% c("drops", "dropped", "dropping")] <- "drop"
  tokens[tokens %in% c("shaders")] <- "shader"
  tokens[tokens %in% c("issues")] <- "issue"
  tokens[tokens %in% c("problems")] <- "problem"
  tokens[tokens %in% c("bugs", "bugged", "buggy")] <- "bug"
  tokens[tokens %in% c("optimisation", "optimizations", "optimisations")] <- "optimization"
  tokens[tokens %in% c("disconnects", "disconnected", "disconnecting")] <- "disconnect"
  tokens[tokens %in% c("freezes", "freezing", "froze")] <- "freeze"
  tokens[tokens %in% c("refunds", "refunded", "refunding")] <- "refund"
  tokens[tokens %in% c("servers")] <- "server"
  tokens[tokens %in% c("controls")] <- "control"
  tokens
}

clean_english_text <- function(text) {
  text <- ifelse(is.na(text), "", text)
  text |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("https?://\\S+|www\\.\\S+", " ") |>
    stringr::str_replace_all("&amp;", " and ") |>
    stringr::str_replace_all("[^a-z\\s']+", " ") |>
    stringr::str_squish()
}

tokenize_english_reviews <- function(text) {
  cleaned <- clean_english_text(text)
  lapply(stringr::str_extract_all(cleaned, "[a-z]+"), normalize_english_tokens)
}

filter_reviews_by_sentiment <- function(reviews, sentiment_scope = c("negative", "all", "positive")) {
  sentiment_scope <- match.arg(sentiment_scope)

  if (sentiment_scope == "all" || !"is_negative" %in% names(reviews)) {
    return(reviews)
  }

  if (sentiment_scope == "negative") {
    return(dplyr::filter(reviews, is_negative))
  }

  dplyr::filter(reviews, !is_negative)
}

english_semantic_signal_pattern <- function() {
  paste(
    c(
      "performance", "optimization", "optimisation", "frame", "fps", "shader",
      "crash", "bug", "server", "connection", "disconnect", "loading",
      "balance", "stutter", "lag", "error", "refund", "control", "keyboard",
      "mouse", "difficulty", "combat", "unplayable", "broken"
    ),
    collapse = "|"
  )
}

english_positive_signal_pattern <- function() {
  paste(
    c(
      "recommend", "worth", "enjoy", "enjoyable", "fun", "charming", "cute",
      "beautiful", "excellent", "amazing", "polished", "smooth", "cozy",
      "relaxing", "music", "soundtrack", "story", "visuals", "art",
      "friends", "coop", "cooperative"
    ),
    collapse = "|"
  )
}

normalize_english_positive_term <- function(term) {
  term <- term |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z\\s]+", " ") |>
    stringr::str_squish()

  if (term == "") {
    return(NA_character_)
  }

  if (stringr::str_detect(term, "recommend|worth")) {
    return("worth recommending")
  }

  if (stringr::str_detect(term, "friend|coop|cooperative") &&
      stringr::str_detect(term, "fun|enjoy|great|good|recommend")) {
    return("fun with friends")
  }

  if (stringr::str_detect(term, "music|soundtrack|sound") &&
      stringr::str_detect(term, "good|great|beautiful|amazing|excellent|love|enjoy")) {
    return("great soundtrack")
  }

  if (stringr::str_detect(term, "story|narrative") &&
      stringr::str_detect(term, "good|great|beautiful|interesting|love|enjoy")) {
    return("strong story")
  }

  if (stringr::str_detect(term, "visual|art|graphics") &&
      stringr::str_detect(term, "beautiful|good|great|amazing|excellent|cute")) {
    return("beautiful visuals")
  }

  if (stringr::str_detect(term, "smooth|polished")) {
    return("smooth experience")
  }

  if (stringr::str_detect(term, "cozy|relaxing|charming|cute")) {
    return("charming experience")
  }

  if (stringr::str_detect(term, "fun|enjoy|enjoyable")) {
    return("fun experience")
  }

  NA_character_
}

normalize_english_semantic_term <- function(term, sentiment_scope = "negative") {
  term <- term |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z\\s]+", " ") |>
    stringr::str_squish()

  if (term == "") {
    return(NA_character_)
  }

  if (identical(sentiment_scope, "positive")) {
    return(normalize_english_positive_term(term))
  }

  if (stringr::str_detect(term, "performance") &&
      stringr::str_detect(term, "issue|problem|bad|poor|terrible|unplayable")) {
    return("performance issues")
  }

  if (stringr::str_detect(term, "optim") &&
      stringr::str_detect(term, "bad|poor|terrible|issue|problem|broken")) {
    return("bad optimization")
  }

  if (stringr::str_detect(term, "frame|fps") &&
      stringr::str_detect(term, "drop|stutter|lag")) {
    return("frame drops")
  }

  if (stringr::str_detect(term, "fps") &&
      stringr::str_detect(term, "low|poor|bad")) {
    return("low fps")
  }

  if (stringr::str_detect(term, "shader") &&
      stringr::str_detect(term, "compilation|compile|cache")) {
    return("shader compilation")
  }

  if (stringr::str_detect(term, "server|connection") &&
      stringr::str_detect(term, "issue|problem|disconnect|error|offline")) {
    return("server issues")
  }

  if (stringr::str_detect(term, "crash") &&
      stringr::str_detect(term, "often|constant|random|frequent|keep|keeps")) {
    return("frequent crashes")
  }

  if (stringr::str_detect(term, "fatal error")) {
    return("fatal errors")
  }

  if (stringr::str_detect(term, "loading") &&
      stringr::str_detect(term, "screen|time|slow|issue|problem")) {
    return("loading issues")
  }

  if (stringr::str_detect(term, "balance") &&
      stringr::str_detect(term, "bad|poor|terrible|issue|problem")) {
    return("poor balance")
  }

  if (stringr::str_detect(term, "control|keyboard|mouse") &&
      stringr::str_detect(term, "bad|poor|issue|problem")) {
    return("control issues")
  }

  if (stringr::str_detect(term, "difficulty") &&
      stringr::str_detect(term, "spike|bad|poor|issue|problem|hard")) {
    return("difficulty issues")
  }

  if (stringr::str_detect(term, "combat") &&
      stringr::str_detect(term, "slow|bad|poor|issue|problem")) {
    return("combat pacing issues")
  }

  single_term_map <- c(
    crash = "crashes",
    bug = "bugs",
    refund = "refunds",
    stutter = "stuttering",
    lag = "lag",
    disconnect = "disconnects",
    broken = "broken experience",
    unplayable = "unplayable",
    error = "errors"
  )

  if (term %in% names(single_term_map)) {
    return(unname(single_term_map[[term]]))
  }

  NA_character_
}

is_quality_english_semantic_term <- function(term, phrase_stopwords, sentiment_scope = "negative") {
  if (is.na(term) || term == "") {
    return(FALSE)
  }

  tokens <- strsplit(term, " ", fixed = TRUE)[[1]]

  if (any(tokens %in% english_title_stopwords())) {
    return(FALSE)
  }

  if (!identical(sentiment_scope, "positive") && all(tokens %in% phrase_stopwords)) {
    return(FALSE)
  }

  if (identical(sentiment_scope, "positive")) {
    negative_terms <- c(
      "performance issues", "bad optimization", "frame drops", "low fps",
      "shader compilation", "server issues", "frequent crashes", "fatal errors",
      "loading issues", "poor balance", "control issues", "difficulty issues",
      "combat pacing issues", "crashes", "bugs", "refunds", "stuttering", "lag",
      "disconnects", "broken experience", "unplayable", "errors"
    )

    return(!term %in% negative_terms && stringr::str_detect(term, english_positive_signal_pattern()))
  }

  if (length(tokens) == 1) {
    return(term %in% c(
      "crashes", "bugs", "refunds", "stuttering", "lag", "disconnects",
      "broken experience", "unplayable", "errors"
    ))
  }

  stringr::str_detect(term, english_semantic_signal_pattern()) ||
    term %in% c(
      "performance issues", "bad optimization", "frame drops",
      "shader compilation", "server issues", "frequent crashes",
      "fatal errors", "loading issues", "poor balance", "control issues",
      "difficulty issues", "combat pacing issues"
    )
}

extract_english_review_semantic_candidates <- function(tokens, max_ngram = 4,
                                                       phrase_stopwords = english_stopwords(),
                                                       sentiment_scope = "negative") {
  tokens <- tokens[
    stringr::str_detect(tokens, "^[a-z]+$") &
      stringr::str_length(tokens) >= 3
  ]

  if (length(tokens) == 0) {
    return(character())
  }

  signal_pattern <- if (identical(sentiment_scope, "positive")) {
    english_positive_signal_pattern()
  } else {
    english_semantic_signal_pattern()
  }

  candidates <- make_english_semantic_windows(
    tokens,
    max_window = max_ngram,
    signal_pattern = signal_pattern
  )

  semantic_terms <- vapply(
    candidates,
    normalize_english_semantic_term,
    character(1),
    sentiment_scope = sentiment_scope
  )
  semantic_terms <- semantic_terms[
    vapply(
      semantic_terms,
      is_quality_english_semantic_term,
      logical(1),
      phrase_stopwords = phrase_stopwords,
      sentiment_scope = sentiment_scope
    )
  ]

  unique(semantic_terms)
}

make_english_semantic_windows <- function(tokens, max_window = 4,
                                          signal_pattern = english_semantic_signal_pattern()) {
  signal_index <- which(stringr::str_detect(tokens, signal_pattern))

  if (length(signal_index) == 0) {
    return(character())
  }

  candidates <- unlist(lapply(signal_index, function(index) {
    window_start <- max(1, index - 2)
    window_end <- min(length(tokens), index + 2)
    starts <- window_start:index

    unlist(lapply(starts, function(start) {
      ends <- start:min(window_end, start + max_window - 1)
      vapply(
        ends,
        function(end) paste(tokens[start:end], collapse = " "),
        character(1)
      )
    }), use.names = FALSE)
  }), use.names = FALSE)

  unique(candidates)
}

extract_english_semantic_keywords <- function(reviews, text_col = "review_clean", top_n = 20,
                                              sentiment_scope = c("negative", "all", "positive"),
                                              max_ngram = 4, min_count = 2,
                                              extra_stopwords = character()) {
  sentiment_scope <- match.arg(sentiment_scope)
  scoped_reviews <- filter_reviews_by_sentiment(reviews, sentiment_scope)
  stopwords <- english_stopwords(extra_stopwords)
  tokens_by_review <- tokenize_english_reviews(scoped_reviews[[text_col]])

  semantic_terms <- unlist(
    lapply(
      tokens_by_review,
      extract_english_review_semantic_candidates,
      max_ngram = max_ngram,
      phrase_stopwords = stopwords,
      sentiment_scope = sentiment_scope
    ),
    use.names = FALSE
  )

  tibble::tibble(keyword = semantic_terms) |>
    dplyr::filter(!is.na(keyword), keyword != "") |>
    dplyr::count(keyword, sort = TRUE) |>
    dplyr::filter(n >= min_count) |>
    dplyr::mutate(
      review_share = n / max(1, nrow(scoped_reviews)),
      scope = paste0(sentiment_scope, "_reviews")
    ) |>
    dplyr::slice_head(n = top_n)
}

chinese_base_stopwords <- function() {
  c(
    "游戏", "内容", "玩家", "东西", "感觉", "这个", "那个", "真的", "就是",
    "还是", "可以", "不是", "没有", "一个", "什么", "现在", "然后", "因为",
    "所以", "但是", "如果", "已经", "这么", "那么", "比较", "时候", "自己",
    "一下", "一些", "里面", "还有", "觉得", "来说", "进行", "目前", "完全",
    "直接", "怎么", "也是", "一样", "出来", "这种", "不会", "不能", "需要",
    "一直", "很多", "非常", "最后", "确实", "只能", "可能"
    , "rnm", "sb"
  )
}

chinese_domain_stopwords <- function() {
  c(
    "怪物", "猎人", "怪猎", "荒野", "卡普空", "卡普", "普空", "capcom",
    "武器", "世界", "剧情", "好玩", "monster", "hunter", "wilds", "world",
    "欧米", "欧米茄", "dlc", "任务", "画面", "地图"
  )
}

chinese_stopwords <- function(extra_base = character(), extra_domain = character()) {
  unique(c(
    chinese_base_stopwords(),
    chinese_domain_stopwords(),
    extra_base,
    extra_domain
  ))
}

chinese_segmentation_dictionary <- function() {
  unique(c(
    chinese_base_stopwords(),
    chinese_domain_stopwords(),
    "更新之后", "更新以后", "更新后", "更新完", "更新", "补丁", "版本",
    "优化很差", "优化太差", "优化不好", "优化很烂", "优化差", "优化烂",
    "优化问题", "优化", "性能", "性能问题",
    "帧数下降", "帧数低", "帧率低", "低帧率", "掉帧严重", "掉帧明显",
    "掉帧卡顿", "掉帧", "卡顿", "卡死", "帧数", "帧率", "fps",
    "闪退崩溃", "闪退", "崩溃", "crash", "黑屏", "报错",
    "联机掉线", "服务器问题", "联机", "掉线", "断线", "断开", "服务器", "联网",
    "键鼠适配差", "键鼠适配", "键鼠", "键盘", "鼠标", "适配差", "适配", "按键", "手柄",
    "退款", "退钱",
    "着色器编译", "着色器", "编译", "加载慢", "加载", "加载时间",
    "操作", "手感", "视角", "锁定", "镜头", "战斗节奏慢", "战斗", "战斗节奏", "节奏慢", "节奏",
    "怪物设计不错", "怪物设计差", "怪物设计", "设计不错", "设计差", "设计", "数值", "难度", "任务", "地图", "画面",
    "推荐", "值得推荐", "值得", "有趣", "画风", "美术", "音乐", "配乐", "音效",
    "剧情不错", "故事不错", "操作简单", "上手简单", "体验不错", "合作好玩",
    "适合朋友", "朋友", "联机好玩", "可爱", "轻松", "欢乐", "喜欢",
    "垃圾", "问题", "难受", "不错", "太差", "很差", "不好", "很烂",
    "差", "慢", "烂", "高", "低"
  ))
}

prepare_chinese_dictionary <- function() {
  dictionary <- chinese_segmentation_dictionary()
  dictionary_lengths <- nchar(dictionary, type = "chars")
  order_index <- order(dictionary_lengths, decreasing = TRUE)
  dictionary <- dictionary[order_index]
  dictionary_lengths <- dictionary_lengths[order_index]
  initials <- substr(dictionary, 1, 1)
  split_index <- split(seq_along(dictionary), initials)

  lapply(split_index, function(index) {
    list(
      terms = dictionary[index],
      lengths = dictionary_lengths[index]
    )
  })
}

chinese_meaningful_pattern <- function() {
  paste0(
    "(",
    paste(
      c(
        "优化", "掉帧", "帧数", "帧率", "卡顿", "闪退", "崩溃", "退款",
        "退钱", "掉线", "断线", "联机", "服务器", "键鼠", "键盘", "鼠标",
        "适配", "操作", "手感", "视角", "锁定", "任务", "更新", "bug",
        "问题", "差", "慢", "烂", "垃圾", "加载", "编译", "着色器", "设计",
        "战斗", "节奏", "不错", "难受", "卡死", "黑屏", "报错", "帧",
        "fps", "crash", "好玩", "推荐", "有趣", "画风", "美术", "音乐",
        "配乐", "音效", "体验", "朋友", "合作", "联机好玩", "可爱",
        "轻松", "欢乐", "喜欢", "值得", "上手", "简单"
      ),
      collapse = "|"
    ),
    ")"
  )
}

clean_chinese_text <- function(text) {
  text <- ifelse(is.na(text), "", text)
  text |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("https?://\\S+|www\\.\\S+", " ") |>
    stringr::str_replace_all("[^\\u4E00-\\u9FFFa-z0-9]+", " ") |>
    stringr::str_squish()
}

segment_chinese_reviews <- function(text) {
  cleaned <- clean_chinese_text(text)

  if (requireNamespace("jiebaR", quietly = TRUE)) {
    cutter <- jiebaR::worker()
    return(lapply(cleaned, function(x) jiebaR::segment(x, cutter)))
  }

  # Fallback to a domain dictionary longest-match segmenter when jiebaR is absent.
  dictionary_by_initial <- prepare_chinese_dictionary()
  lapply(cleaned, segment_chinese_fallback, dictionary_by_initial = dictionary_by_initial)
}

segment_chinese_fallback <- function(text, dictionary_by_initial) {
  units <- stringr::str_extract_all(text, "[\\u4E00-\\u9FFF]+|[a-z0-9]+")[[1]]

  unlist(lapply(units, function(unit) {
    if (stringr::str_detect(unit, "^[a-z0-9]+$")) {
      return(unit)
    }

    segment_chinese_han_sequence(unit, dictionary_by_initial)
  }), use.names = FALSE)
}

segment_chinese_han_sequence <- function(sequence, dictionary_by_initial) {
  sequence_length <- nchar(sequence, type = "chars")
  tokens <- character(sequence_length)
  token_count <- 0
  position <- 1

  while (position <= sequence_length) {
    matched <- ""
    matched_length <- 0
    current_char <- substr(sequence, position, position)
    candidates <- dictionary_by_initial[[current_char]]

    if (!is.null(candidates)) {
      terms <- candidates$terms
      term_lengths <- candidates$lengths

      for (term_index in seq_along(terms)) {
        term <- terms[[term_index]]
        term_length <- term_lengths[[term_index]]
        if (position + term_length - 1 > sequence_length) {
          next
        }

        candidate <- substr(sequence, position, position + term_length - 1)
        if (identical(candidate, term)) {
          matched <- term
          matched_length <- term_length
          break
        }
      }
    }

    if (matched != "") {
      token_count <- token_count + 1
      tokens[[token_count]] <- matched
      position <- position + matched_length
    } else {
      token_count <- token_count + 1
      tokens[[token_count]] <- substr(sequence, position, position)
      position <- position + 1
    }
  }

  tokens[seq_len(token_count)]
}

normalize_chinese_tokens <- function(tokens) {
  tokens <- stringr::str_to_lower(stringr::str_squish(tokens))
  tokens[tokens %in% c("卡", "卡顿", "掉帧", "帧数低", "帧率低", "fps低", "低帧率")] <- "掉帧卡顿"
  tokens[tokens %in% c("闪退", "崩溃", "crash", "crashes", "crashed")] <- "闪退崩溃"
  tokens[tokens %in% c("掉线", "断线", "断开")] <- "联机掉线"
  tokens[tokens %in% c("退款", "退钱")] <- "退款"
  tokens[tokens %in% c("键盘", "鼠标", "键鼠")] <- "键鼠"
  tokens[tokens %in% c("帧数", "帧率", "fps")] <- "帧率"
  tokens[tokens %in% c("shader", "shaders", "着色器")] <- "着色器"
  tokens[tokens %in% c("优化差", "优化烂", "优化很差", "优化不好", "优化问题")] <- "优化问题"
  tokens
}

is_valid_chinese_phrase_token <- function(token) {
  !is.na(token) &&
    token != "" &&
    !stringr::str_detect(token, "^([\\u4E00-\\u9FFF])\\1+$") &&
    stringr::str_detect(token, "[\\u4E00-\\u9FFFa-z0-9]") &&
    (stringr::str_length(token) >= 2 || token %in% c("差", "慢", "烂", "高", "低", "卡"))
}

chinese_semantic_quality_pattern <- function() {
  paste0(
    "(",
    paste(
      c(
        "优化很差", "优化问题", "帧数下降", "掉帧卡顿", "更新之后卡顿",
        "联机掉线", "键鼠适配差", "怪物设计不错", "怪物设计差",
        "战斗节奏慢", "闪退崩溃", "退款", "服务器问题", "加载慢",
        "性能问题", "操作.*(差|难受|问题)", "手感.*(差|问题)",
        "视角.*(差|问题)", "设计.*(不错|差|问题)", "bug",
        "值得推荐", "体验不错", "剧情不错", "故事不错", "操作简单",
        "上手简单", "合作好玩", "适合朋友", "联机好玩", "音乐不错",
        "配乐不错", "画风不错", "美术风格不错", "角色可爱", "轻松欢乐",
        "战斗体验不错", "玩法有趣"
      ),
      collapse = "|"
    ),
    ")"
  )
}

chinese_negative_semantic_terms <- function() {
  c(
    "更新后卡顿", "更新之后卡顿", "优化很差", "掉帧", "掉帧卡顿", "卡顿",
    "联机掉线", "服务器问题", "键位问题", "战斗节奏慢", "怪物设计差",
    "闪退崩溃", "退款", "加载慢", "性能问题", "bug"
  )
}

extract_chinese_positive_regex_semantic_candidates <- function(text) {
  text <- clean_chinese_text(text)
  candidates <- character()

  if (stringr::str_detect(text, "推荐|值得.{0,3}(买|入|玩)|值得推荐")) {
    candidates <- c(candidates, "值得推荐")
  }

  if (stringr::str_detect(text, "(合作|联机|多人|朋友).{0,8}(好玩|有趣|欢乐|快乐|推荐)")) {
    candidates <- c(candidates, "合作好玩", "适合朋友")
  }

  if (stringr::str_detect(text, "(剧情|故事).{0,6}(不错|好|优秀|喜欢|有趣)")) {
    candidates <- c(candidates, "剧情不错")
  }

  if (stringr::str_detect(text, "(音乐|配乐|音效).{0,6}(不错|好|棒|优秀|喜欢)")) {
    candidates <- c(candidates, "音乐不错")
  }

  if (stringr::str_detect(text, "(画风|美术|画面).{0,6}(不错|好|漂亮|精美|可爱|喜欢)")) {
    candidates <- c(candidates, "美术风格不错")
  }

  if (stringr::str_detect(text, "(操作|上手).{0,6}(简单|顺手|舒服|流畅|容易)")) {
    candidates <- c(candidates, "操作简单")
  }

  if (stringr::str_detect(text, "(体验|游玩).{0,6}(不错|很好|舒服|优秀|满意)")) {
    candidates <- c(candidates, "体验不错")
  }

  if (stringr::str_detect(text, "(战斗|玩法|系统).{0,8}(有趣|不错|好玩|喜欢)")) {
    candidates <- c(candidates, "玩法有趣")
  }

  if (stringr::str_detect(text, "可爱|萌")) {
    candidates <- c(candidates, "角色可爱")
  }

  if (stringr::str_detect(text, "轻松|欢乐|快乐")) {
    candidates <- c(candidates, "轻松欢乐")
  }

  if (stringr::str_detect(text, "好玩|有趣|喜欢")) {
    candidates <- c(candidates, "体验不错")
  }

  unique(candidates)
}

extract_chinese_regex_semantic_candidates <- function(text, sentiment_scope = "negative") {
  text <- clean_chinese_text(text)
  candidates <- character()

  if (identical(sentiment_scope, "positive")) {
    return(extract_chinese_positive_regex_semantic_candidates(text))
  }

  if (stringr::str_detect(text, "更新.{0,6}(之后|以后|后|完).{0,8}(卡顿|掉帧|帧数低|帧率低|fps低)")) {
    candidates <- c(candidates, "更新后卡顿")
  }

  if (stringr::str_detect(text, "优化.{0,5}(很差|太差|不好|很烂|差|烂|垃圾|问题)")) {
    candidates <- c(candidates, "优化很差")
  }

  if (stringr::str_detect(text, "掉帧|低帧率") ||
      stringr::str_detect(text, "(帧数|帧率|fps).{0,5}(下降|低|不稳)")) {
    candidates <- c(candidates, "掉帧")
  }

  if (stringr::str_detect(text, "卡顿|很卡|非常卡|卡界面|卡死")) {
    candidates <- c(candidates, "卡顿")
  }

  if (stringr::str_detect(text, "(联机|服务器|联网).{0,6}(掉线|断线|断开)")) {
    candidates <- c(candidates, "联机掉线")
  }

  if (stringr::str_detect(text, "(服务器).{0,6}(问题|错误|崩|炸|不稳)")) {
    candidates <- c(candidates, "服务器问题")
  }

  if (stringr::str_detect(text, "(键位|按键|键盘|鼠标|键鼠).{0,8}(修改|自定义|适配|操作|冲突).{0,8}(差|不好|难受|问题|不能|不让)") ||
      stringr::str_detect(text, "键鼠适配差")) {
    candidates <- c(candidates, "键位问题")
  }

  if (stringr::str_detect(text, "战斗.{0,4}节奏.{0,4}(慢|拖)")) {
    candidates <- c(candidates, "战斗节奏慢")
  }

  if (stringr::str_detect(text, "怪物.{0,4}设计.{0,4}(不错|好)")) {
    candidates <- c(candidates, "怪物设计不错")
  }

  if (stringr::str_detect(text, "怪物.{0,4}设计.{0,4}(差|问题|烂)")) {
    candidates <- c(candidates, "怪物设计差")
  }

  if (stringr::str_detect(text, "闪退|崩溃|crash|报错")) {
    candidates <- c(candidates, "闪退崩溃")
  }

  if (stringr::str_detect(text, "退款|退钱")) {
    candidates <- c(candidates, "退款")
  }

  if (stringr::str_detect(text, "加载.{0,5}(慢|久|问题|时间长)")) {
    candidates <- c(candidates, "加载慢")
  }

  if (stringr::str_detect(text, "bug")) {
    candidates <- c(candidates, "bug")
  }

  unique(candidates)
}

normalize_chinese_semantic_term <- function(term, sentiment_scope = "negative") {
  term <- stringr::str_to_lower(stringr::str_squish(term))

  if (term == "") {
    return(NA_character_)
  }

  if (identical(sentiment_scope, "positive")) {
    if (stringr::str_detect(term, "推荐|值得")) {
      return("值得推荐")
    }

    if (stringr::str_detect(term, "朋友|合作|联机|多人") &&
        stringr::str_detect(term, "好玩|有趣|欢乐|快乐|推荐")) {
      return("合作好玩")
    }

    if (stringr::str_detect(term, "剧情|故事") &&
        stringr::str_detect(term, "不错|好|优秀|喜欢|有趣")) {
      return("剧情不错")
    }

    if (stringr::str_detect(term, "音乐|配乐|音效") &&
        stringr::str_detect(term, "不错|好|棒|优秀|喜欢")) {
      return("音乐不错")
    }

    if (stringr::str_detect(term, "画风|美术|画面") &&
        stringr::str_detect(term, "不错|好|漂亮|精美|可爱|喜欢")) {
      return("美术风格不错")
    }

    if (stringr::str_detect(term, "操作|上手") &&
        stringr::str_detect(term, "简单|顺手|舒服|流畅|容易")) {
      return("操作简单")
    }

    if (stringr::str_detect(term, "体验|游玩") &&
        stringr::str_detect(term, "不错|很好|舒服|优秀|满意|好玩|有趣")) {
      return("体验不错")
    }

    if (stringr::str_detect(term, "战斗|玩法|系统") &&
        stringr::str_detect(term, "有趣|不错|好玩|喜欢")) {
      return("玩法有趣")
    }

    if (stringr::str_detect(term, "可爱|萌")) {
      return("角色可爱")
    }

    if (stringr::str_detect(term, "轻松|欢乐|快乐")) {
      return("轻松欢乐")
    }

    if (term %in% c(
      "值得推荐", "体验不错", "剧情不错", "故事不错", "操作简单",
      "上手简单", "合作好玩", "适合朋友", "联机好玩", "音乐不错",
      "配乐不错", "画风不错", "美术风格不错", "角色可爱",
      "轻松欢乐", "战斗体验不错", "玩法有趣"
    )) {
      return(term)
    }

    return(NA_character_)
  }

  if (stringr::str_detect(term, "更新|补丁|版本") &&
      stringr::str_detect(term, "卡顿|掉帧|帧数低|帧率低|fps低")) {
    return("更新后卡顿")
  }

  if (stringr::str_detect(term, "优化") &&
      stringr::str_detect(term, "很差|太差|不好|很烂|差|烂|垃圾|问题|卡顿")) {
    return("优化很差")
  }

  if (stringr::str_detect(term, "掉帧|低帧率") ||
      (stringr::str_detect(term, "帧数|帧率|fps") &&
       stringr::str_detect(term, "下降|低|不稳"))) {
    return("掉帧")
  }

  if (stringr::str_detect(term, "卡顿|很卡|非常卡|卡界面|卡死")) {
    return("卡顿")
  }

  if (stringr::str_detect(term, "联机|服务器|联网") &&
      stringr::str_detect(term, "掉线|断线|断开")) {
    return("联机掉线")
  }

  if (stringr::str_detect(term, "服务器") &&
      stringr::str_detect(term, "问题|错误|不稳|崩|炸")) {
    return("服务器问题")
  }

  if (stringr::str_detect(term, "键位|按键|键鼠|键盘|鼠标") &&
      stringr::str_detect(term, "修改|自定义|适配|操作|冲突|差|不好|难受|问题|不能|不让")) {
    return("键位问题")
  }

  if (stringr::str_detect(term, "战斗") &&
      stringr::str_detect(term, "节奏|慢|拖")) {
    return("战斗节奏慢")
  }

  if (stringr::str_detect(term, "怪物|设计") &&
      stringr::str_detect(term, "不错|好")) {
    return("怪物设计不错")
  }

  if (stringr::str_detect(term, "怪物|设计") &&
      stringr::str_detect(term, "差|问题|烂")) {
    return("怪物设计差")
  }

  if (stringr::str_detect(term, "闪退|崩溃|crash|报错")) {
    return("闪退崩溃")
  }

  if (stringr::str_detect(term, "退款|退钱")) {
    return("退款")
  }

  if (stringr::str_detect(term, "加载") &&
      stringr::str_detect(term, "慢|久|问题|时间")) {
    return("加载慢")
  }

  if (stringr::str_detect(term, "性能") &&
      stringr::str_detect(term, "问题|差|低|不好")) {
    return("性能问题")
  }

  if (term %in% c("bug", "bugs")) {
    return("bug")
  }

  if (term %in% c("掉帧卡顿", "掉帧", "卡顿", "优化很差", "闪退崩溃", "联机掉线", "退款", "键位问题")) {
    return(term)
  }

  NA_character_
}

is_quality_chinese_semantic_term <- function(term, sentiment_scope = "negative") {
  if (is.na(term) || term == "") {
    return(FALSE)
  }

  if (term %in% chinese_stopwords()) {
    return(FALSE)
  }

  if (stringr::str_detect(term, "卡普|普空|capcom|荒野|怪猎|猎人") &&
      !stringr::str_detect(term, "怪物设计")) {
    return(FALSE)
  }

  if (stringr::str_detect(term, "^([\\u4E00-\\u9FFF])\\1+$")) {
    return(FALSE)
  }

  if (identical(sentiment_scope, "positive") &&
      term %in% chinese_negative_semantic_terms()) {
    return(FALSE)
  }

  stringr::str_detect(term, chinese_semantic_quality_pattern())
}

extract_chinese_review_semantic_candidates <- function(text, tokens, max_ngram = 5,
                                                       sentiment_scope = "negative") {
  tokens <- normalize_chinese_tokens(tokens)
  tokens <- tokens[vapply(tokens, is_valid_chinese_phrase_token, logical(1))]

  ngram_candidates <- make_chinese_semantic_windows(tokens, max_window = max_ngram)

  semantic_terms <- c(
    extract_chinese_regex_semantic_candidates(text, sentiment_scope = sentiment_scope),
    vapply(
      ngram_candidates,
      normalize_chinese_semantic_term,
      character(1),
      sentiment_scope = sentiment_scope
    )
  )

  semantic_terms <- semantic_terms[
    vapply(
      semantic_terms,
      is_quality_chinese_semantic_term,
      logical(1),
      sentiment_scope = sentiment_scope
    )
  ]

  unique(semantic_terms)
}

make_chinese_semantic_windows <- function(tokens, max_window = 5) {
  if (length(tokens) == 0) {
    return(character())
  }

  signal_index <- which(stringr::str_detect(tokens, chinese_meaningful_pattern()))

  if (length(signal_index) == 0) {
    return(character())
  }

  candidates <- unlist(lapply(signal_index, function(index) {
    window_start <- max(1, index - 2)
    window_end <- min(length(tokens), index + 2)
    starts <- window_start:index

    unlist(lapply(starts, function(start) {
      ends <- start:min(window_end, start + max_window - 1)
      vapply(
        ends,
        function(end) paste0(tokens[start:end], collapse = ""),
        character(1)
      )
    }), use.names = FALSE)
  }), use.names = FALSE)

  unique(candidates)
}

extract_chinese_semantic_keywords <- function(reviews, text_col = "review_clean", top_n = 20,
                                              sentiment_scope = c("negative", "all", "positive"),
                                              max_ngram = 5, min_count = 2) {
  sentiment_scope <- match.arg(sentiment_scope)
  scoped_reviews <- filter_reviews_by_sentiment(reviews, sentiment_scope)
  text <- scoped_reviews[[text_col]]
  tokens_by_review <- segment_chinese_reviews(text)

  semantic_terms <- unlist(
    Map(
      extract_chinese_review_semantic_candidates,
      text = text,
      tokens = tokens_by_review,
      MoreArgs = list(max_ngram = max_ngram, sentiment_scope = sentiment_scope)
    ),
    use.names = FALSE
  )

  tibble::tibble(keyword = semantic_terms) |>
    dplyr::filter(!is.na(keyword), keyword != "") |>
    dplyr::count(keyword, sort = TRUE) |>
    dplyr::filter(n >= min_count) |>
    dplyr::mutate(
      review_share = n / max(1, nrow(scoped_reviews)),
      scope = paste0(sentiment_scope, "_reviews")
    ) |>
    dplyr::slice_head(n = top_n)
}

plot_keyword_bars <- function(data, term_col, count_col = "n", title,
                              fill = "#2C7FB8", base_family = "", top_n = 20) {
  plot_data <- data |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(.plot_term = stats::reorder(.data[[term_col]], .data[[count_col]]))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .plot_term, y = .data[[count_col]])) +
    ggplot2::geom_col(fill = fill, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = title,
      x = stringr::str_to_title(stringr::str_replace_all(term_col, "_", " ")),
      y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = 12, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#111927", color = NA),
      panel.background = ggplot2::element_rect(fill = "#111927", color = NA),
      plot.title = ggplot2::element_text(color = "#E8EEF8", face = "bold"),
      axis.title = ggplot2::element_text(color = "#A4B1CD"),
      axis.text = ggplot2::element_text(color = "#D8E2F2"),
      panel.grid.major.x = ggplot2::element_line(color = "rgba(164,177,205,0.16)"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

estimate_text_box <- function(term, size) {
  text_width <- stringi::stri_width(term)
  list(
    width = max(0.10, text_width * size * 0.010),
    height = max(0.08, size * 0.036)
  )
}

boxes_overlap <- function(candidate, placed_boxes) {
  if (length(placed_boxes) == 0) {
    return(FALSE)
  }

  any(vapply(placed_boxes, function(box) {
    candidate$xmin < box$xmax &&
      candidate$xmax > box$xmin &&
      candidate$ymin < box$ymax &&
      candidate$ymax > box$ymin
  }, logical(1)))
}

place_wordcloud_terms <- function(terms, counts, sizes, seed = 123) {
  set.seed(seed)

  term_count <- length(terms)
  x_limit <- 1.15
  y_limit <- 0.85
  placed_boxes <- list()

  layout <- tibble::tibble(
    term = terms,
    n = counts,
    size = sizes,
    x = rep(NA_real_, term_count),
    y = rep(NA_real_, term_count)
  )

  for (i in seq_len(term_count)) {
    box_size <- estimate_text_box(terms[[i]], sizes[[i]])
    angle_offset <- stats::runif(1, 0, 2 * pi)
    theta <- seq(0, 22 * pi, length.out = 1500) + angle_offset
    radius <- seq(0, 1, length.out = 1500)
    candidate_x <- radius * cos(theta) * x_limit
    candidate_y <- radius * sin(theta) * y_limit
    placed <- FALSE

    for (j in seq_along(candidate_x)) {
      candidate <- list(
        xmin = candidate_x[[j]] - box_size$width / 2,
        xmax = candidate_x[[j]] + box_size$width / 2,
        ymin = candidate_y[[j]] - box_size$height / 2,
        ymax = candidate_y[[j]] + box_size$height / 2
      )

      inside_bounds <- candidate$xmin >= -x_limit &&
        candidate$xmax <= x_limit &&
        candidate$ymin >= -y_limit &&
        candidate$ymax <= y_limit

      if (inside_bounds && !boxes_overlap(candidate, placed_boxes)) {
        layout$x[[i]] <- candidate_x[[j]]
        layout$y[[i]] <- candidate_y[[j]]
        placed_boxes[[length(placed_boxes) + 1]] <- candidate
        placed <- TRUE
        break
      }
    }

    if (!placed) {
      layout$x[[i]] <- NA_real_
      layout$y[[i]] <- NA_real_
    }
  }

  layout |>
    dplyr::filter(!is.na(x), !is.na(y))
}

plot_keyword_cloud <- function(data, term_col = "term", count_col = "n", title,
                               max_terms = 45, palette = c("#2C7FB8", "#31A354", "#756BB1", "#D95F0E"),
                               base_family = "", seed = 123) {
  plot_data <- data |>
    dplyr::filter(!is.na(.data[[term_col]]), .data[[term_col]] != "") |>
    dplyr::arrange(dplyr::desc(.data[[count_col]])) |>
    dplyr::slice_head(n = max_terms) |>
    dplyr::mutate(
      .term = .data[[term_col]],
      .count = .data[[count_col]],
      .size = scales::rescale(log1p(.count), to = c(3.5, 10.5)),
      .color = palette[((dplyr::row_number() - 1) %% length(palette)) + 1],
      .family = ifelse(base_family == "", "sans", base_family)
    )

  if (requireNamespace("ggwordcloud", quietly = TRUE)) {
    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(label = .term, size = .count, color = .color, family = .family)
      ) +
        ggwordcloud::geom_text_wordcloud_area(
          fontface = "bold",
          seed = seed,
          eccentricity = 0.7,
          grid_size = 4,
          max_grid_size = 256,
          rm_outside = TRUE,
          use_richtext = FALSE
        ) +
        ggplot2::scale_size_area(max_size = 24) +
        ggplot2::scale_color_identity() +
        ggplot2::labs(title = title) +
        ggplot2::theme_void(base_family = base_family) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            color = "#E8EEF8",
            face = "bold",
            size = 18,
            hjust = 0.5,
            margin = ggplot2::margin(b = 12)
          ),
          plot.margin = ggplot2::margin(16, 16, 16, 16)
        )
    )
  }

  cloud_layout <- place_wordcloud_terms(
    terms = plot_data$.term,
    counts = plot_data$.count,
    sizes = plot_data$.size,
    seed = seed
  ) |>
    dplyr::mutate(.color = plot_data$.color)

  ggplot2::ggplot(cloud_layout, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(
      ggplot2::aes(label = term, size = size, color = .color),
      family = base_family,
      fontface = "bold"
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_equal(xlim = c(-1.25, 1.25), ylim = c(-0.95, 0.95), clip = "off") +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = "#E8EEF8",
        face = "bold",
        size = 18,
        hjust = 0.5,
        margin = ggplot2::margin(b = 12)
      ),
      plot.margin = ggplot2::margin(16, 16, 16, 16)
    )
}

plot_keyword_wordcloud <- function(data, term_col = "term", count_col = "n", title,
                                   max_terms = 45,
                                   palette = c("#2C7FB8", "#31A354", "#756BB1", "#D95F0E"),
                                   base_family = "", seed = 123) {
  plot_keyword_cloud(
    data = data,
    term_col = term_col,
    count_col = count_col,
    title = title,
    max_terms = max_terms,
    palette = palette,
    base_family = base_family,
    seed = seed
  )
}
