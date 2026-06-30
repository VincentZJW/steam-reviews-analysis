# Steam Reviews Analysis

This R project analyzes recent Steam reviews with a focus on review trends,
negative-review signals, and semantic player feedback topics.

## Project Structure

```text
steam-reviews-analysis/
├─ app.R          # Interactive Shiny dashboard
├─ data/
│  ├─ raw/        # Raw Steam review exports
│  ├─ clean/      # Cleaned review data and summary tables
│  └─ cache/      # Local dashboard cache snapshots
├─ R/             # Reusable R functions
├─ scripts/       # Ordered analysis scripts
├─ outputs/
│  ├─ figures/    # Generated charts
│  └─ tables/     # Generated CSV outputs
├─ reports/       # Report drafts or Quarto files
├─ README.md
└─ steam-reviews-analysis.Rproj
```

## Main Workflow

Run the scripts from the project root:

```bash
Rscript scripts/01_fetch_recent_reviews.R
Rscript scripts/02_clean_reviews.R
Rscript scripts/03_summarize_reviews.R
Rscript scripts/04_detect_alerts.R
Rscript scripts/05_visualize_trends.R
Rscript scripts/06_keyword_analysis.R
```

## Interactive Dashboard

Launch the local Shiny dashboard from the project root:

```bash
Rscript -e 'shiny::runApp(".", port = 3857, launch.browser = TRUE)'
```

The dashboard supports Steam game search, appid selection, cached live review
fetching, KPI cards, review detail tables, daily/weekly/monthly trends,
semantic keywords, word clouds, and negative-rate alert monitoring.

Core dashboard packages:

```r
install.packages(c(
  "shiny", "bslib", "plotly", "DT", "ggplot2", "dplyr", "readr",
  "stringr", "lubridate", "scales", "tidyr", "slider", "httr2",
  "jsonlite", "tibble", "htmltools", "ggwordcloud", "stringi"
))
```

## Current Outputs

Trend figures are saved to `outputs/figures/`, including daily review volume,
negative review rate, 7-day rolling negative rate, sentiment share, and language
distribution.

Semantic keyword outputs focus on negative reviews and are saved as:

- `outputs/tables/english_semantic_keywords.csv`
- `outputs/tables/chinese_semantic_keywords.csv`
- `outputs/figures/top_english_semantic_keywords.png`
- `outputs/figures/top_chinese_semantic_keywords.png`
- `outputs/figures/english_semantic_keywords_wordcloud.png`
- `outputs/figures/chinese_semantic_keywords_wordcloud.png`

The keyword analysis intentionally filters game-title fragments, brand words,
generic gameplay terms, and low-information words so the final results better
describe player complaints and experience themes.
