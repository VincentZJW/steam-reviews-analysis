# Steam Reviews Analysis TODO

本清单基于当前项目审计结果整理。执行原则：每次只完成一个任务，验证通过后再进入下一个任务；除非任务明确要求，不同时修改多个无关文件。

优先级说明：

- P0：阻塞项目可复现运行或会造成数据错误，优先处理。
- P1：完善核心分析流程和项目交付物。
- P2：增强展示效果、简历表达和长期维护性。

## 阶段 1：整理项目结构

### T01 添加 `.DS_Store` 到忽略规则

- 优先级：P0
- 依赖：无
- 任务目标：避免 macOS 系统文件继续进入版本控制。
- 需要修改的文件：`.gitignore`
- 预期输出：`.gitignore` 中包含 `.DS_Store` 规则。
- 如何验证是否完成：运行 `git status --short`，新增或修改的 `.DS_Store` 不再显示为未跟踪文件。
- 是否可能影响现有代码：否，只影响版本控制行为。

### T02 清理已被 Git 跟踪的 `.DS_Store`

- 优先级：P0
- 依赖：T01
- 任务目标：从 Git 跟踪列表中移除 `.DS_Store`，保持仓库干净。
- 需要修改的文件：`.DS_Store`、`R/.DS_Store`、`data/.DS_Store`、`data/raw/.DS_Store`
- 预期输出：系统文件不再出现在 `git ls-files` 结果中。
- 如何验证是否完成：运行 `git ls-files .DS_Store R/.DS_Store data/.DS_Store data/raw/.DS_Store`，没有输出。
- 是否可能影响现有代码：否，但会改变 Git 跟踪状态。

### T03 新建标准输出目录

- 优先级：P0
- 依赖：无
- 任务目标：为后续表格、图表和报告建立稳定输出位置。
- 需要修改的文件：`outputs/figures/.gitkeep`、`outputs/tables/.gitkeep`、`reports/.gitkeep`
- 预期输出：项目中存在 `outputs/figures/`、`outputs/tables/`、`reports/`。
- 如何验证是否完成：运行 `test -d outputs/figures && test -d outputs/tables && test -d reports`。
- 是否可能影响现有代码：否，新增目录不改变当前脚本行为。

### T04 确定 processed 数据目录命名

- 优先级：P0
- 依赖：T03
- 任务目标：将当前 `data/clean/` 的语义统一为更标准的 `data/processed/`。
- 需要修改的文件：`data/processed/.gitkeep`
- 预期输出：项目中存在 `data/processed/`，作为后续清洗和指标输出目录。
- 如何验证是否完成：运行 `test -d data/processed`。
- 是否可能影响现有代码：否，本任务只新增目录；后续脚本迁移时才会影响路径。

### T05 规划旧 `data/clean/` 到 `data/processed/` 的迁移

- 优先级：P1
- 依赖：T04、阶段 3 和阶段 4 对脚本路径更新完成后
- 任务目标：在脚本路径全部切换后，移除或停止使用旧的 `data/clean/`。
- 需要修改的文件：`data/clean/` 下的旧输出文件
- 预期输出：清洗数据和指标数据统一写入 `data/processed/`。
- 如何验证是否完成：从 raw 数据重跑流程后，`data/processed/` 包含所有必要输出，脚本不再引用 `data/clean/`。
- 是否可能影响现有代码：是，路径迁移会影响所有读取旧目录的脚本。

## 阶段 2：规范 R 包依赖和运行环境

### T06 建立依赖说明

- 优先级：P0
- 依赖：无
- 任务目标：明确项目运行需要的 R 包，避免换环境后缺包。
- 需要修改的文件：`README.md`
- 预期输出：README 中包含依赖包列表和安装命令。
- 如何验证是否完成：在 README 中能找到 `httr2`、`jsonlite`、`tibble`、`dplyr`、`lubridate`、`stringr`、`readr`、`ggplot2`、`plotly`、`scales` 等包名。
- 是否可能影响现有代码：否，只修改文档。

### T07 增加脚本运行路径说明

- 优先级：P0
- 依赖：T06
- 任务目标：说明所有脚本应从项目根目录运行，避免相对路径报错。
- 需要修改的文件：`README.md`
- 预期输出：README 中包含 `Rscript scripts/02_clean_reviews.R` 这类从根目录执行的示例。
- 如何验证是否完成：按 README 命令从项目根目录运行，路径解析正常。
- 是否可能影响现有代码：否，只修改文档。

### T08 选择是否引入 `renv`

- 优先级：P1
- 依赖：T06
- 任务目标：决定是否使用 `renv` 锁定包版本，提升长期可复现性。
- 需要修改的文件：`renv.lock`、`.Rprofile`、`README.md`
- 预期输出：若启用 `renv`，仓库包含 `renv.lock`；若不启用，README 明确说明使用普通包安装方式。
- 如何验证是否完成：新环境中可按 README 恢复依赖并运行脚本。
- 是否可能影响现有代码：可能，会改变依赖恢复方式，但不应改变分析结果。

## 阶段 3：数据导入与清洗

### T09 修复 ID 精度损失问题

- 优先级：P0
- 依赖：阶段 2 基础说明完成
- 任务目标：读取 `recommendationid` 和 `author.steamid` 时保持字符类型，避免 17 位 Steam ID 被 double 四舍五入。
- 需要修改的文件：`scripts/02_clean_reviews.R`
- 预期输出：清洗后的 `author.steamid` 与 raw CSV 中原始字符完全一致。
- 如何验证是否完成：抽样对比 raw 与 processed 中相同评论的 `author.steamid`，数值字符串不发生变化。
- 是否可能影响现有代码：是，会改变已生成 clean 数据中的 Steam ID 字符值，但这是必要的数据修复。

### T10 强化 `clean_reviews()` 输入字段检查

- 优先级：P0
- 依赖：T09
- 任务目标：在清洗函数开始处检查必要字段是否存在，缺字段时给出清晰错误。
- 需要修改的文件：`R/clean_reviews.R`
- 预期输出：缺少 `review`、`voted_up`、`created_at` 等字段时，函数主动报出可读错误。
- 如何验证是否完成：用缺少字段的小样例调用 `clean_reviews()`，能得到明确错误信息。
- 是否可能影响现有代码：可能，原本静默失败或产生底层错误的情况会变成主动报错。

### T11 将清洗输出目录切换到 `data/processed/`

- 优先级：P0
- 依赖：T04、T09、T10
- 任务目标：把清洗后的 CSV 写入标准 processed 目录。
- 需要修改的文件：`scripts/02_clean_reviews.R`
- 预期输出：生成 `data/processed/reviews_chn_clean.csv` 和 `data/processed/reviews_eng_clean.csv`。
- 如何验证是否完成：运行 `Rscript scripts/02_clean_reviews.R` 后，检查两个 processed 文件存在且行数合理。
- 是否可能影响现有代码：是，后续脚本必须同步读取 `data/processed/`。

### T12 保留或补充 `app_id` 字段

- 优先级：P1
- 依赖：T11
- 任务目标：让中英文数据都明确记录所属 Steam app id，便于扩展到多个游戏。
- 需要修改的文件：`scripts/01_fetch_recent_reviews.R`、`scripts/02_clean_reviews.R` 或 `R/clean_reviews.R`
- 预期输出：processed 数据中包含 `app_id` 字段。
- 如何验证是否完成：读取 processed CSV，检查 `app_id` 存在且值为当前游戏 appid。
- 是否可能影响现有代码：是，字段结构会增加一列。

### T13 评估是否重新抓取游戏时长字段

- 优先级：P1
- 依赖：T12
- 任务目标：确认 Steam API 返回中是否包含 `author.playtime_forever`、`author.playtime_at_review` 等字段，并决定是否重新抓取 raw 数据。
- 需要修改的文件：`scripts/01_fetch_recent_reviews.R`
- 预期输出：raw 数据中保留可用于时长分析的字段，或 README 中说明当前数据不包含时长字段。
- 如何验证是否完成：检查 raw CSV 列名是否包含游戏时长字段。
- 是否可能影响现有代码：是，如果重新抓取 raw 数据，后续清洗输出会变化。

### T14 合并中英文清洗数据

- 优先级：P1
- 依赖：T11、T12
- 任务目标：生成一份统一分析用数据，支持语言对比和总体分析。
- 需要修改的文件：`scripts/02_clean_reviews.R`
- 预期输出：`data/processed/reviews_all_clean.csv`
- 如何验证是否完成：合并数据行数等于中文和英文 clean 数据行数之和。
- 是否可能影响现有代码：可能，后续指标脚本应优先读取合并数据。

## 阶段 4：指标构建

### T15 将汇总脚本改名为指标构建脚本

- 优先级：P0
- 依赖：T11
- 任务目标：把 `03_summarize_reviews.R` 的职责明确为构建分析指标。
- 需要修改的文件：`scripts/03_summarize_reviews.R`、`scripts/03_build_metrics.R`
- 预期输出：使用 `scripts/03_build_metrics.R` 作为第三步脚本。
- 如何验证是否完成：运行 `Rscript scripts/03_build_metrics.R` 能正常生成指标表。
- 是否可能影响现有代码：是，脚本文件名变化需要同步 README。

### T16 恢复每日汇总表写出

- 优先级：P0
- 依赖：T15
- 任务目标：修复当前流程中 `04_detect_alerts.R` 依赖日汇总但第三步不生成日汇总的问题。
- 需要修改的文件：`scripts/03_build_metrics.R`
- 预期输出：`data/processed/reviews_chn_daily_summary.csv`
- 如何验证是否完成：从 raw 开始运行 `02 -> 03 -> 04`，不再因为缺少日汇总而失败。
- 是否可能影响现有代码：是，会改变第三步脚本输出内容。

### T17 生成英文每日汇总表

- 优先级：P1
- 依赖：T16
- 任务目标：让英文数据也进入核心指标体系。
- 需要修改的文件：`scripts/03_build_metrics.R`
- 预期输出：`data/processed/reviews_eng_daily_summary.csv`
- 如何验证是否完成：英文每日汇总日期范围与英文 clean 数据日期范围一致。
- 是否可能影响现有代码：否，新增输出为主。

### T18 生成合并语言维度每日汇总表

- 优先级：P1
- 依赖：T14、T17
- 任务目标：支持按语言比较评论量和差评率。
- 需要修改的文件：`R/summarize_reviews.R` 或 `R/functions.R`、`scripts/03_build_metrics.R`
- 预期输出：`data/processed/reviews_daily_by_language.csv`
- 如何验证是否完成：表中包含 `review_date`、`language`、`total_reviews`、`negative_rate`。
- 是否可能影响现有代码：可能，会新增或调整汇总函数。

### T19 构建 7 日滚动差评率

- 优先级：P1
- 依赖：T16
- 任务目标：平滑每日差评率，减少低评论量日期的噪声。
- 需要修改的文件：`R/summarize_reviews.R` 或 `R/functions.R`、`scripts/03_build_metrics.R`
- 预期输出：每日汇总表包含 `negative_rate_7d`。
- 如何验证是否完成：每日汇总中第 7 天之后滚动字段不全为空，数值范围在 0 到 1 之间。
- 是否可能影响现有代码：是，指标表字段会增加。

### T20 构建语言分布表

- 优先级：P1
- 依赖：T14
- 任务目标：统计不同语言评论数量和占比。
- 需要修改的文件：`scripts/03_build_metrics.R`
- 预期输出：`outputs/tables/language_distribution.csv`
- 如何验证是否完成：表中包含 `language`、`reviews`、`share`，占比合计约等于 1。
- 是否可能影响现有代码：否，新增输出为主。

### T21 构建关键词频次表

- 优先级：P1
- 依赖：T14
- 任务目标：从评论文本中提取高频词，展示文本分析能力。
- 需要修改的文件：`R/functions.R` 或新增文本处理函数文件、`scripts/03_build_metrics.R`
- 预期输出：`outputs/tables/keyword_frequency.csv`
- 如何验证是否完成：表中包含 `keyword`、`n`、`language`，且去除了明显停用词。
- 是否可能影响现有代码：可能，可能引入新的文本处理依赖。

### T22 构建高峰日期和异常波动表

- 优先级：P1
- 依赖：T16、T19
- 任务目标：识别评论量高峰、差评率异常升高的日期。
- 需要修改的文件：`scripts/04_detect_alerts.R`
- 预期输出：`outputs/tables/review_alerts.csv`
- 如何验证是否完成：运行 `Rscript scripts/04_detect_alerts.R` 后生成异常日期表，并包含异常类型字段。
- 是否可能影响现有代码：是，当前 `04_detect_alerts.R` 会从占位脚本变为正式输出脚本。

### T23 构建游戏时长与评价关系指标

- 优先级：P2
- 依赖：T13
- 任务目标：如果数据包含游戏时长，按时长分组比较正负反馈。
- 需要修改的文件：`scripts/03_build_metrics.R`
- 预期输出：`outputs/tables/playtime_sentiment_summary.csv`
- 如何验证是否完成：表中包含时长分组、评论数、差评率。
- 是否可能影响现有代码：否，如果没有时长字段，该任务应跳过并在文档中说明。

## 阶段 5：可视化输出

### T24 将可视化逻辑拆到单独脚本

- 优先级：P0
- 依赖：T16
- 任务目标：把当前 `03_summarize_reviews.R` 中的画图逻辑迁移到第四步可视化脚本。
- 需要修改的文件：`scripts/04_visualize_reviews.R`
- 预期输出：存在独立的 `04_visualize_reviews.R`，只负责读取指标表并生成图表。
- 如何验证是否完成：运行 `Rscript scripts/04_visualize_reviews.R` 不需要重新构建指标。
- 是否可能影响现有代码：是，脚本职责会调整。

### T25 输出每日评论量趋势图

- 优先级：P1
- 依赖：T24
- 任务目标：保存每日评论数量变化图。
- 需要修改的文件：`scripts/04_visualize_reviews.R`
- 预期输出：`outputs/figures/daily_review_volume.png`
- 如何验证是否完成：图片文件存在，且图中横轴为日期、纵轴为评论数。
- 是否可能影响现有代码：否，新增图表输出。

### T26 输出每日差评率趋势图

- 优先级：P1
- 依赖：T24
- 任务目标：保存每日差评率变化图。
- 需要修改的文件：`scripts/04_visualize_reviews.R`
- 预期输出：`outputs/figures/daily_negative_rate.png`
- 如何验证是否完成：图片文件存在，纵轴为百分比格式。
- 是否可能影响现有代码：否，新增图表输出。

### T27 输出 7 日滚动差评率趋势图

- 优先级：P1
- 依赖：T19、T24
- 任务目标：保存平滑后的差评率趋势图。
- 需要修改的文件：`scripts/04_visualize_reviews.R`
- 预期输出：`outputs/figures/rolling_7d_negative_rate.png`
- 如何验证是否完成：图片文件存在，并使用 `negative_rate_7d` 字段。
- 是否可能影响现有代码：否，新增图表输出。

### T28 输出正评 / 差评占比图

- 优先级：P1
- 依赖：T16、T24
- 任务目标：展示总体评价倾向。
- 需要修改的文件：`scripts/04_visualize_reviews.R`
- 预期输出：`outputs/figures/sentiment_share.png`
- 如何验证是否完成：图片文件存在，正评和差评占比合计为 100%。
- 是否可能影响现有代码：否，新增图表输出。

### T29 输出评论语言分布图

- 优先级：P1
- 依赖：T20、T24
- 任务目标：展示中英文评论数量和占比。
- 需要修改的文件：`scripts/04_visualize_reviews.R`
- 预期输出：`outputs/figures/language_distribution.png`
- 如何验证是否完成：图片文件存在，语言类别与语言分布表一致。
- 是否可能影响现有代码：否，新增图表输出。

### T30 输出高频关键词柱状图

- 优先级：P1
- 依赖：T21、T24
- 任务目标：展示评论文本中的高频关键词。
- 需要修改的文件：`scripts/04_visualize_reviews.R`
- 预期输出：`outputs/figures/keyword_frequency.png`
- 如何验证是否完成：图片文件存在，展示 Top N 关键词和频次。
- 是否可能影响现有代码：否，新增图表输出。

### T31 输出游戏时长与评价倾向关系图

- 优先级：P2
- 依赖：T23、T24
- 任务目标：如果数据包含游戏时长，展示时长分组与差评率关系。
- 需要修改的文件：`scripts/04_visualize_reviews.R`
- 预期输出：`outputs/figures/playtime_vs_sentiment.png`
- 如何验证是否完成：图片文件存在，横轴为时长分组，纵轴为差评率或正负评价数量。
- 是否可能影响现有代码：否，如果没有时长字段，该任务应跳过并在文档中说明。

## 阶段 6：Quarto / README 项目文档

### T32 创建 Quarto 报告骨架

- 优先级：P1
- 依赖：T16、T24
- 任务目标：建立最终分析报告入口。
- 需要修改的文件：`reports/steam_reviews_analysis.qmd`
- 预期输出：报告包含项目背景、数据来源、方法、指标、图表和结论章节。
- 如何验证是否完成：运行 `quarto render reports/steam_reviews_analysis.qmd` 可以生成 HTML 报告。
- 是否可能影响现有代码：否，新增报告文件。

### T33 在报告中加入数据清洗说明

- 优先级：P1
- 依赖：T32、阶段 3 完成
- 任务目标：解释去重、文本清理、日期标准化、负评字段构建和 ID 字符读取。
- 需要修改的文件：`reports/steam_reviews_analysis.qmd`
- 预期输出：报告中有清洗流程说明和清洗前后行数对比。
- 如何验证是否完成：报告能说明 raw 到 processed 的主要变化。
- 是否可能影响现有代码：否，只修改报告。

### T34 在报告中加入核心指标和图表

- 优先级：P1
- 依赖：阶段 4、阶段 5 完成
- 任务目标：把每日评论量、差评率、滚动差评率、语言分布、关键词等结果写入报告。
- 需要修改的文件：`reports/steam_reviews_analysis.qmd`
- 预期输出：报告中嵌入主要图表和关键发现。
- 如何验证是否完成：渲染后的报告能完整展示 `outputs/figures/` 中的核心图。
- 是否可能影响现有代码：否，只修改报告。

### T35 重写 README 项目说明

- 优先级：P1
- 依赖：阶段 1 到阶段 5 基本完成
- 任务目标：让 README 成为项目首页，清楚说明项目目标、结构、运行方式和主要发现。
- 需要修改的文件：`README.md`
- 预期输出：README 包含项目简介、目录结构、数据来源、运行步骤、输出结果、关键图表和后续改进。
- 如何验证是否完成：只看 README 就能理解并复现项目主流程。
- 是否可能影响现有代码：否，只修改文档。

### T36 补充数据限制与伦理说明

- 优先级：P2
- 依赖：T35
- 任务目标：说明数据来自 Steam 公开评论，指出抽样范围、时间范围、语言范围和潜在偏差。
- 需要修改的文件：`README.md`、`reports/steam_reviews_analysis.qmd`
- 预期输出：文档中明确当前数据限制和解释边界。
- 如何验证是否完成：README 和报告都能找到数据限制说明。
- 是否可能影响现有代码：否，只修改文档。

## 阶段 7：最终检查与简历项目总结

### T37 从 raw 数据执行完整流水线

- 优先级：P0
- 依赖：阶段 1 到阶段 5 完成
- 任务目标：验证项目从原始数据到图表输出可以端到端复现。
- 需要修改的文件：无，必要时根据失败结果回到对应脚本修复。
- 预期输出：`data/processed/`、`outputs/tables/`、`outputs/figures/` 全部生成预期文件。
- 如何验证是否完成：依次运行 `Rscript scripts/02_clean_reviews.R`、`Rscript scripts/03_build_metrics.R`、`Rscript scripts/04_visualize_reviews.R`、`Rscript scripts/04_detect_alerts.R`，全部成功。
- 是否可能影响现有代码：否，本任务是验证；若失败才产生后续修复任务。

### T38 检查 Git 工作区和文件追踪状态

- 优先级：P0
- 依赖：T37
- 任务目标：确认仓库只包含应提交的代码、数据样例、报告和图表，不包含临时文件。
- 需要修改的文件：`.gitignore` 或误加入的临时文件
- 预期输出：`git status --short` 中没有无关系统文件或临时输出。
- 如何验证是否完成：运行 `git status --short` 并人工确认变更列表。
- 是否可能影响现有代码：否，主要影响版本控制清洁度。

### T39 整理项目亮点摘要

- 优先级：P1
- 依赖：T35、T37
- 任务目标：提炼适合简历或作品集展示的 3 到 5 条项目亮点。
- 需要修改的文件：`README.md`
- 预期输出：README 中包含“项目亮点”或“Portfolio Highlights”部分。
- 如何验证是否完成：亮点能覆盖数据获取、清洗、指标构建、可视化和报告能力。
- 是否可能影响现有代码：否，只修改文档。

### T40 编写简历项目描述

- 优先级：P2
- 依赖：T39
- 任务目标：把项目转化为简历上的简洁描述。
- 需要修改的文件：`README.md` 或新增 `docs/resume_summary.md`
- 预期输出：包含 1 段项目简介和 3 条 bullet 成果描述。
- 如何验证是否完成：描述中有工具、方法、指标和可量化输出，适合直接放入简历。
- 是否可能影响现有代码：否，只修改文档。

## 推荐执行顺序

1. 先完成 T01 到 T04，建立干净目录和版本控制基础。
2. 再完成 T06、T07，确保运行环境说明清楚。
3. 接着处理 T09 到 T11，优先修复 ID 精度和 processed 输出。
4. 然后完成 T15、T16，修复当前最明显的指标流程断点。
5. 再推进 T17 到 T22，补齐语言对比、滚动指标、关键词和异常检测。
6. 之后完成 T24 到 T31，把分析结果变成可交付图表。
7. 最后完成 T32 到 T40，完善报告、README 和简历表达。
