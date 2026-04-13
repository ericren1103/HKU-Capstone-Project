# 加载必要包
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lmtest)
  library(sandwich)
})

# =========================
# 1) 路径与参数设置
# =========================

# 简化路径逻辑：
# 1) 若当前在项目根目录（含 data/master_panel.csv），直接使用当前目录；
# 2) 若当前在 code 目录（上一层含 data/master_panel.csv），使用上一层；
# 3) 否则报错并提示先 setwd 到项目根目录或 code 目录。
cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (file.exists(file.path(cwd, "data", "master_panel.csv"))) {
  project_root <- cwd
} else if (file.exists(file.path(cwd, "..", "data", "master_panel.csv"))) {
  project_root <- normalizePath(file.path(cwd, ".."), winslash = "/", mustWork = FALSE)
} else {
  stop(
    "未找到 data/master_panel.csv。\n请先 setwd() 到 7006_group_project 根目录或其 code 子目录再运行脚本。"
  )
}

data_path <- file.path(project_root, "data", "master_panel.csv")
out_dir <- file.path(project_root, "results", "lag_sensitivity")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 敏感性分析只关注这两个核心关键词
target_keywords <- c("天氣", "防中暑")

# 需要测试的 Newey-West lag
lags_to_test <- c(2, 4, 6)

# 回归控制项（与主回归一致）
rhs_formula <- "time + intervention + time_since_intervention + temp_max_avg + extreme_hot_days + season_sin + season_cos + rainfall_total + humidity_avg"


# =========================
# 2) 读取数据与函数定义
# =========================

data <- read_csv(data_path, show_col_types = FALSE)

run_one_keyword_one_lag <- function(df, y_var, lag_k) {
  # 构造含中文/空格变量名的公式
  formula_str <- paste0("`", y_var, "` ~ ", rhs_formula)
  model <- lm(as.formula(formula_str), data = df)
  
  # Newey-West(HAC) 稳健标准误
  ct <- coeftest(model, vcov = NeweyWest(model, lag = lag_k))
  
  # 提取 level（intervention）与 trend（time_since_intervention）
  out <- tibble(
    keyword = y_var,
    lag = lag_k,
    level_coef = ct["intervention", "Estimate"],
    level_se = ct["intervention", "Std. Error"],
    level_p = ct["intervention", "Pr(>|t|)"],
    trend_coef = ct["time_since_intervention", "Estimate"],
    trend_se = ct["time_since_intervention", "Std. Error"],
    trend_p = ct["time_since_intervention", "Pr(>|t|)"]
  )
  out
}


# =========================
# 3) 执行 lag 敏感性分析
# =========================

results <- list()

for (kw in target_keywords) {
  for (lg in lags_to_test) {
    results[[length(results) + 1]] <- run_one_keyword_one_lag(data, kw, lg)
  }
}

result_df <- bind_rows(results) %>%
  arrange(keyword, lag)

# 保存宽表，便于报告粘贴
result_wide <- result_df %>%
  pivot_longer(
    cols = c(level_coef, level_se, level_p, trend_coef, trend_se, trend_p),
    names_to = "metric",
    values_to = "value"
  ) %>%
  separate(metric, into = c("effect", "stat"), sep = "_") %>%
  pivot_wider(names_from = c(effect, lag), values_from = value)

write_csv(result_df, file.path(out_dir, "lag_sensitivity_long.csv"))
write_csv(result_wide, file.path(out_dir, "lag_sensitivity_wide.csv"))


# =========================
# 4) 画图输出（p值对比图）
# =========================

# 为作图整理成长表：两种效应（level/trend）的 p 值随 lag 的变化
plot_df <- result_df %>%
  select(keyword, lag, level_p, trend_p) %>%
  pivot_longer(cols = c(level_p, trend_p), names_to = "effect", values_to = "p_value") %>%
  mutate(
    effect = recode(effect,
                    "level_p" = "Level",
                    "trend_p" = "Trend")
  )

p <- ggplot(plot_df, aes(x = lag, y = p_value, color = effect, group = effect)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.6) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "#d62728") +
  geom_hline(yintercept = 0.10, linetype = "dotted", color = "#ff7f0e") +
  facet_wrap(~ keyword, ncol = 1, scales = "fixed") +
  scale_x_continuous(breaks = lags_to_test) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Lag Sensitivity Test (Newey-West): 天氣 vs 防中暑",
    subtitle = "p-values under alternative lag lengths (2, 4, 6)",
    x = "Newey-West lag",
    y = "p-value",
    color = "Effect"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.6, "lines"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e6e6e6", linewidth = 0.35),
    plot.margin = margin(t = 10, r = 12, b = 12, l = 12)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(
  filename = file.path(out_dir, "lag_sensitivity_pvalues.png"),
  plot = p,
  width = 10,
  height = 8.5,
  dpi = 300
)

# 额外输出一个白底高对比版本（便于汇报/海报粘贴）
ggsave(
  filename = file.path(out_dir, "lag_sensitivity_pvalues_white.png"),
  plot = p,
  width = 10.5,
  height = 8.5,
  dpi = 360,
  bg = "white"
)


# =========================
# 5) 控制台输出简洁摘要
# =========================

cat("\n=== Lag Sensitivity Results (天氣, 防中暑) ===\n")
print(result_df)
cat("\n输出目录：", out_dir, "\n")
cat("图文件：", file.path(out_dir, "lag_sensitivity_pvalues.png"), "\n")
