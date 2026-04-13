# 加载必要的包
library(tidyverse)
library(lubridate)
library(ggplot2)
library(sandwich)   # 仅用于模型，绘图不需要

# 读取数据（请根据实际路径修改）
data <- read_csv("C:/Users/86159/Desktop/master_panel.csv")

# 确保 week_start 为日期格式
data$week_start <- ymd(data$week_start)

# 定义真实干预日期（2023-05-22 周一）
real_intervention_date <- ymd("2023-05-22")

# 定义假干预日期（2022-05-23 周一）
placebo_intervention_date <- ymd("2022-05-23")

# ---------- 1. 真实干预模型（使用原始 intervention 和 time_since_intervention）----------
# 注意：数据中已经包含这些列，直接使用
model_real <- lm(`天氣` ~ time + intervention + time_since_intervention + 
                 temp_max_avg + extreme_hot_days + season_sin + season_cos + 
                 rainfall_total + humidity_avg, 
                 data = data)
data$fitted_real <- fitted(model_real)

# ---------- 2. 假干预模型（构造 placebo 变量）----------
data_placebo <- data  # 复制数据，避免覆盖
# 计算假干预的周起始（2022-05-23 所在周的周一）
placebo_week <- floor_date(placebo_intervention_date, unit = "week")

data_placebo <- data_placebo %>%
  mutate(
    intervention_placebo = ifelse(week_start >= placebo_week, 1, 0),
    tsi_placebo = ifelse(intervention_placebo == 1,
                         time - min(time[intervention_placebo == 1]) + 1,
                         0)
  )

model_placebo <- lm(`天氣` ~ time + intervention_placebo + tsi_placebo + 
                    temp_max_avg + extreme_hot_days + season_sin + season_cos + 
                    rainfall_total + humidity_avg, 
                    data = data_placebo)
data_placebo$fitted_placebo <- fitted(model_placebo)

# 合并拟合值到原始数据（为了绘图统一）
data <- data %>%
  left_join(data_placebo %>% select(week_start, fitted_placebo), by = "week_start")

# ---------- 3. 绘图 ----------
p <- ggplot(data, aes(x = week_start)) +
  # 原始数据（灰色半透明线）
  geom_line(aes(y = `天氣`), color = "gray50", alpha = 0.6, size = 0.8) +
  # 真实干预模型拟合线（红色）
  geom_line(aes(y = fitted_real), color = "red", size = 1.2) +
  # 假干预模型拟合线（蓝色）
  geom_line(aes(y = fitted_placebo), color = "blue", size = 1.2) +
  # 真实干预竖线（红色虚线）
  geom_vline(xintercept = real_intervention_date, linetype = "dashed", color = "red", size = 0.8) +
  # 假干预竖线（蓝色虚线）
  geom_vline(xintercept = placebo_intervention_date, linetype = "dashed", color = "blue", size = 0.8) +
  labs(
    title = "Comparison of Real vs Placebo Intervention (Weather Search)",
    subtitle = "Red: Real intervention (2023-05-22, dashed line); Blue: Placebo (2022-05-23, dashed line); Gray: Observed data",
    x = "Week",
    y = "Google Trends Index (天氣)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  # 可选：添加图例（手动）
  annotate("text", x = ymd("2023-01-01"), y = 90, label = "Real fit (red)", color = "red", hjust = 0) +
  annotate("text", x = ymd("2023-01-01"), y = 85, label = "Placebo fit (blue dashed)", color = "blue", hjust = 0)

# 显示图形
print(p)