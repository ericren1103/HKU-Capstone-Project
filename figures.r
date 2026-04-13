#如需绘制其他关键词的图像，直接替换模型代码以及绘图代码里面的关键词即可

#图形解读：灰线-原始搜索量；红线-模型拟合值（控制了天气、季节等）；蓝虚线-干预周（2023-05-22）

# 加载包
library(tidyverse)
library(ggplot2)
library(lubridate)

# 读取数据（请修改目标文件路径）
data <- read_csv("C:/Users/86159/Desktop/7006_GoogleTrends_data/master_panel.csv")

# 确保 week_start 为日期格式
data$week_start <- ymd(data$week_start)

# 定义干预日期
intervention_date <- ymd("2023-05-22")

# 对两个关键变量分别建模（用于获取拟合值）
# 模型1：天氣
model_weather <- lm(`天氣` ~ time + intervention + time_since_intervention + 
                    temp_max_avg + extreme_hot_days + season_sin + season_cos + 
                    rainfall_total + humidity_avg, data = data)
data$fitted_weather <- fitted(model_weather)

# 模型2：防中暑
model_prevent <- lm(`防中暑` ~ time + intervention + time_since_intervention + 
                    temp_max_avg + extreme_hot_days + season_sin + season_cos + 
                    rainfall_total + humidity_avg, data = data)
data$fitted_prevent <- fitted(model_prevent)

# 绘制天氣图
p1 <- ggplot(data, aes(x = week_start)) +
  geom_line(aes(y = `天氣`), color = "gray50", alpha = 0.7, size = 0.8) +
  geom_line(aes(y = fitted_weather), color = "red", size = 1) +
  geom_vline(xintercept = intervention_date, linetype = "dashed", color = "blue", size = 0.8) +
  labs(title = "Search Volume for '天氣' (Weather)",
       subtitle = "Red line: ITS fitted values; Blue dashed line: intervention week (2023-05-22)",
       x = "Week", y = "Google Trends Index") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# 绘制防中暑图
p2 <- ggplot(data, aes(x = week_start)) +
  geom_line(aes(y = `防中暑`), color = "gray50", alpha = 0.7, size = 0.8) +
  geom_line(aes(y = fitted_prevent), color = "red", size = 1) +
  geom_vline(xintercept = intervention_date, linetype = "dashed", color = "blue", size = 0.8) +
  labs(title = "Search Volume for '防中暑' (Prevent Heatstroke)",
       subtitle = "Red line: ITS fitted values; Blue dashed line: intervention week (2023-05-22)",
       x = "Week", y = "Google Trends Index") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# 显示图形
print(p1)
print(p2)

# 保存为 PNG（可选）
ggsave("weather_ITS.png", p1, width = 10, height = 6, dpi = 300)
ggsave("prevent_ITS.png", p2, width = 10, height = 6, dpi = 300)