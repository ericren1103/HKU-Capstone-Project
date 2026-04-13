# 加载必要的包
library(tidyverse)
library(sandwich)
library(lmtest)
library(broom)

# 读取数据（请根据实际路径修改）
data <- read_csv("C:/Users/86159/Desktop/master_panel.csv")

# 定义因变量列表（共18个，根据 CSV 文件中的列名）
dependent_vars <- c(
  "air conditioning subsidy",
  "cooling center",
  "Hong Kong temperature",
  "Hong Kong weather",
  "prevent heatstroke",
  "ways to cool down",
  "weather",
  "中暑",
  "中暑症狀",
  "熱中暑",
  "酷熱",
  "空調補貼",
  "避暑中心",
  "天氣",
  "開冷氣",
  "防中暑",
  "香港天氣",
  "香港溫度"
)

# 存储结果的空数据框
results <- data.frame()

# 循环每个因变量
for (y_var in dependent_vars) {
  # 构造公式：使用反引号包裹变量名（处理空格和中文）
  formula_str <- paste0("`", y_var, "` ~ time + intervention + time_since_intervention + temp_max_avg + extreme_hot_days + season_sin + season_cos + rainfall_total + humidity_avg")
  
  # 运行线性模型
  model <- lm(as.formula(formula_str), data = data)
  
  # 计算 Newey-West 标准误（滞后4周）
  coeftest_result <- coeftest(model, vcov = NeweyWest(model, lag = 4))
  
  # 提取 intervention 的系数、标准误、p值
  coeff_intervention <- coeftest_result["intervention", "Estimate"]
  se_intervention    <- coeftest_result["intervention", "Std. Error"]
  p_intervention     <- coeftest_result["intervention", "Pr(>|t|)"]
  
  # 提取 time_since_intervention 的系数、标准误、p值
  coeff_tsi <- coeftest_result["time_since_intervention", "Estimate"]
  se_tsi    <- coeftest_result["time_since_intervention", "Std. Error"]
  p_tsi     <- coeftest_result["time_since_intervention", "Pr(>|t|)"]
  
  # 将结果存入数据框
  results <- rbind(results, data.frame(
    Dependent = y_var,
    Intervention_Coeff = coeff_intervention,
    Intervention_SE = se_intervention,
    Intervention_p = p_intervention,
    TSI_Coeff = coeff_tsi,
    TSI_SE = se_tsi,
    TSI_p = p_tsi,
    R_squared = summary(model)$r.squared,
    Adj_R_squared = summary(model)$adj.r.squared
  ))
}


# 重新定义干预变量（假干预日期：2022-05-23）
intervention_date_placebo <- ymd("2022-05-23")
intervention_week_placebo <- floor_date(intervention_date_placebo, unit = "week")

data_placebo <- data_placebo %>%
  mutate(
    intervention_placebo = ifelse(week_start >= intervention_week_placebo, 1, 0),
    tsi_placebo = ifelse(intervention_placebo == 1,
                         time - min(time[intervention_placebo == 1]) + 1,
                         0)
  )

# 用假干预变量重新建模（变量名改为 intervention_placebo 和 tsi_placebo）
model_placebo_correct <- lm(`天氣` ~ time + intervention_placebo + tsi_placebo + 
                            temp_max_avg + extreme_hot_days + season_sin + season_cos + 
                            rainfall_total + humidity_avg, 
                            data = data_placebo)

# 查看结果
coeftest(model_placebo_correct, vcov = NeweyWest(model_placebo_correct, lag = 4))