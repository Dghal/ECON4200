library(tidyverse)
library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(car)
library(mediation)
library(corrplot)


data_raw <- read_excel("C:\\Users\\Steve\\Desktop\\new_ir2.xlsx")


# 宽转长
data_long <- data_raw %>%
  pivot_longer(cols = -c(code, name, field, nature, fcode, regis_place), 
               names_to = c(".value", "year"),
               names_pattern = "([a-zA-Z_]+)(\\d{4})") %>% mutate(
    ln_size = log((size / 1e8) + 1), 
    ln_mc = log((mc / 1e8) + 1),
    ESG = case_when(
      ESG == "AAA" ~ 9, ESG == "AA" ~ 8, ESG == "A" ~ 7,
      ESG == "BBB" ~ 6, ESG == "BB" ~ 5, ESG == "B" ~ 4,
      ESG == "CCC" ~ 3, ESG == "CC" ~ 2, ESG == "C" ~ 1,
      TRUE ~ NA_real_
    )) %>% mutate(ESG = as.numeric(ESG), year = as.factor(year))


#clean


na_percent <- colMeans(is.na(data_long)) * 100

print(na_percent)

# NA超过10%的变量
print(na_percent[na_percent > 10])


nrow(data_long) # 过滤前的样本量

data_long_clean <- data_long %>% filter(!is.na(ESG) & !is.na(ln_size) & !is.na(ROA) 
                                        & !is.na(leverage) & !is.na(tobinq)) # 关键变量缺失的行删掉


# # 用中位数填充
# fill_median <- function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)
# 
# data_long_clean <- data_long_clean %>% mutate(across(c(asset_turnover, ins_hold, CR, ROE), fill_median))
# 
# colSums(is.na(data_long))


# Winsorization
winsorization <- function(x, p = 0.01) {
  q <- 1-p
  lower <- quantile(x, probs = p, na.rm = TRUE)
  upper <- quantile(x, probs = q, na.rm = TRUE)
  x <- ifelse(x < lower, lower, x) 
  x <- ifelse(x > upper, upper, x) 
  return(x) 
}


winsor_vars <- c("ln_size", "ln_mc", "tobinq", "CR", "ins_hold", "IR")

data_long_clean <- data_long_clean %>% mutate(across(winsor_vars, winsorization))


#data_long_clean <- data_long_clean %>% mutate(tobinq = log(1 + tobinq))

# 检查清洗后样本量
nrow(data_long_clean)
# 相关性矩阵
correlation_matrix <- cor(data_long_clean[, c("ESG", "ln_mc", "ln_size", "ROA", 
                                              "leverage", "tobinq", "growth", "CR", 
                                              "ins_hold", "asset_turnover", "IR")], use = "pairwise.complete.obs")
print(correlation_matrix)

# 相关性图
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)


# data_long_clean <- data_long_clean %>% group_by(code) %>%
# mutate(ESG_change = ESG - lag(ESG))






# 交互项处理：国企 vs 民营 vs 外资
data_long_clean <- data_long_clean %>%
  mutate(nature_soe = case_when( nature %in% c("央企国资控股", "省属国资控股", "地市国资控股", "其他国有") ~ "国企",
                                 nature == "外资企业" ~ "外资",
                                 TRUE ~ "民企")) %>% mutate(nature_soe = factor(nature_soe, 
                                                                              levels = c("国企", "外资", "民企")))

# 交互项处理：重污染行业
heavy_pollution_codes <- c("B06", "B07", "B08", "B09", "C17", "C19", "C22", "C25",
                           "C26", "C27", "C28", "C30", "C31", "C32", "C33", "D44")
data_long_clean <- data_long_clean %>%
  mutate(field_id = if_else(fcode %in% heavy_pollution_codes, 1L, 0L)) %>%
  mutate(field_id = factor(field_id, levels = c(0, 1), labels = c("非重污染", "重污染")))



# 交互项回归模型


#国企/私企/外资
#basic
model_interaction_soe <- lm(ln_mc ~ ESG * nature_soe + ln_size + ROA + leverage + 
                              tobinq + growth + CR + factor(year) + field_id, data = data_long_clean)
summary(model_interaction_soe)

# 固定效应国企/私企/外资
model_fe <- plm(ln_mc ~ ESG * nature_soe + ln_size + ROA + leverage + 
                  tobinq + growth + CR + year, 
                data = data_long_clean, index = c("code", "year"), model = "within")
summary(model_fe)

# 随机效应国企/私企/外资

model_re <- plm(ln_mc ~ ESG * nature_soe + ln_size + ROA + leverage + 
                  tobinq + growth + CR + factor(year) + field_id, 
                data = data_long_clean, index = c("code", "year"), model = "random")
summary(model_re)

# Hausman 检验
phtest(model_fe, model_re)


#重污染/污染
#basic
model_interaction_pollute <- lm(ln_mc ~ ESG * field_id + ln_size + 
                                  ROA + leverage + tobinq + growth + nature_soe + factor(year), 
                                data = data_long_clean)
summary(model_interaction_pollute)

# 固定效应污染/重污染
model_fe2 <- plm(ln_mc ~ ESG * field_id + ln_size + ROA + leverage + 
                   tobinq + growth + CR + factor(year), 
                 data = data_long_clean, index = c("code", "year"), model = "within")
summary(model_fe2)



# 随机效应污染/重污染
model_re2 <- plm(ln_mc ~ ESG * field_id + ln_size + ROA + leverage + 
                   tobinq + growth + CR + factor(year), 
                 data = data_long_clean, index = c("code", "year"), model = "random")
summary(model_re2)

# Hausman 检验
phtest(model_fe2, model_re2)


# OLS 基础回归
model_basic <- lm(ln_mc ~ ESG + ln_size + ROA + leverage + 
                    tobinq + growth + CR + nature_soe + field_id + factor(year), 
                  data = data_long_clean)
summary(model_basic) 

# 固定效应
model_fe3 <- plm(ln_mc ~ ESG + ln_size + ROA + leverage + 
                   tobinq + growth + CR + nature_soe + field_id + factor(year), data = data_long_clean, 
                 index = c("code", "year"), model = "within")
summary(model_fe3)

# 随机效应
model_re3 <- plm(ln_mc ~ ESG + ln_size + ROA + leverage + 
                   tobinq + growth + CR + nature_soe + field_id + factor(year), data = data_long_clean, 
                 index = c("code", "year"), model = "random")
summary(model_re3)

# Hausman test
phtest(model_fe3, model_re3)


# 多重共线性检测
# vif(model_basic)



# 中介效应分析

# esg to 资产周转率
model_esg_at <- lm(formula = asset_turnover ~ ESG + ln_size + ROA + leverage + tobinq + 
                     growth + CR + field_id + year + nature_soe, data = data_long_clean)


# esg to 机构持股
model_esg_ih <-lm(ins_hold ~ ESG + ln_size + ROA + leverage + tobinq + growth + CR + 
                    field_id + year + nature_soe, data = data_long_clean)

# esg to IR
model_esg_ir <-lm(IR ~ ESG + ln_size + ROA + leverage + tobinq + growth + CR + 
                    field_id + year + nature_soe, data = data_long_clean)

# 资产周转率 to mc
model_med1 <- lm( ln_mc ~ ESG + asset_turnover + ln_size + ROA + leverage + 
                    tobinq + growth + CR + field_id + year + nature_soe, data = data_long_clean)


# 机构持股 to mc
model_med2 <- lm(ln_mc ~ ESG + ins_hold + ln_size + ROA + leverage + 
                   tobinq + growth + CR + field_id + year + nature_soe, data = data_long_clean)

#IR to mc
model_med3 <- lm(ln_mc ~ ESG + IR + ln_size + ROA + leverage + 
                   tobinq + growth + CR + field_id + year + nature_soe, data = data_long_clean)

# # 确保中介变量是数值型
# data_long_clean <- data_long_clean %>%
# mutate(ESG = as.numeric(ESG), asset_turnover = as.numeric(asset_turnover), 
# ins_hold = as.numeric(ins_hold))


# 资产周转率的中介效应
mediate1 <- mediate(model_esg_at, model_med1, treat = "ESG", mediator = "asset_turnover", 
                    sims = 100, boot = TRUE)
summary(mediate1)

# 机构持股的中介效应
mediate2 <- mediate(model_esg_ih, model_med2, treat = "ESG", mediator = "ins_hold", 
                    sims = 100, boot = TRUE)
summary(mediate2)


#中介效应cost
mediate3 <- mediate(model_esg_ir, model_med3, treat = "ESG", mediator = "IR", 
                    sims = 1000, boot = TRUE)
summary(mediate3)

# 绘制中介效应图
plot(mediate1)
plot(mediate2)
