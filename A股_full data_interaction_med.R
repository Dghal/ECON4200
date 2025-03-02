library(tidyverse)
library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
data_raw <- read_excel("C:\\Users\\Steve\\Desktop\\data_esg to mc - 副本.xlsx")


#宽转长
data_long <- data_raw %>%
  pivot_longer( cols = -c(code, name, field, nature, fcode, regis_place), 
                names_to = c(".value", "year"),
                names_pattern = "([a-zA-Z_]+)(\\d{4})"
  ) %>%mutate(
    ln_size = log((size / 1e8) + 1), 
    ln_mc = log((mc / 1e8) + 1),
    ESG = case_when( ESG == "AAA" ~ 9, ESG == "AA" ~ 8, ESG == "A" ~ 7,
                     ESG == "BBB" ~ 6, ESG == "BB" ~ 5, ESG == "B" ~ 4,
                     ESG == "CCC" ~ 3, ESG == "CC" ~ 2, ESG == "C" ~ 1,
                     TRUE~ NA_real_ ))


data_long <- data_long %>%mutate(ESG = as.numeric(ESG)) 
data_long<- data_long%>% mutate(year = as.factor(year)) 


# clean dataset
#去NA
data_long_clean <- data_long%>%
  filter( !is.na(nature) & !is.na(ln_size) & !is.na(ROA) &
            !is.na(leverage) & !is.na(tobinq) & !is.na(TAT) &
            !is.na(growth) & !is.na(CR) & !is.na(ROE) & !is.na(ins_hold) & !is.na(asset_turnover) & !is.na(ESG)
          )
#去异常
remove_outliers <- function(df, var) {
  q <- quantile(df[[var]], probs = c(0.25, 0.75), na.rm = TRUE)
  lower_bound <- q[1] - 1.5 * ( q[2] - q[1] )
  upper_bound <- q[2] + 1.5 * ( q[2] - q[1] )
  df %>% filter(.data[[var]] >= lower_bound & .data[[var]] <= upper_bound)
  }
# 需要处理的变量
variable_outlier <- c("ln_size", "ROA", "leverage", "tobinq", 
                      "TAT", "growth", "CR", "ROE", "ln_mc", "ins_hold", "asset_turnover")
# 去异常
for (i in variable_outlier) {
  data_long_clean <- remove_outliers(data_long_clean, i)
  }

# performance?
# data_long_clean <- data_long_clean %>%mutate(firm_performance = ln_mc + ROE)

#清洗后样本量
nrow(data_long_clean) 

#变量名检查
colnames(data_long_clean)

#data_long_clean <- as_tibble(data_long_clean)

#相关性矩阵
correlation_matrix <- cor(data_long_clean[, c("ESG", "ln_mc", "ln_size", "ROA", 
                                              "leverage", "tobinq", "growth", "CR", 
                                              "ins_hold", "asset_turnover")])

print(correlation_matrix)

#图
library(corrplot)
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)

# 交互项soe
data_long_clean <- data_long_clean%>%
  mutate( nature_soe = if_else(nature %in% c("民营企业", "外资企业"), 0L, 1L)) %>%
  mutate(nature_soe = factor(nature_soe, levels = c(0, 1), 
                             labels = c("民营外资合并", "国企")))

data_long_clean <- data_long_clean %>% mutate(nature_soe = as.factor(nature_soe)) 


# interaction effect soe
model_interaction_soe <- lm(formula = ln_mc ~ ESG * nature_soe + ln_size + ROA + leverage + 
                              tobinq + growth + CR + year, data = data_long_clean)
summary(model_interaction_soe)


# 交互项fcode
heavy_pollution_codes <- c("B06", "B07", "B08", "B09", "C17", "C19", "C22", "C25",
                           "C26", "C27", "C28", "C30", "C31", "C32", "C33", "D44")

data_long_clean <- data_long_clean%>%
  mutate(field_id = if_else(fcode %in%heavy_pollution_codes, 1L, 0L)) %>%
  mutate(field_id = factor(field_id, levels = c(0, 1), labels = c("非重污染", "重污染")))

data_long_clean <- data_long_clean %>% mutate(field_id = as.factor(field_id)) 

# interaction effect 污染非污染
model_interaction_pollute <- lm(formula = ln_mc ~ ESG * field_id + ln_size + 
                                  ROA + leverage + tobinq + growth + nature_soe + year, 
                                data = data_long_clean)

summary(model_interaction_pollute)

# 基础ols回归
model_first <- lm(formula = ln_mc ~ ESG + ln_size + ROA + 
                    leverage + tobinq + growth + CR + nature_soe + field_id + year, 
                  data = data_long_clean)

summary(model_first)

#多重共线性
library(car)

vif(model_first)


library(mediation)

# esg to 资产周转率
model_esg_at <- lm(formula = asset_turnover ~ ESG + ln_size + ROA + leverage + tobinq + 
                     growth + CR + field_id + year + nature_soe, data = data_long_clean)

# esg to 机构持股
model_esg_ih <-lm(ins_hold ~ ESG + ln_size + ROA + leverage + tobinq + growth + CR + 
                    field_id + year + nature_soe, data = data_long_clean)

# 资产周转率 to mc
model_med1 <- lm( ln_mc ~ ESG + asset_turnover + ln_size + ROA + leverage + 
                    tobinq + growth + CR + field_id + year + nature_soe, data = data_long_clean)

# 机构持股 to mc
model_med2 <- lm(ln_mc ~ ESG + ins_hold + ln_size + ROA + leverage + 
                   tobinq + growth + CR + field_id + year + nature_soe, data = data_long_clean)

data_long_clean <- data_long_clean %>% mutate(ESG = as.numeric(ESG), asset_turnover = as.numeric(asset_turnover), 
                                              ins_hold = as.numeric(ins_hold))

# 中介效应资产周转
mediate1 <- mediate( model_esg_at, model_med1, treat = "ESG", mediator = "asset_turnover", 
                     sims = 100, boot = TRUE)

# 中介效应机构持股
mediate2 <- mediate( model_esg_ih, model_med2, treat = "ESG", mediator = "ins_hold", 
                     sims = 100, boot = TRUE)


summary(mediate1)
summary(mediate2)

# 图
plot(mediate1)
plot(mediate2)
