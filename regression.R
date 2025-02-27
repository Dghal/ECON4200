library(tidyverse)
library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)

data_raw <- read_excel("C:\\Users\\Steve\\Desktop\\data_esg to mc.xlsx")


esg_levels <- c("AAA" = 9, "AA" = 8, "A" = 7, "BBB" = 6, 
                "BB" = 5, "B" = 4, "CCC" = 3, "CC" = 2, "C" = 1)

# 宽转长
data_long <- data_raw %>%
  pivot_longer(
    cols = -c(code, name, field, nature, fcode),        
    names_to = c(".value", "year"),
    names_pattern = "([a-zA-Z_]+)(\\d{4})"
  ) %>%
  mutate(
    year = as.numeric(year),
    ln_size = log((size / 1e8) + 1),
    ln_mc   = log((mc   / 1e8) + 1),
    ESG = case_when(
      ESG == "AAA" ~ 9,
      ESG == "AA"  ~ 8,
      ESG == "A"   ~ 7,
      ESG == "BBB" ~ 6,
      ESG == "BB"  ~ 5,
      ESG == "B"   ~ 4,
      ESG == "CCC" ~ 3,
      ESG == "CC"  ~ 2,
      ESG == "C"   ~ 1,
      TRUE         ~ NA_real_  # or NA
    )
  )

# data clean listwise delete
data_long_clean <- data_long %>%
  filter(
    !(is.na(nature) | is.na(ln_size) | is.na(ROA) |
        is.na(leverage) | is.na(tobinq) | is.na(TAT) |
        is.na(growth) | is.na(CR) | is.na(ROE))
  ) #%>%
# 填补ESG
#  mutate(ESG = ifelse(is.na(ESG), median(ESG, na.rm = TRUE), ESG)) 

# 去离群
remove_outliers <- function(df, var){
  q <- quantile(df[[var]], probs = c(0.25, 0.75), na.rm = TRUE)
  iqr_val <- q[2] - q[1]
  lower <- q[1] - 1.5 * iqr_val
  upper <- q[2] + 1.5 * iqr_val
  df %>%
    filter(.data[[var]] >= lower & .data[[var]] <= upper)
}

vars_for_outlier <- c("ln_size", "ROA", "leverage",
                      "tobinq", "TAT", "growth", "CR",
                      "ROE", "ln_mc")

for(v in vars_for_outlier){
  data_long_clean <- remove_outliers(data_long_clean, v)
}

data_long_clean <- data_long_clean %>%
  mutate(firm_performance = ln_mc + ROE)

# regression
pdata <- pdata.frame(data_long_clean, index = c("code", "year"))
summary(pdata)

model_field_dummy <- lm(
  ln_mc ~ ESG + ln_size + ROA + ROE + leverage +
    tobinq + growth + CR  + factor(nature) + factor(field) + factor(year),
  data = data_long_clean
)

summary(model_field_dummy)

library(dplyr)

data_long_clean <- data_long_clean %>%
  mutate(
    nature_soe = if_else(
      nature %in% c("民营企业", "外资企业"), 
      0L,  # 民营或外资，为0
      1L   # 否则记为1
    )
  )

# 转成 factor 类型
data_long_clean$nature_soe <- factor(
  data_long_clean$nature_soe,
  levels = c(0, 1),
  labels = c("民营外资合并", "国企") 
)

model_binary <- lm(
  ln_mc ~ ESG * nature_soe  + 
    ln_size + ROA + ROE + leverage +
    tobinq + growth + CR +
    factor(field) + factor(year),
  data = data_long_clean
)

summary(model_binary)


# 重污染企业的 field_code 
heavy_pollution_codes <- c("B06", "B07", "B08", "B09",
                           "C17", "C19", "C22", "C25",
                           "C26", "C27", "C28", "C30",
                           "C31", "C32", "C33", "D44")

# 1=重污染, 0=其他
data_long_clean <- data_long_clean %>%
  mutate(
    field_id = if_else(fcode %in% heavy_pollution_codes, 1L, 0L)
  )

#  factor 
data_long_clean$field_id <- factor(
  data_long_clean$field_id,
  levels = c(0, 1),
  labels = c("Non-HeavyPollution", "HeavyPollution")
)

#ESG 做交互项的回归模型
model_pollution_interaction <- lm(
  ln_mc ~ ESG * nature_soe +
    ln_size + ROA + ROE + leverage +
    tobinq + growth + CR + factor(nature_soe) + factor(year),
  data = data_long_clean
)

summary(model_pollution_interaction)
