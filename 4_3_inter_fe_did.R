library(tidyverse)
library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(car)
library(mediation)
library(corrplot)




data_raw <- read_excel("C:\\Users\\Steve\\Desktop\\newleveldata.xlsx")

# 宽转长
data_long <- data_raw %>%
  pivot_longer(cols = -c(code, name, field, nature, fcode, regis_place), 
               names_to = c(".value", "year"),
               names_pattern = "([a-zA-Z_]+)(\\d{4})") %>% 
  mutate(
    ln_size = log((size / 1e8) + 1), 
    ln_mc = log((mc / 1e8) + 1),
    ln_shares = log((shares / 1e8) + 1),
    ESG = case_when(
      ESG == "AAA" ~ 97.5, ESG == "AA" ~ 92.5, ESG == "A" ~ 87.5,
      ESG == "BBB" ~ 82.5, ESG == "BB" ~ 77.5, ESG == "B" ~ 72.5,
      ESG == "CCC" ~ 67.5, ESG == "CC" ~ 62.5, ESG == "C" ~ 57.5,
      TRUE ~ NA_real_
      )
    ) %>% 
  mutate(ESG = as.numeric(ESG), year = as.factor(year))


# 处理na
data_long_clean <- data_long %>%
  filter(!is.na(ESG) & !is.na(ln_size) & !is.na(ROA) & !is.na(leverage) & !is.na(tobinq))


# Winsorization
winsorization <- function(x, p = 0.01) {
  q <- 1 - p
  lower <- quantile(x, probs = p, na.rm = TRUE)
  upper <- quantile(x, probs = q, na.rm = TRUE)
  x <- ifelse(x < lower, lower, x) 
  x <- ifelse(x > upper, upper, x) 
  return(x) 
  }


winsor_vars <- c("ln_size", "ln_mc", "tobinq", "CR", "ins_hold")

data_long_clean <- data_long_clean %>% 
  mutate(across(all_of(winsor_vars), winsorization))


# 生成滞后变量
data_long_clean <- data_long_clean %>%
  group_by(code) %>%
  arrange(year) %>% 
  mutate(
    ESG_lag = lag(ESG), ln_size_lag = lag(ln_size), ROE_lag = lag(ROE),
    leverage_lag = lag(leverage),
    tobinq_lag = lag(tobinq), CR_lag = lag(CR),
    ins_hold_lag = lag(ins_hold), 
    ln_shares_lag = lag(ln_shares)
    ) %>% ungroup()



# 交互项
data_long_clean <- data_long_clean %>%
  mutate(nature_soe = case_when(
    nature %in% c("央企国资控股", "省属国资控股", "地市国资控股", "其他国有") ~ "国企",
    nature == "外资企业" ~ "外资",
    TRUE ~ "民企"
    )) %>% mutate(nature_soe = factor(nature_soe, levels = c("国企", "外资", "民企")))

heavy_pollution_codes <- c("B06", "B07", "B08", "B09", "C17", "C19", "C22", "C25","C26", 
                           "C27", "C28", "C30", "C31", "C32", "C33", "D44")


data_long_clean <- data_long_clean %>%
  mutate(field_id = if_else(fcode %in% heavy_pollution_codes, 1L, 0L)) %>%
  mutate(field_id = factor(field_id, levels = c(0, 1), labels = c("非重污染", "重污染")))



model_soe <- plm( ln_mc ~ ESG_lag * nature_soe + ln_size_lag + ln_shares_lag + ROE_lag + leverage_lag +
                    tobinq_lag + CR_lag + field_id,
                  data = data_long_clean,
                  index = c("code", "year"),
                  model = "within",
                  effect = "twoways"
                  )


summary(model_soe)



# 基础回归
# 固定效应
model_fe3 <- plm(ln_mc ~ ESG_lag + ln_size_lag + ln_shares_lag + ROE_lag + leverage_lag +
                   tobinq_lag + CR_lag + factor(year), 
                 data = data_long_clean, index = c("code", "year"), model = "within")

summary(model_fe3)


# vif多重共线性
vif(lm(ln_mc ~ ESG_lag + ln_size_lag + ln_shares_lag + ROE_lag + leverage_lag + tobinq_lag 
       + CR_lag + factor(year), data = data_long_clean))





high_pollution_codes <- c("B06", "B07", "B08", "B09", "C17", "C19", "C22", "C25", "C26",
                          "C27", "C28", "C30", "C31", "C32", "C33", "D44")


data_long_clean <- data_long_clean %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 2014 & year <= 2018) %>%
  mutate(
    window = case_when(
      year <= 2015 ~ "pre", 
      year >= 2016 ~ "post"
      )
    )


data_long_clean <- data_long_clean %>%
  mutate(
    Treat = ifelse(fcode %in% high_pollution_codes, 1, 0),
    Post = ifelse(window == "post", 1, 0),
    DID = Treat * Post
    ) %>%
  group_by(code) %>%
  arrange(year) %>%
  mutate(
    DID_lag = lag(DID),
    Treat_lag = lag(Treat),
    Post_lag = lag(Post)
    ) %>% ungroup()


#DID 滞后
did_lag <- plm(ln_mc ~ DID_lag + Treat_lag + Post_lag + 
                 ln_size_lag + ln_shares_lag + ROE_lag + leverage_lag + 
                 tobinq_lag + CR_lag + factor(year), 
               data = data_long_clean, index = c("code", "year"), 
               model = "within", effect = "twoways")


summary(did_lag)

#DID 滞后
did_lag2 <- plm(ln_mc ~ DID_lag + ESG_lag + Treat_lag + Post_lag + 
                  ln_size_lag + ln_shares_lag + ROE_lag + leverage_lag + 
                  tobinq_lag + CR_lag + factor(year), 
                data = data_long_clean, index = c("code", "year"), 
                model = "within", effect = "twoways")


summary(did_lag2)


library(dplyr)
library(ggplot2)


esg_avechange_data <- data_long_clean %>% filter(year >= 2014 & year <= 2018) %>%
  mutate( year = as.numeric(year),
          pollution_type = case_when(
            Treat == 1 ~ "重污染行业",
            Treat == 0 ~ "非重污染行业"
          )) %>% group_by(year, pollution_type) %>%
  summarise(
    ESG_ave = mean(ESG, na.rm = TRUE),
    .groups = "drop" )


ggplot(esg_avechange_data, aes(x = year, y = ESG_ave, color = pollution_type)) + 
  geom_line(size = 2) +
  geom_point(size = 3)+
  geom_vline(xintercept = 2016, linetype = "dashed")+
  labs(
    title = "2014–2018年ESG平均得分变化趋势",
    subtitle = "重污染行业 vs 非重污染行业",
    x ="年份",
    y ="ESG 平均得分",
    color ="行业类型"
    ) + scale_x_continuous(breaks = 2014:2018) + theme_minimal(base_size = 11)

