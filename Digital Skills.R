################################################################################################################################################################################################
# 1.load the library
library(dplyr)
library(ggplot2)
library(broom)
library(skimr)
library(interactions)
library(tidyverse)
library(haven)
library(scales)
library(car)
library(rpart)
library(rpart.plot)
library(glmnet)
library(randomForest)
library(MASS)
library(Metrics)
library(stargazer)
################################################################################################################################################################################################
# 2. Import data.
###
setwd("C:/Users/wei/OneDrive - Bournemouth University/BI Individual assignment/Participation Survey 202223")
data <- haven::read_dta("participation_2022_23_annual_data_open.dta")
################################################################################################################################################################################################
# Outcome variable : Pos. = 918	Variable = CDTRANBC. How important do you think it is for you to continue to develop digital or online skills in your current career or job
print(data$CDTRANBC)
data %>% count(SSDIGSPEED)

#
data_mutate <- data %>%
  mutate(
    CDTRANBC_new = case_when(
      CDTRANBC == 1 ~ 4,
      CDTRANBC == 2 ~ 3,
      CDTRANBC == 3 ~ 2,
      CDTRANBC == 4 ~ 1,
      CDTRANBC < 0 ~ NA,
      CDTRANBC > 4 ~ NA
    ),
    
    AGESHORT = case_when(
      AGESHORT %in% c(4,5) ~ 4,
      AGESHORT < 0 ~ NA,
      AGESHORT > 4 ~ NA,
      TRUE ~ AGESHORT
    ),
    
    GENDER = case_when(
      GENDER < 0 ~ NA,
      GENDER > 2 ~ NA,
      TRUE ~ GENDER
    ),
    
    
    QWork = case_when(
      QWork %in% c(4,5,6,8,9,10) ~ 1,
      QWork == 7 ~ 2,
      QWork == 2 ~ 3,
      QWork == 1 ~ 4,
      QWork < 0 ~ NA,
      QWork >10 ~ NA
    ),
      
    EDUCAT4 = case_when(
      EDUCAT4 %in% c(5,6,7) ~ 1,
      EDUCAT4 %in% c(3,4) ~ 2,
      EDUCAT4 == 2 ~ 3,
      EDUCAT4 == 1 ~ 4,
      EDUCAT4 < 0 ~ NA
    ),
    
    FINHARD = case_when(
      FINHARD == 5 ~ 1,
      FINHARD == 4 ~ 2,
      FINHARD %in% c(2,3) ~ 3,
      FINHARD == 1 ~ 4,
      FINHARD < 0 ~ NA,
      FINHARD >5 ~ NA
    ),
    
    CINTOFT = case_when(
      CINTOFT == 5 ~ 1,
      CINTOFT %in% c(3,4) ~ 2,
      CINTOFT == 2 ~ 3,
      CINTOFT == 1 ~ 4,
      CINTOFT > 5 ~ NA,
      CINTOFT < 0 ~ NA
    ),
      
    CDIGTRA12 = case_when(
      CDIGTRA12 == 2 ~ 1,
      CDIGTRA12 == 1 ~ 2,
      CDIGTRA12 < 0 ~ NA
    ),
    
    OCCUPATION = case_when(
      OCCUPATION %in% c(5,6) ~ 1,
      OCCUPATION %in% c(1,4) ~ 2,
      OCCUPATION %in% c(2,7) ~ 3,
      OCCUPATION %in% c(3,8) ~ 4,
      OCCUPATION < 0 ~ NA
    ),
    
    EmpNo = case_when(
      EmpNo  < 0 ~ NA,
      TRUE ~ EmpNo
    ),
    
    SVise = case_when(
      SVise == 2 ~ 1,
      SVise == 1 ~ 2,
      SVise < 0 ~ NA
    ),
    
    SSDIGSPEED = case_when(
      SSDIGSPEED == 999.0 ~ NA,     
      SSDIGSPEED < 0 ~ NA,
      SSDIGSPEED %in% c(1, 2, 3) ~ 1,
      SSDIGSPEED == 4 ~ 2,
      SSDIGSPEED == 5 ~ 3,
      SSDIGSPEED == 6 ~ 4
    )
    
  ) %>% 
  
 mutate(
    CDTRANBC_new_factor = factor(CDTRANBC_new, ordered = TRUE),
    AGESHORT_factor = factor(AGESHORT,
                             levels = c(1, 2, 3, 4), 
                             labels = c("16-24", "25-44", "45-64", "65+"),
                             ordered = TRUE),
    GENDER_factor = factor(GENDER),
    QWork_factor = factor(QWork,
                          levels = c(1, 2, 3, 4), 
                          labels = c("Not working", "Full time education", "Part time working", "Full time working"),
                          ordered = TRUE),
    EDUCAT4_factor = factor(EDUCAT4,
                            levels = c(1, 2, 3, 4), 
                            labels = c("Under RFQ1", "GCSE-A level", "Undergraduate", "Higher degree"),
                            ordered = TRUE),
    FINHARD_factor = factor(FINHARD, 
                            levels = c(1, 2, 3, 4), 
                            labels = c("Very difficult", "Quite difficult", "Doing ok", "Living comfortble"),
                            ordered = TRUE),
    CINTOFT_factor = factor(CINTOFT,
                            levels = c(1, 2, 3, 4), 
                            labels = c("Less often", "Few times a week", "Few times a day", "All the time"),
                            ordered = TRUE),
    CDIGTRA12_factor = factor(CDIGTRA12),
    EmpNo_factor = factor(EmpNo,
                          levels = c(1, 2, 3, 4), 
                          labels = c("1-24", "25-249", "250-499", "500+"),
                          ordered = TRUE),
    OCCUPATION_factor = factor(OCCUPATION,
                               levels = c(1, 2, 3, 4), 
                               labels = c("Low skills", "Medium digital skills", "Medium high digital skills", "High igital skills"), ordered = TRUE),
    
    SVise_factor = factor(SVise),
    SSDIGSPEED_factor = factor(SSDIGSPEED,
                               levels = c(1, 2, 3, 4), 
                               labels = c("1-24Mbps", "24-100Mbps", "100-1000Mbps", "1000+Mbps"),
                               ordered = TRUE)
  )


selected_data <- data_mutate %>% 
  dplyr::select(
    # Key variables
    CDTRANBC_new,
    
    # Demographic characteristics
    AGESHORT, GENDER, QWork, EDUCAT4, FINHARD,
    
    # Digital skills related
    CINTOFT, CDIGTRA12,
    
    # occupation-related
    OCCUPATION,EmpNo,SVise,SSDIGSPEED
  )


data_mutate %>% count(SSDIGSPEED)

data_mutate %>% count(CDTRANBC_new)

summary(data_mutate[c("CDTRANBC_new_factor", "AGESHORT_factor")])


model_correct2 <- polr(CDTRANBC_new_factor~ AGESHORT_factor + QWork_factor, 
                      data = data_mutate, 
                      Hess = TRUE)

summary(model_correct2)






ggplot(data_mutate, aes(x = factor(AGESHORT))) +
  geom_bar(fill = "lightblue", color = "black")


model_correct <- polr(CDTRANBC_new_factor~ AGESHORT_factor, 
                      data = data_mutate, 
                      Hess = TRUE)

summary(model_correct)



plot_data <- data_mutate %>%
  filter(
    !is.na(CDTRANBC_new_factor) &  
      !is.na(AGESHORT_factor)     
  )

ggplot(plot_data, aes(x = AGESHORT_factor, fill = CDTRANBC_new_factor)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "RdYlGn") + 
  labs(
    title = "年龄组与电子技能重要性认知",
    subtitle = "基于 20,816 个完整回复",
    x = "年龄组 (AGESHORT_factor)",
    y = "回复百分比",
    fill = "重要性认知 \n(CDTRANBC_new_factor)" # \n 是换行
  ) +
  theme_minimal()

######
data_summary_observed <- plot_data %>%
  group_by(AGESHORT_factor, CDTRANBC_new_factor) %>%
  summarise(count = n()) %>%
  group_by(AGESHORT_factor) %>%
  mutate(
    total_in_group = sum(count),
    proportion = count / total_in_group
  ) %>%
  ungroup() # 完成计算，取消分组

# (可选) 给你的认知等级添加更清晰的标签，用于图例

data_summary_observed$CDTRANBC_new_factor <- factor(data_summary_observed$CDTRANBC_new_factor, 
                                                    levels = c(1, 2, 3, 4), 
                                                    labels = c("1 (最低)", "2 (中低)", "3 (中高)", "4 (最高)"))



ggplot(data_summary_observed, aes(x = AGESHORT_factor, y = proportion, color = CDTRANBC_new_factor, group = CDTRANBC_new_factor)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "真实数据：年龄组与电子技能重要性认知",
    subtitle = "基于 20,816 个完整回复的观测百分比",
    x = "年龄组",
    y = "回复百分比",
    color = "重要性认知等级" # 图例标题
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()


#####

model_1 <- polr(CDTRANBC_new_factor~ QWork_factor, 
                      data = data_mutate, 
                      Hess = TRUE)

summary(model_1)


data_summary_observed2 <- data_mutate %>%
  filter(
    !is.na(CDTRANBC_new_factor) &  
      !is.na(QWork_factor)           
  ) %>%
  group_by(QWork_factor, CDTRANBC_new_factor) %>%
  summarise(count = n(), .groups = 'drop') %>% # .groups = 'drop' 是好习惯
  group_by(QWork_factor) %>%
  mutate(
    total_in_group = sum(count),
    proportion = count / total_in_group
  ) %>%
  ungroup()


data_summary_observed2$CDTRANBC_new_factor <- factor(data_summary_observed2$CDTRANBC_new_factor, 
                                                    levels = c("1", "2", "3", "4"), 
                                                    labels = c("1 (最低)", "2 (中低)", "3 (中高)", "4 (最高)"))



ggplot(data_summary_observed2, aes(x = QWork_factor, y = proportion, color = CDTRANBC_new_factor, group = CDTRANBC_new_factor)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "真实数据：工作状态与电子技能重要性认知",
    subtitle = "基于模型使用的完整回复",
    x = "工作状态 (QWork_factor)",
    y = "回复百分比",
    color = "重要性认知等级" # 图例标题
  ) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()




summary(data_mutate[c("CDTRANBC_new_factor", "AGESHORT_factor")])
# 3. Clean and tidy the data

# Select import variables

selected_data <- data %>% 
  dplyr::select(
    # Key variables
    CDTRANBC,
    
    # Demographic characteristics
    AGESHORT, GENDER, QWork, EDUCAT4, FINHARD,
    
    # Digital skills related
    CINTOFT, CDIGTRA12, CSMARTU_001,CSMARTU_002,CSMARTU_003,CSMARTU_004,CSMARTU_005,CSMARTU_006,
    
    # occupation-related
    OCCUPATION,EmpNo,SVise
  )


# Screening effective samples
# Filter valid values


print(data$CDTRANBC)
