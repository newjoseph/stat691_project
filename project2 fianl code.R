library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(lme4)
library(car)
library(emmeans)


a <- read_excel('/Users/jasperstone/Desktop/Joseph/2025 Spring/STAT692/Project2/Project2_S25_Data.xlsx')

comments <- a[, 12]
a <- a[, c(1:11)]

a <- a %>%
  mutate(
    Vaccine_Group = as.factor(Vaccine_Group),
    Infestation = as.factor(Infestation),
    Deer_ID = as.factor(Deer_ID),  # Random effect
    Mortality = as.factor(Mortality)  # Ensure binary format
  )

# full model with interaction
model <- glmer(Mortality ~ Vaccine_Group * Infestation + (1 | Deer_ID),
               data = a, family = binomial)
summary(model)

# full model with addictive terms
model_fixed <- glmer(Mortality ~ Vaccine_Group + Infestation + (1 | Deer_ID),
                     data = a, family = binomial)
summary(model_fixed)

# tables
table(a$Vaccine_Group, a$Infestation, a$Mortality)

# VIF value checking
vif(model)
vif(model_fixed)

# remove the values to reduce 0 values

a_filtered <- a[a$Vaccine_Group != "P", ]  # Remove P group
a_filtered <- a_filtered[a_filtered$Infestation != "PRE", ]  # Remove PRE group

a_filtered$Pre_vaccine <- as.factor(ifelse(a_filtered$Vaccine_Group == "C", "Control", "Treatment"))
a_filtered$Infestation <- factor(a_filtered$Infestation, levels = c("THIRD", "SECOND", "FIRST"))  # Set desired order
a_filtered$Pre_vaccine <- factor(a_filtered$Pre_vaccine, levels = c("Treatment", "Control"))
a_filtered$Vaccine_Group <- factor(a_filtered$Vaccine_Group, levels = c("H", "L", "C"))
a_filtered$Vaccine_Group %>% unique


# filtered model with interaction 
model_filtered <- glmer(Mortality ~ Pre_vaccine * Infestation + (1 | Deer_ID),
                        data = a_filtered, family = binomial)
summary(model_filtered)

# filtered model with addictive terms 
model_filtered_fix <- glmer(Mortality ~ Pre_vaccine + Infestation + (1 | Deer_ID),
                            data = a_filtered, family = binomial)
summary(model_filtered_fix)

# Checking VIF
vif(model_filtered)
vif(model_filtered_fix)






ggplot(a_filtered, aes(x = Infestation, y = as.numeric(Mortality), color = Pre_vaccine)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.2)) +
  stat_summary(fun = mean, geom = "line", aes(group = Pre_vaccine)) +
  labs(title = "Mortality Rate by Infestation Time & Vaccine Group", y = "Mean Mortality Rate")

emmeans(model_filtered, pairwise ~ Pre_vaccine | Infestation, type = "response")
emmeans(model_filtered, pairwise ~Infestation | Pre_vaccine , type = "response")




