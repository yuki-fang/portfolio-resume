# Marketing Models
# HW2
# May 23rd, 2025

library(dplyr)
library(MatchIt)
library(ggplot2)

setwd("/Applications/IMC462/HW2")  

kakao = read.csv("kakao_all.csv")

summary(kakao)
head(kakao)
str(kakao)

# Factorize vars 
kakao$tg = as.factor(kakao$tg)
kakao$age <- as.factor(kakao$age)
kakao$ii <- as.factor(kakao$ii)
kakao$income <- as.factor(kakao$income)
kakao$gender <- as.factor(kakao$gender)
kakao$education <- as.factor(kakao$education)
kakao$week <- as.factor(kakao$week)
kakao$panel_id <- as.factor(kakao$panel_id)

summary(kakao$log_t_non_kakao_game)
summary(kakao$t_non_kakao_game)

# Add log-transformed variables using log1p() safely
kakao$log_t_kakao_game        <- log1p(kakao$t_kakao_game)
kakao$log_t_non_kakao_game    <- log1p(kakao$t_non_kakao_game)
kakao$log_t_non_kakao         <- log1p(kakao$t_non_kakao)
kakao$log_t_kakao_talk        <- log1p(kakao$t_kakao_talk)
kakao$log_t_non_kakao_talk    <- log1p(kakao$t_non_kakao_talk)
kakao$log_t_kakao_story       <- log1p(kakao$t_kakao_story)
kakao$log_t_non_kakao_story   <- log1p(kakao$t_non_kakao_story)
kakao$log_t_anipang           <- log1p(kakao$t_anipang)

### Descriptive Stats: show why we log-transformed time vars 
library(tidyr)

kakao_post = subset(kakao, week == 2)

# Side by Side histogram
par(mfrow = c(1, 2))

# Original
hist(kakao$t_non_kakao,
     breaks = 50,
     col = "steelblue",
     main = "Original t_non_kakao",
     xlab = "Time (seconds)",
     ylab = "Frequency")

# Log-Transformed
hist(kakao$log_t_non_kakao,
     breaks = 50,
     col = "lightblue",
     main = "Log-Transformed log_t_non_kakao",
     xlab = "log1p(Time)",
     ylab = "Frequency")

#time usage of non kakao games: treatment vs. control
# Summarize average non-Kakao game time by week and treatment group
summary_plot_data <- kakao %>%
  group_by(week, tg) %>%
  summarise(
    mean_time = mean(log_t_non_kakao_game, na.rm = TRUE),
    se = sd(log_t_non_kakao_game, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot
ggplot(summary_plot_data, aes(x = week, y = mean_time, group = tg, color = tg, shape = tg)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_time - se, ymax = mean_time + se), width = 0.1) +
  scale_shape_manual(values = c("0" = 16, "1" = 17)) +  # 16 = circle (control), 17 = triangle (treatment)
  labs(
    title = "Avg. Non-Kakao Game Time (Log-Transformed), Treatment vs. Control",
    x = "Week",
    y = "Time Spent (seconds)",
    color = "Group",
    shape = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

ggplot(kakao, aes(x = log_t_non_kakao_game, fill = factor(week))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ tg, labeller = labeller(tg = c("0" = "Control", "1" = "Treatment"))) +
  labs(title = "Distribution of Non-Kakao Game Time (Log-Transformed) by Group & Week",
       x = "Time Spent (sec)", y = "Density", fill = "Week") +
  theme_minimal()

# number usage of non kakao games: treatment vs. control
# Summarize average non-Kakao game time by week and treatment group
summary_plot_data <- kakao %>%
  group_by(week, tg) %>%
  summarise(
    mean_number = mean(n_non_kakao_game, na.rm = TRUE),
    se = sd(n_non_kakao_game, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot
ggplot(summary_plot_data, aes(x = week, y = mean_number, group = tg, color = tg, shape = tg)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_number - se, ymax = mean_number + se), width = 0.1) +
  scale_shape_manual(values = c("0" = 16, "1" = 17)) +  # 16 = circle (control), 17 = triangle (treatment)
  labs(
    title = "Avg. Number of Non-Kakao Games Used, Treatment vs. Control",
    x = "Week",
    y = "Number of Games",
    color = "Group",
    shape = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

ggplot(kakao, aes(x = log_t_non_kakao_game, fill = factor(week))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ tg, labeller = labeller(tg = c("0" = "Control", "1" = "Treatment"))) +
  labs(title = "Distribution of Non-Kakao Game Time (Log-Transformed) by Group & Week",
       x = "Time Spent (sec)", y = "Density", fill = "Week") +
  theme_minimal()


kakao_pre = subset(kakao, week == 1)
str(kakao_pre)

### 1:1 Matching, Without Replacement, Without Caliper
NN1 = matchit(tg ~ age + income + education + gender + log_t_kakao_game + log_t_non_kakao_game +
                log_t_non_kakao + log_t_kakao_talk + log_t_non_kakao_talk + log_t_kakao_story +
                log_t_non_kakao_story + n_non_kakao + n_kakao_game + n_non_kakao_talk + 
                n_non_kakao_story + n_non_kakao_game, data = kakao_pre, method = "nearest", ratio = 1)
summary(NN1)

NN1_n = matchit(tg ~ age + income + education + gender + log_t_anipang + log_t_kakao_game + log_t_non_kakao_game +
                log_t_non_kakao + log_t_kakao_talk + log_t_non_kakao_talk + log_t_kakao_story +
                log_t_non_kakao_story + n_non_kakao + n_kakao_game + n_non_kakao_talk + 
                n_non_kakao_story + n_non_kakao_game, data = kakao_pre, method = "nearest", ratio = 1)
summary(NN1_n)

round(mean(NN1$distance),3) #average propensity score 

#install.packages("cobalt")
library(cobalt)
love.plot(NN1,
          stats = "mean.diffs",
          threshold = 0.1,
          var.order = "unadjusted",
          abs = FALSE,
          colors = c("grey", "blue"),
          shapes = c("circle", "triangle"),
          sample.names = c("Before Matching", "After Matching"),
          stars = "raw",
          line = TRUE,
          title = "Standardized Mean Differences Before and After Matching")

love.plot(NN1_n,
          stats = "mean.diffs",
          threshold = 0.1,
          var.order = "unadjusted",
          abs = FALSE,
          colors = c("grey", "blue"),
          shapes = c("circle", "triangle"),
          sample.names = c("Before Matching", "After Matching"),
          stars = "raw",
          line = TRUE,
          title = "Standardized Mean Differences Before and After Matching")


#Compare the Distributions of Propensity Scores between Treatment and Control Groups, between Raw and Matched Samples
plot(NN1, type = "hist") 

#make a Data Set from Matched Sample for Follow-Up Analyses
m.dat = get_matches(NN1)
str(m.dat)

id.dat = m.dat[c("panel_id", "weights")]

m.kakao = merge(kakao, id.dat, by = "panel_id")
str(m.kakao)

## DID Model
# dummy did -- time var 
did_modelNN1 <- lm(log_t_non_kakao_game ~ ii + panel_id + week + log_t_kakao_talk +
                     log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
                     log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
                     n_non_kakao_game + n_non_kakao, data = m.kakao, weights = weights)
summary(did_modelNN1)

coef(summary(did_modelNN1))["ii1", ]  # just show treatment effect


# dummy did -- num var 
did_modelNN1_n <- lm(n_non_kakao_game ~ ii + panel_id + week + log_t_kakao_talk +
             log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
             log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
             log_t_non_kakao_game + n_non_kakao, data = m.kakao, weights = weights)

summary(did_modelNN1_n)

coef(summary(did_modelNN1_n))["ii1", ]  # just show treatment effect

# FE estimation -- time var 
library(plm)
panel_fe1 <- pdata.frame(m.kakao, index = c("panel_id", "week"))

fe_model1 <- plm(
  log_t_non_kakao_game ~ ii + week
  + log_t_kakao_game   + n_kakao_game
  + log_t_non_kakao    + n_non_kakao
  + log_t_kakao_talk   + log_t_kakao_story
  + log_t_non_kakao_talk + n_non_kakao_talk
  + log_t_non_kakao_story + n_non_kakao_story,
  data   = panel_fe1,
  model  = "within",       
)
summary(fe_model1)

# FE estimation -- num var 
fe_model1n <- plm(
  n_non_kakao_game ~ ii + week
  + log_t_kakao_game   + n_kakao_game
  + log_t_non_kakao    + n_non_kakao
  + log_t_kakao_talk   + log_t_kakao_story
  + log_t_non_kakao_talk + n_non_kakao_talk
  + log_t_non_kakao_story + n_non_kakao_story,
  data   = panel_fe1,
  model  = "within",       
)
summary(fe_model1n)

### 1:1 Matching, Without Replacement, 0.05 Caliper 
NN2 = matchit(tg ~ age + income + education + gender + log_t_kakao_game + log_t_non_kakao_game +
                log_t_non_kakao + log_t_kakao_talk + log_t_non_kakao_talk + log_t_kakao_story +
                log_t_non_kakao_story + n_non_kakao + n_kakao_game + n_non_kakao_talk + 
                n_non_kakao_story + n_non_kakao_game, data = kakao_pre, method = "nearest", ratio = 1, caliper = 0.05)
summary(NN2)

round(mean(NN2$distance),3) #average propensity score 

#Compare the Distributions of Propensity Scores between Treatment and Control Groups, between Raw and Matched Samples
plot(NN2, type = "hist") 

love.plot(NN2,
          stats = "mean.diffs",
          threshold = 0.1,
          var.order = "unadjusted",
          abs = FALSE,
          colors = c("grey", "blue"),
          shapes = c("circle", "triangle"),
          sample.names = c("Before Matching", "After Matching"),
          stars = "raw",
          line = TRUE,
          title = "Standardized Mean Differences Before and After Matching")

#make a Data Set from Matched Sample for Follow-Up Analyses
m2.dat = get_matches(NN2)
str(m2.dat)

id2.dat = m2.dat[c("panel_id", "weights")]

m2.kakao = merge(kakao, id2.dat, by = "panel_id")
str(m2.kakao)

#DID Model
#time var 
did_modelNN2 <- lm(log_t_non_kakao_game ~ ii + panel_id + week + log_t_kakao_talk +
                     log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
                     log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
                     n_non_kakao_game + n_non_kakao, data = m2.kakao, weights = weights)
summary(did_modelNN2)

#number var 
did_modelNN2_n <- lm(n_non_kakao_game ~ ii + panel_id + week + log_t_kakao_talk +
             log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
             log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
             log_t_non_kakao_game + n_non_kakao, data = m2.kakao, weights = weights)
summary(did_modelNN2_n)

# FE estimation -- time var 
panel_fe2 <- pdata.frame(m2.kakao, index = c("panel_id", "week"))

fe_model2 <- plm(
  log_t_non_kakao_game ~ ii + week
  + log_t_kakao_game   + n_kakao_game
  + log_t_non_kakao    + n_non_kakao
  + log_t_kakao_talk   + log_t_kakao_story
  + log_t_non_kakao_talk + n_non_kakao_talk
  + log_t_non_kakao_story + n_non_kakao_story,
  data   = panel_fe2,
  model  = "within",       
)
summary(fe_model2)

# FE estimation -- num var 
fe_model2n <- plm(
  n_non_kakao_game ~ ii + week
  + log_t_kakao_game   + n_kakao_game
  + log_t_non_kakao    + n_non_kakao
  + log_t_kakao_talk   + log_t_kakao_story
  + log_t_non_kakao_talk + n_non_kakao_talk
  + log_t_non_kakao_story + n_non_kakao_story,
  data   = panel_fe2,
  model  = "within",       
)
summary(fe_model2n)

### 1:1 Matching, Without Replacement, Caliper = 0.1
NN3 = matchit(tg ~ age + income + education + gender + log_t_kakao_game + log_t_non_kakao_game +
                log_t_non_kakao + log_t_kakao_talk + log_t_non_kakao_talk + log_t_kakao_story +
                log_t_non_kakao_story + n_non_kakao + n_kakao_game + n_non_kakao_talk + 
                n_non_kakao_story + n_non_kakao_game, data = kakao_pre, method = "nearest", ratio = 1, caliper = 0.1)
summary(NN3)

round(mean(NN3$distance),3) #average propensity score 

#Compare the Distributions of Propensity Scores between Treatment and Control Groups, between Raw and Matched Samples
plot(NN3, type = "hist") 

love.plot(NN3,
          stats = "mean.diffs",
          threshold = 0.1,
          var.order = "unadjusted",
          abs = FALSE,
          colors = c("grey", "blue"),
          shapes = c("circle", "triangle"),
          sample.names = c("Before Matching", "After Matching"),
          stars = "raw",
          line = TRUE,
          title = "Standardized Mean Differences Before and After Matching")

#make a Data Set from Matched Sample for Follow-Up Analyses
m3.dat = get_matches(NN3)
str(m3.dat)

id3.dat = m3.dat[c("panel_id", "weights")]

m3.kakao = merge(kakao, id3.dat, by = "panel_id")
str(m3.kakao)

#DID Model
#time var 
did_modelNN3 <- lm(log_t_non_kakao_game ~ ii + panel_id + week + log_t_kakao_talk +
                     log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
                     log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
                     n_non_kakao_game + n_non_kakao, data = m3.kakao, weights = weights)
summary(did_modelNN3)

#number var
did_modelNN3_n <- lm(n_non_kakao_game ~ ii + panel_id + week + log_t_kakao_talk +
             log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
             log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
             log_t_non_kakao_game + n_non_kakao, data = m3.kakao, weights = weights)
summary(did_modelNN3_n)

# FE estimation -- time var 
panel_fe3 <- pdata.frame(m3.kakao, index = c("panel_id", "week"))

fe_model3 <- plm(
  log_t_non_kakao_game ~ ii + week
  + log_t_kakao_game   + n_kakao_game
  + log_t_non_kakao    + n_non_kakao
  + log_t_kakao_talk   + log_t_kakao_story
  + log_t_non_kakao_talk + n_non_kakao_talk
  + log_t_non_kakao_story + n_non_kakao_story,
  data   = panel_fe3,
  model  = "within",       
)
summary(fe_model3)

# FE estimation -- num var 
fe_model3n <- plm(
  n_non_kakao_game ~ ii + week
  + log_t_kakao_game   + n_kakao_game
  + log_t_non_kakao    + n_non_kakao
  + log_t_kakao_talk   + log_t_kakao_story
  + log_t_non_kakao_talk + n_non_kakao_talk
  + log_t_non_kakao_story + n_non_kakao_story,
  data   = panel_fe3,
  model  = "within",       
)
summary(fe_model3n)

### 1:1 Matching, Without Replacement, Caliper = 0.25
NN4 = matchit(tg ~ age + income + education + gender + log_t_kakao_game + log_t_non_kakao_game +
                log_t_non_kakao + log_t_kakao_talk + log_t_non_kakao_talk + log_t_kakao_story +
                log_t_non_kakao_story + n_non_kakao + n_kakao_game + n_non_kakao_talk + 
                n_non_kakao_story + n_non_kakao_game, data = kakao_pre, method = "nearest", ratio = 1, caliper = 0.25)
summary(NN4)

round(mean(NN4$distance),3) #average propensity score 


#Compare the Distributions of Propensity Scores between Treatment and Control Groups, between Raw and Matched Samples
plot(NN4, type = "hist") 

love.plot(NN4,
          stats = "mean.diffs",
          threshold = 0.1,
          var.order = "unadjusted",
          abs = FALSE,
          colors = c("grey", "blue"),
          shapes = c("circle", "triangle"),
          sample.names = c("Before Matching", "After Matching"),
          stars = "raw",
          line = TRUE,
          title = "Standardized Mean Differences Before and After Matching")

#make a Data Set from Matched Sample for Follow-Up Analyses
m4.dat = get_matches(NN4)
str(m4.dat)

id4.dat = m4.dat[c("panel_id", "weights")]

m4.kakao = merge(kakao, id4.dat, by = "panel_id")
str(m4.kakao)

#DID Model
#time var 
did_modelNN4 <- lm(log_t_non_kakao_game ~ ii + panel_id + week + log_t_kakao_talk +
                     log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
                     log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
                     n_non_kakao_game + n_non_kakao, data = m4.kakao, weights = weights)
summary(did_modelNN4)

#number var
did_modelNN4_n <- lm(n_non_kakao_game ~ ii + panel_id + week + log_t_kakao_talk +
             log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
             log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
             log_t_non_kakao_game + n_non_kakao, data = m4.kakao, weights = weights)
summary(did_modelNN4_n)

# FE estimation -- time var 
panel_fe4 <- pdata.frame(m4.kakao, index = c("panel_id", "week"))

fe_model4 <- plm(
  log_t_non_kakao_game ~ ii + week
  + log_t_kakao_game   + n_kakao_game
  + log_t_non_kakao    + n_non_kakao
  + log_t_kakao_talk   + log_t_kakao_story
  + log_t_non_kakao_talk + n_non_kakao_talk
  + log_t_non_kakao_story + n_non_kakao_story,
  data   = panel_fe4,
  model  = "within",       
)
summary(fe_model4)

# FE estimation -- num var 
fe_model4n <- plm(
  n_non_kakao_game ~ ii + week
  + log_t_kakao_game   + n_kakao_game
  + log_t_non_kakao    + n_non_kakao
  + log_t_kakao_talk   + log_t_kakao_story
  + log_t_non_kakao_talk + n_non_kakao_talk
  + log_t_non_kakao_story + n_non_kakao_story,
  data   = panel_fe4,
  model  = "within",       
)
summary(fe_model4n)

### 2:1 Matching, Without Replacement, Without Caliper
NN5 = matchit(tg ~ age + income + education + gender + log_t_kakao_game + log_t_non_kakao_game +
                log_t_non_kakao + log_t_kakao_talk + log_t_non_kakao_talk + log_t_kakao_story +
                log_t_non_kakao_story + n_non_kakao + n_kakao_game + n_non_kakao_talk + 
                n_non_kakao_story + n_non_kakao_game, data = kakao_pre, method = "nearest", ratio = 2, replace = FALSE)
summary(NN5)

#Compare the Distributions of Propensity Scores between Treatment and Control Groups, between Raw and Matched Samples
plot(NN5, type = "hist") 

love.plot(NN5,
          stats = "mean.diffs",
          threshold = 0.1,
          var.order = "unadjusted",
          abs = FALSE,
          colors = c("grey", "blue"),
          shapes = c("circle", "triangle"),
          sample.names = c("Before Matching", "After Matching"),
          stars = "raw",
          line = TRUE,
          title = "Standardized Mean Differences Before and After Matching")

#make a Data Set from Matched Sample for Follow-Up Analyses
m5.dat = get_matches(NN5)
str(m5.dat)

id5.dat = m5.dat[c("panel_id", "weights")]

m5.kakao = merge(kakao, id5.dat, by = "panel_id")
str(m5.kakao)

#Estimate the Treatment Effect
summary(lm(log_t_non_kakao_game ~ tg + age + income + education + gender +
             log_t_kakao_story + log_t_kakao_game + log_t_non_kakao_talk + log_t_non_kakao_story +
             log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + 
             n_non_kakao_game + n_non_kakao, data = m5.kakao, weights = weights))

#DID Model
#time var 
did_modelNN5 <- lm(log_t_non_kakao_game ~ ii + age + income + education + gender + log_t_kakao_talk +
                     log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
                     log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
                     n_non_kakao_game + n_non_kakao, data = m5.kakao, weights = weights)
summary(did_modelNN5)

#number var
summary(lm(n_non_kakao_game ~ ii + age + income + education + gender + log_t_kakao_talk +
             log_t_kakao_story + log_t_non_kakao_story + log_t_kakao_game + log_t_non_kakao_talk +
             log_t_non_kakao + n_non_kakao_talk + n_non_kakao_story + n_kakao_game +
             log_t_non_kakao_game + n_non_kakao, data = m5.kakao, weights = weights))
