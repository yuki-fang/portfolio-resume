# Marketing Models
# Week 4 - HW1
# April 20th, 2025

library(dplyr)

setwd("/Applications/IMC462/HW1")  

ad = read.csv("pre-roll_ad.csv") %>% 
  mutate(
    gender_new     = factor(gender_new, levels = c("Female","Male")),
    ad_brand_cat   = factor(ad_brand_cat),          
    media_platform = factor(media_platform,
                            levels = c("MOBILE APP","MOBILE WEB","PC WEB")),
    age_new        = factor(age_new, levels = c(10,20,30,40,50,60),
                            labels = c("10s","20s","30s","40s","50s","60s+"))
  )

summary(ad)
head(ad)
str(ad) 

##### Linear Probability Model
fit_LPM = lm(ad_complete ~ gender_new*ad_brand_cat + media_platform + tt + day + age_new + genre_new + clip_duration, data=ad)
summary(fit_LPM)

## ANOVA for testing gender*ad brand interaction term 
LPM_no_interact = lm(ad_complete ~ gender_new + ad_brand_cat + media_platform + tt + day + age_new + genre_new + clip_duration, data = ad)

anova(LPM_no_interact, fit_LPM)

## DROP1
drop1(fit_LPM, test = "F")


##### Logit Regression Model

##Estimation 
fit_logit = glm(ad_complete ~ gender_new*ad_brand_cat + media_platform + tt + day + age_new + genre_new + clip_duration, 
                 family = binomial (link = logit), data=ad)
summary(fit_logit)

## Log-likelihood value of the full model
logLik(fit_logit)

## Likelihood ratio test (LRT) for overall significance of the full model
library(lmtest)
lrtest(fit_logit)

## LRT for testing gender*ad brand interaction term 
logit_no_interact = glm(ad_complete ~ gender_new + ad_brand_cat + media_platform + tt + day + age_new + genre_new + clip_duration, 
                        family = binomial (link = logit), data = ad)

lrtest(logit_no_interact, fit_logit)

## LRT for testing gender main IV
logit_no_gender = glm(ad_complete ~ ad_brand_cat + media_platform + tt + day + age_new + genre_new + clip_duration,
                   family = binomial(link = "logit"), data = ad)

lrtest(logit_no_gender, fit_logit)

## LRT for testing ad brand category main IV
logit_no_adbrand = glm(ad_complete ~ gender_new + media_platform + tt + day + age_new + genre_new + clip_duration,
                      family = binomial(link = "logit"), data = ad)

lrtest(logit_no_adbrand, fit_logit)

## Wald test for gender 
exp(-0.567890)
(exp(-0.567890)-1)*100


##### Probit Regression Model

##Estimation 
fit_probit = glm(ad_complete ~ gender_new*ad_brand_cat + media_platform + tt + day + age_new + genre_new + clip_duration, 
                 family = binomial (link = probit), data=ad)
summary(fit_probit)

## Log-likelihood value of the full model
logLik(fit_probit)

## Likelihood ratio test (LRT) for overall significance of the full model
lrtest(fit_probit)

## LRT for testing gender*ad brand interaction term 
probit_no_interact = glm(ad_complete ~ gender_new + ad_brand_cat + media_platform + tt + day + age_new + genre_new + clip_duration, 
                         family = binomial (link = probit), data = ad)

lrtest(probit_no_interact, fit_probit)

## LRT for testing gender main IV
probit_no_gender = glm(ad_complete ~ ad_brand_cat + media_platform + tt + day + age_new + genre_new + clip_duration,
                      family = binomial(link = "probit"), data = ad)

lrtest(probit_no_gender, fit_probit)

## LRT for testing ad brand category main IV
probit_no_adbrand = glm(ad_complete ~ gender_new + media_platform + tt + day + age_new + genre_new + clip_duration,
                       family = binomial(link = "probit"), data = ad)

lrtest(probit_no_adbrand, fit_probit)

##### Interval Regression Model
library(survival)
ad$cens = 3

##Estimation 
irm = survreg(Surv(l_ad_stop,u_ad_stop, cens, type="interval") ~ gender_new*ad_brand_cat + media_platform + tt + day + clip_duration + age_new + genre_new, 
              data=ad, dist="gaussian")
summary(irm)

## Likelihood ratio test (LRT) for overall significance of the full model
lrtest(irm)

str(ad)


## Predicted Ad Completion Plot
library(ggplot2)
library(dplyr)
library(Rmisc)

plot_data <- summarySE(
  data = ad,
  measurevar = "predicted",
  groupvars = c("gender_new", "ad_brand_cat")
)

ggplot(plot_data, aes(x = ad_brand_cat, y = predicted, fill = gender_new)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = predicted - ci, ymax = predicted + ci),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    title = "Predicted Ad Completion by Gender and Ad Brand Category",
    x = "Ad Brand Category",
    y = "Predicted Probability",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
str(ad)

ad$avg_watch_time <- (ad$l_ad_stop + ad$u_ad_stop) / 2
summary_data <- summarySE(ad, measurevar = "avg_watch_time", 
                          groupvars = c("ad_brand_cat", "gender_new"))
# Create the heatmap
ggplot(summary_data, aes(x = ad_brand_cat, y = gender_new, fill = avg_watch_time)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the heatmap
  theme_minimal() +
  labs(title = "Heatmap of Average Watch Time by Ad Brand Category and Gender",
       x = "Ad Brand Category",
       y = "Gender",
       fill = "Average Watch Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
