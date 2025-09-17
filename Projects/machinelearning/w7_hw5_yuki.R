# Machine Learning
# Week 7 - HW5
# May 17th, 2025

library(ggplot2)
library(dplyr)
library(tidyr)
library(splines)
setwd("/Applications/IMC463/w7_hw5")

freshmart = read.csv("FreshMart_sales_data_sample.csv")

summary(freshmart)

### - - - Part 1: Pricing Strategy Analysis - - - ###
## Question 1: OLS model to predict sales with product_price, competitor_price, tv_ad_spend, email_marketing,
##             social_media_spend, is_holiday

ols_p1q1 <- lm(sales ~ product_price + competitor_price + tv_ad_spend + email_marketing + social_media_spend + is_holiday,
            data = freshmart)
summary(ols_p1q1)
ols_coef <- coef(ols_p1q1)[c("product_price", "competitor_price")]
print(ols_coef)

##Question 2: Use cv.glmnet() to find the optimal λ that minimizes cross-validated MSE (use the default 10-fold CV; set.seed(12345))
library(MASS)
library(glmnet)

set.seed(12345)  # For reproducibility
preds <- as.matrix(freshmart[, c("product_price", "competitor_price", "tv_ad_spend", "email_marketing",
                                 "social_media_spend","is_holiday")])
outcome <- freshmart$sales

cv_fit <- cv.glmnet(preds, outcome, alpha = 0)  # 10-fold CV by default
best_lambda_ridge <- cv_fit$lambda.min
print(best_lambda_ridge)

##Question 3: Compare the Ridge coefficients to OLS coefficients for product_price and competitor_price
ridge_p1q3 <- lm.ridge(sales ~ product_price + competitor_price + tv_ad_spend +
                         email_marketing + social_media_spend + is_holiday,
                       data = freshmart, lambda = 27.332)
ridge_coef <- ridge_p1q3$coef[c("product_price", "competitor_price")]
print(ridge_coef)

ols_coef["competitor_price"] * 0.01
ridge_coef["competitor_price"] * 0.01

### - - - Part 2: Marketing Spend Optimization - - - ###
##Question 1: Predict sales using Lasso regression (cv.glmnet) with Tv_ad_spend, Social_media_spend
##            Email_marketing; Control variables: product_price, competitor_price, is_holiday
set.seed(12345)  # For reproducibility
preds_p2 <- as.matrix(freshmart[, c("product_price", "competitor_price", "is_holiday", "email_marketing",
                                 "social_media_spend","tv_ad_spend")])
outcome_p2 <- freshmart$sales

cv_fit_p2 <- cv.glmnet(preds_p2, outcome_p2, alpha = 1)  #alpha = 1 for Lasso
best_lambda_lasso <- cv_fit_p2$lambda.min
print(best_lambda_lasso)

##Question 2: Make recommendations on which channels to continue or cut funding
coef(cv_fit_p2, s = "lambda.min")

## Question 3:stepwise or backward selection on your full OLS model to select predictors that improve performance
fit.lm = lm(sales~1, freshmart)
fit.lm_stepwise = step(fit.lm, scope=~ product_price + competitor_price + tv_ad_spend +
                         email_marketing + social_media_spend + is_holiday, 
               direction = "both")
summary(fit.lm_stepwise)

### - - - Part 3: Modeling Seasonal Sales Patterns & TikTok Moments - - - ###
##Question 1:  Create a scatterplot of sales vs. day_of_year with LOESS 
##              (you can try span = 0.1, 0.3, 0.6) to visualize seasonal trends
ggplot(freshmart,aes(x = day_of_year, y = sales)) +
  geom_point(
    data = freshmart,  # Use original data
    aes(x = day_of_year, y = sales),
    alpha = 0.3,          # Semi-transparent
    color = "black"      # Light gray for background
  )+
  geom_smooth(method = "loess", span = 0.3, color = "red", se = FALSE)

##Question 2: Linear Model with B-Splines (Manual Knot Placement)
lm_bspline <- lm(sales ~ bs(day_of_year, knots = c(100, 150, 250)) + 
                   temperature + tv_ad_spend + email_marketing + is_holiday, 
                 data = freshmart)
summary(lm_bspline)

glm_bspline <- glm(sales ~ bs(day_of_year, knots = c(100, 150, 250)) + 
                     temperature + tv_ad_spend + email_marketing + is_holiday, 
                   family = gaussian, data = freshmart)
summary(glm_bspline)

dim(bs(freshmart$day_of_year, knots = c(100, 150, 250)))

##Question 2: 
library(mgcv)

gam_auto <- gam(
  sales ~ s(day_of_year) + temperature + tv_ad_spend + email_marketing + is_holiday,
  family = gaussian,
  data = freshmart,
  method = "REML"  # Preferred for smoothness selection
)
summary(gam_auto)

summary(gam_auto)$s.table

plot(gam_auto, residuals = TRUE,
     main = "Smooth Effect of Day of Year on Sales") 

install.packages("gratia")
library(gratia)  # for derivative and plotting tools

# Plot derivative of the smooth
deriv <- derivatives(gam_auto, term = "s(day_of_year)")

draw(deriv) + ggtitle("Rate of Change of Sales Over Day of Year")

#Question 3 
AIC(lm_bspline, glm_bspline, gam_auto)

plot(gam_auto, select = 1, shade = TRUE, seWithMean = TRUE,
     main = "Smooth Effect of Day of Year on Sales",
     xlab = "Day of Year", ylab = "Smooth Function")

# Get the values used in the plot
smooth_vals <- plot(gam_auto, select = 1, seWithMean = TRUE)

# Extract x and y values from the first plot object
x_vals <- smooth_vals[[1]]$x  # day_of_year
y_vals <- smooth_vals[[1]]$fit  # smooth effect on sales

# Day with highest smooth effect
peak_day <- x_vals[which.max(y_vals)]
peak_effect <- max(y_vals)

# Day with lowest smooth effect
low_day <- x_vals[which.min(y_vals)]
low_effect <- min(y_vals)

# View results
cat("📈 Highest smooth effect on sales: Day", round(peak_day), "with smooth =", round(peak_effect, 2), "\n")
cat("📉 Lowest smooth effect on sales: Day", round(low_day), "with smooth =", round(low_effect, 2), "\n")


# Get index order of sorted smooth effects
ordered_indices <- order(y_vals)

# Lowest two effects
low1_idx <- ordered_indices[1]
low2_idx <- ordered_indices[2]

# Highest two effects
high2_idx <- ordered_indices[length(y_vals) - 1]
high1_idx <- ordered_indices[length(y_vals)]

# Values
cat("📉 Lowest smooth effect on sales:\n")
cat("- Day", round(x_vals[low1_idx]), "with smooth =", round(y_vals[low1_idx], 2), "\n")
cat("- Day", round(x_vals[low2_idx]), "with smooth =", round(y_vals[low2_idx], 2), "\n\n")

cat("📈 Highest smooth effect on sales:\n")
cat("- Day", round(x_vals[high1_idx]), "with smooth =", round(y_vals[high1_idx], 2), "\n")
cat("- Day", round(x_vals[high2_idx]), "with smooth =", round(y_vals[high2_idx], 2), "\n")


#Question 4: 
freshmart$viral_campaign <- as.factor(freshmart$viral_campaign) 

gam_interaction <- gam(
  sales ~ s(day_of_year, by = viral_campaign) + viral_campaign +
    temperature + tv_ad_spend + email_marketing + is_holiday,
  family = gaussian,
  data = freshmart,
  method = "REML"  # Recommended for automatic smoothness selection
)
summary(gam_interaction)

summary(gam_interaction)$s.table

plot(gam_interaction, shade = TRUE, seWithMean = TRUE)

# Create prediction grid
new_data <- expand.grid(
  day_of_year = seq(min(freshmart$day_of_year), max(freshmart$day_of_year), length.out = 100),
  viral_campaign = factor(c(0, 1))  # ensure it's treated as a factor
)

# Add control variables (held constant)
new_data$temperature <- mean(freshmart$temperature, na.rm = TRUE)
new_data$tv_ad_spend <- mean(freshmart$tv_ad_spend, na.rm = TRUE)
new_data$email_marketing <- mean(freshmart$email_marketing, na.rm = TRUE)
new_data$is_holiday <- mean(freshmart$is_holiday, na.rm = TRUE) 

# Predict with SEs
pred <- predict(gam_auto, newdata = new_data, type = "response", se.fit = TRUE)
new_data$pred <- pred$fit
new_data$se <- pred$se.fit
new_data$upper <- new_data$pred + 1.96 * new_data$se
new_data$lower <- new_data$pred - 1.96 * new_data$se

# Plot
ggplot(new_data, aes(x = day_of_year, y = pred, color = viral_campaign)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = viral_campaign), alpha = 0.2, linetype = "dotted") +
  labs(
    title = "GAM Interaction: Day of Year × Viral Campaign",
    x = "Day of Year",
    y = "Predicted Sales",
    color = "Viral Campaign",
    fill = "Viral Campaign"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), legend.position = "top", text = element_text(size = 12))


# Split into two groups
vc_0 <- new_data %>% filter(viral_campaign == 0)
vc_1 <- new_data %>% filter(viral_campaign == 1)

# Calculate difference in predicted sales (boost)
sales_boost <- data.frame(
  day_of_year = vc_0$day_of_year,
  boost = vc_1$pred - vc_0$pred
)

# Find day(s) with maximum boost
max_boost_day <- sales_boost %>% filter(boost == max(boost))
print(max_boost_day)




