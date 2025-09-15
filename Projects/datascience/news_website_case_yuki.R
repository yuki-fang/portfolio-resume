library(tidyverse)
library(ggplot2)
library(psych)
library(car)

setwd("/Applications/IMC460")  

## ----- question 1 ----- ##

# read table, create new vars 
np = read.table("np.csv", header=T, na.strings=".") %>%
  arrange(SubscriptionId, t) %>%
  group_by(SubscriptionId) %>%
  mutate(nextchurn = lead(churn),
         nextprice=lead(currprice),
         t = t)

summary(np)

# summarize new vars 
summary(np[, c("nextchurn", "nextprice")])

## ----- question 2 ----- ##

# a: regularity + intensity 
model1 <- glm(nextchurn ~ t + trial + nextprice + regularity + intensity, 
              data = np, family = binomial)
summary(model1)

cor_matrix <- cor(np[, c("t", "trial", "nextprice", "regularity", "intensity")], use = "complete.obs")
print(cor_matrix)

pairs.panels(np[, c("t", "trial", "nextprice", "regularity", "intensity")], 
             stars = TRUE,    # Display significance stars
             ellipses = FALSE) # Disable ellipses around scatterplots

vif(model1) #check multicollinearity 

# b: regularity only
model2 <- glm(nextchurn ~ t+trial+nextprice+regularity, data=np, family=binomial)
summary(model2)

pairs.panels(np[, c("t", "trial", "nextprice", "regularity")], 
             stars = TRUE,    # Display significance stars
             ellipses = FALSE) # Disable ellipses around scatterplots

vif(model2)

# c: intensity only 
model3 <- glm(nextchurn ~ t+trial+nextprice+intensity, data=np, family=binomial)
summary(model3) 

pairs.panels(np[, c("t", "trial", "nextprice", "intensity")], 
             stars = TRUE,    # Display significance stars
             ellipses = FALSE) # Disable ellipses around scatterplots

vif(model3)

## ----- question 3 ----- ##

# adding content variables without regularity 
head(np) #check all content vars 

model_content <- glm(nextchurn ~ t + trial + nextprice + sports1 + news1 + crime1 + life1 + obits1 + business1 + opinion1, 
                     data = np, family = binomial)
summary(model_content)

vif(model_content)

# with regularity
model_content_regularity <- glm(nextchurn ~ t + trial + nextprice + sports1 + news1 + crime1 + life1 + obits1 + business1 + opinion1 + regularity, 
                                data = np, family = binomial)
summary(model_content_regularity)

vif(model_content_regularity)

## ----- question 4 ----- ##

# with devices + regularity 
model_devices <- glm(nextchurn ~ t + trial + nextprice + regularity + mobile + tablet + desktop, 
                          data = np, family = binomial)
summary(model_devices)

vif(model_devices)

# with devices 
model_devices2 <- glm(nextchurn ~ t + trial + nextprice  + mobile + tablet + desktop, 
                     data = np, family = binomial)
summary(model_devices2)

vif(model_devices2)

## ----- question 5 ----- ##

# full model 
model_full <- glm(nextchurn ~ t+trial+nextprice+regularity+sports1+news1+crime1+life1+obits1+business1+opinion1+mobile+tablet+desktop, 
                     data = np, family = binomial)
summary(model_full)

model_full <- glm(nextchurn ~ t+trial+nextprice+sports1+news1+crime1+life1+obits1+business1+opinion1+mobile+tablet+desktop, 
                  data = np, family = binomial)
summary(model_full)

vif(model_full)

cor.test(np$desktop, np$news1, method = "pearson")

cor.test(np$desktop, np$sports1, method = "pearson")

cor.test(np$regularity, np$intensity, method = "pearson")

cor.test(np$regularity, np$desktop, method = "pearson")

cor.test(np$regularity, np$trial, method = "pearson")

cor.test(np$regularity, np$t, method = "pearson")

cor.test(np$nextchurn, np$regularity, method = "pearson")


model_2 <- lm(regularity ~ t+trial+nextprice+sports1+news1+crime1+life1+obits1+business1+opinion1, 
                  data = np)

summary(model_2)

model_2 <- lm(regularity ~ sports1+news1+crime1+life1+obits1+business1+opinion1, 
              data = np)

summary(model_2)

model_2 <- lm(regularity ~ mobile+tablet+desktop, 
              data = np)

summary(model_2)