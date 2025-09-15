library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)

setwd("/Applications/IMC460/")  

summary.kmeans = function(fit) 
{
  p = ncol(fit$centers)
  K = nrow(fit$centers)
  n = sum(fit$size)
  xbar = t(fit$centers)%*%fit$size/n
  print(data.frame(
    n=c(fit$size, n),
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*(fit$size-1)), fit$tot.withinss/(p*(n-K)))), 4)
  ))
  cat("SSE=", fit$tot.withinss, "; SSB=", fit$betweenss, "; SST=", fit$totss, "\n")
  cat("R-Squared = ", fit$betweenss/fit$totss, "\n")
  cat("Pseudo F = ", (fit$betweenss/(K-1))/(fit$tot.withinss/(n-K)), "\n\n");
  invisible(list(Rsqr=fit$betweenss/fit$totss, 
                 F=(fit$betweenss/(K-1))/(fit$tot.withinss/(n-K))))
}

plot.kmeans = function(fit,boxplot=F)
{
  require(lattice)
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
  )
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){
                  panel.dotplot(...)
                  panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1),
                xlab="Cluster Mean"
  ))
  invisible(plotdat)
}

# Read and clean data
data <- read.csv("cable2.csv")
data <- drop_na(data)

# Summary statistics
summary(data)
sd(data$video)
sd(data$internet)
sd(data$phone)

# Boxplots to check distributions
boxplot(data$video, main = "Video Spending")
boxplot(data$internet, main = "Internet Spending")
boxplot(data$phone, main = "Phone Spending")

# Extract raw spending data for clustering
data_Raw <- data[, c("video", "internet", "phone")]

# Set seed for reproducibility
set.seed(2271)

# Run k-means clustering on raw data
fit6 = kmeans(data_Raw, 6, nstart=100)
fit7 = kmeans(data_Raw, 7, nstart=100)
fit8 = kmeans(data_Raw, 8, nstart=100)
fit9 = kmeans(data_Raw, 9, nstart=100)
fit10 = kmeans(data_Raw, 10, nstart=100)

# Display cluster summaries
summary(fit6)
plot(fit6)

summary(fit7)
plot(fit7)

summary(fit8)
plot(fit8)

summary(fit9)
plot(fit9)

summary(fit10)
plot(fit10)

# SSE & Pseudo F
# Try different values of K
k_values <- 4:10
sse_values <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  fit <- kmeans(spending_data, centers = k_values[i], nstart = 100)  # Use raw data
  sse_values[i] <- fit$tot.withinss  # SSE
}

# Create DataFrame
sse_df <- data.frame(K = k_values, SSE = sse_values)

# Plot SSE vs. Number of Clusters
ggplot(sse_df, aes(x = K, y = SSE)) +
  geom_line() + geom_point() +
  ggtitle("Elbow Method: SSE vs. Number of Clusters") +
  xlab("Number of Clusters (K)") +
  ylab("Total Within-Cluster Sum of Squares (SSE)") +
  theme_minimal()

sse_df


# Calculate Pseudo F-Statistic
pseudo_f_values <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  fit <- kmeans(spending_data, centers = k_values[i], nstart = 100)  # Use raw data
  pseudo_f_values[i] <- (fit$betweenss / (k_values[i] - 1)) / (fit$tot.withinss / (nrow(spending_data) - k_values[i]))
}

# Create DataFrame
pseudo_f_df <- data.frame(K = k_values, Pseudo_F = pseudo_f_values)

# Plot Pseudo F-Statistic
ggplot(pseudo_f_df, aes(x = K, y = Pseudo_F)) +
  geom_line() + geom_point() +
  ggtitle("Pseudo F-Statistic vs. Number of Clusters") +
  xlab("Number of Clusters (K)") +
  ylab("Pseudo F-Statistic") +
  theme_minimal()

pseudo_f_df

# question 4
head(data)

# k = 6 
# Summarize age by clusters (no change needed)
data$clus6 = factor(fit6$cluster, levels=1:6) 

tapply(data$age, fit6$cluster, mean) 

data %>%
  group_by(clus6) %>%
  summarize(n = n(), xbar = mean(age), sd = sd(age))

library(ggplot2)

ggplot(data, aes(x = age, fill = clus6)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "dodge", boundary = 0) +
  facet_wrap(~clus6, scales = "free") +  # Facet by cluster
  labs(title = "Age Distribution for Each Cluster (K=6)", x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since we are faceting by cluster

ggplot(data, aes(x = income, fill = clus6)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "dodge", boundary = 0) +
  facet_wrap(~clus6, scales = "free") +  
  labs(title = "Income Distribution for Each Cluster (K=6)", x = "Income", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = HHsize, fill = clus6)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "dodge", boundary = 0) +
  facet_wrap(~clus6, scales = "free") +  
  labs(title = "Household Size Distribution for Each Cluster (K=6)", x = "Household Size", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(married), fill = clus6)) +
  geom_bar(position = "dodge") +
  facet_wrap(~clus6) +  
  labs(title = "Marital Status Distribution for Each Cluster (K=6)", x = "Marital Status", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(children), fill = clus6)) +
  geom_bar(position = "dodge") +
  facet_wrap(~clus6) +  
  labs(title = "Children Distribution for Each Cluster (K=6)", x = "Has Children (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Summary for Age
age_summary <- data %>%
  group_by(clus6) %>%
  summarize(n = n(), xbar = mean(age, na.rm = TRUE), sd = sd(age, na.rm = TRUE))

# Summary for Income
income_summary <- data %>%
  group_by(clus6) %>%
  summarize(n = n(), xbar = mean(income, na.rm = TRUE), sd = sd(income, na.rm = TRUE))

# Summary for Household Size
HHsize_summary <- data %>%
  group_by(clus6) %>%
  summarize(n = n(), xbar = mean(HHsize, na.rm = TRUE), sd = sd(HHsize, na.rm = TRUE))

# Summary for Married (binary)
married_summary <- data %>%
  group_by(clus6) %>%
  summarize(n = n(), xbar = mean(married, na.rm = TRUE), sd = sd(married, na.rm = TRUE))

# Summary for Children (binary)
children_summary <- data %>%
  group_by(clus6) %>%
  summarize(n = n(), xbar = mean(children, na.rm = TRUE), sd = sd(children, na.rm = TRUE))

# Display the summary tables
age_summary
income_summary
HHsize_summary
married_summary
children_summary

# k = 7 
# Summarize age by clusters (no change needed)
data$clus7 = factor(fit7$cluster, levels=1:7) 

tapply(data$age, fit7$cluster, mean) 

data %>%
  group_by(clus7) %>%
  summarize(n = n(), xbar = mean(age), sd = sd(age))

library(ggplot2)

ggplot(data, aes(x = age, fill = clus7)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "dodge", boundary = 0) +
  facet_wrap(~clus7, scales = "free") +  # Facet by cluster
  labs(title = "Age Distribution for Each Cluster (K=7)", x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since we are faceting by cluster

ggplot(data, aes(x = income, fill = clus7)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "dodge", boundary = 0) +
  facet_wrap(~clus7, scales = "free") +  
  labs(title = "Income Distribution for Each Cluster (K=7)", x = "Income", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = HHsize, fill = clus7)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "dodge", boundary = 0) +
  facet_wrap(~clus7, scales = "free") +  
  labs(title = "Household Size Distribution for Each Cluster (K=7)", x = "Household Size", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(married), fill = clus7)) +
  geom_bar(position = "dodge") +
  facet_wrap(~clus7) +  
  labs(title = "Marital Status Distribution for Each Cluster (K=7)", x = "Marital Status", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(children), fill = clus7)) +
  geom_bar(position = "dodge") +
  facet_wrap(~clus7) +  
  labs(title = "Children Distribution for Each Cluster (K=7)", x = "Has Children (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Summary for Age
age_summary <- data %>%
  group_by(clus7) %>%
  summarize(n = n(), xbar = mean(age, na.rm = TRUE), sd = sd(age, na.rm = TRUE))

# Summary for Income
income_summary <- data %>%
  group_by(clus7) %>%
  summarize(n = n(), xbar = mean(income, na.rm = TRUE), sd = sd(income, na.rm = TRUE))

# Summary for Household Size
HHsize_summary <- data %>%
  group_by(clus7) %>%
  summarize(n = n(), xbar = mean(HHsize, na.rm = TRUE), sd = sd(HHsize, na.rm = TRUE))

# Summary for Married (binary)
married_summary <- data %>%
  group_by(clus7) %>%
  summarize(n = n(), xbar = mean(married, na.rm = TRUE), sd = sd(married, na.rm = TRUE))

# Summary for Children (binary)
children_summary <- data %>%
  group_by(clus7) %>%
  summarize(n = n(), xbar = mean(children, na.rm = TRUE), sd = sd(children, na.rm = TRUE))

# Display the summary tables
age_summary
income_summary
HHsize_summary
married_summary
children_summary

