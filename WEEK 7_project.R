install.packages("lme4")
library(lme4)

#LOAD DATASET
data(sleepstudy)

#EXPLORE DATASET

#to check the top and last 5 rows of our dataset 
head(sleepstudy)
tail(sleepstudy)

#displays the structure of an object
str(sleepstudy)

# summary of the dataset
summary(sleepstudy)

#Dimension of my dataset
dim(sleepstudy)

#checking for missing value
which(is.na(sleepstudy$frequency))


#VISUALIZATION
#Visualize the data using appropriate plots to understand the 
#distribution and relationships

install.packages("ggplot2")
library(ggplot2)

#To see the distribution of reaction times 
ggplot(sleepstudy, aes(x = Reaction)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  ggtitle("Distribution of Reaction Times") +
  xlab("Reaction Time (ms)") +
  ylab("Frequency")


#To see how reaction times vary across different days
ggplot(sleepstudy, aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Reaction Time by Days of Sleep Deprivation") +
  xlab("Days of Sleep Deprivation") +
  ylab("Reaction Time (ms)")

#Line Plot for Reaction Time Trends
#To observe trends in 
#reaction time over the days for each subject
ggplot(sleepstudy, aes(x = Days, y = Reaction, group = Subject, color = Subject)) +
geom_line() +
  ggtitle("Reaction Time Trends Over Days per Subject") +
  xlab("Days of Sleep Deprivation") +
  ylab("Reaction Time (ms)")

#Scatter Plot with a Smoothed Line
#To visualize the overall trend between days 
#of sleep deprivation and reaction time
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Overall Trend of Reaction Time Over Days") +
  xlab("Days of Sleep Deprivation") +
  ylab("Reaction Time (ms)")

#Faceted Plot by Subject
#To compare reaction time trends for each subject separately
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_line(aes(color = Subject)) +
  facet_wrap(~ Subject) +
  ggtitle("Reaction Time Trends by Subject") +
  xlab("Days of Sleep Deprivation") +
  ylab("Reaction Time (ms)")


#MODEL
#According to the experiment , the baseline line for sleep 
#was measured on the third day of the experiment
#so I began the analysis on the third day and named the column , "no_sleep_days"

library(tidyverse)
library(dplyr)
data <- sleepstudy %>%
  filter(Days >= 2) %>%
  mutate(no_sleep_days = Days - 2)

head(data)



#Model 1
#here , we have different intercepts for the subjects 
model_1 <- lmer(Reaction~ no_sleep_days +(1|Subject), data = data)

summary(model_1)

#model 2
model_2 <- lmer(Reaction~ no_sleep_days + (no_sleep_days | Subject), data = data)
summary(model_2)



#Residual analysis for model 1
#the residuals from the model. 
#Residuals are the differences between the observed values and the values
#predicted by the model.
residuals <- residuals(model_1)  # 
residuals

#Plotting Residuals vs. Fitted Values
#This is to check for homoscedasticity (constant variance of residuals)
plot(fitted(model_1), residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


#histogram of residuals
hist(residuals, breaks = 30, main = "Histogram of Residuals", 
     xlab = "Residuals", col = "lightblue")

# q-q plot of residuals
qqnorm(residuals)
qqline(residuals, col = "red")

#residual analysis for the second model
res <- residuals(model_2)
par(mfrow =c(1,3))

# Plot 1 histogram

hist(res)

# Plot 2 Q-Q Plot
qqnorm(res)
qqline(res)

#Plot 3  Residual plot

plot(fitted(model_2), res)

