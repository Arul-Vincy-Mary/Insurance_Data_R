data = read.csv("insurance.csv",header=T)
#Predicting insurance premiums using linear regression - problem statement
str(data)
dim(data)
data_copy = data

#missing values
sum(is.na(charges))

#summary
summary(data)

#correlation between the variables
cor(data?c("age", "bmi", "children", "charges")])

#MODEL BUILDING
#1st model
m1 <- lm(formula = charges ~., data = data)
m1
summary(m1)
#m1 is better model 
#updated model 1
#transforming BMI to categorical value
data$bmi30 <- ifelse(data$bmi > 30, 1, 0)
#remodel ?egression
m1_updated <- lm(charges ~ age + children + bmi + sex + bmi30 + bmi30*smoker + region,
                              data = data)
summary(m1_updated)
#To find the correlation between the new and existing variables
library(dplyr)
install.packages(?GGally")
library(ggplot2)
data %>% mutate(Smoking = ifelse(smoker=="yes", 1, 0)) %>%
  select(age,bmi,bmi30,children,Smoking,charges) %>%
  ggcorr(label=TRUE, name="Correlation")
#standardised residual plot
plot(m1_updated$fitted.values,rstandard(m1_update?))




