# PLEASE download the dataset to desktop
setwd("~/Desktop")

# Install necessary packages
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("leaps")
library(leaps)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidymodels)

# Load in the raw data
raw_data<- read.csv("Sleep_Efficiency.csv", header = TRUE, sep = ",")
head(raw_data)

# Tidy the data to what we need
tidy<- select(raw_data, Age,Gender,Sleep.efficiency,Sleep.duration,
              REM.sleep.percentage,Deep.sleep.percentage,Awakenings,
              Caffeine.consumption,Alcohol.consumption,Smoking.status,
              Exercise.frequency)
tidy$Gender<- as.factor(tidy$Gender)
tidy$Smoking.status<- as.factor(tidy$Smoking.status)
tidy <- na.omit(tidy)

# Construct a full model that contains every variable as explanatory terms
full= lm(Sleep.efficiency~Age+Gender+Sleep.duration+REM.sleep.percentage+
           Deep.sleep.percentage+Awakenings+Caffeine.consumption+
           Alcohol.consumption+Smoking.status+Exercise.frequency,data = tidy)
summary(full)

# Visualize a side by side boxplot between Male and Female
gender_boxplot <- boxplot(Sleep.efficiency ~ Gender, data = tidy,
                          main = "Sleep efficiency by Gender", xlab= "Gender", 
                          ylab = "Sleep Efficiency", cex.lab=1.5, cex.axis=1.5,
                          cex.main=1.5)



# Visualize a side by side boxplot between smoking person and no-smoking 
smoking_boxplot <- boxplot(Sleep.efficiency ~ Smoking.status, 
                           data = tidy, main = "Sleep efficiency by Smoking Status", 
                           xlab= "Smoking Status", ylab = "Sleep Efficiency", 
                           cex.lab=1.5, cex.axis=1.5, cex.main=1.5)



# Plot scatterplot for Sleep Efficiency and Sleep duration
plot(Sleep.efficiency ~ Sleep.duration, data = tidy,
     main =sprintf("Sleep Efficiency vs. %s", "Sleep duration"), 
     xlab=sprintf("%s", "Sleep Duration"), ylab="Sleep Efficiency")
# Compute correlation between sleep efficiency and sleep duration
cor(tidy$Sleep.efficiency, tidy$Sleep.duration)   

# Remove Sleep.duration from the linear model call it fit1
fit1= lm(Sleep.efficiency~Age+Gender+REM.sleep.percentage+
           Deep.sleep.percentage+Awakenings+Caffeine.consumption+
           Alcohol.consumption+Smoking.status+Exercise.frequency,data = tidy)
summary(fit1)


# Plot scatterplot for Sleep Efficiency and Deep sleep percentage grouped and colored by Gender
tidy%>% ggplot(aes(x=Deep.sleep.percentage, y = Sleep.efficiency, group=Gender, col = Gender)) + 
  geom_point()
# Remove Gender from our linear model and name it fit2
fit2= lm(Sleep.efficiency~Age+REM.sleep.percentage+
           Deep.sleep.percentage+Awakenings+Caffeine.consumption+
           Alcohol.consumption+Smoking.status+Exercise.frequency,data = tidy)
summary(fit2)

# Plot residual plot for fit2 model
res = resid(fit2)
plot(fitted(fit2), res)
abline(0,0,col = "red",lwd=2)

# Create correlation matrix of all the numerical variables
sleep_no_categorical <- tidy[ -c(2,10)]
cor_matrix <- cor(sleep_no_categorical)
cor_matrix <- round(cor_matrix, 2)

# Forward selection
s=regsubsets(Sleep.efficiency~Age+Gender+Sleep.duration+REM.sleep.percentage+
               Deep.sleep.percentage+Awakenings+Caffeine.consumption+
               Alcohol.consumption+Smoking.status+Exercise.frequency,
             data = tidy,method = "exhaustive")
ss=summary(s)
ss$which

# Create training and test datasets
train_indices <- sample.int(nrow(tidy), size=310)
train <- tidy[train_indices, ]
summary(train)

# Train final model on the training data
train_model <- lm(formula=Sleep.efficiency~Age+REM.sleep.percentage+
                    Deep.sleep.percentage+Awakenings+Caffeine.consumption+
                    Alcohol.consumption+Smoking.status+Exercise.frequency, data=train)

# Calculating Root mean square error
rmse <- sqrt(mean((tidy$Sleep.efficiency[-train_indices] - 
                     predict(train_model, tidy[-train_indices,]))^2))


# Final single random prediction
newdata=data.frame(Age=24,REM.sleep.percentage=26,Deep.sleep.percentage=56,
                   Awakenings=4,Smoking.status="No",Caffeine.consumption=25,
                   Alcohol.consumption=0,Exercise.frequency=1)
predict(fit2,newdata=newdata)

