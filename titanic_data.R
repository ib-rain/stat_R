library(dplyr)
library(ggplot2)
library(vcd)

# Считаем данные
titanic <- read.csv("https://stepic.org/media/attachments/course/524/train.csv")
titanic <- na.omit(titanic)
glimpse(titanic)
titanic <- mutate(titanic, 
                  Survived = factor(Survived, labels = c("No", "Yes")), 
                  Pclass = factor(Pclass, labels = c("First", "Second", "Third")), 
                  Sex = factor(Sex, labels = c("Female", "Male")))

# Построим мозаичный график
mosaic(~ Sex + Survived | Pclass, data=titanic) 

#(Intercept only model)
simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
coef(simple_fit)
table(titanic$Survived)
odds <- 290 / 424
log(odds) 
summary(simple_fit)

#1nom model
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
coef(fit1)
table(titanic$Survived, titanic$Sex)

odds_male <- 93 / 360
odds_female <- 197 / 64

log(odds_female)
log(odds_male)

odds_ratio <- odds_male / odds_female
log(odds_ratio)

#2nom model
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")
coef(fit2)
summary(fit2)

table(titanic$Survived, titanic$Pclass , titanic$Sex)

# (Intercept) 
female_p1_odds <- 82 / 3
log(female_p1_odds)

# Sexmale  
male_p1_odds <- 40  /  61 
log(male_p1_odds)
log(male_p1_odds / female_p1_odds )

# PclassSecond
female_p2_odds <- 68  /  6 
log(female_p2_odds / female_p1_odds )

# PclassThird
female_p3_odds <- 47  /  55 
log(female_p3_odds / female_p1_odds )

# SexMale:PclassSecond
male_p2_odds <- 15 / 84
log(male_p2_odds / female_p2_odds ) - log(male_p1_odds / female_p1_odds )

#Sexmale:factorThird 
male_p3_odds <- 38 / 215
log(male_p3_odds / female_p3_odds ) - log(male_p1_odds / female_p1_odds )


#comparison
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")

anova(fit1, fit2, test="Chisq")

#nom and numerical independent variables
new_data <- data.frame(Sex = "Female", Pclass = "First")
predict(fit2, newdata = new_data, type = "response")


fit3 <- glm(Survived ~ Sex + Pclass, titanic, family = "binomial")
summary(fit3)
anova(fit3, test="Chisq")

predict(fit3, newdata = new_data)
