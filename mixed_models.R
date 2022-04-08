lmer(DV ~ IV + (1 + IV | RV), data = my_data)

install.packages('lme4')
install.packages('mlmRev')
install.packages('lmerTest')
install.packages('Rcpp')

library(Rcpp)
library(mlmRev)
library(lme4)
library(ggplot2)

data("Exam")

str(Exam)
help(Exam)




ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point()


ggplot(data = Exam, aes(x = standLRT, y = normexam, col = school)) + 
  geom_point()



# 1 main effect lm
Model1 <- lm(normexam ~ standLRT, data=Exam)
summary(Model1)

Exam$Model1_pred <- predict(Model1)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point() + 
  geom_line(data = Exam, aes(x = standLRT, y = Model1_pred), col = 'blue', size = 1)



Model1 <- lmer(normexam ~ standLRT, data=Exam)


# 1 main effect + random free member
Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)

Exam$Model2_pred <- predict(Model2)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + geom_point(alpha = 0.2) + geom_line(data = Exam, aes(x = standLRT, y = Model2_pred, col = school))







# 1 main effect + random free member (intercept) + random degree
Model3 <- lmer(normexam ~ standLRT + (1 + standLRT|school), data=Exam)
summary(Model3)

Exam$Model3_pred <- predict(Model3)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model3_pred, col = school))




# 1 main effect + random degree ("0 +" = w/o free member)
Model4 <- lmer(normexam ~ standLRT + (0 + standLRT|school), data=Exam)
summary(Model4)

Exam$Model4_pred <- predict(Model4)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model4_pred, col = school))










# Uncorrelated random effects
Model5 <- lmer(normexam ~ standLRT + (1|school) + (0 + standLRT|school), data=Exam)
summary(Model5)






###################################################################################

# Comparison


Model2 <- lmer(normexam ~ standLRT + (1|school), REML = FALSE, data=Exam)
summary(Model2)

Model0 <- lmer(normexam ~ 1 + (1|school), REML = FALSE, data = Exam)
summary(Model0)

anova(Model0, Model2)






# p-values after you add this lib!!!
library(lmerTest)

Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)






# Generalized mixed regression

Exam$school_type <- ifelse(Exam$type == 'Mxd', 1, 0)

Model5 <- glmer(school_type ~ normexam + (1|school), family = "binomial", data = Exam)

summary(Model5)






# Predictions on new dataset
predict(Model2, Exam)

new_Exam <- Exam[sample(1:nrow(Exam), 100), ]
new_Exam$school <- sample(101:200)

predict(Model2, new_Exam, allow.new.levels = T)



# Studying effects: fixed and random

fixef(Model3)
ranef(Model3)
