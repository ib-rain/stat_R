library(psych)
library(ggplot2)
df = mtcars

cor.test(x=df$mpg, y=df$hp)
cor.test(x=df$mpg, y=df$hp, method = 'spearman')

cor.test(~mpg+hp, df)

plot(x=df$mpg, y=df$hp)

ggplot(df, aes(x=mpg, y=hp, col = factor(cyl))) + geom_point(size=5)

df_numetic = df[,c(1,3:7)]

pairs(df_numetic)
cor(df_numetic)

corr.test(df_numetic)

corr.calc <- function(x){
  t = cor.test(x = x[[1]], y = x[[2]], method = 'pearson')
  return(c(t$estimate, t$p.value))
}
corr.calc( mtcars[, c(1,5)] ) 
corr.calc( iris[,1:2] ) 



#
step6 <-  read.table("step6.csv",  header=TRUE, sep=',' )

filtered.cor <- function(x){
  x = x[sapply(x,is.numeric)]
  cor = cor(x)
  diag(cor) = 0
  
  if(abs(max(cor) > abs(min(cor)))){
    return(max(cor))
  } else return(min(cor))
}


filtered.cor(step6)
#[1] 0.235997

filtered.cor(iris)
#[1] 0.9628654  

iris1 = iris
iris1$Petal.Length <- -iris1$Petal.Length
filtered.cor(iris1)

test_data <- as.data.frame(list(V7 = c("t", "t", "t", "t", "t", "t", "t", "t"), V3 = c(-1, -2, -0.3, 0.5, -1.4, -1.3, -1.4, 0.3), V6 = c(-0.7, 0.5, -0.2, 0.7, 0, 0.2, 0.5, -1.2), V5 = c(-0.8, -3.4, -0.5, -0.8, 0.3, 0.4, 0.6, -0.4), V4 = c(-0.4, 0.1, 1.2, 0.8, 0.2, 0.7, -0.4, -0.8), V2 = c(-0.7, 0.5, -0.2, 0.7, 0, 0.2, 0.5, -1.2), V1 = c(-0.5, 0.4, -2, -0.1, -0.5, -0.4, 2, -1.3)))

filtered.cor(test_data)

#
filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])}
#

smart_cor <- function(x){
  if (!(shapiro.test(x[[1]])$p.value > 0.05 & shapiro.test(x[[2]])$p.value > 0.05)){
    return(cor.test(x[[1]], x[[2]], method = 'spearman')$estimate)
  } else return(cor.test(x[[1]], x[[2]], method = 'pearson')$estimate)
}

test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor(test_data)
#[1] -0.1031003

#

fit = lm(mpg ~ hp, df)
fit
summary(fit)


ggplot(df, aes(x=hp, y=mpg)) + geom_point(size=5) + geom_smooth()
ggplot(df, aes(x=hp, y=mpg)) + geom_point(size=5) + geom_smooth(method = 'lm')


ggplot(df, aes(x=hp, y=mpg, col = factor(cyl))) + geom_point(size=5) + geom_smooth()
ggplot(df, aes(x=hp, y=mpg, col = factor(cyl))) + geom_point(size=5) + geom_smooth(method = 'lm')

ggplot(df, aes(x=hp, y=mpg, col = factor(cyl))) + geom_point(size=5) + geom_smooth(method = 'lm') + facet_grid(.~cyl)
ggplot(df, aes(x=hp, y=mpg)) + geom_point(size=5) + geom_smooth(method = 'lm') + facet_grid(.~cyl)


ggplot(df, aes(x=hp, y=mpg)) + geom_smooth(method = 'lm', se = T ) + facet_grid(.~cyl)
ggplot(df, aes(x=hp, y=mpg)) + geom_smooth(method = 'lm', se = F ) + facet_grid(.~cyl)

fitted_mpg = data.frame(mpg = df$mpg, fitted = fit$fitted.values)

new_hp = data.frame(hp = c(100,150,129,300))

predict(fit, new_hp)

#
my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

aggregate(mpg ~ cyl, my_df, mean)

ggplot(my_df, aes(cyl,mpg)) + geom_point()

#

df = read.table('dataset_11508_12.txt', sep=' ')

fit = lm(V1 ~ V2, df)
summary(fit)


ideal046_diamonds = subset(diamonds, cut == 'Ideal' & carat == 0.46)

fitd = lm(price ~ depth, ideal046_diamonds)
fit_coef = fitd$coefficients

regr.calc = function(df){
  if (cor.test(df[[1]], df[[2]], method = 'pearson')$p.value < 0.05){
    fit = lm(df[[1]]~df[[2]])
    df$fit = fit$fitted.values
    return(df)
  } else return("There is no sense in prediction")
}

my_df = iris[,1:2]
regr.calc(my_df)

my_df = iris[,c(1,4)]
regr.calc(my_df)
my_df


my_plot = ggplot(iris, aes(x=Sepal.Width, y = Petal.Width, col = Species))+ geom_point(size=2) + geom_smooth(method = 'lm')
my_plot

#

ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
  geom_point()+
  geom_smooth()



ggplot(mtcars, aes(mpg, disp))+
  geom_point(aes(col = factor(am)))+
  geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
  geom_point()+
  geom_smooth(aes(col = factor(am)))

#3.2

swiss_df = data.frame(swiss)
str(swiss)

hist(swiss_df$Fertility)

fit = lm(Fertility ~ Examination + Catholic, swiss_df)
summary(fit)

fit2 = lm(Fertility ~ Examination * Catholic, swiss_df)
summary(fit2)
confint(fit2)

#

test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
test_data1 <- as.data.frame(list(x_1 = c(6, 13, 7, 4, 4, 10, 10, 11, 13, 11), x_2 = c(39, 33, 33, 37, 34, 44, 42, 33, 42, 49), y = c(14, 8, NA, 12, 11, NA, NA, NA, 6, NA)))
test_data2 <- as.data.frame(list(x_1 = c(10, 9, 12, 14, 9, 16, 7, 7, 11, 14), x_2 = c(32, 30, 49, 32, 15, 36, 28, 26, 18, 28), y = c(14, 10, 8, 15, 17, 17, 3, NA, 14, 8)))

fill_na = function(data){
  fit = lm(y ~ x_1 + x_2, data)
  #data$y_full = ifelse(is.na(data$y), fit$fitted.values, data$y)
  data$y_full = ifelse(is.na(data$y), predict(fit, data), data$y)
  #data$y_full = predict(fit, data[1:2,])
  return(data)
}

fill_na(test_data)
fill_na(test_data1)
fill_na(test_data2)

#
fill_na <- function(my_df){    
  fit <- lm(y ~ x_1+x_2, my_df)    
  my_df$y_full = ifelse(is.na(my_df$y), predict(fit, my_df), my_df$y)    
  return(my_df)}
#

df = mtcars[,c(1,3,4,5,6)]
df

fit = lm(wt ~ mpg + disp + hp, df)
summary(fit)

fit = lm(rating ~ complaints * critical, attitude)
summary(fit)

#

hist(swiss_df$Catholic)
swiss_df$religious <- as.factor(ifelse(swiss$Catholic > 60, 'Lots', 'Few'))
fit3 <- lm(Fertility ~ Examination + religious, data = swiss_df)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss_df)
summary(fit4)
fit41 <- lm(Fertility ~ Examination*religious, data = swiss_df)
summary(fit41)
#https://stepik.org/lesson/11509/step/8?unit=2532
#about changing order of factors

# plots
library(ggplot2)
ggplot(swiss_df, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss_df, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth()

ggplot(swiss_df, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss_df, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() 

ggplot(swiss_df, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth()

ggplot(swiss_df, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')


fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss_df)
summary(fit5)


#

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))

fit = lm(mpg ~ wt * am, mtcars)
summary(fit)
#?????? ???????????????? ???????????????? intercept ?? ???????????? ?????????????
#???????????? ?????????????? ?? ?????????? ?? ???????????????????????????? ???????????????? ?????????????? ?? ?????????????? ??????????

#https://stepik.org/lesson/11509/step/14?unit=2532
#???????????????? ???????????????? ???? ????, ?????? ?????? ???????? ???????????????? mpg (miles per gallon), ?????? ???????? ?????????? ???????????? ?????????????? (???? ?????????? ?????????????? ?????????????? ???????????? ???????????? ???????????????? ??????????????).
#?? ?????????????? ?? ???????????? ???????????????? ?????????????? ?????? ?????????????? ???????????? ???? ???????????? ??????????????
#?? ?????????? ?? ???????????? ???????????????? ?????????????? ???????????? ?????????????? ????????

ggplot(mtcars, aes(x = wt, y = mpg, col = am)) + 
  geom_smooth(method = 'lm')

#

rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)
#fit_full predicts better

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)
#prediction quality is the same

optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)

model_full <- lm(rating ~ ., data = attitude) 

model_null <- lm(rating ~ 1, data = attitude)

scope = list(lower = model_null, upper = model_full)

optimal_fit <-  step(object = model_full, scope = scope, direction = 'backward')

best = lm(rating ~ learning + complaints, attitude)

anova(model_full, best)
lcs = LifeCycleSavings

fit = lm(sr ~ .^2, LifeCycleSavings)
summary(fit)

#3.4

data(swiss)
swiss

pairs(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point()



ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()

ggplot(swiss, aes(x = Education)) + 
  geom_histogram()


ggplot(swiss, aes(x = log(Education))) + 
  geom_histogram()



ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth()

#

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

hist(log(my_vector))


beta.coef <- function(x){
  x = scale(x)
  return(lm(x[,1] ~ x[,2])$coefficients)
}

beta.coef(mtcars[,c(1,3)])
beta.coef(swiss[,c(1,4)])

#
importance_calc <- beta.coef <- function(x){    
  x <-scale(x)    
  return(lm(x[,1] ~ x[,2])$coefficients)}
#


normality.test = function(x){
  res = c()
  for (i in names(x))
    res = append(res, shapiro.test(x[[i]])$p.value)
  names(res) = names(x)
  return(res)
}

normality.test(mtcars[,1:6])
normality.test(iris[,-5])

#
normality.test  <- function(x){    
  return(sapply(x, FUN =  shapiro.test)['p.value',])}
#

#3.5
# linearity 

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)


swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)


anova(lm2, lm1)

swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)


ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) + 
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd=1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd=1)


ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) + geom_hline(yintercept =0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(yintercept=0, col = 'red', lwd = 1)


# independence of errors

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()

# Homoscedasticity

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)

library(gvlma)

ds = read.csv('homosc.csv')
fit = lm(DV~IV, ds)
gvlma(fit)


# Errors Normally distributed

ggplot(swiss, aes(x = lm1_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)


ggplot(swiss, aes(x = lm2_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)
#p < 0.05 -- not normal

resid.norm  <- function(fit){
  resid = fit$residuals
  col = ''
  if(shapiro.test(resid)$p.value <0.05){
    col = 'red'
  } else col = 'green'
  ggplot(as.data.frame(resid), aes(x = fit$residuals)) + 
    geom_histogram(fill = col)
}

fit <- lm(mpg ~ disp, mtcars)
resid.norm(fit)
fit <- lm(mpg ~ wt, mtcars)
resid.norm(fit)

#
resid.norm <- function(fit) {    
  resid.norm.pv <- shapiro.test(fit$residuals)$p.value    
  plt <- ggplot(data.frame(fit$model), aes(x = fit$residuals)) +    
    geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))    
  return(plt)}
#

high.corr = function(x){
  cor = cor(x)
  diag(cor) = 0
  
  max_abs_cor = 0
  if(abs(max(cor) > abs(min(cor)))){
    max_abs_cor=max(cor)
  } else max_abs_cor = min(cor)
  #return(dimnames(which(cor == max_abs_cor, arr.ind = T))[[1]])
  return(rownames(which(cor == max_abs_cor, arr.ind = T)))
}

#
high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))}
#

high.corr(iris[,-5])

#3.6

df = read.csv('train.csv', sep=";", stringsAsFactors = T)
str(df)

ggplot(df, aes(read, math, col = gender)) + geom_point(size = 3) + facet_grid(.~hon)

fit = glm(hon ~ read + math + gender, df, family = 'binomial')
summary(fit)

exp(fit$coefficients)

head(predict(object = fit))
head(predict(object = fit, type = 'response'))

df$prob = predict(object = fit, type = 'response')


fit = glm(am ~ disp + vs + mpg, mtcars, family = 'binomial')

log_coef = fit$coefficients
log_coef

ggplot(data = ToothGrowth, aes(x = supp, y = len, fill = factor(dose))) + geom_boxplot()

library(ROCR)

pred_fit = prediction(df$prob, df$hon)
perf_fit = performance(pred_fit, 'tpr', 'fpr')
plot(perf_fit, colorize = T, print.cutoffs.at = seq(0.1, by = 0.1))
auc = performance(pred_fit, measure = 'auc') 
auc

perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.225, lwd = 2)

df$pred_resp  <- factor(ifelse(df$prob > 0.225, 1, 0), labels = c("N", "Y"))

df$correct  <- ifelse(df$pred_resp == df$hon, 1, 0)

ggplot(df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

mean(df$correct)


test_df  <- read.csv("test.csv", sep = ";", stringsAsFactors = T)

fit  <- glm(hon ~ read + math + gender, test_df, family = "binomial")

test_df$hon  <- NA


test_df$hon  <- predict(fit, newdata = test_df, type = "response")
View(test_df)

df = read.csv('https://stepik.org/media/attachments/lesson/11478/data.csv')

fit = glm(admit ~ rank * gpa, df, family = "binomial")
sum(is.na(df$admit))

df$predicted = ifelse(is.na(df$admit), predict(fit, newdata = df, type = "response"), NA)

sum(is.na(df$admit) & df$predicted >= 0.4)

#3.7
library(stargazer)
library(xtable)
fit1  <- lm(mpg ~ cyl+disp, mtcars)
fit2 <- aov(mpg~am*vs,mtcars)
fit_table1 = xtable(fit1)
fit_table2 = xtable(fit2)
print(fit_table1, type = "html", file = "fit_table1.html")
print(fit_table2, type = "html", file = "fit_table2.html")
stargazer(fit1, type = "html",
          dep.var.labels = "mpg",
          covariate.labels = c("cyl","disp"), out = "models1.html")