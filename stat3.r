library(ggplot2)

qplot(x = hp, y = mpg, data = mtcars)
qplot(x = hp^0.5, y = mpg, data = mtcars)
qplot(x = hp^-0.5, y = mpg, data = mtcars)
qplot(x = -hp^-0.5, y = mpg, data = mtcars)

fit1 = lm(mpg ~ hp, mtcars)
fit2 = lm(mpg ~ I(-hp^-0.7), mtcars)
summary(fit1)
summary(fit2)


qplot(x = log(hp), y = log(mpg), data = mtcars)
fit3 = lm(log(mpg) ~ log(hp), mtcars)
summary(fit3)

qqnorm(fit3$residuals)
qqline(fit3$residuals, col = "steelblue", lwd = 2)

#1.5
library(dplyr)
d2 = sample_n(diamonds, 500)

qplot(x = price, y = carat, data = d2) + geom_smooth(method = lm)
fit_1 = lm(carat ~ price, d2)
summary(fit_1)

plot(fit_1)


#1.7
qplot(x = speed, y = dist, data = cars)
fit1 = lm(dist~speed, cars)
summary(fit1)

cars2 = mutate(cars, speed_2 = speed^2, speed_3 = speed^3)
pairs(cars2)

fit2 = lm(dist~., cars2)
summary(fit2)


#
library(car)
library(DAAG)

fits = lm(Fertility~., swiss)
summary(fits)

cor.test(~Fertility+Examination, swiss)

vif(fits)

fits2 = lm(Fertility~., select(swiss, -Examination))
summary(fits2)
vif(fits2)


#1.8-2
hetero_test = function(dt){
  frml = as.formula(paste(colnames(dt)[1],'~', paste(colnames(dt)[-1], collapse ='+')))
  fit = lm(frml, dt)
  
  frml2 = as.formula(paste('fit$residuals^2~', paste(colnames(dt)[-1], collapse ='+')))
  fit2 = lm(frml2, dt)
  
  return(summary(fit2)$r.squared)
}

hetero_test(mtcars)

#
hetero_test <-  function(test_data){
  fit <- lm(test_data[,1] ~ ., test_data[-1])
  fit_2 <- lm(fit$residuals^2 ~.,test_data[-1])
  summary(fit_2)$r.squared 
}
#

#1.8-3
library(dplyr)

VIF = function(test_data){
  td = test_data[,-1]
  res = setNames(numeric(length(td)), colnames(td))
  for(i in 1:length(td)){
    Rsq_i = summary(lm(td[,i] ~ ., td[-i]))$r.squared 
    res[i] = 1 / (1 - Rsq_i)
  }
  return(res)
}

VIF(mtcars)

set.seed(42)

test_data <- data.frame(y = rnorm(30, 5), x1 = rnorm(30, 5))
test_data$x2 <- test_data$x1^2
VIF(test_data)

#
VIF <- function(data)
{
  sapply(names(data[-1]), function(name) {
                                          m = lm(as.formula(paste(name, "~ .")), data[-1])
                                          r2 = summary(m)$r.squared
                                          1 / (1 - r2)
                                          })
}
#

#1.8-4
library(dplyr)

smart_model = function(dt){
  if(length(dt) == 2)
    return(lm(dt[,1] ~ ., dt[-1])$coefficients)
  
  vifs = VIF(dt)
  while(max(vifs) >= 10){
    maxvif_name = names(vifs)[which(vifs==max(vifs))][1]
    dt = select(dt, -maxvif_name)
    
    if(length(dt) == 2)
      break
    
    vifs = VIF(dt)
  }
  return(lm(dt[,1] ~ ., dt[-1])$coefficients)
}

smart_model = function(dt){
  repeat {
    if(length(dt) == 2)
      break
    
    vifs = VIF(dt)
    
    if(max(vifs) < 10)
      break
    
    maxvif_name = names(vifs)[which(vifs==max(vifs))][1]
    dt = select(dt, !(maxvif_name))
  }
  
  return(lm(dt[,1] ~ ., dt[-1])$coefficients)
}

smart_model = function(dt){
  repeat {
    if(length(dt) == 2)
      break
    
    vifs = VIF(dt)
    
    if(max(vifs) < 10)
      break
    
    maxvif_idx = which(vifs==max(vifs), arr.ind = T)[[1]]+1
    dt = select(dt, -maxvif_idx)
  }
  
  #return(lm(dt[,1] ~ ., dt[-1])$coefficients)
  return(lm(dt)$coefficients)
}
smart_model(mtcars)
#(Intercept)          hp        drat          wt        qsec          vs          am 
#13.80810376 -0.01225158  0.88893522 -2.60967758  0.63983256  0.08786399  2.42417670 
#gear        carb 
#0.69389707 -0.61286048

set.seed(42)
test_data <- data.frame(y = rnorm(30, 5), x1 = rnorm(30, 5))
test_data$x2 <- test_data$x1^2
smart_model(test_data)

test_data2 <- as.data.frame(list(y = c(5.81, 5.12, 5.26, 4.81, 6.07, 5.68, 3.04, 3.91, 7.63, 4.01, 5.23, 5.71, 5.68, 3.86, 5.76, 7.05, 6.19, 6.99, 5.51, 4.33, 5.1, 4.81, 5.79, 5.93, 4.12, 5.72, 4.64, 7.38, 5.77, 4.87), x1 = c(5.88, 5.01, 5.94, 4.1, 4.84, 5.38, 4.57, 4.26, 4.26, 6.58, 4.85, 5.37, 4.48, 2.8, 2.35, 6.34, 4.87, 5.21, 3.92, 6.48, 2.85, 4.73, 4.97, 3.68, 3.57, 6.66, 5.51, 4.36, 6.38, 7.36), x2 = c(34.53, 25.06, 35.33, 16.84, 23.44, 28.97, 20.93, 18.14, 18.13, 43.29, 23.49, 28.87, 20.11, 7.82, 5.55, 40.17, 23.68, 
                                                                                                                                                                                                                                                                                                                                                                                                                           27.13, 15.35, 42.01, 8.14, 22.33, 24.75, 13.51, 12.76, 44.33, 30.36, 19.01, 40.73, 54.1)))

smart_model(test_data2)

test_data3 <- as.data.frame(list(y = c(5, 6.42, 5.73, 3.12, 3.83, 4.47, 5.15, 5.83, 4.21, 8, 4.48, 4.31, 4.18, 5.16, 4.21, 6.8, 5.4, 4.55, 4.26, 6.01, 5.62, 4.37, 5.11, 3.22, 6.77, 4.77, 5.69, 3.38, 4.95, 4.73), x1 = c(4.9, 7.39, 5.81, 5.51, 3.14, 5.17, 4.84, 5.67, 5.89, 4.84, 5.27, 4.4, 4.98, 4.83, 5.47, 5.58, 6.07, 6.54, 3.96, 5.06, 4.53, 3.71, 4.7, 5.22, 5.69, 4.55, 5.41, 3.69, 5.02, 4.71), x2 = c(24.04, 54.6, 33.79, 30.38, 9.89, 26.76, 23.39, 32.16, 34.66, 23.41, 27.73, 19.39, 24.75, 23.28, 29.97, 31.1, 36.88, 42.84, 15.65, 
                                                                                                                                                                                                                                                                                                                                                                                                                   25.57, 20.52, 13.74, 22.09, 27.25, 32.32, 20.68, 29.27, 13.61, 25.19, 22.2)))

smart_model(test_data3)

test_data4 = as.data.frame(list(X1 = c(1.1, 1.7, 1.5, 0.3, 1, -1.7, 1.3, -0.5, -0.2, -1.5), X2 = c(-1.35, 1.26, 1.18, -2.11, 0.31, -0.17, -0.45, 0.66, -0.9, 0.14), X3 = c(-1.7, 1.3, 1.2, -2.1, 0.1, -0.2, -0.7, 0.5, -0.9, 0.1), X4 = c(0, 0.3, -0.6, 1, -1.1, -1.6, 1.9, -2.3, 0.1, 0.7), X5 = c(0.22, 0.36, -0.94, 1.11, -1.33, -1.52, 2.33, -2.3, 0.3, 0.75)))

smart_model(test_data4)


# beautiful recursion
smart_model <- function(d) {
  vif <- VIF(d)
  if (max(vif) > 10) { 
    smart_model(d[, -(which.max(vif) + 1)])
  } else lm(d)$coeff
}
#
#lm(data)  ===  lm(data[, 1] ~ ., data[-1])

#1.8-5
transform_x = function(td){
  transform_tukey = function(x, pwr){
    if(pwr > 0){
      x = x^pwr
    } else if(pwr == 0) {
      x = log(x)
    } else {
      x = -(x^pwr)
    }
    return(x)
  }
  
  y = td[[1]]
  x = td[[2]]
  cor_val = cor(y,x)
  
  for(i in seq(-2, 2, 0.1)){
    x_tr = transform_tukey(td[[2]], i)
    cor_val_tr = cor(y,x_tr)
    
    if(cor_val_tr > cor_val){
      cor_val = cor_val_tr
      x = x_tr
    }
  }
  return(x)
}

set.seed(42)
test_data <- data.frame(y = rnorm(10, 10, 1), x = rnorm(10, 10, 1))  
transform_x(test_data)

# very interesting
transform_x <- function(test_data) {
  y <- test_data[[1]]
  x <- test_data[[2]]
  # ???????????????? ???????????????? ????????????
  l <- seq(-2, 2, 0.1)
  # ??????????????????????????
  d <- outer(x, l, "^")
  # ???????????????? ????????
  d[, l < 0] <- -d[, l < 0]
  # ?????? ????????????, ?????? ???????????? ?????????? ????????
  d[, l == 0] <- log(x)
  
  # ???????????? ????????????????????
  r <- cor(d, y)[, 1]
  # ?????????? ???????????????????????? ????????????????
  d[, which.max(abs(r))]
}
#
transform_x = function(data)
{
  do_transform = function(x, lambd) {
    if (lambd > 0) x ^ lambd else if (lambd < 0) -(x ^ lambd) else log(x) }
  
  x_data = data[,2] 
  y_data = data[,1]
  lambdas = seq(-2, 2, 0.1)
  corrs = sapply(lambdas, 
                 function(lambd) cor(do_transform(x_data, lambd), y_data))
  lambda = lambdas[which.max(abs(corrs))]
  #print(lambda)
  do_transform(x_data, lambda)
}
#

###

#2.3
install.packages('lme4')
install.packages('mlmRev')
install.packages('lmerTest')

library(Rcpp)
library(mlmRev)
library(lme4)
library(ggplot2)

fit1=lm(standLRT ~ normexam + school, data = Exam)
fit2=lm(standLRT ~ normexam + school:normexam, data = Exam)
fit3=lm(standLRT ~ normexam*school, data = Exam)

summary(fit1)
summary(fit2)
summary(fit3)


#2.5-1
exp_data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv", stringsAsFactors = T)
str(exp_data)

exp_data$scenario = as.factor(exp_data$scenario)
str(exp_data)
ggplot(exp_data, aes(x=scenario, y = frequency, fill = attitude)) + geom_boxplot()

ggplot(exp_data, aes(x=frequency, fill = subject)) + geom_density(alpha = 0.2) + facet_grid(.~gender)
ggplot(exp_data, aes(x=frequency, fill = subject)) + geom_density(alpha = 0.2) + facet_grid(rows=vars(gender))


library(lme4)
fit_1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=exp_data)
fit_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=exp_data)

fit_3 <- lmer(frequency ~ attitude + gender + (1 + attitude|subject) + (1 + attitude|scenario), data=exp_data)

summary(fit_1)
summary(fit_2)
summary(fit_3)

###3

#3.3-2 -- nice bootstrap
median_cl_boot <- function(x){
  sample_median = median(x)
  
  diff_median = vector('numeric', 1000)
  for(i in 1:1000){
    diff_median[i] = sample_median - median(sample(x, length(x), replace = T))
  }
  percentiles = quantile(diff_median, probs = c(0.05, 0.95))
  
  return(percentiles+sample_median)
}

a = c(0:5000)

median_cl_boot(a)
median(a)

#https://stepik.org/lesson/45350/step/2?thread=solutions&unit=23653
#
median_cl_boot <- function(x){
  med <- median(x)
  vec_1 <- NULL
  sort(sapply(c(1:1000), function(c) median(sample(x,(length(x)/2),replace=T))-med))[c(50,950)]+med
}
#

#3.3-3
library(dplyr)

#sample() samples not rows but columns!!!! -- this code works only due to size = length(df)

#df[sample(nrow(df), 5), ] as alternative

coeffs_cl_boot <- function(df){
  sample_lm_coeff = lm(df)$coefficients
  
  diff_lm_coeffs = setNames(data.frame(matrix(ncol = 2, nrow = 0)), names(sample_lm_coeff))
  
  for(i in 1:1000){
    diff_lm_coeffs = rbind(diff_lm_coeffs, sample_lm_coeff - lm(sample(df, length(df), replace = T))$coefficients)
  }
  
  percentiles = c(quantile(diff_lm_coeffs[[1]], probs = c(0.05, 0.95)), quantile(diff_lm_coeffs[[2]], probs = c(0.05, 0.95)))

  return(percentiles+sample_lm_coeff)
  
}

library(dplyr)

slope_cl_boot <- function(df){
  sample_lm_coeff = lm(df)$coefficients[[2]]
  
  diff_lm_coeff = vector('numeric', 1000)
  
  for(i in 1:1000){
    diff_lm_coeff[i] = sample_lm_coeff - lm(sample_n(df, nrow(df)-2, replace = T))$coefficients[[2]]
  }
  
  percentiles = quantile(diff_lm_coeff, probs = c(0.025, 0.975))
  
  return(percentiles+sample_lm_coeff)
  
}

lm(mtcars[,c(1,7)])$coefficients[[2]]

slope_cl_boot(mtcars[,c(1,7)])

lm(cars)

slope_cl_boot(cars)

#
slope_cl_boot <- function(x){
  fit = lm(y ~ x, d)
  slope = coef(fit)[2]
  samples = replicate(1000, d[sample(1:nrow(d), nrow(d), replace = T), ], simplify = F)
  samples_slopes = sapply(samples, function(i) coef(lm(i[[2]] ~ i[[1]]))[2])
  dist = slope - samples_slopes
  percentiles = quantile(dist, probs = c(0.025,0.975))
  conf_int = percentiles + slope
}
#
slope_cl_boot <- function(x, bootNums = 1000){
  xRows <- nrow(x)
  cor <- lm(y ~ x, data = x)$coefficients[2]
  cors <- replicate(bootNums, lm(y ~ x, data = x[sample(xRows, replace=T),])$coefficients[2] - cor)
  cor + quantile(cors, c(.025,.975))
}
#
slope_cl_boot <- function(x) {
  bootstrapped_values <- 
    sapply(1:1000, (function(v) lm(x)$coefficients[2] - lm(x[sample(1:nrow(x), nrow(x), r = T), ])$coefficients[2]))
  return(mean(bootstrapped_values) + c(-1, 1) * 1.96 * sd(bootstrapped_values) + lm(x)$coefficients[2])
}
#
sampleslope <- function(formula, df, d) {
  s <- df[d,]
  fit <- lm(formula, data=s)
  return(fit$coef[2])
}
library(boot)
slope_cl_boot <- function(df){
  b <- boot(df, sampleslope, 1000,  formula=y~x)
  ci <- boot.ci(b, type="basic")
  return(ci$basic[1,4:5])
}
#