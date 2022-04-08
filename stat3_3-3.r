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