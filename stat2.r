drivers = cbind(c(20,11,7),c(15,12,9))
chisq.test(drivers)

#?????????????????????? ??????????????
patients <- rbind(c(18, 7), c(6, 13))
#???????????????? ???????????? ?? ??????????????
colnames(patients) <- c("Yes", "No")
rownames(patients) <- c("Placebo", "Aspirin")
#?????? ????????????, ?????????????? ?????? ??????????
mosaicplot(patients, color=T, shade=T, ylab="Thrombosis", xlab="Group")
#?? ?????? ?????? ?????????? ?? ???????????????? ?????????????????????????? ??????????????, ?????????????? ???? ????????????
mosaicplot(patients, color=T, shade=T, ylab="Thrombosis", xlab="Group", cex.axis=1, main="")

#?????????????????????? ??????????????
patients2 <- rbind(c(25, 1), c(3, 30))
#???????????????? ???????????? ?? ??????????????
colnames(patients2) <- c("Yes", "No")
rownames(patients2) <- c("Placebo", "Aspirin")
#?????? ?????? ????????????
mosaicplot(patients2, color=T, shade=T, ylab="Thrombosis", xlab="Group", cex.axis=1, main="")


fisher.test(cbind(c(1,3),c(3,1)))

#1.9

NA_position  <- function(x, y){
  all(is.na(x) == is.na(y))
}

#1.9-2
smart_test = function(df){
  tbl = table(df)
  if (min(tbl) < 5)
    return(fisher.test(tbl)$p.value)
  
  tst = chisq.test(tbl)
  return(c(tst$statistic, tst$parameter, tst$p.value))
}

# ???????????????????? ???????????????????? ?? ??????????????
table(mtcars[,c("am", "vs")])
#vs
#am   0  1
#0 12  7
#1  6  7
smart_test(mtcars[,c("am", "vs")])
#[1] 0.3475355 1.0000000 0.5555115


# ???????????????????????? ???????????????????? ?? ??????????????
table(mtcars[1:20,c("am", "vs")])
#vs
#am  0 1
#0 8 6
#1 2 4
smart_test(mtcars[1:20,c("am", "vs")])
#[1] 0.628483

#

smart_test <- function(test_data){
  test_table <- table(test_data) 
  if (min(test_table) < 5){        
    fit  <- fisher.test(test_table)        
    result  <- fit$p.value      
  } else {        
    fit  <- chisq.test(test_table)        
    result  <- c(fit$statistic, fit$parameter, fit$p.value)        
  }        
  return(result)        
}
#

#1.9-3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)

most_significant = function(df){
  res = data.frame(matrix(ncol = 2, nrow = 0))
  colnames(res) = c('val', 'p.value')
  
  for(colname in colnames(df)){
    tbl = table(df[colname])
    res[nrow(res)+1, ] = c(colname, chisq.test(tbl)$p.value)
  }
  return(res$val[res[[2]] == min(res[[2]])])
}

most_significant(test_data)
#[1] "V3" 

#
most_significant  <- function(test_data){    
  chisq_tests <- sapply(test_data, function(col) chisq.test(table(col))$p.value)
  min_p  <- which(chisq_tests == min(chisq_tests))
  return(colnames(test_data)[min_p])
}
#

#1.9-4
idata = iris

str(idata)
summary(idata)

idata_means = sapply(idata[,c(-5)], function(col) mean(col))

idata$important_cases = as.factor(ifelse(colSums(t(iris[,1:4]) > idata_means) >= 3, "Yes", "No"))
str(idata)

table(idata$important_cases)

(idata[,1:4] > idata_means)
sum(rowSums(idata[,c(1,2,3,4)] > idata_means) >= 3)
#vs
sum(colSums(t(iris[,1:4]) > idata_means) >= 3)

#https://stepik.org/lesson/26186/step/5?discussion=283045&unit=8128

#
importance_calc <- function(v1, v2, threshold=3){    
  ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])))
#

#1.9-5
test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

get_important_cases = function(df){
  df_is_num = sapply(df, function(col) is.numeric(col))
  crit_num = sum(df_is_num) / 2
  
  #df_means = sapply(df[df_is_num], function(col) mean(col))
  df_means = colMeans(df[df_is_num])
  
  df$important_cases = factor(ifelse(colSums(t(df[df_is_num]) > df_means) > crit_num, T, F),
                              levels = c(F, T), labels = c("No", "Yes"))
  
  return(df)
}

df = get_important_cases(test_data)
str(df)
df

test_data1 <- as.data.frame(list(V1 = c(19, 19, 20, 17, 23, 18, 15, 17),
                                 V2 = c(14, 22, 21, 21, 22, 20, 26, 14),
                                 V3 = c(17, 30, 24, 19, 15, 25, 26, 10),
                                 V4 = c(18, 21, 13, 25, 18, 16, 17, 18),
                                 V5 = c(14, 18, 13, 19, 24, 12, 21, 18),
                                 V6 = c(20, 15, 25, 23, 11, 23, 19, 16)))

df1 = get_important_cases(test_data1)
str(df1)
df1

#
get_important_cases  <- function(d){    
  m <-  colMeans(d)    
  compare_to_means <- apply(d, 1, function(x) as.numeric(x > m))    
  is_important <- apply(compare_to_means, 2, sum) > ncol(d)/2    
  is_important <- factor(is_important, levels = c(FALSE, TRUE), labels = c('No', 'Yes'))    
  d$important_cases <- is_important    
  return(d)
}
#

#1.9-6
stat_mode = function(vec){
  vec_table = table(vec)
  return(as.numeric(names(vec_table[vec_table == max(vec_table)])))
}

v <- c(1, 2, 3, 3, 3, 4, 5)
stat_mode(v)

v1 <- c(1, 1, 1, 2, 3, 3, 3)
stat_mode(v1)

v2 <- c(1, 2, 3, 3, 4, 4, 5)
stat_mode(v2)

#
stat_mode <- function(v){        
  mode_positions <- which(table(v) == max(table(v)))    
  as.numeric(names(table(v))[mode_positions])
}
#

#1.9-7
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv", stringsAsFactors = T)
str(test_data)

max_resid <- function(x){
  xtable = table(x)
  stdres = chisq.test(xtable)$stdres
  idx = which(stdres == max(stdres), arr.ind = T)
  return(c(row.names(stdres)[idx[1]],colnames(stdres)[idx[2]]))
}

max_resid(test_data)

#
max_resid <- function(test_data){    
  d <- table(test_data)    
  chi <- chisq.test(d)    
  ind <- which(chi$stdres==max(chi$stdres), arr.ind = T)    
  return(c(row.names(d)[ind[1]],colnames(d)[ind[2]]))    
}
#

#1.9-8
library("ggplot2")
str(diamonds)

ggplot(data = diamonds, aes(x = color, fill = cut)) + geom_bar(position = "dodge")

###

#2.8-1
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
# ?????????????????? ???????????????????? ?? ???????????? 
test_data <- transform(test_data, x = factor(x), y = factor(y)) 

get_coefficients = function(df){
  fit = glm(y ~ x, df, family = "binomial")
  return(exp(fit$coefficients))
}

get_coefficients(test_data) 


#2.8-2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
test_data

var_names = c("X4", "X2", "X1")

centered = function(df, var_names){
  for(var_name in var_names){
    var_mean = mean(df[[var_name]])
    df[var_name] = df[var_name] - var_mean
  }
  return(df)
}

centered(test_data, var_names)

#
centered <- function(test_data, var_names){    
  test_data[var_names] <- sapply(test_data[var_names], function(x) x - mean(x))    
  return(test_data)    
}
#

#2.8-3
get_features <- function(df){
  fit = glm(is_prohibited ~ ., df, family = "binomial")
  result = anova(fit, test = "Chisq")
  idxs = result[c(-1),]$`Pr(>Chi)`<0.05
  
  if(sum(idxs) == 0)
    return("Prediction makes no sense")
  
  return(row.names(result[c(-1),])[idxs])
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv", stringsAsFactors = T)
str(test_data)
get_features(test_data)
#[1] "Prediction makes no sense"

test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv", stringsAsFactors = T)
str(test_data1)
get_features(test_data1)
#[1] "length" "width"  "type"  
# ???????????????? ?????????????????? ?????????????? ???????????????????? ???????????? ???????????????? ???????????????? ????????????????????, ?? ???? typeBad ?????? typeSuitcase! ???????? ?????? ???????????????????? ???????????? ?????????????? ???????????????????? ?? ??????????.

test_data2 <- data.frame(is_prohibited = factor( rep(1:2, each = 15)),weight = c( 81,77,53,92,73,88,82,75,70,81,74,96,67,84,73,87,88,84,81,75,64,85,80,76,81,76,75,76,79,76,81,77,53,92,73,88,82,75,70,81,74,96,67,84,73,87,88,84,81,75,64,85,80,76,81,76,75,76,79,76 ),length = c( 50,53,49,47,51,48,49,47,51,50,49,50,48,52,54,52,49,50,51,50,48,50,52,49,50,48,47,49,50,49,50,53,49,47,51,48,49,47,51,50,49,50,48,52,54,52,49,50,51,50,48,50,52,49,50,48,47,49,50,49 ),width = c( 21,18,18,19,21,16,20,21,21,22,17,19,22,17,18,24,21,19,20,20,20,21,20,24,18,18,20,22,19,20,21,18,18,19,21,16,20,21,21,22,17,19,22,17,18,24,21,19,20,20,20,21,20,24,18,18,20,22,19,20 ),type = factor(c( 1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2 )))
str(test_data2)
get_features(test_data2)

#
get_features <- function(test_data){    
  fit <- glm(is_prohibited ~., test_data, family = 'binomial')    
  result <- anova(fit, test = 'Chisq')    
  if (all(result$`Pr(>Chi)`[-1] > 0.05)){      
    return('Prediction makes no sense')}    
  return(rownames(result)[-1] [result$`Pr(>Chi)`[-1] < 0.05])  
}
#

#2.8-4
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv", stringsAsFactors = T)
str(test_data)
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv", stringsAsFactors = T)
str(data_for_predict)

most_suspicious = function(td, pd){
  fit <- glm(is_prohibited ~., td, family = 'binomial') 
  prediction = predict(fit, newdata = pd, type = "response")
  return(pd$passangers[prediction == max(prediction)])
}

most_suspicious(test_data, data_for_predict)
#[1] Svetozar # ?????????????????? ?????????? ????????????????!

#
most_suspicious <- function(test_data, data_for_predict){    
  fit <- glm(is_prohibited ~., test_data, family = 'binomial')    
  probs <- predict(fit, newdata = data_for_predict, type = 'response')    
  index <- which(probs == max(probs))    
  passanger_name <- data_for_predict$passangers[index]    
  return(passanger_name)    
}
#

#2.8-5
normality_test = function(df){
  df_is_num = sapply(df, function(col) is.numeric(col))
  return(sapply(df[df_is_num], function(col) shapiro.test(col)$p.value))
}

normality_test(iris)
test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")
normality_test(test)

#
normality_test <- function(dataset){    
  numeric_var <- sapply(dataset, is.numeric)  
  sapply(dataset[numeric_var], function(x) shapiro.test(x)$p.value)    
}
#

#2.8-6
smart_anova = function(td){
  case = 0
  
  for(td_s in split(td$x, td$y)){
    if (shapiro.test(td_s)$p.value < 0.05)
      case = 1 
  }
  
  if(bartlett.test(x ~ y, td)$p.value < 0.05){
    case = 1
  }
  
  if(case == 0){
    p_value = c("ANOVA"=summary(aov(x ~ y, td))[[1]]$'Pr(>F)'[1])
  } else {
    p_value = c("KW" = kruskal.test(x ~ y, td)$p.value)
  }
  return(p_value)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv", stringsAsFactors = T)
str(test_data)
smart_anova(test_data)

test_data2 <- as.data.frame(list(x = c(-0.07, 0.36, 1.14, 1.55, -0.63, -0.11, -0.61, -1.79, 1.62, -0.7, -0.62, -0.09, 0.84, 0.12, -0.15, -0.7, -1.57, 1.34, 1.06, 1.08, -0.83, 0.84, 0.73, 0.16, -0.13, -0.85, 0.27, 1.11, 0.8, 0.18), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))
test_data2$y <-  factor(test_data2$y, labels = c('A', 'B', 'C'))
str(test_data2)
smart_anova(test_data2)

test_data3 <- as.data.frame(list(x = c(2.82, -1.79, 0.06, 1.53, 0.3, 2.11, -2.24, 2.72, -1.95, 17, 3.8, 0, 0, 2.43, 0.85, 0.42, 1.41, 0.52, 0.12, 15, 0.21, 0, 0.27, 1.68, 0.41, 0.01, 0.63, 0.22, 0.02, 18), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))
test_data3$y <-  factor(test_data3$y, labels = c('A', 'B', 'C'))
str(test_data3)
smart_anova(test_data3)

#
smart_anova <- function(test){  
  p_normal <- unlist(by(test[, 1], test[, 2], function(x) shapiro.test(x)$p.value))   
  sd_equal <- bartlett.test(x ~ y, test)$p.value  
  if (all(p_normal > 0.05) & sd_equal > 0.05){    
    fit <- aov(x ~ y, test)    
    result <- c(ANOVA = summary(fit)[[1]]$'Pr(>F)'[1])    
    return(result)  
  } else {    
    fit <- kruskal.test(x ~ y, test)    
    result <- c(KW = fit$p.value)    
    return(result)    
  }    
}
#

#2.8-7
library(dplyr)

normality_by = function(test){
  test %>%
    group_by(test[,c(2,3)]) %>%
    summarise(p_value = shapiro.test(.data[[colnames(test)[1]]])$p.value)
}


test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv", stringsAsFactors = T)
str(test_data)

shapiro.p_value = function(x){
  return(shapiro.test(x)$p.value)
}

normality_by = function(test){
  res = aggregate(test[,1], by = list(test[,2],test[,3]), FUN = shapiro.p_value)
  names(res) = c(colnames(test)[2:3], "p_value")
  return(res)
}

normality_by(mtcars[, c("mpg", "am", "vs")])
normality_by(test_data)

#
normality_by <- function(test){    
  grouped_data <- aggregate(test[,1],by=list(test[,2], test[,3]),                                  
                            FUN = function(x) {shapiro.test(x)$p.value})                                  
  names(grouped_data) <- c(colnames(test)[2:3],'p_value')                                  
  return(grouped_data)    
}

#?????????????????? dplyr (?????? ??????????????, ?????? ???? ?????????? ?????????? ???????????????????? ?? ????????????):
library(dplyr)    
normality_by <- function(test_data){    
  result <- test_data %>% group_by(y, z) %>%     
    summarize(p_value = shapiro.test(x)$p.value)     
  return(result)    
}


#?????????? ?????????? ?????????????? ?? dplyr:
library(dplyr)    
get_p_value <- function(x){      
  shapiro.test(x)$p.value    
}    
normality_by <- function(test){    
  grouped <- test %>%    
    group_by_(.dots = colnames(.)[2:3]) %>%         
    summarise_each(funs(get_p_value))         
  names(grouped)[3] <- 'p_value'         
  return(grouped)         
}
#

#2.8-8
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density(alpha = 0.2) 


###

ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) + geom_point()

d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 3)
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw() 

#

library(ggplot2) 
library(ggrepel) # ?????? ?????????????????????? ?????????????? ?????????? ???? ??????????????

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data)
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 8)

#

library(ape)
set.seed(222)
tr <- rtree(20, tip.label = c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")) 
#?????????? ????????????
plot.phylo(tr) 
#???????????? ???????????? 
plot.phylo(tr, use.edge.length=FALSE)

#3.6-1
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
str(test_data)

dist_matrix <- dist(swiss, method = "euclidean", diag = FALSE, upper = FALSE, p = 2) # ???????????? ?????????????? ????????????????????
fit <- hclust(dist_matrix, method = "complete", members = NULL) # ?????????????????????????? ?????????????????????????? 
cluster <- cutree(fit, 3) # ?????????? ???????????????? ?????? ?????????????? ????????????????????

smart_hclust = function(test_data, cluster_number){
  dist_matrix <- dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  
  test_data$cluster = factor(cutree(fit, cluster_number))
  return(test_data)
}

smart_hclust(test_data, 3)

#
smart_hclust <- function(test_data, n_cluster){    
  d <- dist(test_data)    
  fit <- hclust(d)    
  test_data$cluster <- factor(cutree(fit, k = n_cluster))    
  return(test_data)    
}
#

#3.6-2
get_difference = function(test_data, n_cluster){
  dist_matrix = dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit = hclust(dist_matrix, method = "complete", members = NULL)
  cluster = factor(cutree(fit, n_cluster))
  
  res = c()
  for(colname in colnames(test_data)){
    fit1 <- aov(test_data[[colname]] ~ cluster)    
    if(summary(fit1)[[1]]$'Pr(>F)'[1] < 0.05)
      res = append(res, colname)
  }
  return(res)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
get_difference(test_data, 2)
#[1] "V2" 

test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
get_difference(test_data2, 2)
#[1] "V1" "V2"

#
get_difference <- function(test_data, n_cluster){    
  dist_matrix <- dist(test_data)    
  fit <- hclust(dist_matrix)    
  test_data$cluster <- as.factor(cutree(fit, n_cluster))    
  p_val <- sapply(test_data[,-ncol(test_data)],    
                  function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
  return(names(p_val)[p_val < 0.05])    
}
#

#3.6-3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
test_data

get_pc = function(dt){
  PCx = prcomp(dt, retx = T)$x
  dt$PC1 = PCx[,1]
  dt$PC2 = PCx[,2]
  return(dt)
}

get_pc = function(dt){
  return(cbind(dt, prcomp(dt, retx = T)$x[,1:2]))
}

get_pc(test_data)


#3.6-4
get_pca2 = function(dt){
  PCs = prcomp(dt, retx = T)
  return(cbind(dt, PCs$x[, 1:min(which(summary(PCs)$importance['Cumulative Proportion',] > 0.9))]))
}

result  <- get_pca2(swiss)
str(result)

a = summary(prcomp(swiss))
min(which(summary(prcomp(swiss, retx = T))$importance[3,] > 0.9))

#
get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fit$x[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}
#

#3.6-5
is_multicol = function(td){
  cor_m = cor(td)
  diag(cor_m) = 0
  res = rownames(which(round(abs(cor_m), digits = 5) == 1, arr.ind = T))
  if(is.null(res))
    return("There is no collinearity in the data")
  return(res)
}
#all.equal

test_data0 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
is_multicol(test_data0)
#[1] "There is no collinearity in the data"


test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
is_multicol(test_data1)
#[1] "V2" "V1"

test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
is_multicol(test_data2)
#[1] "V2" "V1" "V4" "V3"

test_data3 <- as.data.frame(list(V1 = c(2, 13, 9, 5, 11), V2 = c(17, 0, 2, -3, 24), V3 = c(11, -6, -4, -9, 18), V4 = c(9, 20, 16, 12, 18), V5 = c(14, 12, 15, 11, -8)))
is_multicol(test_data3)
#V1 V2 V3 V4

test_data4 <- as.data.frame(list(V1 = c(11, 19, 19, 12, 17), V2 = c(3, 0, 0, -3, -5), V3 = c(5, 13, 13, 6, 11), V4 = c(3, 6, 6, 9, 11), V5 = c(8, 4, 3, 9, 16)))
is_multicol(test_data4)
#V1 V2 V3 V4

#
is_multicol <- function(d){    
  d <- abs(cor(d))     
  d[lower.tri(d)] <- 0    
  diag(d) <- 0    
  index <- which((1-d) < 1e-10, arr.ind = T)    
  if (length(index) == 0){      
    return('There is no collinearity in the data')    
  } else {      
    return(rownames(d)[index])      
  }      
}
#

#3.6-6
library(ggplot2)
# ?????????????? ???????????????? ???????????????????? cluster ?? ???????????? swiss
dist_matrix = dist(swiss, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
fit = hclust(dist_matrix, method = "complete", members = NULL) 
swiss$cluster = factor(cutree(fit, 2))

# ?????????????????? ??????, ?????????? ???????????????? ????????????
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster)) + geom_point() + geom_smooth(method = "lm")
my_plot

#
dist_matrix <- dist(swiss)    
fit <- hclust(dist_matrix)     
swiss$cluster <- as.factor(cutree(fit, 2))    

my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster)) +      
  geom_point() +      
  geom_smooth(method = 'lm')
#