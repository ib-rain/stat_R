df = read.csv("grants.csv", stringsAsFactors = T)

str(df)

df$status = factor(df$status, labels = c("NF", "F"))

t1 = table(df$status)
t1

t2 = table(df$status, df$field)
t2

t2 = table(status=df$status, field=df$field)
t2
dim(t2)

prop.table(t2,1)
prop.table(t2,2)


t3 = table(years = df$years_in_uni, field = df$field, status = df$status)
t3


dimnames(HairEyeColor)
HairEyeColor

prop.table(HairEyeColor[,,'Male'],2)['Red','Blue']

red_men <- prop.table(HairEyeColor[,,'Male'],2)['Red','Blue']
red_men

table(HairEyeColor[,,'Female'])

sum(HairEyeColor[,,'Female'][,'Green'])

sum(HairEyeColor[, 'Green','Female'])

barplot(t1)
barplot(t2, legend.text = T, args.legend = list(x="topright"))
barplot(t2, legend.text = T, args.legend = list(x="topright"), beside = T)

mosaicplot(t2)


mydata <- as.data.frame(HairEyeColor)

obj <- ggplot(data = subset(mydata, Sex == 'Female'), aes(x = Hair, y = Freq, fill = Eye)) + geom_bar(stat="identity", position = 'dodge' ) + scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj

subset(mydata, Sex == 'Female')


binom.test(x=5, n=20, p=0.5)

binom.test(t1)
#declines are not more probably statistically under alpha=0.05 (although pretty close)

chi = chisq.test(t1)
chi$exp
chi$obs

chisq.test(t2)


fisher.test(t2)

brown = subset(as.data.frame(HairEyeColor), Hair == 'Brown' & Sex == 'Female')
brown$Freq

tb = table(brown$Eye,brown$Freq)
tb

chisq.test(brown$Freq)

HairEyeColor["Brown", , "Female"]
chisq.test(HairEyeColor["Brown", , "Female"])


md = diamonds

td = table(md$cut, md$color)
main_stat = chisq.test(td)$statistic
main_stat


md$factor_price = as.factor(as.integer(md$price > mean(md$price)))
md$factor_price

md$factor_carat = as.factor(as.integer(md$carat > mean(md$carat)))
md$factor_carat

td = table(md$factor_price, md$factor_carat)
main_stat = chisq.test(td)$statistic
main_stat
td

#
diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0))    
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0))    
main_stat_master <- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic
#

mt = mtcars
tt = table(mt$am, mt$vs)
fisher_test = fisher.test(tt)$p.value

#
tbl = table(mtcars$am, mtcars$vs)    
fit  <- fisher.test(tbl)    
fisher_test_master = fit$p.value
#

#2.2

df = iris
str(df)

df1 = subset(df, Species != 'setosa')
df2 = subset(df, Species %in% c('versicolor', 'virginica'))

table(df1$Species)
table(df2$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x=Sepal.Length)) + geom_histogram(fill='white', col='black', binwidth = 0.4) + facet_grid(Species~ .)

ggplot(df1, aes(x=Sepal.Length, fill = Species)) + geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length)) + geom_boxplot()

shapiro.test(df1$Sepal.Length)
#p-value for H0 -- not different from normal, p > alpha -- accept H0


shapiro.test(df1$Sepal.Length[df1$Species == 'versicolor'])

shapiro.test(df1$Sepal.Length[df1$Species == 'virginica'])

bartlett.test(Sepal.Length ~ Species, df1)
#H0 -- homoskedastic

t1 = t.test(Sepal.Length ~ Species, df1)
#very small p, reject H0 = means are not equal

str(t1)
t1$p.value


t1 = t.test(Sepal.Length ~ Species, df1, var.equal = T)

t.test(df1$Sepal.Length, mu = 8)
t.test(df1$Sepal.Length, mu = 6.2)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)


t_stat = t.test(ToothGrowth$len[ToothGrowth$supp == 'OJ' & ToothGrowth$dose == 0.5], ToothGrowth$len[ToothGrowth$supp == 'VC' & ToothGrowth$dose == 2])$statistic

#
correct_data <- subset(ToothGrowth, supp=='OJ' & dose==0.5 | supp=='VC' & dose==2)    
t_stat <- t.test(len ~ supp, correct_data)$statistic
#

dl = read.csv("lekarstva.csv")
t.test(dl$Pressure_before, dl$Pressure_after, paired = T)

df1 = subset(df, Species != 'setosa')

ggplot(df1, aes(Species, Sepal.Length)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar") + 
  stat_summary(fun = mean, geom = 'point', size = 4)


ggplot(df1, aes(Species, Sepal.Length)) + stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 2)


wilcox.test(Petal.Length ~ Species, df1)

ggplot(df1, aes(Species, Sepal.Length)) + geom_boxplot()

wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)


d = read.table("dataset_11504_15.txt")

if (bartlett.test(V1 ~ V2, d)$p.value >= 0.05){
  x = t.test(V1 ~ V2, d, var.equal = T)$p.value
} else x = wilcox.test(V1 ~ V2, d)$p.value
round(x, 4)

x = t.test(V1 ~ V2, d)
str(x)
print(round(x$estimate,2))
print(x$p.value)


d = read.table("dataset_11504_16.txt")

x = t.test(d$V1, d$V2)
if (x$p.value < 0.05) {
  print(c(round(mean(d$V1), 2), round(mean(d$V2), 2), round(x$p.value,2)))
} else print("The difference is not significant")

#
df <- read.csv("dataset_11504_16.txt", sep = " ", header = FALSE)
ifelse(t.test(df$V1, df$V2)$p.value < 0.05, 
       print(c(mean(df$V1), mean(df$V2), t.test(df$V1, df$V2)$p.value)), 
       print("The difference is not significant"))
#

#2.3 anova

mydata = read.csv('shops.csv')

boxplot(price ~ origin, mydata)

ggplot(mydata, aes(x = origin, y = price)) + geom_boxplot()

fit = aov(price~origin, mydata)
summary(fit)


fit1 = aov(price~origin+store, mydata)
summary(fit1)
# * means statistically significant effect

model.tables(fit1, 'means')

# Interaction

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)


fit5 = aov(yield ~ N * P, npk)
summary(fit5)

fit6 = aov(yield ~ N + P + K, npk)
summary(fit6)



# Pairwise comparisons

ggplot(mydata, aes(x = food, y = price)) + geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)


TukeyHSD(fit5)

ggplot(iris, aes(x = Species, y = Sepal.Width)) + geom_boxplot()
fit6 <- aov(Sepal.Width ~ Species, iris)
summary(fit6)


TukeyHSD(fit6)


#

mydata2 = read.csv('therapy_data.csv', stringsAsFactors = T)
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)


fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)


fit2 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()

fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)


fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)

#

md3 = read.csv('Pillulkin.csv', stringsAsFactors = T)

md3$patient = as.factor(md3$patient)

str(md3)

fit = aov(temperature ~ pill + Error(patient/pill), md3)
summary(fit)

fitb = aov(temperature ~ pill * doctor + Error(patient/(pill + doctor)), md3)
summary(fitb)

obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2)) 

obj

#2.4

mycalc = function(x,y=10){
  return(c(x+y,x-y))
}
mycalc(10,15)

mycalc(10)

distr1 = rnorm(100)
hist(distr1)

distr1[1:30] = NA

distr1[is.na(distr1)] = mean(distr1, na.rm = T)

distr1

my_na_rm = function(x){
  if (is.numeric(x)){
    if (shapiro.test(x)$p.value > 0.05){
      print('Normal, using mean')
      x[is.na(x)] = mean(x, na.rm = T)
    } else {
      print('Not normal, using median')
      x[is.na(x)] = median(x, na.rm = T)
    }
    return(x)
  } else print('X is not numeric')
  
}

distr1 = rnorm(100)
hist(distr1)

distr1[1:30] = NA
distr1[is.na(distr1)] = mean(distr1, na.rm = T)

distr1

distr1[1:30] = NA
my_na_rm(distr1)

d1 = rnorm(100)
d2 = runif(200)

d1[1:30] = NA
d2[1:30] = NA

my_na_rm(d1)
my_na_rm(d2)

source('my_na_rm.R')

#

NA.position = function(x){
  which(is.na(x), arr.ind = T)
}

my_vector <- c(1, 2, 3, NA, NA)
NA.position(my_vector)
#[1] 4 5

NA.counter = function(x){
  sum(is.na(x))
}

NA.counter(my_vector)

#

setwd('Grants data/')
dir()

read_csvs = function(){
  df = data.frame()
  n_read <<- 0
  for (i in dir(pattern = '*.csv')){
    temp_df = read.csv(i)
    df = rbind(temp_df, df)
    n_read <<- n_read + 1
  }
  print(paste(as.character(n_read), "files were read"))
  return(df)
}

grants = read_csvs()
n_read

# n <<- 0 -- scoping assignment (you can access it outside the function)

filtered.sum = function(x){
  return(sum(x[x>0], na.rm = T))
}
filtered.sum(c(1, -2, 3, NA, NA))

my_vector = c(-6.83, -6.26, -0.99, -0.14, 0.38, 1, 0.52, -0.9, -0.09, -0.99, -1.76, -3.95, 4.51, 1.64, -2.57, 0.2, 16.65, 18.02, 1.27, 3.05, 18.84, 1.64, 0.63, 1.05, -1.87, -0.32, -2.48, 0.97, -0.27, 1.03)


outliers.rm <- function(x){
  iqr = IQR(x, na.rm = T)
  upper = quantile(x, probs = 0.75, na.rm = T, names = F) + 1.5 * iqr
  lower = quantile(x, probs = 0.25, na.rm = T, names = F) - 1.5 * iqr
  
  return(x[!(x > upper | x < lower)])
}

outliers.rm(my_vector)

#
outliers.rm <- function(x){    
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  return(x[abs(x - q/2) <= 2*IQR(x)])
}
#

