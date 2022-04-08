myvar1 = 42
vec1 = 1:62
vec2 = c(-1,2,3,6,12, 23)

vec2[1:3]
the_best_vector = c(1:5000, 7000:10000)


the_best_vector[the_best_vector < 20 | (the_best_vector < 30 & the_best_vector > 21)]


vec1[vec1 > mean(vec1)]

data[[1]][2]

df = data.frame(col = data)

my_vector = -5:1
my_vector_2 <- my_vector[abs(my_vector - mean(my_vector)) < sd(my_vector)]
mean(my_vector)-sd(my_vector)

library(readr)

mydata = read.csv('R/evals.csv', stringsAsFactors=TRUE)

str(mydata)
summary(mydata)
names(mydata)

mydata$gender

mydata$ten_point_score = mydata$score * 2

mydata$row_num = 1:nrow(mydata)

nrow(mydata)
ncol(mydata)

mydata[1,2]

mydata[5,]
mydata[,1] == mydata$score

mydata[mydata$gender=='male', 1:3]

subset(mydata, gender=='female')

md2 = subset(mydata, gender=='male')
md3 = subset(mydata, gender=='female')

md4 = rbind(md2, md3)

md5 = mydata[,1:10]
md6 = mydata[,11:23]
md7 = cbind(md5, md6)


library(help = "datasets")
help(mtcars)
my_data = mtcars


my_data$even_gear = (mtcars$gear+1) %% 2

mpg_4 <- mtcars$mpg[mtcars$cyl == 4]

mini_mtcars = mtcars[c(3,7,10,12,nrow(mtcars)),]

new_data <- subset(mtcars, cyl != 3 & qsec > mean(qsec))

a = -2
if (a>0)
  print('pos')


if (a>0){
  print('pos')
} else if (a == 0) {
  print('0')
} else {
  print('neg')
}

a = c(1,-1)

ifelse(a>0, 'pos', 'not pos')

for (i in 1:100){
  print(i)
}

md2$quality = ifelse(md2$score > 4, 'good', 'bad')

i=1
while (i<51){
  print(i)
  i = i + 1
}

mtcars$new_var = ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)

str(AirPassengers)
?AirPassengers
AirPassengers


good_months = c()

for (i in 2:length(AirPassengers)){
  if (AirPassengers[i-1] < AirPassengers[i])
    good_months = append(good_months, AirPassengers[i])
}

good_months

moving_average <- numeric(length(AirPassengers)-10+1)

for (i in 10:length(AirPassengers)){
  moving_average[(i-9)] = mean(AirPassengers[(i-9):i])
}

moving_average[length(AirPassengers)-10+1]=mean(AirPassengers[135:144])

moving_average

#1.5

df = mtcars
str(df)

df$vs = factor(df$vs, labels=c("V","S"))

df$am = factor(df$am, labels=c("auto","manual"))

median()
mean()
sd()
range()

mean_disp = mean(df$disp)

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

result = mean(mtcars$qsec[mtcars$cyl !=3 & mtcars$mpg > 20])

mean_hp_vs = aggregate(x=df$hp, by = list(df$vs), FUN = mean)

colnames(mean_hp_vs) = c('VS', 'mean hp')

mean_hp_vs = aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs+am, df, mean)

aggregate(x=df[,-c(8,9)], by=list(df$am), FUN = median)

aggregate(x=df[,c(1,3)], by=list(df$am,df$vs), FUN = sd)

aggregate(cbind(mpg,disp) ~ am + vs, df, sd)

aggregate(cbind(hp,disp) ~ am, df, sd)

library(psych)
library(ggplot2)

?describe

describe(df)
descr = describe(df[,-c(8,9)])

dby_list = describeBy(x = df[,-c(8,9)], group = df$vs, digits = 1)

dby_mat = describeBy(x = df[,-c(8,9)], group = df$vs, mat = TRUE, digits = 1)

dby3 = describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)

dby4 = describeBy(df$qsec, group = list(df$vs, df$am), mat = T, digits = 1, fast = T)

dby5 = describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, fast = T)
dby5

df = mtcars

sum(is.na(df))

df$mpg[1:10] = NA

sum(is.na(df))

mean(df$mpg, na.rm = T)

aggregate(mpg ~ am, df, sd)
aggregate(mpg ~ am, df, sd, na.rm = T)

x <- 5
x %in% c(3, 4, 5)

df = airquality
df
df_m = subset(df, Month %in% c(7,8,9))
result = aggregate(Ozone ~ Month, df_m, FUN = length)

length(df_m$Ozone)

sum(is.na(df_m$Ozone))

dbyX = describeBy(df, group = list(df$Month))
dbyX


df = iris


df[df$Species == 'virginica']

virginicas = subset(df, Species=='virginica', select = -Species)
d_vir = describe(virginicas)

my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] = NA

fixed_vector = replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))

my_vector
fixed_vector

#1.6

df = mtcars

df$vs = factor(df$vs, labels=c("V","S"))

df$am = factor(df$am, labels=c("auto","manual"))

hist(df$mpg, breaks=20, xlab = 'MPG')

boxplot(mpg~am, df, ylab = 'MPG')

plot(df$mpg, df$hp)

ggplot(df, aes(x=mpg))+geom_histogram(fill = "white", col = "black", binwidth = 2)

ggplot(df, aes(x=mpg))+geom_dotplot()

ggplot(df, aes(x=mpg, fill=am))+geom_density(alpha=0.3)

ggplot(df, aes(x=am, y=hp))+geom_boxplot()

ggplot(df, aes(x=am, y=mpg, col=vs))+geom_boxplot()


ggplot(df, aes(x=mpg, y=hp, col=vs))+geom_point(size=5)

ggplot(df, aes(x=mpg, y=hp, col=vs, size = qsec))+geom_point()

myplot = ggplot(df, aes(x=mpg, y=hp, col=vs, size = qsec))

myplot +geom_point()

#

df1 = airquality

boxplot(Ozone~Month, df1)

plot1 = ggplot(df, aes(x=mpg, y = disp, col = hp))+geom_point()

###
# import pandas as pd
# import seaborn as sns
# 
# mtcar = pd.read_csv(mtcar_path)
# sns.scatterplot(data=mtcar, x="mpg", y="disp", hue="hp")
###


ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))

ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()

ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()

#ggplot(iris, aes(Sepal.Length)) + geom_histogram(fill = Species)

ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(col = Species))


#ggplot(aes(Sepal.Length, Sepal.Width, col = Species)) + geom_point(iris, size = Petal.Length)

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species, size = Petal.Length)) + geom_point()
#????????????????????, ???????????????????? ???? ???????????? ?????????? ???????????? ???????? ?????????????? ???????????? ?????????????? aes()
#???????????? ???????????????????? ?????????????? ggplot() ???????????? ???????? ???????????? iris.

#

getwd()
setwd("~/R")

write.csv(df, "df.csv")

desc_df = describe(df)
write.csv(desc_df, "desc_df.csv")

my_mean = mean(10**6:10^9)
save(my_mean, file="my_mean.Rdata")
