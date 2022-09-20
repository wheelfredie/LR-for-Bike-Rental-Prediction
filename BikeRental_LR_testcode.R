#----
setwd("C:/ST1131 Datasets")


df= read.csv("day.csv")
attach(df)

#1 ----

y= cnt

summary(y)
sd(y)

hist(y)

plot(y, main="Scatterplot of y")

boxplot(y, main="Boxplot of y")

length(y)


#2 ----

#Response and season (Categorical) 
boxplot(y~season)

new_season= ifelse(season==1, "spring", "not_spring")
boxplot(y~new_season)

#Response and workingday (Categorical)
boxplot(y~workingday)

#Response and weathersit (Categorical)
boxplot(y~weathersit)

new_weathersit= ifelse(weathersit==3, "bad_weather", "good_weather")
boxplot(y~new_weathersit)

#Response and temp (Quantitative)
cor(y, temp)
plot(temp, y, main = "Scatterplot of response against temp")
abline(lm(y~temp), col="red", lwd= 5)


#Response and hum (Quantitative)
cor(y, hum) 
plot(hum, y, main = "Scatterplot of response against hum")
abline(lm(y~hum), col="red", lwd= 5)

new_hum= df$hum^10
cor(y, new_hum) 
plot(new_hum, y, main = "Scatterplot of response against hum")
abline(lm(y~new_hum), col="red", lwd= 5)


#Response and windspeed (Quantitative)
cor(y, windspeed)
plot( windspeed,y , main = "Scatterplot of response against windspeed")
abline(lm(y~df$windspeed), col="red", lwd= 5)

new_windspeed= df$windspeed^2
cor(y, new_windspeed) 
plot(new_windspeed, y, main = "Scatterplot of response against windspeed")
abline(lm(y~new_windspeed), col="red", lwd= 5)


#3----

c_new_weathersit= factor(new_weathersit)
c_new_season= factor(new_season)

M1= lm(y~ temp + c_new_weathersit + c_new_season, data= df)
summary(M1)


#Qn4) ----
#----rejected M1 ----
#qq plot of Standardized residuals:
sr_1= rstandard(M1)
qqnorm(sr_1,datax = TRUE)
qqline(sr_1,datax = TRUE)

hist(sr_1, prob=TRUE)


# Standardized residuals vs Predicted y^
plot(M1$fitted.values,sr_1, col = 2 ,ylim=c(-4,4))
abline(h=0)
abline(h=3)
abline(h=-3)

# outliers (none)
which(sr_1>3 | sr_1<(-3))

#cook distance (none)
cook_m1= cooks.distance(M1)
which(cook_m1>1)

#Q5) ----
summary(M1)


#(M2) ----
sqrt_y= sqrt(cnt)

M2= lm(sqrt_y~ temp + c_new_weathersit + c_new_season
       + temp*c_new_weathersit + temp*c_new_season, data= df)
summary(M2)

sr_2= rstandard(M2)

hist(sr_2, prob=TRUE)

qqnorm(sr_2,datax = TRUE)
qqline(sr_2,datax = TRUE)



#(M3) ----
sqrt_y= sqrt(cnt)
new_2_hum= df$hum^2

M3= lm(sqrt_y~ temp + new_2_hum + new_windspeed 
       + c_new_weathersit + c_new_season
       + temp*c_new_season 
       , data= df)

summary(M3)

sr_3= rstandard(M3)

hist(sr_3, prob=TRUE)

qqnorm(sr_3,datax = TRUE)
qqline(sr_3,datax = TRUE)

plot(M3$fitted.values,sr_3, col = 2 ,ylim=c(-4,4), main= "SR against y-hat")
abline(h=0)
abline(h=3)
abline(h=-3)

# outliers (none)
which(sr_3>3 | sr_3<(-3))


#cook distance (none)
cook_m3= cooks.distance(M3)
which(cook_m3>1)




