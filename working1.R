setwd("C:/Projects/springboard-capstone")
envdat <- read.delim("../data/envdat.txt") # Previously created tidy dataset
envdat_inseason <- subset(envdat, day_of_yr >= 90 & day_of_yr < 300) # U.S. Midwest crop growing season
envdat_train <- subset(envdat_inseason, year < 2010)
envdat_test <- subset(envdat_inseason, year >= 2010)
#envdat_train <- subset(envdat, year < 2010)
#envdat_test <- subset(envdat, year >= 2010)

# Correlation among dependent variable (agdu) and independent variables
# cor(dplyr::select(envdat, -county, -date)) # all numeric variables
# cor(dplyr::select(envdat, agdu, day_of_yr, latitude, year, ersst))

envdat_num <- dplyr::select(envdat_inseason, -county, -date)

library(ggplot2)
library(reshape)

data <- names(dimnames(melt(cor(select(envdat_inseason, -county, -date)))


library(dplyr)
library(tidyr)

cor_highest <- melt(cor(select(envdat_inseason, -county, -date))) %>%
  dplyr::rename(cor_coeff = value) %>%
  filter(abs(cor_coeff) > 0.3 & cor_coeff != 1) %>%
  arrange(X1, X2)

cor1 <- data.frame(cor(dplyr::select(envdat_inseason, -county, -date))) # all numeric variables
cor2 <- cbind(rename(data.frame(rownames(cor1)), var1 = rownames.cor1.), cor1) %>%
  gather(var2, cor_coeff, -var1) %>%
  filter(abs(cor_coeff) > 0.3 & cor_coeff != 1) %>%
  arrange(var1, var2)

cor2 <- tidyr::gather(
  cbind(dplyr::rename(data.frame(rownames(cor1)), var1 = rownames.cor1.), cor1),
  var2, cor_coeff, -var1)

cor2 <- tidyr::gather(
  cbind(data.frame(rownames(cor1)), cor1),
  var2, cor_coeff, -rownames.cor1.)

head(tidyr::gather(x, key, val, -agdu))

%>% dplyr::arrange()
#write.csv(x, "C:/Users/TLGOCK/Documents/temp/x.csv")

library(ggplot2)
library(reshape2)
corplot <- qplot(x=Var1, y=Var2, data=melt(cor(
  dplyr::select(envdat_inseason, -county, -date), use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))
corplot2 <- corplot+theme(axis.text.x=element_text(angle=90,vjust = 0))
print(corplot2)

gs1 <- lm(agdu ~ day_of_yr + latitude + ersst, data = envdat_train)
summary(gs1)
# Model 1 test data
pred1 <- predict(gs1, newdata = envdat_test)
gs1pred <- predict(gs1, envdat_test)
xx <- data.frame(obs = envdat_test$agdu, pred = gs1pred^2)
## R sq for test data
caret::defaultSummary(xx)
plot(predict(gs1), residuals(gs1))
plot(xx)

gs1 <- lm(sqrt(agdu) ~ day_of_yr + latitude + ersst, data = envdat_train)
summary(gs1)
# Model 1 test data
pred1 <- predict(gs1, newdata = envdat_test)
gs1pred <- predict(gs1, envdat_test)
xx <- data.frame(obs = envdat_test$agdu, pred = gs1pred^2)
## R sq for test data
caret::defaultSummary(xx)
plot(predict(gs1), residuals(gs1))
plot(xx)

gs1 <- lm(sqrt(agdu) ~ poly(day_of_yr, 3) + latitude + ersst, data = envdat_train)
summary(gs1)
# Model 1 test data
pred1 <- predict(gs1, newdata = envdat_test)
gs1pred <- predict(gs1, envdat_test)
xx <- data.frame(obs = envdat_test$agdu, pred = gs1pred^2)
## R sq for test data
caret::defaultSummary(xx)
plot(predict(gs1), residuals(gs1))
plot(xx)



#By far, the strongest correlation between response variable agdu and potential predictor variables was with day of year (0.93). However, this relationship is known to be non-linear since GDUs tend to accumulate more slowly during the early spring, and late fall than they do during the summer.

plot(envdat_inseason$day_of_yr, envdat_inseason$agdu)

#To examine the relationship further, a regression model was considered for the training data set () with only day_of_yr. Plotting predicted values versus residuals shows a non-linear distribution with high heteroscedasticity.
lm1 <- lm(agdu ~ day_of_yr, data = envdat_train)
plot(predict(lm1), residuals(lm1))

#A cubic polynomial model addresses the issue of non-linearity but heteroscedasticity remains.
lm1 <- lm(agdu ~ poly(day_of_yr, 3), data = envdat_train)
plot(predict(lm1), residuals(lm1))

#Heteroscedasticity was addressed with a square root tranformation of response varible agdu, as suggested by ...
lm1 <- lm(sqrt(agdu) ~ poly(day_of_yr, 3), data = envdat_train)
plot(predict(lm1), residuals(lm1))



library (leaps)
envdat_train2 <- dplyr::select(envdat_train, -county, -date)
envdat_train2$doy2 <- envdat_train2$day_of_yr^2
envdat_train2$doy3 <- envdat_train2$day_of_yr^3

gs1f <- regsubsets(sqrt(agdu) ~ . , data = envdat_train2, nvmax = 10, method = "forward")
summary(gs1f)
gs1b <- regsubsets(sqrt(agdu) ~ . , data = envdat_train2, nvmax = 10, method = "backward")
summary(gs1b)

gs1 <- regsubsets(sqrt(agdu) ~ . , nbest = 1, data = envdat_train2)
plot(gs1, scale = "adjr2")
library(car)
subsets(gs1, statistic="adjr2")







# model 1
gs1 <- lm(sqrt(agdu) ~ poly(day_of_yr, 3) + latitude + year + ersst, data = envdat_train)
summary(gs1)
plot(gs1)

gs1pred <- predict(gs1, envdat_test)
xx <- data.frame(obs = envdat_test$agdu, pred = gs1pred^2)
caret::defaultSummary(xx)
plot(xx)

#xy <- data.frame(envdat_test$county, envdat_test$day_of_yr, agdu_obs = envdat_test$agdu, agdu_pred = gs1pred^2)
xy <- data.frame(envdat_test, agdu_pred = gs1pred^2)

loc1 <- "Iowa County, IA"
plant_yr1 <- 2011
plant_day1 <- 102
gdu_mat1 <- 1300

gdu_mat2 <- 1500

xy1 <- subset(xy, county == loc & year == plant_yr & day_of_yr == plant_day - 1)
xy2 <- dplyr::mutate(subset(xy, county == loc & year == plant_yr & day_of_yr >= plant_day),
                     agdu_ap_pred = agdu_pred - xy1$agdu_pred)
xy3 <- dplyr::filter(xy2, abs(agdu_ap_pred - gdu_mat) == min(abs(agdu_ap_pred - gdu_mat)))

xy3s2 <- dplyr::filter(xy2, abs(agdu_ap_pred - gdu_mats2) == min(abs(agdu_ap_pred - gdu_mats2)))


plant_yr
plant_day
gdu_mat
xy3$day_of_yr
xy3$date

library(ggplot2)
ggplot(data = xy2, aes(x = day_of_yr, y = agdu_ap_pred)) +
  geom_line(aes(group=1), color = "blue", size = 1) +
  geom_segment(aes(x = 100, y = gdu_mat, xend = xy3$day_of_yr, yend = gdu_mat),
               size = 1, linetype = 2) +
  geom_segment(aes(x = xy3$day_of_yr, y = 0, xend = xy3$day_of_yr, yend = gdu_mat),
               size = 1, linetype = 2)
  



ggplot(data = xy2, aes(x = day_of_yr, y = agdu_ap_pred)) +
  geom_line(aes(group=1), color = "blue", size = 2) +
geom_line(data = xy3, aes(x = day_of_yr, y = agdu_ap_pred)) +
  geom_line(aes(group=2), color = "red", size = 1)



#####
# Scenario 1 inputs:
loc1 <- "Iowa County, IA"
plant_yr1 <- 2011
plant_day1 <- 102
gdu_mat1 <- 1300

# Scenario 2 inputs:
loc2 <- "Iowa County, IA"
plant_yr2 <- 2011
plant_day2 <- 116
gdu_mat2 <- 1200

scenario1.1 <- subset(xy, county == loc1 & year == plant_yr1 & day_of_yr == plant_day1 - 1)
scenario1.2 <- dplyr::mutate(subset(xy, county == loc1 & year == plant_yr1 & day_of_yr >= plant_day1),
                             agdu_ap_pred = agdu_pred - scenario1.1$agdu_pred)
scenario1.3 <- dplyr::filter(scenario1.2, abs(agdu_ap_pred - gdu_mat1) == min(abs(agdu_ap_pred - gdu_mat1)))

scenario2.1 <- subset(xy, county == loc2 & year == plant_yr2 & day_of_yr == plant_day2 - 1)
scenario2.2 <- dplyr::mutate(subset(xy, county == loc2 & year == plant_yr2 & day_of_yr >= plant_day2),
                             agdu_ap_pred = agdu_pred - scenario2.1$agdu_pred)
scenario2.3 <- dplyr::filter(scenario2.2, abs(agdu_ap_pred - gdu_mat2) == min(abs(agdu_ap_pred - gdu_mat2)))

library(ggplot2)
ggplot(mapping = aes(x = day_of_yr, y = agdu_ap_pred)) +
  geom_line(data = scenario1.2, color = "blue", size = 1) +
  geom_segment(aes(x = min(scenario1.2$day_of_yr), y = gdu_mat1,
                   xend = scenario1.3$day_of_yr, yend = gdu_mat1),
               size = 1, linetype = 2) +
  geom_segment(aes(x = scenario1.3$day_of_yr, y = 0,
                   xend = scenario1.3$day_of_yr, yend = gdu_mat1),
               size = 1, linetype = 2) +
  geom_line(data = scenario2.2, color = "red", size = 1) +
  geom_segment(aes(x = min(scenario2.2$day_of_yr), y = gdu_mat2,
                   xend = scenario2.3$day_of_yr, yend = gdu_mat2),
               size = 1, linetype = 2) +
  geom_segment(aes(x = scenario2.3$day_of_yr, y = 0,
                   xend = scenario2.3$day_of_yr, yend = gdu_mat2),
               size = 1, linetype = 2)


# Alt method:
pred1 <- predict(gs1, newdata = envdat_test)
#write.table(pred1, "C:/Users/TLGOCK/Documents/temp/pred1.txt", sep = "\t")
#plot(pred1)
sse = sum((sqrt(envdat_test$agdu) - pred1)^2)
sst = sum((sqrt(envdat_test$agdu) - mean(sqrt(envdat_test$agdu)))^2)
1 - sse/sst


library(dplyr)
el_nino <- read.csv("../data/el_nino.csv")
el_nino <- rename(el_nino, year = Year, "1" = DJF, "2" = JFM, "3" = FMA,
                  "4" = MAM, "5" = AMJ, "6" = MJJ, "7" = JJA, "8" = JAS,
                  "9" = ASO, "10" = SON, "11" = OND, "12" = NDJ)
el_nino2 <- tidyr::gather(el_nino, "month", "ersst_plus6mo", 2:13)
el_nino2 <- mutate(el_nino2, month = as.integer(el_nino2$month))
el_nino2 <- mutate(el_nino2, year_plus6mo = ifelse(month < 7, year, year + 1))
el_nino2 <- mutate(el_nino2, month_plus6mo = ifelse(month < 7, month + 6, month - 6))

envdat <- left_join(envdat, el_nino2, by = c("year" = "year_plus6mo", "month" = "month_plus6mo"))


unique(envdat$month[envdat$month < 5])

plot(envdat$day_of_yr, envdat$gdu)



cor.test(envdat$day_of_yr, envdat$agdu, method = 'pearson')
plot(envdat$day_of_yr, envdat$agdu)

cor.test(growseason$day_of_yr, growseason$agdu, method = 'pearson')
plot(growseason$day_of_yr, growseason$agdu)

library("car")
scatterplotMatrix(envdat, columns = c("day_of_yr", "agdu"))

library(GGally)
ggpairs(envdat, columns = c("day_of_yr", "agdu", "county"))

# PCA on standardized variables
sddf <- scale(dplyr::select(envdat, year, day_of_yr, max_air_temp, min_air_temp, 
                            precip, ERSST))
pcdf <- prcomp(sddf)
summary(pcdf)
screeplot(pcdf, type="lines")

######################
setwd("C:/Projects/springboard-capstone")
el_nino <- read.csv("../data/el_nino.csv")
el_nino <- dplyr::rename(el_nino, year = Year, "1" = DJF, "2" = JFM, "3" = FMA,
                         "4" = MAM, "5" = AMJ, "6" = MJJ, "7" = JJA, "8" = JAS,
                         "9" = ASO, "10" = SON, "11" = OND, "12" = NDJ)
el_nino_tidy <- tidyr::gather(el_nino, "month", "ersst", 2:13)
el_nino_tidy$month <- as.integer(el_nino_tidy$month)
el_nino_lag <- dplyr::mutate(el_nino_tidy,
                             yr_lag3mo = as.integer(ifelse(month < 10, year, year + 1)),
                             mo_lag3mo = as.integer(ifelse(month < 10, month + 3, month - 9)),
                             yr_lag6mo = as.integer(ifelse(month < 7, year, year + 1)),
                             mo_lag6mo = as.integer(ifelse(month < 7, month + 6, month - 6)))
el_nino_lag3mo <- dplyr::select(el_nino_lag, year = yr_lag3mo,
                                month = mo_lag3mo, ersst_lag3mo = ersst)
el_nino_lag6mo <- dplyr::select(el_nino_lag, year = yr_lag6mo,
                                month = mo_lag6mo, ersst_lag6mo = ersst)
el_nino_all <- dplyr::left_join(el_nino_tidy, el_nino_lag3mo)
el_nino_all <- dplyr::left_join(el_nino_all, el_nino_lag6mo)
el_nino_all <- dplyr::arrange(el_nino_all, year, month)