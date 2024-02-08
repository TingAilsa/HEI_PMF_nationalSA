library(RobustLinearReg)

# create x axis (t)
t <- 1:100
# create values that follow a linear relation with the x axis
x <- rnorm(100,35,4)*t/100
# add some outliers
x[c(10,12,76,34,21)] <- x[c(10,12,76,34,21)] + 40
tsen_model <- theil_sen_regression(x~t)
lm_model <- lm(x~t)
# compare linear regression with theil_sen_regression
plot(x~t)
abline(tsen_model,col='blue')
abline(lm_model,col='red')


aac8f7t1 = read.csv("/Users/TingZhang/Downloads/AAAA_C8_F7.csv")
aac8f7t1$X = NULL

lm_pmf= lm(PM2.5 ~ ., data = aac8f7t1)
summary(lm_pmf)

lm_pmf= lm(PM2.5 ~ . - 1, data = aac8f7t1)
summary(lm_pmf)

library(mblm)
# prepare inputs for Theil-Sen regression
tsen_x = as.matrix(aac8f7t1[, -which(names(aac8f7t1) == "PM2.5")]) 
tsen_y = aac8f7t1$PM2.5
tsen_model <- mblm(tsen_y ~ tsen_x - 1, repeated = TRUE)
summary(tsen_model)

