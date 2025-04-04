# Load CSV file
birth <- read.csv("D:/STatic FOR DS/daily-total-female-births-CA.csv")
View(birth)

any(is.na(birth))
##-------------
library(tseries)
Y <- ts(birth[,2], frequency = 7)


layout(matrix(c(1,1)))

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(Y, main = "Total Female Births From 1 Jan 1959 to 31 Dec 1959 in California", xlab = "Week", ylab = "births", type='o')
acf(Y, main="ACF")
pacf(Y, main="PACF")
adf.test(Y)

library(forecast)
train <- head(Y, round(length(Y) * 0.6))
h <- length(Y) - length(train)
test <- tail(Y, h)
train
test
autoplot(train) + autolayer(test)


#SNAIVE
fit <- snaive(train, h = 7*22)
summary(fit)
checkresiduals(fit)
layout(matrix(c(1,1)))

fr <- forecast(fit,h=7*22)
plot(fr)
lines(test, col = "green")
accuracy(fit, test)


#ETS model
library(forecast)
fit1 <- ets(train)
autoplot(fit1)
summary(fit1)
checkresiduals(fit1)

fr1 <- forecast(fit1, h=7*22)
accuracy(fr1, test)
layout(matrix(c(1,1)))
plot(fr1)
lines(test, col="green")


#holt winter multi
library(forecast)
fit2 <- HoltWinters(train,
                  seasonal="mult")
fit2
summary(fit2)
checkresiduals(fit2)

layout(matrix(c(1,1)))
fr2 <- forecast(fit2, h=7*22)
accuracy(fr2, test)

forecast <- predict(object=fit2, n.ahead=7*22,
                    prediction.interval=T, level=.95)
plot(fit2,forecast)
lines(test, col="green")
