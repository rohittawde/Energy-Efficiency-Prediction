# Linear Regression implementation in R
data <- read.csv("~/Nick/R_implementation/expanded_heating.csv", header = TRUE)

# Initializing Response and Predictors
X <- data[,1:52]
Y <- data$Y1

train_index <- sample(1:nrow(X), 0.8*nrow(X))
test_index <- setdiff(1:nrow(X), train_index)

X_train <- data[train_index, -53]
Y_train <- data[train_index, "Y1"]

X_test <- data[test_index, -53]
Y_test <- data[test_index, "Y1"]

model <- lm(Y_train~., data = X_train)
summary(model)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

y_pred <- fitted(model)
res_ <- residuals(model)
pred_test <- predict(model, X_test)

RMSE_test <- RMSE(Y_test, pred_test)
RMSE_train <- RMSE(Y_train, y_pred)

qqp <- qqnorm(res_);qqline(res_)
critical_correlation <- cor(qqp$x, qqp$y)

library(moments)

skew_ <- skewness(res_)
kurtosis_ <- kurtosis(res_)
akike <- AIC(model, k=2)
n <- nobs(model)
bayesian <- AIC(model, k = log(n))

plot(res_)

plot(resid(model) ~ fitted(model), xlab = "Fitted Values", ylab = "Residuals")
hist(res_, 15)

#library(leaps)
#leaps <- regsubsets(Y_train~.^2, data = X_train, nbest = 1, method = "exhaustive")
#summary(leaps)
#library(car)
#subsets(leaps, statistic = "cp")


