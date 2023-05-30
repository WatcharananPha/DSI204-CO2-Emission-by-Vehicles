library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(caret)
library(corrplot)
library(patchwork)


df <- read.csv("CO2 Emissions_Canada.csv")

names(df)

# Cleaning Outlier
q <- quantile(df$Engine.Size.L., probs=c(0.25,0.75))
iqr <- q[2] - q[1]
df <- df[(df$Engine.Size.L. >= q[1] - 1.5*iqr) & (df$Engine.Size.L. <= q[2] + 1.5*iqr),]
hist(df$Engine.Size.L., data = df,xlab = "Car Make", ylab = "CO2 Emissions (g/km)",
     main = "CO2 Emissions by Car Make (Cleaned Outlier)")

X <- df[, (names(df) %in% c("Fuel.Type" ,"Engine.Size.L.","Fuel.Consumption.City..L.100.km.", "Fuel.Consumption.Hwy..L.100.km.","Fuel.Consumption.Comb..L.100.km."  ))]
y <- df[, names(df) == "CO2.Emissions.g.km."]



transformer <- dummyVars(~  Fuel.Type, data= X)
new_X <- data.frame(predict(transformer, newdata = X))

X <- X[, names(X) != "Fuel.Type"]
X <- cbind(X,new_X)

corrplot(round(cor(X),2), method = "circle", type = "upper", tl.cex = 0.65,addCoef.col = "black",number.cex = 0.5
         , digits = 2)

#X <- X[, (!names(X) %in% c("Fuel.Consumption.City..L.100.km.","Fuel.Consumption.Comb..L.100.km.", "Fuel.Consumption.Hwy..L.100.km.","Fuel.TypeZ","Fuel.TypeD","Fuel.TypeN"))]
X <- X[, (!names(X) %in% c("Engine.Size.L.","Fuel.Consumption.City..L.100.km.", "Fuel.Consumption.Hwy..L.100.km.","Fuel.TypeZ"))]
corrplot(round(cor(X),2), method = "circle", type = "upper", tl.cex = 0.65,addCoef.col = "black",number.cex = 0.5
         , digits = 2)


new_df <- data.frame(cbind(X,y))

plots <- list()
feature_vars <- names(X)
for (var in feature_vars) {
  p <- ggplot(new_df, aes(x = .data[[var]], y = y)) + 
    geom_point() + 
    stat_smooth(method = "lm", se = FALSE, color = "red") +
    ggtitle(paste("Scatterplot of", var, "vs. target")) + 
    xlab(var) + 
    ylab("target")
  plots[[var]] <- p
}

wrap_plots(plots, ncol = 2)

set.seed(204)
trainIndex <- createDataPartition(new_df$y, p = 0.7, list = FALSE)
train <- new_df[trainIndex, ]
test <- new_df[-trainIndex, ] 
# Fit the regression model
model <- lm(y ~ ., data = new_df)

# Make predictions on the test set
predictions <- predict(model, newdata = test)

# Evaluate the model's performance
mse <- mean((test$y - predictions)^2)
rmse <- sqrt(mse)
r_squared <- summary(model)$r.squared
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Plot the residuals
resid <- y_pred - test$y

res_df <- data.frame(resid)

# plot residuals with horizontal line at 0
ggplot(res_df, aes(x = 1:nrow(res_df), y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Residual plot with horizontal line at 0")

shapiro.test(resid)

hist(resid)
qqnorm(model$residuals)
qqline(model$residuals)

