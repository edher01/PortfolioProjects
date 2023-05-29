#read the file
library(readr)
wine <- read_delim("winequality-red.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(wine)
dim(wine)

#Clean dataset
sum(is.na(wine))

#Linear Regression before CD
attach(wine)
lr <- lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
           `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, data = wine)
summary(lr)

#Cook Distance
cd.lm <- cooks.distance(lr)
cutoff <- 4 * mean(cd.lm)
influential <- which(cd.lm > cutoff)
influential
length(influential)
new_wine <- wine[!row.names(wine) %in% influential, ]
View(new_wine)
dim(wine)
dim(new_wine)

#Pay attention to correlation between predictors, if there two predictors are highly correlated (more than 75%), you should remove one of them 
new_wine_corr = cor(new_wine)
dim(new_wine_corr)
new_wine_corr [1:3, 1:3]

library(caret)
highcorr = findCorrelation(new_wine_corr, cutoff = 0.75)
length(highcorr)
highcorr

#Mutlicolinearity
library(corrplot) # For visualizing the correlation matrix
library(car)

# Compute the correlation matrix
cor_mat <- cor(wine[, -12]) # Exclude the first column (response variable)

# Visualize the correlation matrix
corrplot(cor_mat, method = "circle")

# Calculate VIF values
vif_vals <- vif(lm(quality ~ ., data = wine)) 

print(vif_vals)

#Linear Regression After CD using new_wine
#Linear Regression1 quality ~ all
lrlr <- lm(quality ~ `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
             `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, data = new_wine)
summary(lrlr)

#Residuals vs fitted plot, and other diagnostic plots
plot(lrlr)

#Linear Regression2 quality ~ acid + alc + sign var
lrlr2 <- lm(quality ~ `volatile acidity` + chlorides + 
              `free sulfur dioxide` + `total sulfur dioxide` + pH + sulphates + alcohol, data = new_wine)
summary(lrlr2)

#Residuals vs fitted plot, and other diagnostic plots
plot(lrlr2)

#Linear Regression3 quality ~ sulphates
lrlr3 <- lm(quality ~ sulphates, data = new_wine)
summary(lrlr3)

#Residuals vs fitted plot, and other diagnostic plots
plot(lrlr3)

#Linear Regression3 Log(quality)
lrlr4 <- lm(log(quality) ~ `volatile acidity` + `citric acid` + `residual sugar` +
              chlorides + log(`free sulfur dioxide`) + `total sulfur dioxide` +
              density + pH + sulphates + log(alcohol), data = new_wine)

# Print the summary of the model
summary(lrlr4)

plot(lrlr4)

#Using the commends you learnt to illustrate the influence of various confidence intervals on the results
lrlr2 <- lm(quality ~ `volatile acidity` + chlorides + 
              `free sulfur dioxide` + `total sulfur dioxide` + pH + sulphates + alcohol, data = new_wine)
summary(lrlr2)

# Calculate predicted values and confidence intervals
pred <- predict(lrlr2, interval = "confidence", level = 0.95)
pred_df <- cbind.data.frame(new_wine, pred)
plot(pred)

pred

# Plot regression line with 95% confidence interval
library(ggplot2)
ggplot(data = pred_df, aes(x = `volatile acidity` + chlorides + 
                             `free sulfur dioxide` + `total sulfur dioxide` + pH + sulphates + alcohol, y = quality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(data = pred_df, aes(x = `volatile acidity` + chlorides + 
                                    `free sulfur dioxide` + `total sulfur dioxide` + pH + sulphates + alcohol, y = fit), se = TRUE, alpha = 0.2, level = 0.95, color = "blue") +
  geom_smooth(data = pred_df, aes(x = `volatile acidity` + chlorides + 
                                    `free sulfur dioxide` + `total sulfur dioxide` + pH + sulphates + alcohol, y = lwr), se = FALSE, linetype = "dashed", level = 0.95, color = "blue") +
  geom_smooth(data = pred_df, aes(x = `volatile acidity` + chlorides + 
                                    `free sulfur dioxide` + `total sulfur dioxide` + pH + sulphates + alcohol, y = upr), se = FALSE, linetype = "dashed", level = 0.95, color = "blue")

#The solid blue line represents the regression line.
#The light blue shaded area represents the 95% confidence interval around the regression line.
#The dashed blue lines represent the upper and lower bounds of the 95% confidence interval.

attach(new_wine)
#Dummy Variable
median(quality)
quality01 = rep(0,nrow(new_wine))
quality01[quality > median(quality)] = 1

dataset01 = data.frame(new_wine,quality01)
names(dataset01)
dataset01$quality = NULL
names(dataset01)

summary(dataset01)
dim(dataset01)

set.seed(1)
train = sample(1532, 1000)

dtstrain = dataset01[train,]
View(dtstrain)
dim(dtstrain)

dtstest = dataset01[-train,]
dtstest
dim(dtstest)

cm <- glm(quality01 ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + 
            free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, dtstrain,family = "binomial")
summary(cm)

# 0.5
# make a prediction on training data:
glm.probtr01 = predict(cm, type = 'response')
glm.probtr01[0:20]

glm.predtr01 = rep(0,nrow(dtstrain))
glm.predtr01[0:20]

glm.predtr01[glm.probtr01 > 0.5] = 1
glm.predtr01[0:20]
glm.predtr01

table(glm.predtr01, dtstrain$quality01)
mean(glm.predtr01 != dtstrain$quality01)

#training error: 11.2%

# make a prediction on testing data:
glm.probts01 = predict(cm,dtstest,type = 'response')
glm.probts01[0:20]

glm.predts01 = rep(0,nrow(dtstest))
glm.predts01[0:20]

glm.predts01[glm.probts01 > 0.5] = 1
glm.predts01[0:20]

table(glm.predts01, dtstest$quality01)
mean(glm.predts01 != dtstest$quality01)
# testing error = 10.3%

# 0.4
# make a prediction on training data:
glm.probtr01 = predict(cm, type = 'response')
glm.probtr01[0:20]

glm.predtr01 = rep(0,nrow(dtstrain))
glm.predtr01[0:20]

glm.predtr01[glm.probtr01 > 0.4] = 1
glm.predtr01[0:20]
glm.predtr01

table(glm.predtr01, dtstrain$quality01)
mean(glm.predtr01 != dtstrain$quality01)

#training error: 12.6%

# make a prediction on testing data:
glm.probts01 = predict(cm,dtstest,type = 'response'
                       )
glm.probts01[0:20]

glm.predts01 = rep(0,nrow(dtstest))
glm.predts01[0:20]

glm.predts01[glm.probts01 > 0.4] = 1
glm.predts01[0:20]

table(glm.predts01, dtstest$quality01)
mean(glm.predts01 != dtstest$quality01)
# testing error = 10.9%

#kNN:
dtstrainx=cbind(new_wine)[train,]
dtstestx=cbind(new_wine)[-train,]

summary(dtstrainx)
dtstrainx = scale(dtstrainx)
summary(dtstrainx)

summary(dtstestx)
dtstestx = scale(dtstestx)
summary(dtstestx)

train.quality01=dtstrain$quality01
test.quality01=dtstest$quality01

set.seed(1)
library(class)

knn.pred1 = knn(dtstrainx, dtstestx, train.quality01, k=1)
table(knn.pred1, test.quality01)
mean(knn.pred1 != test.quality01)
# test error= 1.50%

knn.pred3 = knn(dtstrainx, dtstestx, train.quality01, k=3)
table(knn.pred3, test.quality01)
mean(knn.pred3 != test.quality01)
#test error=1.88%

knn.pred5 = knn(dtstrainx, dtstestx, train.quality01, k=5)
table(knn.pred5, test.quality01)
mean(knn.pred5 != test.quality01)
#test error=1.88%

knn.pred7 = knn(dtstrainx, dtstestx, train.quality01, k=7)
table(knn.pred7, test.quality01)
mean(knn.pred7 != test.quality01)
#test error=1.32%

knn.pred9 = knn(dtstrainx, dtstestx, train.quality01, k=9)
table(knn.pred9, test.quality01)
mean(knn.pred9 != test.quality01)
#test error=2.26%

#               Model                               Test error
# Logistic regression (threshold 0.4)                 10.9%
# Logistic regression (threshold 0.5)                 10.3%
# kNN (OPTIMAL K)                                     1.32%

#MODEL DEPLOYMENT

#LINEAR REGRESSION DEPlOYMENT
# Make a prediction for a new wine with the following characteristics:
# volatile acidity = 0.6
# chlorides = 0.08
# free sulfur dioxide = 30
# total sulfur dioxide = 100
# pH = 3.4
# sulphates = 0.7
# alcohol = 11.5
pred1 <- predict(lrlr, `volatile acidity` = 0.6, 
                 chlorides = 0.08, 
                 `free sulfur dioxide` = 30, 
                 `total sulfur dioxide` = 100, 
                 pH = 3.4, 
                 sulphates = 0.7, 
                 alcohol = 11.5)

# Make the prediction
predicted_quality <- pred1[1]

# Print the predicted quality rating
print(predicted_quality)

#Using the commends you learnt to illustrate the influence of various confidence intervals on the results
lrlr <- lm(lrlr <- lm(quality ~ `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
                        `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, data = new_wine), data = new_wine)

# Calculate predicted values and confidence intervals
pred <- predict(lrlr, interval = "confidence", level = 0.95)
pred_df <- cbind.data.frame(new_wine, pred)
plot(pred)

pred

# Plot regression line with 95% confidence interval
library(ggplot2)
ggplot(data = pred_df, aes(x = `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
                             `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, y = quality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(data = pred_df, aes(x = `volatile acidity` + chlorides + 
                                    `free sulfur dioxide` + `total sulfur dioxide` + pH + sulphates + alcohol, y = fit), se = TRUE, alpha = 0.2, level = 0.95, color = "blue") +
  geom_smooth(data = pred_df, aes(x = `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
                                    `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, y = lwr), se = FALSE, linetype = "dashed", level = 0.95, color = "blue") +
  geom_smooth(data = pred_df, aes(x = `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
                                    `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, y = upr), se = FALSE, linetype = "dashed", level = 0.95, color = "blue")

save.image(file = "myworkspace_EH.RData")
savehistory(file = "myworkspace_EH.Rhistory")

