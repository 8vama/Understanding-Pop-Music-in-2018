library(MASS)
library(ggplot2)
library(reshape2)

library(car)
library(glmnet)
library(caret)
library(tidyverse)

mydata = read.csv("SpotifyPopSongsPopCategory.csv", header = TRUE)

# split into training data and testing data
trainData = mydata[1:2000,]
testData = mydata[2000:2259,]
attach(trainData)

# removing extra columns
trainData$key <- NULL
trainData$duration_ms <- NULL
trainData$id <- NULL
trainData$uri <- NULL
trainData$track_href <- NULL
trainData$time_signature <- NULL

# inspect the first few songs
head(trainData)

trainData$name <- NULL

# check that our data does not contain significant issues
popularity <- trainData$popularity

sd(popularity)
plot(popularity, xlab = "Popularity")
pop =data.frame(trainData$popularity)
ggplot(pop, aes(trainData$popularity))+ geom_bar()


# MODEL 1: NAIVE MLR

residualsHistogram <- function(summary){
  
  res = summary$residuals
  hist(res, breaks = 50)
}

prediction <- function(summary){
  c = summary$coefficients[,1]
  
  yhat = c[2] * testData$energy + c[3] * testData$valence + c[4] * testData$liveness+ c[5]* testData$tempo +c[6]* testData$speechiness + c[7]* testData$instrumentalness + c[8]* testData$acousticness+
    +c[9]* testData$loudness + c[10]* testData$danceability + c[1]
  
  y = testData$popularity
  
  res = yhat - y
  
  hist(res, breaks = 50)
  
}

naive_mlr <- lm((popularity) ~ energy + valence + liveness + tempo + speechiness + instrumentalness + acousticness+ loudness +danceability )
model_sum0 <- summary(naive_mlr)
model_sum0

# model checking
residualsHistogram(naive_mlr)
plot(naive_mlr)

# test the model
prediction(model_sum0)

# Kolmogorov-Smirnov test
ks.test(resid(naive_mlr)/sigma(naive_mlr), "pnorm")

# Model assumptions checking

pairs(popularity~ energy + valence + liveness + tempo + speechiness + instrumentalness + acousticness+ loudness + danceability ,data = mydata ,cex=0.01)

X <- cbind(energy , valence , liveness, tempo , speechiness , instrumentalness , acousticness, loudness , danceability)
c <- cor(X)
round(c,8)

vif(naive_mlr)

# MODEL 2: TRANSFORMATION OF THE RESPONSE
prediction1 <- function(summary){
  c = summary$coefficients[,1]
  
  yhat = c[2] * testData$energy + c[3] * testData$valence + c[4] * testData$liveness+ c[5]* testData$tempo +c[6]* testData$speechiness + c[7]* testData$instrumentalness + c[8]* testData$acousticness+
    +c[9]* testData$loudness + c[10]* testData$danceability + c[1]
  
  y = testData$popularity^1.3
  
  res = yhat - y
  
  hist(res, breaks = 50)
  
}


pop2 =data.frame(trainData$popularity^1.3)
ggplot(pop2, aes((trainData$popularity^1.3)))+ geom_bar()

trans_mlr <- lm((popularity ^1.3) ~ energy + valence + liveness + tempo + speechiness + instrumentalness + acousticness+ loudness +danceability )
model_sum9 <- summary(trans_mlr)
model_sum9

# model checking
residualsHistogram(model_sum9)
plot(trans_mlr)

# prediction
prediction1(model_sum9)


ks.test(resid(trans_mlr)/sigma(trans_mlr), "pnorm")


# MODEL 3: BOX-COX TRANSFORMATION OF THE RESPONSE

# Box-Cox transformation
bc <- boxcox(naive_mlr, optimize=TRUE)

# (lambda <- bc$x[which.max(bc$y)])
lambda <- 1.3


pop3 =data.frame((trainData$popularity^lambda -1)/lambda )
ggplot(pop3, aes((trainData$popularity^lambda -1)/lambda) )+ geom_bar()

bc_trans_mlr <- lm((popularity ^lambda -1)/lambda ~ energy + valence + liveness + tempo + speechiness + instrumentalness + acousticness+ loudness +danceability )

model_sum10 <- summary(bc_trans_mlr)
model_sum10

# model checking
residualsHistogram(model_sum10)
plot(bc_trans_mlr)

ks.test(resid(bc_trans_mlr)/sigma(bc_trans_mlr), "pnorm")

# MODEL 4: LOG-TRANSFORMATION OF SOME REGRESSORS

# visualize the regressors first
x <- data.frame(speechiness, danceability, energy, liveness, valence, instrumentalness, acousticness)
data<-melt(x)

hist <- ggplot(data, aes(x=value, fill=variable)) + geom_histogram(alpha=0.5) + coord_cartesian(xlim=c(0,1), ylim=c(0, 1000)) 
hist
ggsave("ggplot_hist.png", plot=hist)

ggplot(data.frame(tempo), aes(tempo))+ geom_freqpoly()
ggplot(data.frame(loudness), aes(loudness))+ geom_freqpoly()


prediction2 <- function(summary){
  c = summary$coefficients[,1]
  
  yhat = c[2] * testData$energy + c[3] * testData$valence + c[4] * log(testData$liveness) + c[5]* testData$tempo +c[6]* log(testData$speechiness) + c[7]* testData$instrumentalness + c[8]* testData$acousticness+
    +c[9]* testData$loudness + c[10]* testData$danceability + c[1]
  
  y = (testData$popularity^1.3 - 1)/1.3
  # 
  #   par(mfrow = c(1, 2))
  res = yhat - y
  
  hist(res, breaks = 50)
  
}

# log transformation
log_mlr <- lm((popularity^1.3 - 1)/1.3 ~ energy + valence + log(liveness) + tempo + log(speechiness) + instrumentalness + acousticness + loudness + danceability )
model_sum4 <- summary(log_mlr)
model_sum4


plot(log_mlr)

prediction2(model_sum4)


# MODEL 5: CATEGORICAL ANALYSIS

# check the distribution of the binary variable
F <- mode
table(F)


prediction3 <- function(summary){
  c = summary$coefficients[,1]
  
  yhat = c[2] * testData$energy + c[3] * testData$valence + c[4] * log(testData$liveness) + c[5]* testData$tempo +c[6]* log(testData$speechiness) + c[7]* testData$instrumentalness + c[8]* testData$acousticness+
    +c[9]* testData$loudness + c[10]* testData$danceability +c[11]*testData$mode + c[1]
  
  
  y = testData$popularity
  
  res = yhat - y
  
  hist(res, breaks = 50)
  
}

testData$popularity <- (testData$popularity^1.3-1)/1.3
popularity <- (popularity^1.3-1)/1.3
trainData$popularity <- (trainData$popularity^1.3-1)/1.3

categorical_mlr <- lm(popularity ~ energy + valence + log(liveness) + tempo + log(speechiness) + instrumentalness + acousticness + loudness + danceability + mode  )

model_sum1 <- summary(categorical_mlr)
model_sum1
# modelChecking2(model_sum1)


residualsHistogram(categorical_mlr)
plot(categorical_mlr)

prediction3(model_sum1)


# MODEL 6: REDUCED MODEL


# backwards AIC
backAIC <- step(categorical_mlr,direction="backward", data = trainData)
# backwards BIC
backBIC <- step(categorical_mlr,direction="backward", data = trainData, k=log(n))

# forward AIC
mint <- lm(popularity~1,data=trainData)
forwardAIC <- step(mint,scope=list(lower=~1,
                                   upper=~energy + valence + log(liveness) + tempo + log(speechiness) + instrumentalness + acousticness + loudness + danceability + mode  ),
                   direction="forward", data=trainData)

# forward BIC
forwardBIC <- step(mint,scope=list(lower=~1, 
                                   upper=~energy + valence + log(liveness) + tempo + log(speechiness) + instrumentalness + acousticness + loudness + danceability + mode ),
                   direction="forward", data=trainData,k=log(n))


final_mlr <- lm(popularity ~ energy + valence + log(liveness) + acousticness + loudness + danceability + mode  )

model_sum8 <- summary(final_mlr)
model_sum8

plot(final_mlr)


# MODEL 7: RIDGE, LASSO, ELASTIC NET
# Data = considering that we have a data frame named dataF, with its first column being the class
popularity

# Data = considering that we have a data frame named dataF, with its first column being the class
X <- as.matrix(cbind(energy , valence , log(liveness), acousticness, loudness , danceability)) # Removes class
y <- as.double(as.matrix(popularity)) # Only class

dataXY <- as.matrix(cbind(energy , valence , log(liveness),  acousticness, loudness , danceability, popularity))

testDataXY <- as.matrix(cbind(testData$energy , testData$valence , log(testData$liveness),  testData$acousticness, testData$loudness , testData$danceability, testData$popularity))
#Ridge Regression
fit.ridge <- glmnet(X, y, family="gaussian", alpha=0)

#LASSO
fit.lasso <- glmnet(X, y, family="gaussian", alpha=1)

#Elastic Net
fit.elnet <- glmnet(X, y, family="gaussian", alpha=.5)


#Find optimal value of tuning parameter - uses cross validation 
cv_lambda<-cv.glmnet(X, y, alpha=0)
cv_lambda$lambda.min
plot(cv_lambda)
coef(cv_lambda)


#Find the optimal lambda tuning parameter
for (i in 0:10) {
  assign(paste0("fit", i), cv.glmnet(X, y, type.measure="mse", 
                                     alpha=i/10,family="gaussian"))
}

par(mfrow=c(3,2))

# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

lambda <- 10^seq(-3, 3, length = 100)

ridge <- train(
  popularity ~., data = trainData, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)

# Make predictions
predictions <- ridge %>% predict(testData)


# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testData$popularity),
  Rsquare = R2(predictions, testData$popularity)
)


lasso <- train(
  popularity ~., data =trainData, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)

# Make predictions
predictions <- lasso %>% predict(testData)

# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testData$popularity),
  Rsquare = R2(predictions, testData$popularity)
)

# Build the model
elastic <- train(
  popularity ~., data = trainData, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

# Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)

# Make predictions
predictions <- elastic %>% predict(testData)

# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testData$popularity),
  Rsquare = R2(predictions, testData$popularity)
)

models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")







