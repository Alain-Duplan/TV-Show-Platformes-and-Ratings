tv <- read.csv("C:/Users/ALAIN/Downloads/archive/tv_shows.csv")
tv$Rotten.Tomatoes <- as.numeric(sub("%", "",tv$Rotten.Tomatoes,fixed=TRUE))/100
head(tv)
tv <- tv[complete.cases(tv),]
tv.df <- tv [, 3:11]
library(caTools)
library(randomForest)
library(Metrics)
library(caret)
set.seed(1234)
spec <- c(train = .6, test = .4)

g = sample(cut(
  seq(nrow(tv.df)), 
  nrow(tv.df)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(tv.df, g)
train <- res$train
test <- res$test
Imdb.forest <- randomForest(IMDb~., data=train[,-4], mtry = 4, importance =T, na.action=na.omit )
varImpPlot(Imdb.forest, main = "Random Foresting Imdb")
Rotten.forest <- randomForest(Rotten.Tomatoes~., data=train[,-3], mtry = 4, importance =T, na.action=na.omit )
varImpPlot(Rotten.forest, main = "Random Foresting Rotten Tomatoes")
imdb.pred <- predict(Imdb.forest, test)
rotten.pred <- predict(Rotten.forest, test)
imdb.rmse <- rmse(test$IMDb, imdb.pred)
rotten.rmse <- rmse(test$Rotten.Tomatoes, rotten.pred)
imdb.res <- test$IMDb - imdb.pred
imdb.sum <- sum(imdb.res)
rotten.res <- test$Rotten.Tomatoes - rotten.pred 
rotten.sum <- sum(rotten.res)
hist(imdb.res, main = "IMDB Error", xlab = "Residual")
hist(rotten.res, main = "Rotten Tomatoes Error" , xlab = "Residual")
imdb.avg <- mean(imdb.res)
rotten.avg <- mean(rotten.res)

