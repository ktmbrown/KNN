library(ISLR)
library(class)
library(ggplot2)
set.seed(101)

str(Caravan)


summary(Caravan$Purchase)

any(is.na(Caravan))

# Standardize the variables - scale matters!

var(Caravan[,1])

var(Caravan[,2])

purchase <- Caravan$Purchase

standardized.Caravan <- scale(Caravan[,-86])

print(var(standardized.Caravan[,1]))
print(var(standardized.Caravan[,2]))

# test and train
test.set <- 1:1000
test.data <- standardized.Caravan[test.set,]
test.purchase <- purchase[test.set]

train.data <- standardized.Caravan[-test.set,]
train.purchase <- purchase[-test.set]

# knn classification
predicted.purchase <- knn(train.data, test.data, train.purchase, k = 1)
head(predicted.purchase)

# evaluate model : misclass error
misclass.error <- mean(test.purchase != train.purchase)
print(misclass.error)

# k value evaluation
predicted.purchase <- knn(train.data, test.data, train.purchase, k = 3)

# evaluate model : misclass error
misclass.error <- mean(test.purchase != train.purchase)
print(misclass.error)

predicted.purchase <- NULL
error.rate <- NULL

for(i in 1:20) {
      set.seed(101)
      predicted.purchase<- knn(train.data,test.data, train.purchase, k = i)
      error.rate[i] <- mean(test.purchase != predicted.purchase)
}

error.rate

# visualize k elbow method
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
error.df

ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(lty='dotted', col = 'red')

# choose k = 9

