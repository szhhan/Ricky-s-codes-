# Q1
read_digits = function(PATH){
  digits = read.table(PATH)
  digits[,1] = factor(digits[,1])
  return(digits)
}

test = read_digits("F:/year 1/STA 141/digits/test.txt")

# Q2

view_digit = function(which_row, digits){
  if (is.null(nrow(digits))){
    digits = matrix(digits, nrow=1)
    if (length(digits)==257){
      digits = digits[-1]
    }
  }
  digit = digits[which_row,]
  if (length(digit)==257){
    digit = digit[,-1]
  }
  digit = matrix(as.integer(digit), 16, 16, byrow = TRUE)
  m = t(digit)  # transpose the image
  m = m[,nrow(m):1]  # turn up-side-down
  image(m, main=NULL, col = rgb((255:0)/255, (255:0)/255, (255:0)/255), xaxt = "n", yaxt = "n")
}

view_digit(10, test)

# Q3a

# plot it for the train data
train = read_digits("F:/year 1/STA 141/digits/train.txt")
train_split = split(train[,-1], train[,1])
train_split_avearge = lapply(train_split, function(x) apply(x, 2, sum))
par(mfrow=c(2,5))
lapply(train_split_avearge, function(x) view_digit(1, x)) 

# plot it for the test data
test = read_digits("F:/year 1/STA 141/digits/test.txt")
test_split = split(test[,-1], test[,1])
test_split_avearge = lapply(test_split, function(x) apply(x, 2, sum))
par(mfrow=c(2,5))
lapply(test_split_avearge, function(x) view_digit(1, x)) 

# Q3b

pro_cal = function (train){
  
  probtotal <- as.data.frame(matrix(rep_len(0,2560), nrow = 10))
  
  for (i in 1:10) {
    filter <- train[(train[,1]==(i-1)),]
    probdigit <- as.data.frame(lapply(filter[,-1],sum))/nrow(filter)
    probtotal[i,] <- probdigit
  }
  return (probtotal)
}

prob <- pro_cal(train)
colnames(prob) <- seq(256)
rownames(prob) <- seq(0,9)

min_cal =  function(prob){
  
  minnum <- as.data.frame(matrix(rep_len(0,256*2),nrow =2))
  for (i in 1:256){
    min <- prob[order(prob[,i]),]
    if ( (min[2,i]-min[1,i]) >= 0.4){
      minnum[2,i] <- which.min(prob[,i])-1
      minnum[1,i] <- i
    } else {
      minnum[2,i] <- 11
      minnum[1,i] <- i
    }
  }
  return(minnum)
}

max_cal =  function(prob){
  
  maxnum <- as.data.frame(matrix(rep_len(0,256*2),nrow =2))
  for (i in 1:256){
    max <- prob[order(-prob[,i]),]
    if ( (max[1,i]-max[2,i]) >= 0.4){
      maxnum[2,i] <- which.max(prob[,i])-1
      maxnum[1,i] <- i
    } else {
      maxnum[2,i] <- 11
      maxnum[1,i] <- i
    }
  }
  return(maxnum)
}

min <- min_cal(prob)
max <- max_cal(prob)

which(min[2,]!= 11)
which(max[2,]!= 11)



inmin_cal =  function(prob){
  
  minnum <- as.data.frame(matrix(rep_len(0,256*2),nrow =2))
  for (i in 1:256){
    min <- prob[order(prob[,i]),]
    if ( (min[2,i]-min[1,i]) <= 0.05){
      minnum[2,i] <- which.min(prob[,i])-1
      minnum[1,i] <- i
    } else {
      minnum[2,i] <- 11
      minnum[1,i] <- i
    }
  }
  return(minnum)
}

inmax_cal =  function(prob){
  
  maxnum <- as.data.frame(matrix(rep_len(0,256*2),nrow =2))
  for (i in 1:256){
    max <- prob[order(-prob[,i]),]
    if ( (max[1,i]-max[2,i]) <= 0.05){
      maxnum[2,i] <- which.max(prob[,i])-1
      maxnum[1,i] <- i
    } else {
      maxnum[2,i] <- 11
      maxnum[1,i] <- i
    }
  }
  return(maxnum)
}

inmin <- inmin_cal(prob)
inmax <- inmax_cal(prob)
which(inmin[2,] != 11)
which(maxmin[2,] != 11)



# Q4

#predict_knn = function()

#dist(test[1:10,-1], method)

# test_point includes the label part


distance = function (test, train, method){
  dis = as.matrix(dist(rbind(train[,-1], test[,-1]), method = method))
  return (dis)
}

predict_knn = function(test, train, method,k){
  
  distance2 = distance(test, train, method)
  n = nrow(test)
  result=rep_len(0,n)
  for(i in 1:n){
    testrow = as.data.frame(distance2[nrow(train)+i,1:nrow(train)])
    colnames(testrow) = "Distance"
    testrow$labels = train$V1[1:nrow(testrow)]
    order = testrow[order(testrow$Distance),]
    knn = head(order,k)
    frequencylabel = data.frame(table(knn$labels))
    colnames(frequencylabel) =c("Label","Freq")
    
    if (sum(frequencylabel$Freq == max(frequencylabel$Freq))>1) {
      temp_labels = as.character(frequencylabel[frequencylabel$Freq == max(frequencylabel$Freq),"Label"])
      result[i] = sample(temp_labels, size=1)
    } else {
      result[i] = as.character(frequencylabel$Label[which.max(frequencylabel$Freq)])
    }
  }
  return(result)
}

result = predict_knn(test = test[1:50,], train = train, method = "euclidean", k = 10)

mean(result == test[1:50,1])

result2 = predict_knn(test = test[1:730,], train = train[731:7291, ], method = "euclidean", k = 10)

mean(result2 == test[1:730,1])

# Q5


cv_error_knn <- function(k,train, method){
  
  # Data <- train[sample(nrow(train)),]
  # folds <- cut(seq(1,nrow(Data)),breaks = 10, labels = FALSE)
  # error <- matrix(rep_len(0,10), nrow =10)
  # 
  # for (i in seq(10)){
  #   testIndex <- which(folds == i, arr.ind = TRUE)
  #   testData <- Data[testIndex,]
  #   trainData <- Data[-testIndex,]
  #   prediction <- predict_knn(test = testData, train = trainData, method = method, k = k)
  #   error[i] <- mean(prediction != testData[,1])
  # }
  
  
  Data <- train[sample(nrow(train)),]
  folds <- cut(seq(1,nrow(Data)),breaks = 10, labels = FALSE)
  error <- matrix(rep_len(0,10), nrow =10)
  
  
  for (i in seq(10)){
    testIndex <- which(folds == i, arr.ind = TRUE)
    testData <- Data[testIndex,]
    trainData <- Data[-testIndex,]
    #distance2 = as.matrix(dist(rbind(trainData[,-1], testData[,-1]), method = method))
    prediction <- predict_knn(test = testData, train = trainData, method = method, k = k)
    error[i] <- mean(prediction != testData[,1])
  }
  
  
  cv_error <- mean(error)
  return(cv_error)
}


## Q6

#error <- cv_error_knn(k=2,train = train, method = "euclidean")

error <- matrix(rep_len(0,45), nrow =15, ncol = 3)


for (i in seq(15)){
  error[i,1] <- cv_error_knn(k=i,train = train, method = "euclidean")
  error[i,2] <- cv_error_knn(k=i,train = train, method = "manhattan")
  error[i,3] <- cv_error_knn(k=i,train = train, method = "minkowski")
}

colnames(error)= c("Euclidean", "Manhattan", "Minkowski")
error <- as.data.frame(error)
par(mfrow=c(1,1))
plot(error$Euclidean,  typ='l', ylim= c(0.15,0.35), xlab = "k", ylab = "Error Rate", main = "Error Rate for K-nearest Neighbors", lwd = 2)
lines(error$Manhattan, col="green", typ = 'l', lwd=2)
lines(error$Minkowski, col = "blue", typ ='l', lwd=2)
# change legend
legend("topleft",c("Euclidean", "Manhattan", "Minkowski"),col=c('black',  'green', 'blue'), cex=0.5,pt.cex=2,lty=1:1, lwd =2)

errorder <- array(t(rbind(error[,1], error[,2], error[,3])))
errorder <- sort(errorder)

which(error == errorder[1])
# k=1, Minkowski

which(error == errorder[2])
which(error == errorder[3])
# k=1, Euclidean; k= 3, Euclidean


# Q7
cross_validation <- function(k,train, method){
  
  Data <- train[sample(nrow(train)),]
  folds <- cut(seq(1,nrow(Data)),breaks = 10, labels = FALSE)
  prediction <- matrix(rep_len(0,nrow(Data)), nrow = 10)
  
  for (i in seq(10)){
    testIndex <- which(folds == i, arr.ind = TRUE)
    testData <- Data[testIndex,]
    trainData <- Data[-testIndex,]
    prediction[i,1:nrow(testData)] <- predict_knn(test = testData, train = trainData, method = method, k = k)
  }
  
  prediction <- as.numeric(prediction)
  pred_value <- array(t(prediction))
  confusion_matrix <- as.data.frame.matrix(table(unlist(train[,1]), unlist(pred_value)))
  
  return(confusion_matrix)
}

c1 <- cross_validation(k=1, train=train[1:7290, ], method = "minkowski")
c2 <- cross_validation(k=1, train=train[1:7290, ], method = "euclidean")
c3 <- cross_validation(k=3, train=train[1:7290, ], method = "euclidean")

image(1:nrow(c1), 1:ncol(c1), as.matrix(c1), col = rgb((255:0)/255, (255:0)/255, (255:0)/255) ,xlab="True Value", ylab=" Predict Value",xaxt = "n", yaxt = "n")
axis(1, at= seq(10),labels=seq(0,9))
axis(2, at= seq(10),labels=seq(0,9))
image(1:nrow(c2), 1:ncol(c2), as.matrix(c2), col = rgb((255:0)/255, (255:0)/255, (255:0)/255) ,xlab="True Value", ylab=" Predict Value",xaxt = "n", yaxt = "n")
axis(1, at= seq(10),labels=seq(0,9))
axis(2, at= seq(10),labels=seq(0,9))
image(1:nrow(c3), 1:ncol(c3), as.matrix(c3), col = rgb((255:0)/255, (255:0)/255, (255:0)/255) ,xlab="True Value", ylab=" Predict Value",xaxt = "n", yaxt = "n")
axis(1, at= seq(10),labels=seq(0,9))
axis(2, at= seq(10),labels=seq(0,9))



# Q8
misclass <- matrix(rep_len(0,10), nrow =10)

for (i in seq(10)){
  misclass[i] <- 1- c1[i,i]/sum(c1[,i])
}

rownames(misclass) = seq(0,9)
misclas2 <- as.data.frame(misclass[order(misclass)])

best_predict <- function(k,train, method){
  
  Data <- train[sample(nrow(train)),]
  
  folds <- cut(seq(1,nrow(Data)),breaks = 10, labels = FALSE)
  prediction <- matrix(rep_len(0,nrow(Data)), nrow = 10)
  
  for (i in seq(10)){
    testIndex <- which(folds == i, arr.ind = TRUE)
    testData <- Data[testIndex,]
    trainData <- Data[-testIndex,]
    prediction[i,1:nrow(testData)] <- predict_knn(test = testData, train = trainData, method = method, k = k)
  }
  
  prediction <- as.numeric(prediction)
  pred_value <- array(t(prediction))
  Data2 <- cbind(Data,pred_value)
  
  return(Data2)
}

Data2 <- best_predict(k=1, train=train[1:7290, ], method = "minkowski")

Data3 <- Data2[which(Data2[,1]==1),]
Data3same <- Data3[which(Data3[,1]==Data3[,258]),]



# Q9
errortest <- matrix(rep_len(0,45), nrow =15, ncol = 3)
predict_test= function(k,test,train, method){
  predictions <- predict_knn(k=k, test = test, train = train, method = method)
  error <- mean(predictions != test[,1])
  return(error)
}


for (i in seq(15)){
  errortest[i,1] <- predict_test(k=i,test = test, train=train, method = "euclidean")
  errortest[i,2] <- predict_test(k=i,test = test, train=train, method = "manhattan")
  errortest[i,3] <- predict_test(k=i,test = test, train=train, method = "minkowski")
}




errortest <- as.data.frame(errortest)
par(mfrow=c(1,1))
plot(errortest$Euclidean,  typ='l', xlab = "k", ylim= c(0.05,0.085),ylab = "Error Rate", main = "Test Error Rate for K-nearest Neighbors", lwd = 2)
lines(errortest$Manhattan, col="green", typ = 'l', lwd=2)
lines(errortest$Minkowski, col = "blue", typ ='l', lwd=2)

# change legend
legend("topleft",c("Euclidean", "Manhattan", "Minkowski"),col=c('black',  'green', 'blue'), cex=0.5,pt.cex=2, lwd =2)

plot(errortest$Euclidean, typ ='l', ylim = c(0.15,0.35), xlan = "k", ylab = "Error Rate", main = "Test and 10-fold Error Rate for k-nearest Neighbors (Eudlidean)", lwd =2)
lines(error$Euclidean, typ = 'l', col = 'green', lty = 2, lwd =2)
legend("topleft", c("Test Error", "10-fold Train Error"), col = c('black', 'green', 'blue'), cex = 0.5, pt.cex =2, lwd =2, lty= 1:2)

plot(errortest$Manhattan, typ ='l', ylim = c(0.15,0.35), xlan = "k", ylab = "Error Rate", main = "Test and 10-fold Error Rate for k-nearest Neighbors (Manhattan)", lwd =2)
lines(error$Manhattan, typ = 'l', col = 'green', lty = 2, lwd =2)
legend("topleft", c("Test Error", "10-fold Train Error"), col = c('black', 'green', 'blue'), cex = 0.5, pt.cex =2, lwd =2, lty= 1:2)

plot(errortest$Manhattan, typ ='l', ylim = c(0.15,0.35), xlan = "k", ylab = "Error Rate", main = "Test and 10-fold Error Rate for k-nearest Neighbors (Manhattan)", lwd =2)
lines(error$Manhattan, typ = 'l', col = 'green', lty = 2, lwd =2)
legend("topleft", c("Test Error", "10-fold Train Error"), col = c('black', 'green', 'blue'), cex = 0.5, pt.cex =2, lwd =2, lty= 1:2)
