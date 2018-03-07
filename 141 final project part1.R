# Q1
# write the read function as we have did in the previous homeworks, first read, then transfrom into appropriate type
read_digits = function(PATH){
  #read
  digits = read.table(PATH)
  #transfer
  digits[,1] = factor(digits[,1])
  return(digits)
}

#read it into the R
test = read_digits("/Users/ricky/Desktop/digits/test.txt")

# Q2

#write the function that can view the shape of the numbers 
view_digit = function(which_row, digits){
  #check the conditions if nrow is null
  if (is.null(nrow(digits))){
    digits = matrix(digits, nrow=1)
  # for most of conditions, it is test/train, we exclude the first column, which is the true lable, not the pixel 
    if (length(digits)==257){
      digits = digits[-1]
    }
  }
  #to check the exact row label we want to display 
  digit = digits[which_row,]
  # check if this row is normal, then exclude the true label column 
  if (length(digit)==257){
    digit = digit[,-1]
  }
  # transform into matrix and only appear -1,0,1
  digit = matrix(as.integer(digit), 16, 16, byrow = TRUE)
  m = t(digit)  # transpose the image
  m = m[,nrow(m):1]  # turn up-side-down
  #produce the image
  image(m, main=NULL, col = rgb((255:0)/255, (255:0)/255, (255:0)/255), xaxt = "n", yaxt = "n")
}

view_digit(10, test)

# Q3a

# plot it for the train data
# read the train data
train = read_digits("/Users/ricky/Desktop/digits/train.txt")
#split the data into catagories based on the true label
train_split = split(train[,-1], train[,1])
# sum all of Vs together, in order to make the graph by comparisons to each V
train_split_avearge = lapply(train_split, function(x) apply(x, 2, sum))
par(mfrow=c(2,5))
# use the function we made in question 2 to display the graph 
lapply(train_split_avearge, function(x) view_digit(1, x)) 

# plot it for the test data
# the same as we did for the train data
test = read_digits("/Users/ricky/Desktop/digits/test.txt")
test_split = split(test[,-1], test[,1])
test_split_avearge = lapply(test_split, function(x) apply(x, 2, sum))
par(mfrow=c(2,5))
lapply(test_split_avearge, function(x) view_digit(1, x)) 



# Q3b

#write the function to return the average of each V1-256 values 
pro_cal = function (train){
#define the prob matrix  
  probtotal <- as.data.frame(matrix(rep_len(0,2560), nrow = 10))
#calculate the mean for each v1-256 to each labels, and store it  
  for (i in 1:10) {
    filter <- train[(train[,1]==(i-1)),]
    probdigit <- as.data.frame(lapply(filter[,-1],mean))
    probtotal[i,] <- probdigit
  }
  return (probtotal)
}

#restore it into prob matrix, and revise the colname and rowname.
prob <- pro_cal(train)
colnames(prob) <- seq(256)
rownames(prob) <- seq(0,9)

#write a function to filter out significant very small V 
min_cal =  function(prob){
  #define minnum
  minnum <- as.data.frame(matrix(rep_len(0,256),nrow =1))
  #write a loop to select out significant values 
  for (i in 1:256){
    min <- prob[order(prob[,i]),]
    #we choose the index difference to be 0.4
    if ( (min[2,i]-min[1,i]) >= 0.4){
      minnum[1,i] = which.min(prob[,i])-1
     #else just choose 11, it can be any number>10, used to select out 
    } else {
      minnum[1,i] <- 11
 
    }
  }
  return(minnum)
}

#same as above, to filter out significant large V
max_cal =  function(prob){
  
  maxnum <- as.data.frame(matrix(rep_len(0,256),nrow =1))
  for (i in 1:256){
    max <- prob[order(-prob[,i]),]
    if ( (max[1,i]-max[2,i]) >= 0.4){
      maxnum[1,i] <- which.max(prob[,i])-1
    } else {
      maxnum[1,i] <- 11
    }
  }
  return(maxnum)
}

# store these values
min <- min_cal(prob)
max <- max_cal(prob)

# get the values that are significant 
which(min[1,]!= 11)
which(max[1,]!= 11)


# find the value that are very not significant for minimum, similar to above
inmin_cal =  function(prob){
  
  minnum <- as.data.frame(matrix(rep_len(0,256),nrow =1))
  for (i in 1:256){
    min = prob[order(prob[,i]),]
    if ( (min[2,i]-min[1,i]) <= 0.05){
      minnum[1,i] <- which.min(prob[,i])-1
    } else {
      minnum[1,i] <- 11
    }
  }
  return(minnum)
}

# find the value that are very not significant for maximum, similar to above
inmax_cal =  function(prob){
  
  maxnum <- as.data.frame(matrix(rep_len(0,256),nrow =1))
  for (i in 1:256){
    max <- prob[order(-prob[,i]),]
    if ( (max[1,i]-max[2,i]) <= 0.05){
      maxnum[1,i] <- which.max(prob[,i])-1
    } else {
      maxnum[1,i] <- 11
    }
  }
  return(maxnum)
}

#store these values and sort out very not significants, same as above
inmin <- inmin_cal(prob)
inmax <- inmax_cal(prob)
which(inmin[1,] != 11)
which(inmax[1,] != 11)



# Q4

#predict_knn = function()
# test_point includes the label part

#first made a distant function
#made a distant matrix, every point in the matrix is the distance from oee obs to others
# upper part is train, and second part is test
#distance2 = as.matrix(dist(rbind(train[,-1], test[,-1])),method="euclidean")

#made the predict function
predict_knn = function(test, train, method,k){
  #apply the distance matrix
  #make sure to put outside of the for loop, or we may wait very long time for it to run 
  distance2 = as.matrix(dist(rbind(train[,-1], test[,-1])),method=method)
  #define the number of rows that will be tested 
  n = nrow(test)
  # define the result
  result=rep_len(0,n)
  #write the loop 
  for(i in 1:n){
    #select out the test row
    #make sure that test row column is under the train
    testrow = as.data.frame(distance2[nrow(train)+i,1:nrow(train)])
    colnames(testrow) = "Distance"
    #to input the real label from train
    testrow$labels = train$V1[1:nrow(testrow)]
    # to see the cloest distances
    order = testrow[order(testrow$Distance),]
    knn = head(order,k)
    # to see the number of each labels appears
    frequencylabel = data.frame(table(knn$labels))
    colnames(frequencylabel) =c("Label","Freq")
    # to check the situation if there are same numbers of appearing 
    if (sum(frequencylabel$Freq == max(frequencylabel$Freq))>1) {
      temp_labels = as.character(frequencylabel[frequencylabel$Freq == max(frequencylabel$Freq),"Label"])
      result[i] = sample(temp_labels, size=1)
    } else {
      result[i] = as.character(frequencylabel$Label[which.max(frequencylabel$Freq)])
    }
  }
  return(result)
}

# run the result
result = predict_knn(test = test[1:10,], train = train, method = "euclidean", k = 25)

# check the similarity 
mean(result == test[1:10,1])

result2 = predict_knn(test = test, train = train, method = "euclidean", k = 10)

mean(result2 == test[,1])

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

# 2) Not use Q4 equation

predict_knn2 = function(test, train,distance2,k){
  #apply the distance matrix
  #define the number of rows that will be tested 
  n = nrow(test)
  # define the result
  result=rep_len(0,n)
  
  #write the loop 
  for(i in 1:n){
    #select out the test row
    #make sure that test row column is under the train
    testrow = as.data.frame(distance2[nrow(train)+i,1:nrow(train)])
    colnames(testrow) = "Distance"
    #to input the real label from train
    testrow$labels = train$V1[1:nrow(testrow)]
    # to see the cloest distances
    order = testrow[order(testrow$Distance),]
    knn = head(order,k)
    # to see the number of each labels appears
    frequencylabel = data.frame(table(knn$labels))
    colnames(frequencylabel) =c("Label","Freq")
    # to check the situation if there are same numbers of appearing 
    if (sum(frequencylabel$Freq == max(frequencylabel$Freq))>1) {
      temp_labels = as.character(frequencylabel[frequencylabel$Freq == max(frequencylabel$Freq),"Label"])
      result[i] = sample(temp_labels, size=1)
    } else {
      result[i] = as.character(frequencylabel$Label[which.max(frequencylabel$Freq)])
    }
  }
  return(result)
}


cv_error_knn2 <- function(k,train, method){
  
  # sample the data
  Data <- train[sample(nrow(train)),]
  # use another array to record the groups of the data
  folds <- cut(seq(1,nrow(Data)),breaks = 10, labels = FALSE)
  error <- matrix(rep_len(0,10), nrow =10)
  
  distance1 = as.matrix(dist(Data[1:nrow(Data),-1],method="euclidean"))
  # divide the data into 10 groups and record the error
  for (i in seq(10)){
    # filter the data group into test and train
    testIndex <- which(folds == i, arr.ind = TRUE)
    testData <- Data[testIndex,]
    trainData <- Data[-testIndex,]
    distance3 <- rbind(distance1[-testIndex,-1],distance1[testIndex,-1])
    prediction <- predict_knn2(test = testData, train = trainData, distance2 = distance3, k = k)
    error[i] <- mean(prediction != testData[,1])
  }
  
  # Get the overall error rate
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
