library(h2o)
library(data.table)

h2o.init(port = 54321,nthreads = -1)

#' Download MINST Data files
#'
#' CSV training set http://www.pjreddie.com/media/files/mnist_train.csv
#' CSV test set http://www.pjreddie.com/media/files/mnist_test.csv
save_path=c("/Users/rohitpittu/R Projects/DSLA Course/Week 3/MNIST Handwriting Recognition/")
save_path2=c("~/Downloads/MINST/")

download.file("http://www.pjreddie.com/media/files/mnist_train.csv",
                paste0(save_path,"train.csv"))

download.file("http://www.pjreddie.com/media/files/mnist_test.csv",
              paste0(save_path,"test.csv"))

# Reading files into local memory
train<-fread(input = "train.csv")
test<-fread(input = "test.csv")

#Take the 10th row
train[10,]
# Plotting the images
# Create a 28*28 matrix with pixel color values
m = matrix(unlist(train[10,-1]), nrow = 28, byrow = TRUE)

# Plot that matrix
image(m,col=grey.colors(255))



# reverses (rotates the matrix)
rotate <- function(x) t(apply(x, 2, rev)) 

# Plot some of images
par(mfrow=c(2,3))
lapply(1:6, 
       function(x) image(
         rotate(matrix(unlist(train[x,-1]),nrow = 28, byrow = TRUE)),
         col=grey.colors(255),
         xlab=train[x,1]
       )
)

par(mfrow=c(1,1)) # set plot options back to default

# Reading files in H2O
train_h2o<-h2o.importFile()paste0(save_path,"train.csv")
test_h2o<-h2o.importFile(paste0(save_path,"test.csv"))
# Deep neural network model

train_h2o[,1]<-as.factor(train_h2o[,1])

model <- h2o.deeplearning(x = 2:785,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame  = train_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   hidden = c(50,50,50), # three layers of 50 nodes
                   variable_importances = TRUE, # variable importance
                   epochs = 100) # max. no. of epochs


## Using the DNN model for predictions
h2o_yhat_test <- h2o.predict(model, test_h2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_yhat_test)

## Test labels
test_labels<-as.factor(unlist(test[,1]))

## Table of predictions
table(test_labels,df_yhat_test[,1])

## Accuracy of predictions
Acc<-sum(diag(table(test_labels,df_yhat_test[,1])))/nrow(test)
print(paste0("Accuracy = ",Acc*100,"%"))



# View the specified parameters of your deep learning model

model@parameters
# Examine the performance of the trained model

model
# display all performance metrics

h2o.performance(model)

# Variable Importance
model2 <-
  h2o.deeplearning(x = 2:785,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame  = train_h2o, # data in H2O format
                   activation = "Rectifier", # or 'Tanh'
                   #input_dropout_ratio = 0.2, # % of inputs dropout
                   #hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   hidden = c(300,150,50), # three layers of 50 nodes
                   epochs = 100,  # max. no. of epochs
                   variable_importances = TRUE, # variable importance
                   validation_frame = test_h2o, # test frame
                   nfolds = 5 ) # cross validation folds


h2o.performance(model2)

plot(model2)

h2o.varimp_plot(model2,25)

h2o.varimp(model2)

# View the specified parameters of your deep learning model

model2@parameters

## Using the DNN model for predictions
h2o_yhat_test <- h2o.predict(model2, test_h2o)


## Classifying using someother model

library(caret)

inTrain = data.frame(y=as.factor(unlist(train[,1])), train[,-1])
inTest = data.frame(y=as.factor(unlist(test[,1])), test[,-1])
inTrain$y <- as.factor(inTrain$y)
trainIndex = createDataPartition(inTrain$y, p = 0.60,list=FALSE)
training = inTrain[trainIndex,]
cv = inTrain[-trainIndex,]

# SVM. 95/94.
fit <- train(y ~ ., 
             data = head(training, 1000),
             method = 'svmRadial', 
             tuneGrid = data.frame(sigma=0.0107249, C=1))
results <- predict(fit, newdata = head(cv, 1000))
confusionMatrix(results, head(cv$y, 1000))



