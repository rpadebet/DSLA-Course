# Evaluating various Recommender Algos

# Generate a evaluation scheme by splitting data into test and train sets
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9, 
                      given=15,
                      goodRating=5)

# "given",indicates how many ratings are given to the test set to begin recommending.
# others are used to test the error

e

# access the data using getData
train<-getData(e,"train")

# Build a User Based Collaborative Filtering recommender
u<-Recommender(train,method="UBCF")
u

# Build a Item Based Collaborative Filtering recommender
i<-Recommender(train,method="IBCF")
i

# Get predictions for the "known" part of the test data
p_u <- predict(u, getData(e, "known"), type="ratings")
p_i <- predict(i, getData(e, "known"), type="ratings")


# We can compare the models using
error<-rbind(
    UBCF = calcPredictionAccuracy(p_u,getData(e,"unknown")),
    IBCF = calcPredictionAccuracy(p_i,getData(e,"unknown"))
)
error



# Confusion matrix using topN model and k-fold cross validation

scheme<-evaluationScheme(Jester5k[1:1000], method="cross", k=4, 
                         given=3,
                         goodRating=5)

results<-evaluate(scheme,method = "POPULAR",type="topNList",
                  n = c(1,3,5,10,15,20))

# Checking the confusion matrix from the 1st run of the 4 fold runs we did
getConfusionMatrix(results)[[1]]

# Averging results for all the runs
avg(results)

# You can plot the ROC curve: plots TPR against FPR
plot(results,annotate=T)


# Evaluating Different Recommender Systems
scheme_algo<- evaluationScheme(Jester5k[1:1000], method="split", train=0.9, 
                      given=15,
                      goodRating=5,
                      k=5) # 5 fold cross validation

algos<-list(
    "random items" = list(name="RANDOM", param=NULL),
    "popular items" = list(name="POPULAR", param=NULL),
    "user-based CF" = list(name="UBCF", param=list(nn=50)),
    "item-based CF" = list(name="IBCF", param=list(k=50))
)

results<-evaluate(scheme_algo,algos,type="topNList",n=c(1,3,5,10,15,20))

results[["user-based CF"]]
plot(results, annotate=c(1,3), legend="topleft")

# Evaluating the ratings prediction
results<-evaluate(scheme_algo,algos,type="ratings")

plot(results,ylim=c(0,100))


# You can do the same for binary data set