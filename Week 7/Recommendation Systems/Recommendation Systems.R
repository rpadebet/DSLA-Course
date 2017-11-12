data("Jester5k")
Jester5k_df<-as(Jester5k,"data.frame")
write.csv(Jester5k_df,"./Week 7/Jester5K.csv",row.names = FALSE)

Jester5k_df<-read.csv("./Week 7/Recommendation Systems/Jester5K.csv")
head(Jester5k_df)

Jester5k<-as(Jester5k_df,"realRatingMatrix")
getRatingMatrix(Jester5k)

# Sample a 1000 jokes from the matrix
set.seed(1233)
r<-sample(Jester5k,1000)
r

# Ratings of 1st user
rowCounts(r[1,])
# Rated 71 jokes only those are shown
as(r[1,],"list")

# plot histogram of all ratings
hist(getRatings(r),breaks =100,col="blue")
hist(getRatings(normalize(r)),breaks =100,col="red")

# How many jokes has each user rated 
hist(rowCounts(r), breaks=50,col="green")
# What is the average rating for each joke
hist(colMeans(r), breaks=50,col="violet")


# Build the recommender system

# Different algos available to us are
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

# Let us build a Recommender using Popular algo
model <- Recommender(r,method="POPULAR")
model

# attributes available to us from this recommender
names(getModel(model))

# topN model
getModel(model)$topN

as(getModel(model)$topN,"list")

# get ratings
getModel(model)$ratings

as(getModel(model)$ratings,"list")

# Predict or get top 5 recommendations from new jokes
recom<-predict(model,Jester5k[100:104,],n=5)
recom
as(recom,"list")


# We can also predict ratings
recom2<-predict(model,Jester5k[100:104,],n=5,type="ratings")
recom2
as(recom2,"list")
rowCounts(Jester5k[104,]) # Cannot recommend anything because he has rated all jokes


as(recom2,"matrix")[,1:10] # NAs indicate user has rated these

# To see actual ratings along with predicted ratings use ratingMatrix
recom3<-predict(model,Jester5k[100:104,],n=5,type="ratingMatrix")
recom3
as(recom3,"matrix")[,1:10]







