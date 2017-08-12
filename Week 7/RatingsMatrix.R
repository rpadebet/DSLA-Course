library(recommenderlab)



# Set probabilities of rating between 0-5 (6 ratings) equally at 40% &
# 60% probability of NA's 
prob<-c(rep(.4/6,6),.6)

#Sample 50 such ratings
s<-sample(c(as.numeric(0:5), NA), 50,replace=TRUE, prob=prob)

# Build a demo matrix of 5 users and 10 items with the cells containing the ratings
m <- matrix(s,ncol=10, dimnames=list(user=paste("u", 1:5, sep=''),
                                   item=paste("i", 1:10, sep='')))

m

# Can convert this to a realRatingMatrix object

r<-as(m,"realRatingMatrix")
r
getRatingMatrix(r)

# Can convert this to a binaryRatingMatrix 
r_b<-binarize(r,minRating=4)
r_b

# Convert it back to matrix
as(r_b,"matrix")

# Convert the realRatingMAtrix to a list of ratings for each user
as(r,"list")

# Convert the realRatingMatrix to a data frame (user/item/rating dataframe)
as(r,"data.frame")

# Normalize the ratings (use denormalize() to go back to raw ratings)
r_m<-normalize(r)
getRatingMatrix(r_m)

# Plot the raw ratings
image(r,main ="Raw Ratings")
# Plot the normalized ratings
image(r_m,main="Normalized Ratings")


