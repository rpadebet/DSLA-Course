install.packages("arules")
install.packages("arulesViz")


library(arules)
library(arulesViz)
library(dplyr)

data("Groceries")

class(Groceries)    # By default this dataset is a "Trancactions" Class
glimpse(Groceries)
inspect(head(Groceries,3))
head(Groceries@itemInfo,12)


# Converting Transactionds into Transaction id lists
data <- list(
    c("a","b","c"),
    c("a","b"),
    c("a","b","d"),
    c("b","e"),
    c("b","c","e"),
    c("a","d","e"),
    c("a","c"),
    c("a","b","d"),
    c("c","e"),
    c("a","b","d","e"),
    c("a",'b','e','c')
)
data <- as(data, "transactions")   # Use this to convert a list of transactions into transaction class

inspect(data)
class(data)

#Convert transactions to transaction ID lists

tl <- as(data, "tidLists")
inspect(tl)


# Summary of Groceries class object
summary(Groceries)


# Generate the rules
rules <- apriori(Groceries, 
                 parameter = list(supp = 0.001
                                  , conf = 0.80
                                 # ,maxlen=3 # Maximum length of a rule
                                  ))


# Find top 10 rules arranged by Lift
inspect(rules[1:10])

# Summary of the rules
summary(rules)

# Sorting rules
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:10])

# Get rules that lead to buying 'whole milk'
rules <- apriori (data=Groceries, 
                  parameter=list (supp=0.001,conf = 0.08), 
                  appearance = list (default="lhs",rhs="whole milk"), 
                  control = list (verbose=F)) 
rules <- sort (rules, by="confidence", decreasing=TRUE)
inspect(rules[1:10])

# Get rules that lead FROM buying 'whole milk'
rules <- apriori (data=Groceries, 
                  parameter=list (supp=0.001,conf = 0.08), 
                  appearance = list (default="rhs",lhs="whole milk"), 
                  control = list (verbose=F)) 
rules <- sort (rules, by="confidence", decreasing=TRUE)
inspect(rules[1:10])

## Exploring the item frequencies
    # Can be used to boost sales of items by placing them next to frequent items
library(RColorBrewer)

itemFrequencyPlot(Groceries,
                  topN=20,
                  col=brewer.pal(8,'Pastel2'),
                  main='Relative Item Frequency Plot',
                  type="relative",
                  ylab="Item Frequency (Relative)")

## Plotting the rules in the form of a graph
plot(rules[1:20],
     method = "graph",
     interactive = TRUE,
     control = list(type = "items"))

## Parallel coordinates plot
plot(rules[1:20],
     method = "paracoord",
     control = list(reorder = TRUE))

## Interactive plot
plotly_arules(rules[1:20])


# Removing redundant rules (us the following snippet)
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
