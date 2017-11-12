# Market Basket Analysis on Titanic Dataset
    #'The goal of unsupervised learning is to discover previously unknown patterns (easy)
    #' that are also interesting (more challenging) from data. 
    #' In most cases unsupervised learning isn’t strong enough to provide much value alone, 
    #' but can provide value for knowledge discovery in highly exploratory settings. 
    #' Particularly knowledge discovery of data quirks and structure. 
    
#' Association analysis is one branch of unsupervised learning 
         #' that takes the form of “When X occurs, Y also occurs Z% of the time”. 

library(arules)
library(data.table)
library(readr)
library(dplyr)

# Load Titanic dataset
Titanic.raw<-read_csv(file ="./Week 7/Market Basket Analysis/titanic.csv" )

# Incluse the Age, Sex, Social class and whether Survived for analysis
Titanic<-Titanic.raw%>%
    select(Pclass,Sex,Age,Survived)%>%
    mutate(Age = as.factor(ifelse(Age<=18,"Child","Adult")))

Titanic$Age<-relevel(Titanic$Age,"Child")

summary(Titanic)

# Convert to data.table
dt<-as.data.table(Titanic)

# Convert the columns to factors to feed into apriori
changeCols<-colnames(dt)
dtnew <- dt[, (changeCols):=lapply(.SD, as.factor), .SDcols = changeCols]

# Explore the dataframe
str(dtnew)
head(dtnew)

# Convert to transactions class so apriori can use
titanic <- as( dtnew, 'transactions' )

inspect(head(titanic,3))
# Get Summary
summary( titanic )

# Generate rules
# train apriori
rules <- apriori( 
    titanic,
    
    # the min/max len denotes the min/max number of items in a itemset
    parameter = list( support = 0.05, confidence = 0.7, minlen = 2, maxlen = 5 ),
    
    # for appearance we can specify we only want rules with rhs 
    # containing "Survived" only (we then specfiy the default parameter
    # to 'lhs' to tell the algorithm that every other variables that
    # has not been specified can go in the left hand side
    appearance = list( rhs = c( 'Survived=0', 'Survived=1' ), default = 'lhs' ),
    
    # don't print the algorthm's training message
    control = list( verbose = FALSE )
)

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules)


# We can convert this to an interactive Dataframe and play with it
library(DT)
rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                        rhs = labels( rhs(rules) ), 
                        quality(rules) )[ order(-lift), ]

DT::datatable(rules_dt)

library(arulesViz)
plot(rules,
     method = "graph",
     control = list(type = "items"))




