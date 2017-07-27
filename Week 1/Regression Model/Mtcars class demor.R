library(ggplot2)
library(car)

# Exploring the data set
?mtcars
head(mtcars,3)
tail(mtcars)
str(mtcars)
View(mtcars)
summary(mtcars)

mtcars$cyl <-as.factor(mtcars$cyl)
mtcars$am <-as.factor(mtcars$am)
mtcars$am <-relevel(mtcars$am,"1")
translabs<-c("0" = "Auto","1" = "Manual")

#Exploring data via plots
plot(mtcars$am,mtcars$mpg)

g<-ggplot(mtcars,aes(x=wt,y=mpg))+
    geom_point(aes(color = cyl))+
    geom_smooth(method = "lm") +
    facet_grid(~am, labeller = as_labeller(translabs) )
print(g)


mdl1 <-lm(mpg ~ wt ,mtcars)
summary(mdl1)

mdl1b <-lm(mpg ~ wt-1 ,mtcars)
summary(mdl1b)

mdl2 <-lm(mpg ~ qsec ,mtcars)
summary(mdl2)

mdl3 <-lm(mpg ~ wt+qsec ,mtcars)
summary(mdl3)
