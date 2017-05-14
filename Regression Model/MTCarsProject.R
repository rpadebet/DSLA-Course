#Loading Libraries
library(ggplot2)
library(car)

# Exploring the data set
?mtcars
head(mtcars)
str(mtcars)
summary(mtcars)

# Factoring and labelling some of the data
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
 
 # Fitting linear models starting with the full model
 full <- lm(mpg ~ .,mtcars)
 summary(full)$coefficients
 
 #Stepping backwards through various models
 search <-step(full,direction = "backward",trace = FALSE)
 search$anova

 # Selecting the model with best AIC
 mdl <-lm(mpg ~ wt+am+qsec-1 ,mtcars)
 summary(mdl)
 extractAIC(mdl)
 
 # The coefficient estimates show that

 # All else equal:
 # An increase in weight of 1000lbs leads to decrease in mpg of 3.9165
 # AT vehicles have a lower but positive (9.6178) mean contribution to mpg but with a 95% confidence interval of (-4.64 to 23.87).
                # Since this interval contains zero, it cannot be confirmed that AT has a positive influence on mpg
 # MT vehicales have a higher positive (12.5536) mean contribution to mpg with a 95% confidence interval of (0.15 to 24.96).
                # Since this interval doesn't contain zero they can be said to be positive influence on mpg
 # qsec indicates that the mpg improves by 1.226 for every one sec increase in time required for first 1/4 mile 
                # i.e. slower cars are better for mpg

 # Further Simplification:
 # Removing qsec or am from the model leads to worse AIC score and biases the contribution of wt to mpg even more 
            # although it leads to significant coefficients for auto/manual transmission variables
 mdl2 <-lm(mpg ~ wt+am-1 ,mtcars)
 summary(mdl2)
 extractAIC(mdl2)
 
 mdl3 <-lm(mpg ~ wt+qsec-1 ,mtcars)
 summary(mdl3)
 extractAIC(mdl3)
 
 mdl4 <-lm(mpg ~ am-1 ,mtcars)
 summary(mdl4)
 extractAIC(mdl4)
 
 anova(mdl,mdl2,mdl3)
 
 
 mdl_i <-lm(mpg ~ wt+am+qsec ,mtcars)
 summary(mdl_i)
 extractAIC(mdl_i)
 
# Diagnostics of the model

 #Residuals Plot : Almost normally distributed around zero without a pattern
  plot(mdl, which = 1)
 # Q-Q plot showing the observations are distributed around the theoretical line
  plot(mdl, which = 2)
 # Cook's distance showing that Chrysler Imperial maybe skewing the model a little bit
  plot(mdl, which = 4)
 # Plot of infuence measures from car package
 influenceIndexPlot(mdl)
  
# Removing the Chrysler Imperial data point and remodelling
which.max(cooks.distance(mdl))
which.max(hatvalues(mdl))
mtcars_mod <- mtcars[-which.max(cooks.distance(mdl)),]

mdl_mod <- lm(mpg ~ wt+am+qsec-1 ,mtcars_mod)
summary(mdl_mod)
extractAIC(mdl_mod)
influenceIndexPlot(mdl_mod)

# leads to significants improved AIC score as well as 
# better confidence intervals around positive effects of transmission on mpg

# With the adjusted dataset, we can infer with at least 95% confidence that manual transmission adds on average 15.866mpg
# and at 90% confidence interval we can infer that automatic transmission adds on average 13.668 mpg, all else equal.
 