# Survival Analysis in R
# Copyright 2013 by Ani Katchova


## Analysis to find out how long unemployment lasts

# install.packages("survival")
# install.packages("survminer")
library(survival)
library(survminer)

mydata<- read.csv("./Week 7/Survival Analysis/survival_unemployment.csv")
attach(mydata)

# Define variables 
time <- spell  # Time of unemployment
event <- event # Finding a job is an event
X <- cbind(logwage, ui, age) # ui is Unemployment insurance
group <- ui

# Descriptive statistics
summary(time)
summary(event)
summary(X)
summary(group)

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)  
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")
ggsurvplot(kmsurvival, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")
ggsurvplot(kmsurvival1, xlab="Time", ylab="Survival Probability",risk.table = T)
# People without unemployment insurance land a job faster ui=1 is get unemployment insurance

plot(kmsurvival1, col=c("orange","purple"), lty=c(1:2), lwd=3, conf.int = TRUE, xmax = 2000)
legend(2, .2, c("Uninsured", "Insured"),lty = c(1:2), col=c("orange","purple"))


# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ X)
summary(coxph)
# higher wage 62% increased hazard rate of event i.e. finding a job
# individuals with unemployment insurance have a 1-0.36 = 64% lower hazard rate of event


# Exponential, Weibull, and log-logistic parametric model coefficients
exponential <- survreg(Surv(time,event) ~ X, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(time,event) ~ X, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(time,event) ~ X, dist="loglogistic")
summary(loglogistic)
