library("lme4")


##Simulation Parameters:
set.seed(100)
m <- 5000
n <- 100

##Model parameters:
t <- seq(1,100)
beta <- 2
alpha <- 10



##Containers to save results:
result.1.sim <- rep(NA,m)
result.2.sim <- rep(NA,m)

##Progressbar
pb <- txtProgressBar(min=0, max=m, style=3)

##Run simualation
for (rpt in 1:m){
  eps1 <-rnorm(n, mean=0, sd=10)
  eps2 <- rnorm(n, mean=10, sd=10)
  y1 <- alpha + beta*t + eps1
  y2 <- alpha + beta*t + eps2
  
}

##Save the simulated data:
t <- c(seq(1,100))
subject <- c(rep("y1",100),rep("y2",100))
Y <- c(y1,y2)
my.df = data.frame(subject, Y, t)


##Our Function:
LMM <-function(LMM)( lmer(Y ~  t + (1|subject), my.df)
)

library(memisc)
getSummary.mer(mod1)$coef
write.csv(getSummary.mer(mod1)$coef,"answer.csv")


##Plot
#install.packages("ggplot2")
options(scipen=999) #turn-off scientific notation
library(ggplot2)

gg <- ggplot(my.df, aes(x=t, y=Y)) + xlim(c(0,100))+ geom_point(aes(col=subject))+ ylim(c(0,210))+ geom_smooth(aes(col=subject), method = "lm", se=F) 
plot(gg)

