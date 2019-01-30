# Problem set 1: Single-species models in discrete time
# 16 Jan 2019
# AP

# Parameters
max_time = 50 
r = 1.4 # pop. growth/death rate or intrinsic growth rate
K = 1 # carrying capacity

N = vector('numeric',length=max_time) # N is the population size
N[1] = 0.1 # no reason for choosing the value of 0.1 for this model. 

# Iterate model
for (t in 1:max_time){
  N[t+1] = N[t] + r*N[t]*(1 - N[t]/K) # determine the new population size from the old population size
}

plot(N) # the x-axis though marked as Index, it means time. Population size over time.

# r = 0.4
#as the value of r increases, the rate of growth increases and reaches the maximum capacity earlier. But as the value of r exceeds 1, the slope starts to loose its curve and becomes more erractic at the point of reaching maximum capacity.
# when r is equaled to some particular values, the graph becomes erattic and bounces around. 
# the population looses its equilibrium close to 1.5 and starts to bifurcate.

#####################################################################################

# 2. To show the results in question 1 more clearly, write R code to build a plot of the equilibrium value versus the value for r. To do this, run the model for 50 years, and only plot the last 10 years. If the solution is an equilibrium point, all 10 years should be the same value.s

# Parameters
max_time = 50 
r = 0.4 # pop. growth/death rate
K = 1 # carrying capacity

# eq = seq(from = 0, to = 4.5, by = 0.5)

N = vector('numeric',length=max_time) # N is the population size
N[1] = 0.1 # no reason for choosing the value of 0.1 for this model. 

# bifurcation datagram


# attempt 4##################################################################

# Parameters 
max_time = 50 
r = 0.4 # pop. growth/death rate
K = 1 # carrying capacity

val1=matrix(data=NA, nrow=10, ncol=1)
# counter = 0
plot(-10,-10, ylim = c(0,1.5), xlim = c(0,4))
for (r in seq(from = 0.1, to = 4.0, by = 0.1)){
  for (t in 1:max_time){
    N[t+1] = N[t] + r*N[t]*(1 - N[t]/K)
    
  }
  # counter=counter+1
  val1[,1] <- r
  val2 <- N[41:50]
  
  points(x=val1, y=val2, pch=19)
}

plot(N)##################Success#############################################


# External example (copied from https://rpubs.com/DistribEcology/880)
library(ggplot2)
rmax <- 30
out.df <- matrix(NA, ncol = 2, nrow = 0)
a <- 0.01
r <- seq(0, rmax, by = 0.01)
n <- 50

for (z in 1:length(r)) {
  
  xl <- vector()
  xl[1] <- 10
  for (i in 2:n) {
    
    xl[i] <- xl[i - 1] * r[z] * exp(-a * xl[i - 1])
    
  }
  uval <- unique(xl[40:n])
  ### Here is where we can save the output for ggplot
  out.df <- rbind(out.df, cbind(rep(r[z], length(uval)), uval))
}
out.df <- as.data.frame(out.df)
colnames(out.df) <- c("r", "N")
ggplot(out.df, aes(x = r, y = N)) + geom_point(size = 0.5)

#######################################################################################

# 3. Fisheries example
cod_pop <- c(1450, 1420, 1050, 2643, 1060, 1080, 1410, 1150, 740, 175, 43, 10, 12, 15, 16, 16, 28, 30, 32, 23, 12, 19, 27)
years <- c(1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 
           1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 
           2004, 2005)

par(mfrow=c(1,2))
plot(years,cod_pop,ylim=c(0,3000),las=1,ylab='Population (tonnes)',xlab='Time (years)',pch=16)
plot(cod_pop[1:(length(cod_pop)-1)],cod_pop[2:length(cod_pop)],ylab='N[t+1]',xlab='N[t]',las=1,pch=16)

mtext(text = 'Atlantic cod population - Canada',side = 3,line = -3,outer=T)

# 3a) What are the parameter values (a and b) that best fit the data in phase plane diagram above?

max_time = 23

N = vector('numeric',length=max_time)
N[1] = 1450

Nt_1=NULL

index=0

mat2=matrix(data=NA, nrow=6, ncol=3)

for(a in seq(from=4, to=6, by=1)){
  for(b in seq(from=4, to=6, by=1)){
    for(t in 1:max_time){
      N[t+1] = (a*N[t])/(1+b*N[t])
      Nt_1=N[24]
    }
    index=index+1
    mat2[index,1]=a
    mat2[index, 2]=b
    mat2[index,3]=Nt_1
  }
}
