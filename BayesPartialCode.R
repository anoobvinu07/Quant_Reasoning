######################################
# Quantitative Reasoning
# Bayesian Statistics: Beetle Example
# 8 February 2019
######################################

setwd(".")

library(R2jags)
library(tidyverse)

#Read in beetle data and set up ------------------------------------------------
bluebug <- read.table("bluebug.txt", sep = '', header = T, stringsAsFactors = F, 
                      row.names = 1)

#Extract count data
obs.data <- as.matrix(bluebug[,3:8])
#Change counts to presence-absence
obs.data[obs.data > 1] <- 1

#Extract covariates and standardize
edge <- bluebug$forest_edge

dates <- as.matrix(bluebug[,10:15])
date <- scale(dates)
date[is.na(date)] <- 0

#Write model in JAGS language ---------------------------------------------------
#Define number of sites, survey periods
sites <- length(unique(bluebug$siteno))
surveys <- dim(obs.data)[2]

# Writing JaGS script
cat("
    model{
    
    #Prior
      alpha0 ~ dnorm(0, 0.1)
      alpha1 ~ dnorm(0, 0.1)

      beta0 ~ dnorm(0, 0.1)
      beta1 ~ dnorm(0, 0.1)

    #Ecological model: occupancy
      for(i in 1:R){
      logit(psi[i]) <- alpha0 + alpha1*edge[i]
      z[i] ~ dbern(psi[i]) # true occupancy state

        #Detection model
          for(j in 1:S){
            logit(p[i,j]) <- beta0 + beta1*date[i,j]
            mu.p[i,j] <- z[i] * p[i,j] # based on the true occupancy state
            obs.data[i,j] ~ dbern(mu.p[i,j])
          }
      }
    
}
    ", file = "bugmodel.txt")

# Define what data go to gibbs sampler
data <- list(R = sites, 
             S = surveys, 
             obs.data = obs.data, 
             edge = edge, 
             date =date)
params <- c("z", "beta1")

# inits
inits <- function(){
  list(alpha0 = rnorm(n=1), 
       alpha1 = rnorm(n=1),
       rnorm = rnorm(n=1),
       beta0 = rnorm(n=1), 
       beta1 = rnorm(n=1), 
       z = apply(obs.data, 1, max, na.rm = T))
}

# Send model to JAGS
jag <- jags(model.file = "bugmodel.txt",
            data = data,
            inits = inits,
            n.chains = 3,
            n.burnin = 1000,
            n.iter = 2000,
            parameters.to.save = params) #n.chains is the markov chains
print(jag)

# Interpret the results--------------------------------------------------------
zs <- jag$BUGSoutput$sims.list$z
n.occ <- rowSums(zs)/sites
mean(n.occ); quantile(n.occ, c(0.025, 0.975))

b1 <- jag$BUGSoutput$sims.list$beta1

ggplot() +
  geom_point(aes(x="date", y=mean(b1))) +
  geom_errorbar(aes(x="date",ymin = quantile(b1,0.025), ymax = quantile(b1,0.975))) +
  geom_hline(aes(yintercept=0), linetype="dashed")

# bayes method are better than frequentist method for heirarchial models