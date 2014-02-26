############################ R Programming: PS 5 ###################################
############### Thomas Carroll Jonathan Homola  Jeong Hyun Kim #####################

# Set the working directory.
setwd("~/Dropbox/2014 Spring/Programming/PS5")
# Clear the workspace: Remove the objects currently on my global.
rm(list=ls())
options(digits=5) # the number of significant digits I want listed
options(stringsAsFactors=F) # This tells R not to turn things into factors as the default
options(max.print=100) # This tells R the maximum number of things to print on my screen

install.packages("pdist") # Install the package if you haven't.
# Load the libraries 
lapply(c("foreign", "plyr", "ggplot2",  "gridExtra", "MASS", "pdist"), 
       library, character.only=TRUE)
# foreign package is to import dataset of other formats (e.g., csv, dta)
# plyr package allows us to use apply family in a more flexible way, and to make parallel.
# ggplot2: For advanced plotting
# gridExtra: to call grid.arrange() that arranges ggplots on one page.
# MASS: to load mvrnorm()
# pdist: to compute distance matrix

# Simulation set up
# 1. Write a function to create a matrix of "voters". There should be N rows and 2 columns, where N is the number of voters. 
# A function voters() takes inputs of 
# n: number of voters
# sig1, sig2: standard deviation of normal distributions. We set these values to a random number from uniform distribution [0,1] as default. 
# mu1, mu2: mean of multivariate normal distribution. We set these values to a random number from standard normal distribution as default. 
# Sigma, Sigma1-3: Variance-Covariance matrix for multivariate normal distribution.
# option: it can take values from 1 to 5. Each number gives different distribution that we will specify below. 

# Before simulation, set the seed value for reproducible results:
set.seed(123)
voters <- function(n, sig1=runif(1), sig2=runif(1), mu1=rnorm(1), mu2=rnorm(1), Sigma=NULL, Sigma1=NULL, Sigma2=NULL, Sigma3=NULL,option){
  # 2. Create voter preferences: Set the option to get different voter preferences matrix.
  # (1) If option==1, Preferences on each dimension are drawn from a standard normal distribution. 
  if (option==1){
    voter.mat <- matrix(rnorm(n*2), nrow=n, ncol=2) 
  }
  # (2) If option==2, preferences on each dimension are drawn from a normal distribution where the variance of each dimension can be set separately. 
  if (option==2){
    # Set the standard deviation equal to sig1 for dimension 1.
    d1 <- rnorm(n, sd=sig1)
    # Set the sd equal to sig2 for dimension 2.
    d2 <- rnorm(n, sd=sig2)
    # Combine the two vectors to voter.mat.
    voter.mat <- cbind(d1, d2)
  }
  # (3) If option==3, preferences on each dimension are drawn from a uniform distribution
  if (option==3){
    voter.mat <- matrix(runif(n*2), nrow=n, ncol=2)
  }
  # (4) If option==4, preferences on each dimension are drawn from a multivariate normal distribution. 
  if (option==4){
    d1 <- mvrnorm(n, mu=mu1, Sigma=Sigma)
    d2 <- mvrnorm(n, mu=mu2, Sigma=Sigma)
    voter.mat <- cbind(d1, d2)
  }
  # (5) If option==5, preferences are drawn from a mixure of three multivariate normal distributions.
  if (option==5){
    Sigma <- matrix(c(10,3,3,2),2,2)
    mvr.1 <- mvrnorm(n*2, mu=runif(2), Sigma=Sigma1)
    mvr.2 <- mvrnorm(n*2, mu=runif(2), Sigma=Sigma2)
    mvr.3 <- mvrnorm(n*2, mu=runif(2), Sigma=Sigma3)
    total <- rbind(mvr.1, mvr.2, mvr.3)
    voter.mat <- matrix(sample(total, size=n*2), nrow=n, ncol=2)
  }
  return(voter.mat)
}

# 3. Function such that voters affiliate with the closest of the two parties. 
# Pick some random position for each party.
party.1 <- rnorm(2)
party.2 <- rnorm(2)
party <- rbind(party.1, party.2)
colnames(party) <- c("d1", "d2")

# Generate a matrix of voter preferences.
voter <-  voters(n=100, option=1)  

# affiliate is a function such that return the matrix with the voter's affiliation with the closest party.
affiliate <- function(party, voter){
  distance <- matrix(pdist(voter, party)@dist, ncol=2, byrow=TRUE)
  affiliation <- distance[,1] > distance[,2]
  affiliation[affiliation=="TRUE"] <- "Party2"
  affiliation[affiliation=="FALSE"] <- "Party1"
  voter <- cbind(voter, affiliation)
  return(voter)
}

# 4. 
colnames(party.position) <- c("issue1", "issue2")

plot1 <- ggplot(voter.position, aes(x=X1, y=X2, colour=affiliation)) + geom_point() 
plot1 <- plot1 + geom_point(aes(x=party.position[1,1], y=party.position[1,2], colour="party1")) # Party 1
plot1 <- plot1 + geom_point(aes(x=party.position[2,1], y=party.position[2,2], colour="party2")) # Party 2
plot1 
