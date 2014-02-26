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
lapply(c("foreign", "plyr", "ggplot2",  "gridExtra", "pdist"), 
       library, character.only=TRUE)
# foreign package is to import dataset of other formats (e.g., csv, dta)
# plyr package allows us to use apply family in a more flexible way, and to make parallel.
# ggplot2: For advanced plotting
# gridExtra: to call grid.arrange() that arranges ggplots on one page:


# Simulation set up

# 1. Write a function to create a matrix of "voters". There should be N rows and 2 columns, where N is the number of voters. 

voters <- function(n, sig1=runif(1), sig2=runif(1), Sigma=matrix(c(10,3,3,2),2,2), option){
# 2. Create voter preferences
  # Set the option to get different voter preferences matrix
  if (option==1){
    voter.1 <- matrix(rnorm(n*2), nrow=n, ncol=2)
    return(voter.1)
  }
  if (option==2){
    voter.2 <- matrix(c(rnorm(n, mean=0, sd=sig1), rnorm(n, mean=0, sd=sig2)),
                      nrow=n, ncol=2)
    return(voter.2)
  }
  if (option==3){
    voter.3 <- matrix(runif(n*2), nrow=n, ncol=2)
    return(voter.3)
  }
  if (option==4){
    voter.4 <- matrix(mvrnorm(n*2, mu=c(0,0), Sigma=Sigma),nrow=n, ncol=2)
    return(voter.4)
  }
  if (option==5){
    mvr.1 <- mvrnorm(n*2, mu=c(0,0), Sigma=Sigma)
    mvr.2 <- mvrnorm(n*2, mu=runif(2), Sigma=Sigma+runif(1))
    mvr.3 <- mvrnorm(n*2, mu=runif(2), Sigma=Sigma+runif(1))
    total <- rbind(mvr.1, mvr.2, mvr.3)
    voter.5 <- matrix(sample(total, size=n*2), nrow=n, ncol=2)
    return(voter.5)
  }
}

# 3. Function such that voters affiliate with the closes of the two parties. 
party.1 <- rnorm(2)
party.2 <- rnorm(2)
party.position <- rbind(party.1, party.2)
party.position
voter.position <-  voters(n=100, option=1)  
distance.1 <- apply((voter.position - party.1)^2, 1, sum)
distance.1 <- sqrt(distance.1)
distance.2 <- apply((voter.position - party.2)^2, 1, sum)
distance.2 <- sqrt(distance.2)
final.distance <- distance.1 - distance.2
voter.position <- data.frame(voter.position)
voter.position$final.distance <- final.distance
voter.position$affiliation <- ifelse(final.distance > 0, "2", "1")

# 4. 
colnames(party.position) <- c("issue1", "issue2")

plot1 <- ggplot(voter.position, aes(x=X1, y=X2, colour=affiliation)) + geom_point() 
plot1 <- plot1 + geom_point(aes(x=party.position[1,1], y=party.position[1,2], colour="party1")) # Party 1
plot1 <- plot1 + geom_point(aes(x=party.position[2,1], y=party.position[2,2], colour="party2")) # Party 2
plot1 
