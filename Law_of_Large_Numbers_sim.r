#SIMULATION PRACTICE ----------------------------------------
#LAW OF LARGE NUMBERS######
#Dictates that the average of results of a large number of random, independent samples # converges on the true experimental probablity, if it exists
#when we repeat an experiment n many times as n-> ∞.

#We can simulate this in RStudio by generating a plot that converges ona given probability as we repeat the experiment and get 
#averge of all the samples. 

---------------------------------------------------------------------------------------------------------------
#WHAT IS THE PROBABILITY THAT A RANDOM NUMBER from 1:1000 IS DIVISIBLE BY 3,5,9?####
#Create function to simulate division-> simdivis

simdivis<-function(){
  num<-sample(1:1000,1) #picks random number from 1-> 1000
  if( num %% 3 == 0|| num %% 5 ==0 || num %% 9 == 0)1 else 0 #if given number is divisible by 3,5 or 9 assign 1 else 0
}

#Repeat experiment n=50 many times 
set.seed(13) #ensures random number generator generate the same set of "random" observations for experiment.
simlist<-replicate(50,simdivis())
mean(simlist)

#Repeat experiment n=2000 many times 
set.seed(13) #ensures random number generator generate the same set of "random" observations for experiment.
simlist<-replicate(2000,simdivis())
mean(simlist)

#Repeat experiment n=100000 many times 
set.seed(13)#ensures random number generator generate the same set of "random" observations for experiment.
n<-10000
simlist<-replicate(n,simdivis())
mean(simlist) 

#The true probability is 0.467 and as we increase n, our mean of random samples approaches 0.467.

------------------------------------------------------------------------------------------------
#VISUALISATION
#This visualisation shows us that as n grows our random sample averages converge to 0.467
  
cumulative_mean<- cumsum(simlist)/1:n

plot(1:n, cumulative_mean,type='p', col="green4",
     xlab="Number of tosses",
     ylab="Simulated probability",
     main= "Convergence to true probablity simulation")

abline(h=0.467, col="salmon", lw= 2)
#Based on an in-class example by Dr. Wei-Min Huang 






