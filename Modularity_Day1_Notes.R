# 13 Feb 2018
# Basic lessons in modularity

# Reference: PVA_ModularityExample.pdf on dropbox

# problem: collaborator wants you to modify a model
  # population going extinct, need to prove it with a model

currentPop <- 52 # current population of individuals
r <- 1.01 #growth rate
k <-60 # carrying capacity
epsilon <- rnorm(1,0,2) #avg winter temperatures, normally distributed w/ mean 0 and std dev 2
t <- 25 #time in years
numberSimulations <- 100

winterMean <- 0
winterSD <- 2

epsilon <- c()
numberExtinct <- 0

pop[1] <- currentPop

for (simulationCounter in 1:numberSimulations){
  for (timeStep in 2:t){
    epsilon[timeStep] <- rnorm(1,winterMean,winterSD)
    pop[timeStep] <- pop[timeStep - 1]*exp(r*(1 - pop[timeStep - 1]/k) + epsilon[timeStep])
    if (pop[timeStep]<1){
      pop[timeStep] <- 0
    }
  }
  if (pop == 0){
    numberExtinct <- numberExtinct + 1
  }
}
probablilityExtinction <- numberExtinct/numberSimulations
probablilityExtinction

# ^badly written code!!,doesn't work, not readable ... redoing...
# the modular approach

# module 1: simulate the population multiple times, keeping all timeteps for all populations

# inputs
p0 # the starting population, 1 number
#model parameters
r
k
nsd #standard deviation of the noise
numsims #number of simulations to do
numsteps # number of time steps to do

#output
#numsims by numsteps+1 matrix, each row has a population time series from a diff simulationCounter
#first entry in each row is p0

# module 2: plotting some or all of the populations

#inputs
m #numsims by numsteps +1 matrix with each row a population time series
pts #vector of which time series to plot, default NA means plot all
logxpl #T/F logic variable indicating whether to plot on log(x+1) scale

#outplot
#the plot

# module 3: calculating extinction risk at any point in future
m
risktimes #vector on indicies from 1 to dim(m)[2] the corresponding times at which extinction risks are given

# step 2: function specifications

# general function description
#
# arguments
#
# outputs



