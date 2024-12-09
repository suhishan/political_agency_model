library(rethinking)
library(tidyverse)
library(conflicted)


# An extremely simple model -----------------------------------------------

t <- c(1,2) # two time periods.

beta <- 0.9 # future discount factor.
pi <- 0.5 # probability that a randomly picked politician is congruent.
# we will update this later to be a probability distribution.
E <- 10 # let ego rent E be 10

# According to the model, nature determines state of the world (s) and pi
# (probability of picking a congruent politician).

# This happens in each period.
N <- 200 #let's conduct 200 simulations of the process.

s <- sample(c(0,1), N, replace = TRUE) # randomized states of the world.
r_1 <- rnorm(N, mean = 40, sd = 10) # for now its normal, we'll think about better distributions later.

#Candidate pool :1 is for congruent and 2 is for dissonants.
cand_pool <- rbinom(N,1, prob = 0.5) +1

# Policy Action is a function of s and cand_pool
payoff <- vector(length = N) # delta here is 1 and no payoff is 0
payoff <- ifelse(cand_pool == 2, ifelse(r_1 > beta*(40 + E),0,1), 1)

# Empirical delta is the probability that the dissonant politicians acted as congruent
# given the insufficiency of rent in period 1.
emp_delta <- 1 - (sum(payoff == 0)/sum(cand_pool == 2))

# Then by bayes rule, the new pi, i.e. perceived pi of voters after seeing the
# payoff is:

big_pi <- pi/ (pi + (1-pi)*emp_delta)



# Let's make a function that does this for us -----------------------------

payoff_func <- function(mu, N, beta, pi, E) {
  s <- sample(c(0,1), N, replace = TRUE) # randomized states of the world.
  r_1 <- rnorm(N[cand_pool == 2], mean = mu, sd = 10) #realized rents in period 1.
  cand_pool <- rbinom(N,1, prob = 0.5) +1 # candidate pool. 1 for congruent and 2 for dissonants
  
  payoff <- vector(length = N) # delta here is 1 and no payoff is 0
  payoff <- ifelse(cand_pool == 2, ifelse(r_1 < beta*(mu + E),1,0), 1)
  
  emp_delta <- 1 - (sum(payoff == 0)/sum(cand_pool == 2))
  big_pi <- pi/ (pi + (1-pi)*emp_delta)
  
  return (list(empirical_delta = emp_delta,
              pi = pi,
              big_pi = big_pi))
}

a <- payoff_func(mu = 30, N = 1e4, beta = 0.9, pi = 0.5, E = 10)
