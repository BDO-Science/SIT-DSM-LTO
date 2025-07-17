
require(boot)

# Stochastic model takes proportion of population affected and turns it into a probability
  # E.g., 0.2 (20%) of population affected by stranding = 0.2 probability of stranding occurring
    # rbinom(1,1,0.2) -> either a 1 or 0
  # Some systems, like Butte Creek, have low but clearly non-zero proportions
    # 0.09 for stranding effect (inps$P.strand.early) and 0.07 for predation effect (inps$High.pred)
    # Average = 0.08
  # Model then applies either the 0 or 1 from rbinom(), and multiplies it by the effect size (-5)

x <- numeric(100000) # Transforms random values using inverse-logit
y <- numeric(100000) # Random values without transformation

prob <- 0.08

for(i in 1:100000){
  x[i] <- inv.logit(sum(rbinom(2,1,prob)*-5))
  y[i] <- sum(rbinom(2,1,prob)*-5)
}
mean(x); mean(y)

# Deterministic model simply takes proportion of population affected and multiplies this by effect size

z1 <- inv.logit(prob*-5+prob*-5) # Deterministic value with inverse-logit transformation
z2 <- prob*-5+prob*-5 # Deterministic effect size without transformation
z1; z2

# You can see that random draws alone (y) produce an expected parameter value 
  # equal to the deterministic value without transformation (z2)
  # I.e., mean(y) = z2
# Random draws combined with the non-linear transformations (x), however, 
  # produces biased values relative to z1
  # I.e., mean(x) != z1

# By comparing mean(x) and fixed value above, you can see that application of stochasticity
  # with non-zero but low probability events (and a size=2) changes the realized effect 
  # of the two covariates in question (stranding and predation) on survival, when combined
  # with an inverse-logit transformation, as applied in the life_cycle_model() function