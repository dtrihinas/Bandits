SoftMaxBandit <- function (n, tau = 0.1) {
  if (n < 0 || tau < 0.0) stop("narms must be > 0 and tau must be in range [0,1]")
  value <- list(narms=n, tau=tau, cnts = matrix(0, ncol = n), vals = matrix(0, ncol = n), alpha = 0.3)
  
  attr(value, "class") <- "SoftMaxBandit"
  
  return(value)
}

selectArm <- function(bandit) {UseMethod("selectArm")}
updateBandit <- function(bandit, chosen_arm, reward) {UseMethod("updateBandit")}

selectArm.SoftMaxBandit <- function(bandit) {
  z <- sum(exp(bandit$vals/bandit$tau))
  probs <- exp(bandit$vals/bandit$tau)/z
  
  return (categorical_draw(probs))
}

updateBandit.SoftMaxBandit <- function(bandit, chosen_arm, reward) {
  bandit$cnts[chosen_arm] <- bandit$cnts[chosen_arm] + 1
  n <- bandit$cnts[chosen_arm]
  
  old_v <- bandit$vals[chosen_arm]
  #new_v <- ((n - 1) / n) * old_v + (1 / n) * reward
  #EWMA
  new_v <- (1 - bandit$alpha) * old_v + bandit$alpha* reward
  bandit$vals[chosen_arm] <- new_v
  
  return(bandit)
}

categorical_draw <- function(probs) {
  z <- runif(1)
  cum_prob <- 0.0
  for(i in seq_along(probs)) {
    cum_prob <- cum_prob + probs[i]
    if (cum_prob > z)
      return(i)
  }
  
  return (length(probs))
    
}


BernoulliArm <- function (p) {
  if (p < 0.0) stop("p must be in range [0,1]")
  value <- list(p=p)
  
  attr(value, "class") <- "BernoulliArm"
  
  return(value)
}

pullArm <- function(arm) {UseMethod("pullArm")}

pullArm.BernoulliArm <- function(arm) {
  if (runif(1) > arm$p) 
    v <- 0.0
  else
    v <- 1.0
  
  return(v)
}

simSoftMaxBandit <- function(arms, nsims, horizon) {
  chosen_arms <- matrix(0.0, ncol=nsims*horizon)
  rewards <- matrix(0.0, ncol=nsims*horizon)
  cumulative_rewards <- matrix(0.0, ncol=nsims*horizon)
  sim_nums <- matrix(0.0, ncol=nsims*horizon)
  times <- matrix(0.0, ncol=nsims*horizon)
  
  bandits <- list()
  
  for (sim in 1:nsims) {
    bandit<- SoftMaxBandit(length(arms))
    
    for (t in 1:horizon) {
      idx = (sim - 1) * horizon + t
      
      sim_nums[idx] = sim
      times[idx] = t
      
      if (t > length(arms)) 
        chosen_arm = selectArm(bandit)
      else
        chosen_arm = t
      
      chosen_arms[idx] = chosen_arm
      
      arm <- arms[[chosen_arm]]
      
      reward = pullArm(arm)
      rewards[idx] = reward
      
      if (t == 1)
        cumulative_rewards[idx] <- reward
      else
        cumulative_rewards[idx] <- cumulative_rewards[idx - 1] + reward
      
      bandit <- updateBandit(bandit, chosen_arm, reward)
      
      t <- t + 1
    }
    bandits[[sim]] <- bandit
    sim <- sim + 1
  }
  
  return(list(bandits=bandits, sim_nums=sim_nums, times=times, chosen_arms=chosen_arms, 
              rewards=rewards, cumulative_rewards=cumulative_rewards))
}

TestRun2 <- function() {
  m <- c(0.1, 0.1, 0.1, 0.1, 0.9)
  m <- sample(m)
  arms <- lapply(m, function(x) BernoulliArm(x))
  
  return(simSoftMaxBandit(arms, 3, 100))
}