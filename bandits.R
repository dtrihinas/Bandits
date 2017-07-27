EpsilonGreedyBandit <- function (n, e = 0.1) {
  if (n < 0 || e < 0.0) stop("narms must be > 0 and epsilon must be in range [0,1]")
  value <- list(narms=n, epsilon=e, cnts = matrix(0, ncol = n), vals = matrix(0, ncol = n))
  
  attr(value, "class") <- "EpsilonGreedyBandit"
  
  return(value)
}

selectArm <- function(bandit) {UseMethod("selectArm")}
updateBandit <- function(bandit, chosen_arm, reward) {UseMethod("updateBandit")}

selectArm.EpsilonGreedyBandit <- function(bandit) {
  if (runif(1) > bandit$epsilon) 
    i <- which.max(bandit$vals)
  else
    i <- round(runif(1,1,length(bandit$vals)))
  
  return(i)
}

updateBandit.EpsilonGreedyBandit <- function(bandit, chosen_arm, reward) {
  bandit$cnts[chosen_arm] <- bandit$cnts[chosen_arm] + 1
  n <- bandit$cnts[chosen_arm]
  
  old_v <- bandit$vals[chosen_arm]
  new_v <- ((n - 1) / n) * old_v + (1 / n) * reward
  bandit$vals[chosen_arm] <- new_v
  
  return(bandit)
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


testEpsilonGreedyBandit <- function(arms, nsims, horizon) {
  chosen_arms <- matrix(0.0, ncol=nsims*horizon)
  rewards <- matrix(0.0, ncol=nsims*horizon)
  cumulative_rewards <- matrix(0.0, ncol=nsims*horizon)
  sim_nums <- matrix(0.0, ncol=nsims*horizon)
  times <- matrix(0.0, ncol=nsims*horizon)
  
  bandits <- list()
  
  for (sim in 1:nsims) {
    bandit<- EpsilonGreedyBandit(length(arms))
    
    for (t in 1:horizon) {
      idx = (sim - 1) * horizon + t
      
      sim_nums[idx] = sim
      times[idx] = t
      
      chosen_arm = selectArm(bandit)
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

TestRun1 <- function() {
  m <- c(0.1, 0.1, 0.1, 0.1, 0.9)
  m <- sample(m)
  arms <- lapply(m, function(x) BernoulliArm(x))
  
  return(testEpsilonGreedyBandit(arms, 3, 100))
}