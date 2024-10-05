## Functions to assess different likelihoods for Bayesian Model
## Written by Jared Grooms
## 04/06/2024

## load in libraries 
library(nimble)

# options(warn=-1)

## Define Nimble Functions for Different Rally Likelihoods #####################
## Log Scale Only

## Zero One Modified Geometric
dZOMG <- nimbleFunction(
  
  run = function(x = integer(), 
                 zeroProb = double(),
                 oneProb = double(),
                 geomProb = double(),
                 log = logical(0, default = 0)) {
    returnType(double())
    ## First handle non-zero and non-one data
    if(x > 1) {
      ## return the log probability if log = TRUE
      if(log) return(log((1 - zeroProb - oneProb) * geomProb * 
                           pow(1 - geomProb, x - 2)))
      ## or the probability if log = FALSE
      else return((1 - zeroProb - oneProb) * geomProb * 
                    pow(1 - geomProb, x - 2))
    } else if (x == 1) {
      ## return the log probability if log = TRUE
      if(log) return(log(oneProb))
      ## or the probability if log = FALSE
      else return(oneProb)
    } else if(x == 0) {
      ## return the log probability if log = TRUE
      if(log) return(log(zeroProb))
      ## or the probability if log = FALSE
      else return(zeroProb)
    }
  }
)

# register the distribution
registerDistributions(list(
  dZOMG = list(
    BUGSdist = "dZOMG(zeroProb, oneProb, geomProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'zeroProb = double()', 
              'oneProb = double()', 'geomProb = double()')
  )))

## Zero One Two Three Modified Geometric
dZ12MG <- nimbleFunction(
  
  run = function(x = integer(), 
                 zeroProb = double(),
                 oneProb = double(),
                 twoProb = double(),
                 geomProb = double(),
                 log = logical(0, default = 0)) {
    returnType(double())
    if(x > 2) {
      if(log) return(log((1 - zeroProb - oneProb - twoProb) * geomProb * pow(1 - geomProb, x - 3)))
      else return((1 - zeroProb - oneProb - twoProb) * geomProb * pow(1 - geomProb, x - 3))
    } else if (x == 2) {
      if(log) return(log(twoProb))
      else return(twoProb)
    } else if (x == 1) {
      if(log) return(log(oneProb))
      else return(oneProb)
    } else if(x == 0) {
      if(log) return(log(zeroProb))
      else return(zeroProb)
    }
  }
)

registerDistributions(list(
  dZ12MG = list(
    BUGSdist = "dZ12MG(zeroProb, oneProb, twoProb, geomProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'zeroProb = double()', 'oneProb = double()', 
              'twoProb = double()', 'geomProb = double()')
  )))

## Zero One Two Three Modified Geometric
dZ123MG <- nimbleFunction(
  
  run = function(x = integer(), 
                 zeroProb = double(),
                 oneProb = double(),
                 twoProb = double(),
                 threeProb = double(),
                 geomProb = double(),
                 log = logical(0, default = 0)) {
    returnType(double())
    if(x > 3) {
      if(log) return(log((1 - zeroProb - oneProb - twoProb - threeProb) * 
                           geomProb * pow(1 - geomProb, x - 4)))
      else return((1 - zeroProb - oneProb - twoProb - threeProb) * geomProb *
                    pow(1 - geomProb, x - 4))
    } else if (x == 3) {
      if(log) return(log(threeProb))
      else return(threeProb)
    } else if (x == 2) {
      if(log) return(log(twoProb))
      else return(twoProb)
    } else if (x == 1) {
      if(log) return(log(oneProb))
      else return(oneProb)
    } else if(x == 0) {
      if(log) return(log(zeroProb))
      else return(zeroProb)
    }
  }
)

registerDistributions(list(
  dZ123MG = list(
    BUGSdist = "dZ123MG(zeroProb, oneProb, twoProb, threeProb, geomProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'zeroProb = double()', 'oneProb = double()', 
              'twoProb = double()', 'threeProb = double()', 
              'geomProb = double()')
  )))

## Zero One Two Three  Four Modified Geometric
dZ1234MG <- nimbleFunction(
  
  run = function(x = integer(), 
                 zeroProb = double(),
                 oneProb = double(),
                 twoProb = double(),
                 threeProb = double(),
                 fourProb = double(),
                 geomProb = double(),
                 log = logical(0, default = 0)) {
    returnType(double())
    if(x > 4) {
      if(log) return(log((1 - zeroProb - oneProb - twoProb - threeProb - fourProb) * 
                           geomProb * pow(1 - geomProb, x - 5)))
      else return((1 - zeroProb - oneProb - twoProb - threeProb - fourProb) * 
                    geomProb * pow(1 - geomProb, x - 5))
    } else if (x == 4) {
      if(log) return(log(fourProb))
      else return(fourProb)
    } else if (x == 3) {
      if(log) return(log(threeProb))
      else return(threeProb)
    } else if (x == 2) {
      if(log) return(log(twoProb))
      else return(twoProb)
    } else if (x == 1) {
      if(log) return(log(oneProb))
      else return(oneProb)
    } else if(x == 0) {
      if(log) return(log(zeroProb))
      else return(zeroProb)
    }
  }
)

registerDistributions(list(
  dZ1234MG = list(
    BUGSdist = "dZ1234MG(zeroProb, oneProb, twoProb, threeProb, fourProb, geomProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'zeroProb = double()', 'oneProb = double()', 
              'twoProb = double()', 'threeProb = double()', 'fourProb = double()', 
              'geomProb = double()')
  )))

## Zero Truncated One Modified Geometric
dZT1MG <- nimbleFunction(
  
  run = function(x = integer(), 
                 oneProb = double(),
                 geomProb = double(),
                 log = logical(0, default = 0)) {
    returnType(double())
    if(x > 1) {
      if(log) return(log((1 - oneProb) / (1 - geomProb) * pow(1 - geomProb, x - 1) * geomProb))
      else return((1 - oneProb) / (1 - geomProb) * pow(1 - geomProb, x - 1) * geomProb)
    } else if (x == 1) {
      if(log) return(log(oneProb))
      else return(oneProb)
    } 
  }
)

registerDistributions(list(
  dZT1MG = list(
    BUGSdist = "dZT1MG(oneProb, geomProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'oneProb = double()', 
              'geomProb = double()')
  )))

## Zero Truncated One Two Modified Geometric
dZT12MG <- nimbleFunction(
  
  run = function(x = integer(), 
                 oneProb = double(),
                 twoProb = double(),
                 geomProb = double(),
                 log = logical(0, default = 0)) {
    returnType(double())
    if(x > 2) {
      if(log) return(log((1 - oneProb - twoProb) / pow(1 - geomProb, 2) * pow(1 - geomProb, x - 1) * geomProb))
      else return((1 - oneProb - twoProb) / pow(1 - geomProb, 2) * pow(1 - geomProb, x - 1) * geomProb)
    } else if (x == 2) {
      if(log) return(log(twoProb))
      else return(twoProb)
    } else if (x == 1) {
      if(log) return(log(oneProb))
      else return(oneProb)
    } 
  }
)

registerDistributions(list(
  dZT12MG = list(
    BUGSdist = "dZT12MG(oneProb, twoProb, geomProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'oneProb = double()', 
              'twoProb = double()', 'geomProb = double()')
  )))

## Zero Truncated One Two Three Modified Geometric
dZT123MG <- nimbleFunction(
  
  run = function(x = integer(), 
                 oneProb = double(),
                 twoProb = double(),
                 threeProb = double(),
                 geomProb = double(),
                 log = logical(0, default = 0)) {
    returnType(double())
    if(x > 3) {
      if(log) return(log((1 - oneProb - twoProb - threeProb) / pow(1 - geomProb, 3) * pow(1 - geomProb, x - 1) * geomProb))
      else return((1 - oneProb - twoProb - threeProb) / pow(1 - geomProb, 3) * pow(1 - geomProb, x - 1) * geomProb)
    } else if (x == 3) {
      if(log) return(log(threeProb))
      else return(threeProb)
    } else if (x == 2) {
      if(log) return(log(twoProb))
      else return(twoProb)
    } else if (x == 1) {
      if(log) return(log(oneProb))
      else return(oneProb)
    } 
  }
)

registerDistributions(list(
  dZT123MG = list(
    BUGSdist = "dZT123MG(oneProb, twoProb, threeProb, geomProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'oneProb = double()', 'twoProb = double()', 
              'threeProb = double()', 'geomProb = double()')
  )))

## Zero Truncated One Two Three Four Modified Geometric
dZT1234MG <- nimbleFunction(
  
  run = function(x = integer(), 
                 oneProb = double(),
                 twoProb = double(),
                 threeProb = double(),
                 fourProb = double(),
                 geomProb = double(),
                 log = logical(0, default = 0)) {
    returnType(double())
    if(x > 4) {
      if(log) return(log((1 - oneProb - twoProb - threeProb - fourProb) / pow(1 - geomProb, 4) * pow(1 - geomProb, x - 1) * geomProb))
      else return((1 - oneProb - twoProb - threeProb - fourProb) / pow(1 - geomProb, 4) * pow(1 - geomProb, x - 1) * geomProb)
    } else if (x == 4) {
      if(log) return(log(fourProb))
      else return(fourProb)
    } else if (x == 3) {
      if(log) return(log(threeProb))
      else return(threeProb)
    } else if (x == 2) {
      if(log) return(log(twoProb))
      else return(twoProb)
    } else if (x == 1) {
      if(log) return(log(oneProb))
      else return(oneProb)
    } 
  }
)

registerDistributions(list(
  dZT1234MG = list(
    BUGSdist = "dZT1234MG(oneProb, twoProb, threeProb, fourProb, geomProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'oneProb = double()', 'twoProb = double()', 
              'threeProb = double()', 'fourProb = double()', 'geomProb = double()')
  )))

## Nimble Code for Various Posteriors Based on Current Likelihoods #############

rally_dist_code = nimbleCode({

  if(like_code == 4) { ## z1mg

    if (hierarchical == 0) {
      p0 ~ dbeta(1,19) ## prior on p0 centered at .05
      p1 ~ dbeta(20,40) ## prior on p1 centered at .33
      p ~ dbeta(10,33) ## prior on p centered at .23
      
      for(i in 1:player_n){
        player_counts[i] ~ dZOMG(p0, p1, p)
      }
    } else {
      for(i in 1:5){
        p0[i] ~ dbeta(a,b) ## prior on p0 centered at .05
        p1[i] ~ dbeta(c,d) ## prior on p1 centered at .33
        p[i] ~ dbeta(e,f) ## prior on p centered at .23
      }
      
      ## hyper parameters 
      a ~ dgamma(shape = 1, rate = 1)
      b ~ dgamma(shape = 19, rate = 1)
      c ~ dgamma(shape = 20, rate = 1)
      d ~ dgamma(shape = 40, rate = 1)
      e ~ dgamma(shape = 10, rate = 1)
      f ~ dgamma(shape = 33, rate = 1)
      
      for(i in 1:player_n){
        player_counts[i] ~ dZOMG(p0[index[i]], p1[index[i]], p[index[i]])
      }
    }
  } else if(like_code == 5) { ## z123mg

    p0 ~ dbeta(1,19) ## prior on p0 centered at .05
    p1 ~ dbeta(20,40) ## prior on p1 centered at .33
    p2 ~ dbeta(20, 46) ## prior on p2 centered at .30
    p3 ~ dbeta(20, 50)## prior on p3 centered at .28
    p ~ dbeta(10,33) ## prior on p centered at .23

    for(i in 1:player_n){
      player_counts[i] ~ dZ123MG(p0, p1, p2, p3, p)
    }
  } else if(like_code == 6) { ## z1234mg

    p0 ~ dbeta(1,19) ## prior on p0 centered at .05
    p1 ~ dbeta(20,40) ## prior on p1 centered at .33
    p2 ~ dbeta(20, 46) ## prior on p2 centered at .30
    p3 ~ dbeta(20, 50)## prior on p3 centered at .28
    p4 ~ dbeta(20, 55)## prior on p3 centered at .25
    p ~ dbeta(10,33) ## prior on p centered at .23

    for(i in 1:player_n){
      player_counts[i] ~ dZ1234MG(p0, p1, p2, p3, p4, p)
    }
  } else if(like_code == 7) { ## zt1mg
    
    p1 ~ dbeta(20,40)
    p ~ dbeta(10,33)
    
    for(i in 1:player_n){
      player_counts[i] ~ dZT1MG(p1, p)
    }
  } else if(like_code == 8) { ## zt123mg
    
    p1 ~ dbeta(20,40)
    p2 ~ dbeta(20, 46)
    p3 ~ dbeta(20, 50)
    p ~ dbeta(10,33)
    
    for(i in 1:player_n){
      player_counts[i] ~ dZT123MG(p1, p2, p3, p)
    }
  } else if(like_code == 9) { ## zt1234mg
    
    p1 ~ dbeta(20,40) 
    p2 ~ dbeta(20, 46) 
    p3 ~ dbeta(20, 50)
    p4 ~ dbeta(20, 55)
    p ~ dbeta(10,33) 
    
    for(i in 1:player_n){
      player_counts[i] ~ dZT1234MG(p1, p2, p3, p4, p)
    }
  } else if(like_code == 10) { ## z12mg
    
    p0 ~ dbeta(1,19)
    p1 ~ dbeta(20,40) 
    p2 ~ dbeta(20, 46) 
    p ~ dbeta(10,33) 
    
    for(i in 1:player_n){
      player_counts[i] ~ dZ12MG(p0, p1, p2, p)
    }
  } else if(like_code == 11) { ## zt12mg
    
    p1 ~ dbeta(20,40) 
    p2 ~ dbeta(20, 46) 
    p ~ dbeta(10,33) 
    
    for(i in 1:player_n){
      player_counts[i] ~ dZT12MG(p1, p2, p)
    }
  }

})

## Function to create nimble data ##############################################

make_nimble_data <- function(data, like_code, num_params, hierarchical = 0, num_players) {
  
  if(hierarchical == 0) {
    
    if(like_code %in% c(4,5,6,10)) {
      
      if(num_params == 3) {
        p0 <- .05 ## initialize starting values
        p1 <- .33
        p <- .23
        nimble_heirarchical_inits <- list(p0 = p0, p1 = p1, p = p)
      } else if (num_params == 4) {
        p0 <- .05 
        p1 <- .33
        p2 <- .30
        p <- .17
        nimble_heirarchical_inits <- list(p0 = p0, p1 = p1, 
                                          p2 = p2, p = p)
      } else if (num_params == 5) {
        p0 <- .05 
        p1 <- .33
        p2 <- .30
        p3 <- .27
        p <- .17
        nimble_heirarchical_inits <- list(p0 = p0, p1 = p1, p2 = p2, 
                                          p3 = p3, p = p)
      } else if (num_params == 6) {
        p0 <- .05 
        p1 <- .33
        p2 <- .30
        p3 <- .27
        p4 <- .24
        p <- .17
        nimble_heirarchical_inits <- list(p0 = p0, p1 = p1, p2 = p2, 
                                          p3 = p3, p4 = p4, p = p)
      }
      
    } else if (like_code %in% c(7, 8, 9, 11)) {
      
      if (num_params == 2) {
        p1 <- .33
        p <- .23
        nimble_heirarchical_inits <- list(p1 = p1, p = p)
      } else if (num_params == 3) {
        p1 <- .33
        p2 <- .30
        p <- .17
        nimble_heirarchical_inits <- list(p1 = p1, p2 = p2, p = p)
      } else if (num_params == 4) {
        p1 <- .33
        p2 <- .30
        p3 <- .27
        p <- .17
        nimble_heirarchical_inits <- list(p1 = p1, p2 = p2, 
                                          p3 = p3, p = p)
      } else if (num_params == 5) {
        p1 <- .33
        p2 <- .30
        p3 <- .27
        p4 <- .24
        p <- .17
        nimble_heirarchical_inits <- list(p1 = p1, p2 = p2, 
                                          p3 = p3, p4 = p4, p = p)
      }
      
    }
    
    player_n <- nrow(data) ## initialize constants
    nimble_heirarchical_constants <- list(player_n = player_n,
                                          like_code = like_code)
    
    player_counts <- data$new_rally_count ## initialize data
    nimble_heirarchical_data <- list(player_counts = player_counts)
    
    master_list <- list(nimble_heirarchical_data, 
                        nimble_heirarchical_constants,
                        nimble_heirarchical_inits)
    
    return(master_list)
    
  } else if (hierarchical == 1) {
    
    if(like_code %in% c(4,5,6,10)) {
      
      if(num_params == 3) {
        p0 <- rep(.05, num_players)
        p1 <- rep(.33, num_players)
        p <- rep(.23, num_players)
        nimble_heirarchical_inits <- list(p0 = p0, p1 = p1, p = p,
                                          a = 5/10, b = 100/10, 
                                          c = 33/10, d = 100/10, 
                                          e = 30/10, f = 100/10)
      } else if (num_params == 4) {
        p0 <- rep(.05, num_players) 
        p1 <- rep(.33, num_players)
        p2 <- rep(.30, num_players)
        p <- rep(.17, num_players)
        nimble_heirarchical_inits <- list(p0 = p0, p1 = p1, 
                                          p2 = p2, p = p,
                                          a = 5/10, b = 100/10, 
                                          c = 33/10, d = 100/10, 
                                          e = 30/10, f = 100/10,
                                          g = 27/10, h = 100/10)
      } else if (num_params == 5) {
        p0 <- rep(.05, num_players) 
        p1 <- rep(.33, num_players)
        p2 <- rep(.30, num_players)
        p3 <- rep(.27, num_players)
        p <- rep(.17, num_players)
        nimble_heirarchical_inits <- list(p0 = p0, p1 = p1, p2 = p2, 
                                          p3 = p3, p = p,
                                          a = 5/10, b = 100/10, 
                                          c = 33/10, d = 100/10, 
                                          e = 30/10, f = 100/10,
                                          g = 27/10, h = 100/10, 
                                          i = 24/10, j = 100/10)
      } else if (num_params == 6) {
        p0 <- rep(.05, num_players) 
        p1 <- rep(.33, num_players)
        p2 <- rep(.30, num_players)
        p3 <- rep(.27, num_players)
        p4 <- rep(.24, num_players)
        p <- rep(.17, num_players)
        nimble_heirarchical_inits <- list(p0 = p0, p1 = p1, p2 = p2, 
                                          p3 = p3, p4 = p4, p = p,
                                          a = 5/10, b = 100/10, 
                                          c = 33/10, d = 100/10, 
                                          e = 30/10, f = 100/10,
                                          g = 27/10, h = 100/10, 
                                          i = 24/10, j = 100/10, 
                                          k = 17/10, l = 100/10)
      }
      
    } else if (like_code %in% c(7, 8, 9, 11)) {
      
      if (num_params == 2) {
        p1 <- rep(.33, num_players)
        p <- rep(.23, num_players)
        nimble_heirarchical_inits <- list(p1 = p1, p = p,
                                          a = 33/10, b = 100/10,
                                          c = 30/10, d = 100/10)
      } else if (num_params == 3) {
        p1 <- rep(.33, num_players)
        p2 <- rep(.30, num_players)
        p <- rep(.17, num_players)
        nimble_heirarchical_inits <- list(p1 = p1, p2 = p2, p = p,
                                          a = 33/10, b = 100/10,
                                          c = 30/10, d = 100/10,
                                          e = 27/10, f = 100/10)
      } else if (num_params == 4) {
        p1 <- rep(.33, num_players)
        p2 <- rep(.30, num_players)
        p3 <- rep(.27, num_players)
        p <- rep(.17, num_players)
        nimble_heirarchical_inits <- list(p1 = p1, p2 = p2, 
                                          p3 = p3, p = p,
                                          a = 33/10, b = 100/10,
                                          c = 30/10, d = 100/10,
                                          e = 27/10, f = 100/10,
                                          g = 24/10, h = 100/10)
      } else if (num_params == 5) {
        p1 <- rep(.33, num_players)
        p2 <- rep(.30, num_players)
        p3 <- rep(.27, num_players)
        p4 <- rep(.24, num_players)
        p <- rep(.17, num_players)
        nimble_heirarchical_inits <- list(p1 = p1, p2 = p2, 
                                          p3 = p3, p4 = p4, p = p,
                                          a = 33/10, b = 100/10,
                                          c = 30/10, d = 100/10,
                                          e = 27/10, f = 100/10,
                                          g = 24/10, h = 100/10,
                                          i = 17/10, j = 100/10)
      }
      
    }
    
    ## player by player counts and player counts sample size
    player_counts <- c()
    temp_n <- c()
    for(i in 1:5){
      counts <- data[data$id == i,]$new_rally_count
      n_counts <- length(counts)
      player_counts <- c(player_counts, counts)
      temp_n <- c(temp_n, n_counts)
    }
    player_n <- length(player_counts)
    
    ## player by player indices
    index <- c()
    for(i in 1:5) {
      index <- c(index, rep(i, temp_n[i]))
    }
    
    ## final nimble data and constants
    nimble_heirarchical_data <- list(player_counts = player_counts)
    nimble_heirarchical_constants <- list(index = index,
                                          player_n = player_n)
    
    master_list <- list(nimble_heirarchical_data, 
                        nimble_heirarchical_constants,
                        nimble_heirarchical_inits)
    return(master_list)
    
  }
}

run_nimble_model <- function(data, monitors, burn, iter, chains, thin) {
  
  rally_heirarchical <- nimbleMCMC(code=rally_dist_code,
                                   constants = data[[2]],
                                   inits = data[[3]],
                                   data = data[[1]],
                                   nchains = chains, niter = iter,
                                   nburnin=burn, thin = thin,
                                   samplesAsCodaMCMC = TRUE,
                                   summary = TRUE, WAIC = TRUE,
                                   monitors = monitors)
  return(rally_heirarchical)
  
}


