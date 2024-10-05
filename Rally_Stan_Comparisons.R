## STAN WAIC Comparisons for Bayesian Rally Length Model
## Written by Jared Grooms
## 4/25/2024


## load in libraries ###########################################################
library(rstan)
library(tidyverse)
library(coda)
library(loo)

## set working directory
setwd("C:/Users/jared/OneDrive/Desktop/Tennis3")

################################################################################

## Prepare the data ############################################################

rally_data <- read.csv("all_data_surfaces.csv", header = TRUE)

## All Serves
men_hard_rally_counts <- rally_data %>% 
  filter(sex == 1 & Surface == "Hard") %>% 
  select(new_rally_count)
men_clay_rally_counts <- rally_data %>% 
  filter(sex == 1 & Surface == "Clay") %>% 
  select(new_rally_count)
men_grass_rally_counts <- rally_data %>% 
  filter(sex == 1 & Surface == "Grass") %>% 
  select(new_rally_count)

## First Serves
men_hard_first <-  rally_data %>% 
  filter(sex == 1 & Surface == "Hard" & X1stIn == 1) %>% 
  select(new_rally_count)
men_clay_first <-  rally_data %>% 
  filter(sex == 1 & Surface == "Clay" & X1stIn == 1) %>% 
  select(new_rally_count)
men_grass_first <-  rally_data %>% 
  filter(sex == 1 & Surface == "Grass" & X1stIn == 1) %>% 
  select(new_rally_count)

## Second Serves
men_hard_second <-  rally_data %>% 
  filter(sex == 1 & Surface == "Hard" & X1stIn == 0) %>% 
  select(new_rally_count)
men_clay_second <-  rally_data %>% 
  filter(sex == 1 & Surface == "Clay" & X1stIn == 0) %>% 
  select(new_rally_count)
men_grass_second <-  rally_data %>% 
  filter(sex == 1 & Surface == "Grass" & X1stIn == 0) %>% 
  select(new_rally_count)

################################################################################

## Compare WAIC scores for all liklihoods across serve and surface

## All Serves ##################################################################

# ## grass
# 
# ## save the Stan data
# stan_data <- list(N = nrow(men_grass_rally_counts),
#                   rally_data = men_grass_rally_counts$new_rally_count)
# 
# ## Z1
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z1_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          warmup = 1000,
#                          init = my_inits,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# grass <- waic(log_like)
# grass_waic_z1 <- grass$waic
# 
# ## Z123
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z123_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          warmup = 1000,
#                          init = my_inits,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# grass <- waic(log_like)
# grass_waic_z123 <- grass$waic
# 
# ## Z1234
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z1234_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          init = my_inits,
#                          warmup = 1000,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# grass <- waic(log_like)
# grass_waic_z1234 <- grass$waic
# 
# ################################################################################
# 
# ## hard
# 
# ## save the Stan data
# stan_data <- list(N = nrow(men_hard_rally_counts),
#                   rally_data = men_hard_rally_counts$new_rally_count)
# 
# ## Z1
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z1_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          warmup = 1000,
#                          init = my_inits,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# hard <- waic(log_like)
# hard_waic_z1 <- hard$waic
# 
# ## Z123
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z123_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          warmup = 1000,
#                          init = my_inits,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# hard <- waic(log_like)
# hard_waic_z123 <- hard$waic
# 
# ## Z1234
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z1234_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          warmup = 1000,
#                          init = my_inits,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# hard <- waic(log_like)
# hard_waic_z1234 <- hard$waic
# 
# ################################################################################
# 
# ## clay
# 
# ## save the Stan data
# stan_data <- list(N = nrow(men_clay_rally_counts),
#                   rally_data = men_clay_rally_counts$new_rally_count)
# 
# ## Z1
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z1_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          warmup = 1000,
#                          init = my_inits,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# clay <- waic(log_like)
# clay_waic_z1 <- clay$waic
# 
# ## Z123
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z123_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          warmup = 1000,
#                          init = my_inits,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# clay <- waic(log_like)
# clay_waic_z123 <- clay$waic
# 
# ## Z1234
# 
# my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))
# 
# ## fit the model
# stan_rally_model <- stan(file = 'z1234_stan.stan',
#                          data = stan_data,
#                          iter = 11000,
#                          warmup = 1000,
#                          init = my_inits,
#                          chains = 1)
# 
# stan_samples <- rstan::extract(stan_rally_model)
# 
# log_like <- stan_samples$log_lik
# clay <- waic(log_like)
# clay_waic_z1234 <- clay$waic
# 
# all_waic <- data.frame(Grass = c(grass_waic_z1, grass_waic_z123, grass_waic_z1234),
#                        Clay = c(clay_waic_z1, clay_waic_z123, clay_waic_z1234),
#                        Hard = c(hard_waic_z1, hard_waic_z123, hard_waic_z1234),
#                        row.names = c("Z1", "Z123", "Z1234"))
# write.csv(all_waic, file = "stan_waic.csv")

## First Serves ################################################################

## grass

## men_grass_first <- data.frame(new_rally_count = men_grass_first[sample(nrow(men_grass_first), 1000), ])

## save the Stan data 
stan_data <- list(N = nrow(men_grass_first),
                  rally_data = men_grass_first$new_rally_count)

## ZT1

my_inits <- list(list(p1 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt1_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
grass <- waic(log_like)
grass_waic_zt1_first <- grass$waic

# plot(stan_samples$p1, type = "l")
# plot(stan_samples$p, type = "l")

## ZT12

my_inits <- list(list(p1 = .1, p2 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt12_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
grass <- waic(log_like)
grass_waic_zt12_first <- grass$waic

# plot(stan_samples$p1, type = "l")
# plot(stan_samples$p2, type = "l")
# plot(stan_samples$p, type = "l")

## ZT123

my_inits <- list(list(p1 = .1, p2 = .1, p3 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt123_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
grass <- waic(log_like)
grass_waic_zt123_first <- grass$waic

# plot(stan_samples$p1, type = "l")
# plot(stan_samples$p2, type = "l")
# plot(stan_samples$p3, type = "l")
# plot(stan_samples$p, type = "l")

## ZT1234

my_inits <- list(list(p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt1234_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         init = my_inits,
                         warmup = 1000,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
grass <- waic(log_like)
grass_waic_zt1234_first <- grass$waic

################################################################################

## hard

## men_hard_first <- data.frame(new_rally_count = men_hard_first[sample(nrow(men_hard_first), 1000), ])

## save the Stan data
stan_data <- list(N = nrow(men_hard_first),
                  rally_data = men_hard_first$new_rally_count)

## ZT1

my_inits <- list(list(p1 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt1_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
hard <- waic(log_like)
hard_waic_zt1_first <- hard$waic

## ZT12

my_inits <- list(list(p1 = .1, p2 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt12_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
hard <- waic(log_like)
hard_waic_zt12_first <- hard$waic

## ZT123

my_inits <- list(list(p1 = .1, p2 = .1, p3 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt123_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
hard <- waic(log_like)
hard_waic_zt123_first <- hard$waic

## ZT1234

my_inits <- list(list(p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt1234_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
hard <- waic(log_like)
hard_waic_zt1234_first <- hard$waic

################################################################################

## clay

## men_clay_first <- data.frame(new_rally_count = men_clay_first[sample(nrow(men_clay_first), 1000), ])

## save the Stan data
stan_data <- list(N = nrow(men_clay_first),
                  rally_data = men_clay_first$new_rally_count)

## ZT1

my_inits <- list(list(p1 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt1_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
clay <- waic(log_like)
clay_waic_zt1_first <- clay$waic

## ZT12

my_inits <- list(list(p1 = .1, p2 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt12_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
clay <- waic(log_like)
clay_waic_zt12_first <- clay$waic

## ZT123

my_inits <- list(list(p1 = .1, p2 = .1, p3 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt123_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
clay <- waic(log_like)
clay_waic_zt123_first <- clay$waic

## ZT1234

my_inits <- list(list(p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'zt1234_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
clay <- waic(log_like)
clay_waic_zt1234_first <- clay$waic

first_waic <- data.frame(Grass = c(grass_waic_zt1_first, grass_waic_zt12_first, grass_waic_zt123_first, grass_waic_zt1234_first),
                       Clay = c(clay_waic_zt1_first, clay_waic_zt12_first, clay_waic_zt123_first, clay_waic_zt1234_first),
                       Hard = c(hard_waic_zt1_first, hard_waic_zt12_first, hard_waic_zt123_first, hard_waic_zt1234_first),
                       row.names = c("ZT1", "ZT12", "ZT123", "ZT1234"))
write.csv(first_waic, file = "stan_waic_first.csv")

## Second Serves ###############################################################

## grass

## men_grass_second <- data.frame(new_rally_count = men_grass_second[sample(nrow(men_grass_second), 100), ])

## save the Stan data 
stan_data <- list(N = nrow(men_grass_second),
                  rally_data = men_grass_second$new_rally_count)

## Z1

my_inits <- list(list(p0 = .1, p1 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z1_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
grass <- waic(log_like)
grass_waic_z1_second <- grass$waic

## Z12

my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z12_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
grass <- waic(log_like)
grass_waic_z12_second <- grass$waic

## Z123

my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z123_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
grass <- waic(log_like)
grass_waic_z123_second <- grass$waic

## Z1234

my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z1234_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         init = my_inits,
                         warmup = 1000,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
grass <- waic(log_like)
grass_waic_z1234_second <- grass$waic

################################################################################

## hard

## men_hard_second <- data.frame(new_rally_count = men_hard_second[sample(nrow(men_hard_second), 100), ])

## save the Stan data
stan_data <- list(N = nrow(men_hard_second),
                  rally_data = men_hard_second$new_rally_count)

## Z1

my_inits <- list(list(p0 = .1, p1 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z1_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
hard <- waic(log_like)
hard_waic_z1_second <- hard$waic

## Z12

my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z12_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
hard <- waic(log_like)
hard_waic_z12_second <- hard$waic

## Z123

my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z123_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
hard <- waic(log_like)
hard_waic_z123_second <- hard$waic

## Z1234

my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z1234_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
hard <- waic(log_like)
hard_waic_z1234_second <- hard$waic

################################################################################

## clay

## men_clay_second <- data.frame(new_rally_count = men_clay_second[sample(nrow(men_clay_second), 100), ])

## save the Stan data
stan_data <- list(N = nrow(men_clay_second),
                  rally_data = men_clay_second$new_rally_count)

## Z1

my_inits <- list(list(p0 = .1, p1 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z1_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
clay <- waic(log_like)
clay_waic_z1_second <- clay$waic

## Z12

my_inits <- list(list(p0 = .1, p1 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z12_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
clay <- waic(log_like)
clay_waic_z12_second <- clay$waic

## Z123

my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z123_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
clay <- waic(log_like)
clay_waic_z123_second <- clay$waic

## Z1234

my_inits <- list(list(p0 = .1, p1 = .1, p2 = .1, p3 = .1, p4 = .1, p = .1))

## fit the model
stan_rally_model <- stan(file = 'z1234_stan.stan',
                         data = stan_data,
                         iter = 11000,
                         warmup = 1000,
                         init = my_inits,
                         chains = 1)

stan_samples <- rstan::extract(stan_rally_model)

log_like <- stan_samples$log_lik
clay <- waic(log_like)
clay_waic_z1234_second <- clay$waic

second_waic <- data.frame(Grass = c(grass_waic_z1_second, grass_waic_z12_second, grass_waic_z123_second, grass_waic_z1234_second),
                         Clay = c(clay_waic_z1_second, clay_waic_z12_second, clay_waic_z123_second, clay_waic_z1234_second),
                         Hard = c(hard_waic_z1_second, hard_waic_z12_second, hard_waic_z123_second, hard_waic_z1234_second),
                         row.names = c("Z1", "Z12", "Z123", "Z1234"))
write.csv(second_waic, file = "stan_waic_second.csv")

