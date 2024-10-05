## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)


## -----------------------------------------------------------------------------------------------------

## load in libraries
library(nimble)
library(tidyverse)
library(ggplot2)
library(coda)
library(ggpubr)
library(vroom)



## -----------------------------------------------------------------------------------------------------

# Define the log-likelihood functions for all serves or second serves ##########

## Z1 
loglike_z1 <- function(params, data) {
  
  ## use params to initialize beginning guess for mle estimates
  p0 <- params[1]
  p1 <- params[2]
  p <- params[3]
  
  ## sum the log likelihood
  sum(log(p0 * (data == 0) + p1 * (data == 1) + (1 - p0 - p1) * p * (1 - p)^(data - 2) * (data >= 2)))
  
}

## Z123
loglike_z123 <- function(params, data) {
  
  ## use params to initialize beginning guess for mle estimates
  p0 <- params[1]
  p1 <- params[2]
  p2 <- params[3]
  p3 <- params[4]
  p <- params[5]
  
  ## sum the log likelihood
  sum(log(p0 * (data == 0) + p1 * (data == 1) + p2 * (data == 2) + p3 * (data == 3) +
            (1 - p0 - p1 - p2 - p3) * p * (1 - p)^(data - 4) * (data >= 4)))
  
}

## Z1234
loglike_z1234 <- function(params, data) {
  
  ## use params to initialize beginning guess for mle estimates
  p0 <- params[1]
  p1 <- params[2]
  p2 <- params[3]
  p3 <- params[4]
  p4 <- params[5]
  p <- params[6]
  
  ## sum the log likelihood
  sum(log(p0 * (data == 0) + p1 * (data == 1) + p2 * (data == 2) + p3 * (data == 3) + p4 * (data == 4) +
            (1 - p0 - p1 - p2 - p3 - p4) * p * (1 - p)^(data - 5) * (data >= 5)))
  
}

## Define the log-likelihood functions for first serves #########################
## Truncated zeros

## ZT1
loglike_zt1 <- function(params, data) {
  
  ## use params to initialize beginning guess for mle estimates
  p1 <- params[1]
  p <- params[2]
  
  ## sum the log likelihood
  sum(log(p1 * (data == 1) + (1-p1) / (1-p) * (1-p)^(data-1)*p*(data>=2)))
  
}

## ZT123
loglike_zt123 <- function(params, data) {
  
  ## use params to initialize beginning guess for mle estimates
  p1 <- params[1]
  p2 <- params[2]
  p3 <- params[3]
  p <- params[4]
  
  ## sum the log likelihood
  sum(log(p1 * (data == 1) +  p2 * (data == 2) + p3 * (data == 3) + 
            (1-p1-p2-p3) / (1-p)^3 * (1-p)^(data-1)*p*(data>=4)))
  
}

## ZT1234
loglike_zt1234 <- function(params, data) {
  
  ## use params to initialize beginning guess for mle estimates
  p1 <- params[1]
  p2 <- params[2]
  p3 <- params[3]
  p4 <- params[4]
  p <- params[5]
  
  ## sum the log likelihood
  sum(log(p1 * (data == 1) +  p2 * (data == 2) + p3 * (data == 3) + p4 *(data == 4) +
            (1-p1-p2-p3-p4) / (1-p)^4 * (1-p)^(data-1)*p*(data>=5)))
  
}



## -----------------------------------------------------------------------------------------------------

# Functions for MLE Estimation #################################################

# Define the function to perform MLE
mle <- function(data, n_param, serve) {
  
  # Initial guess
  params <- rep(0.1, n_param)
  
  ## make ui
  positive <- diag(n_param)
  negative <- -diag(n_param)
  ui <- rbind(positive, negative)
  
  ## make ci 
  c_zero <- rep(0, n_param)
  c_negative <- rep(-1, n_param)
  ci <- c(c_zero, c_negative)
  
  if(serve == 0 | serve == 2) {
    
    if (n_param == 3) {
      ## use Newton Rhapson to estimate mle
      result <- constrOptim(params,
                  loglike_z1,
                  grad = NULL,
                  # grad=zomg_gradient,
                  control = list(fnscale = -1),
                  ui=ui,
                  ci=ci, 
                  data = data)
    } else if (n_param == 5) {
      ## use Newton Rhapson to estimate mle
      result <- constrOptim(params,
                  loglike_z123,
                  grad = NULL,
                  # grad=zomg_gradient,
                  control = list(fnscale = -1),
                  ui=ui,
                  ci=ci, 
                  data = data)
    } else if (n_param == 6) {
      result <- constrOptim(params,
                loglike_z1234,
                grad = NULL,
                # grad=zomg_gradient,
                control = list(fnscale = -1),
                ui=ui,
                ci=ci, 
                data = data)
    } 
  } else {
    if (n_param == 2) {
      ## use Newton Rhapson to estimate mle
      result <- constrOptim(params,
                  loglike_zt1,
                  grad = NULL,
                  # grad=zomg_gradient,
                  control = list(fnscale = -1),
                  ui=ui,
                  ci=ci, 
                  data = data)
    } else if (n_param == 4) {
      ## use Newton Rhapson to estimate mle
      result <- constrOptim(params,
                  loglike_zt123,
                  grad = NULL,
                  # grad=zomg_gradient,
                  control = list(fnscale = -1),
                  ui=ui,
                  ci=ci, 
                  data = data)
    } else if (n_param == 5) {
      ## use Newton Rhapson to estimate mle
      result <- constrOptim(params,
                  loglike_zt1234,
                  grad = NULL,
                  # grad=zomg_gradient,
                  control = list(fnscale = -1),
                  ui=ui,
                  ci=ci, 
                  data = data)
    }
  }
  # Return estimated parameters
  result$par
}

## function to generate new data based on mle estimates and append to data frame for visulaization
rally_distributions <- function(parms, data, n_params, serve) {
  
  rally_probs <- c()
  
  ## All Serves and Second Serves
  if (serve == 0 | serve == 2) {
    if(n_params == 3) {
    
      rallies <- 2:1000
      p0 <- parms[1]
      p1 <- parms[2]
      p <- parms[3]
      
      for (rally in rallies){
        rally_prob <- ((1-p0-p1)/(1-p)^2)*((1-p)^rally*p)
        rally_probs <- c(rally_probs, rally_prob)
      }
      
      all_probs <- c(p0, p1, rally_probs)
      sim_rallies <- sample(0:1000, size = nrow(data), prob = all_probs, replace = TRUE)
      data_sim <- data.frame(new_rally_count = sim_rallies, id = "sim")
      all_data <- rbind(data_sim, data)
      return(all_data)
    } else if(n_params == 5) {
      
      rallies <- 4:1000
      p0 <- parms[1]
      p1 <- parms[2]
      p2 <- parms[3]
      p3 <- parms[4]
      p <- parms[5]
      
      for (rally in rallies){
        rally_prob <- ((1-p0-p1-p2-p3)/(1-p)^4)*((1-p)^rally*p)
        rally_probs <- c(rally_probs, rally_prob)
      }
      
      all_probs <- c(p0, p1, p2, p3, rally_probs)
      sim_rallies <- sample(0:1000, size = nrow(data), prob = all_probs, replace = TRUE)
      data_sim <- data.frame(new_rally_count = sim_rallies, id = "sim")
      all_data <- rbind(data_sim, data)
      return(all_data)
    } else if(n_params == 6) {
  
      rallies <- 5:1000
      p0 <- parms[1]
      p1 <- parms[2]
      p2 <- parms[3]
      p3 <- parms[4]
      p4 <- parms[5]
      p <- parms[6]
      
      for (rally in rallies){
        rally_prob <- ((1-p0-p1-p2-p3-p4)/(1-p)^5)*((1-p)^rally*p)
        rally_probs <- c(rally_probs, rally_prob)
      }
      
      all_probs <- c(p0, p1, p2, p3, p4, rally_probs)
      sim_rallies <- sample(0:1000, size = nrow(data), prob = all_probs, replace = TRUE)
      data_sim <- data.frame(new_rally_count = sim_rallies, id = "sim")
      all_data <- rbind(data_sim, data)
      return(all_data)
    }
  } else if (serve == 1) { ## First Serves
    if(n_params == 2) {
    
      rallies <- 2:1000
      p1 <- parms[1]
      p <- parms[2]
      
      for (rally in rallies){
        rally_prob <- ((1-p1)/(1-p))*((1-p)^(rally-1)*p)
        rally_probs <- c(rally_probs, rally_prob)
      }
      
      all_probs <- c(p1, rally_probs)
      sim_rallies <- sample(1:1000, size = nrow(data), prob = all_probs, replace = TRUE)
      data_sim <- data.frame(new_rally_count = sim_rallies, id = "sim")
      all_data <- rbind(data_sim, data)
      return(all_data)
      
    } else if(n_params == 4) {
      
      rallies <- 4:1000
      p1 <- parms[1]
      p2 <- parms[2]
      p3 <- parms[3]
      p <- parms[4]
      
      for (rally in rallies){
        rally_prob <- ((1-p1-p2-p3)/(1-p)^3)*((1-p)^(rally-1)*p)
        rally_probs <- c(rally_probs, rally_prob)
      }
      
      all_probs <- c(p1,p2,p3, rally_probs)
      sim_rallies <- sample(1:1000, size = nrow(data), prob = all_probs, replace = TRUE)
      data_sim <- data.frame(new_rally_count = sim_rallies, id = "sim")
      all_data <- rbind(data_sim, data)
      return(all_data)
    } else if(n_params == 5) {
      
      rallies <- 5:1000
      p1 <- parms[1]
      p2 <- parms[2]
      p3 <- parms[3]
      p4 <- parms[4]
      p <- parms[5]
      
      for (rally in rallies){
        rally_prob <- ((1-p1-p2-p3-p4)/(1-p)^4)*((1-p)^(rally-1)*p)
        rally_probs <- c(rally_probs, rally_prob)
      }
      
      all_probs <- c(p1,p2,p3,p4, rally_probs)
      sim_rallies <- sample(1:1000, size = nrow(data), prob = all_probs, replace = TRUE)
      data_sim <- data.frame(new_rally_count = sim_rallies, id = "sim")
      all_data <- rbind(data_sim, data)
      return(all_data)
      
    }
  }
}

## write a function to find rally length frequencies
## This will transform our counts to percentages 
rally_freqs <- function(data){
  
  rally_totals <- data %>% group_by(id) %>% count(new_rally_count)
  group_totals <- data %>% group_by(id) %>% count()
  
  joined_totals <- merge(rally_totals, group_totals, by = c("id")) %>% 
  mutate(percents = n.x / n.y)
  
  return(joined_totals)
  
}

## function to prep the real data for rally_distributions
make_visualization_data <- function(data, parms, n_params, serve) {
  
  ## make full data set
  new_data <- data %>% mutate(id = "real")
  new_data <- rally_distributions(parms, new_data, n_params, serve)
  
  ## tranform counts to frequencies
  new_data2 <- rally_freqs(new_data)

  new_data3 <- new_data2 %>% filter(new_rally_count <= 25)
  return(new_data3)
  
}

## function to print out mle estimate plots 
make_mle_plots <- function(data_hard, data_clay, data_grass) {
  
  ## men hard court 
  print(ggplot(data = data_hard, aes(x = factor(new_rally_count), y = percents, fill = id)) +
    geom_col(position = "dodge") +
    ylim(0,.5) + 
    theme_bw() + 
    labs(x = "Rally Length",
         fill = "ID",
         title = "Observed vs Expected Men's Hard Court Rally Lengths") +
    scale_fill_manual(values = c("dodgerblue2","azure4")))
  
  ## men clay court
  print(ggplot(data = data_clay, aes(x = factor(new_rally_count), y = percents, fill = id)) +
    geom_col(position = "dodge") +
    ylim(0,.5) +
    theme_bw() + 
    labs(x = "Rally Length",
         fill = "ID",
         title = "Observed vs Expected Men's Clay Rally Lengths") +
    scale_fill_manual(values = c("sienna3","azure4")))
  
  ## men grass
  print(ggplot(data = data_grass, aes(x = factor(new_rally_count), y = percents, fill = id)) +
    geom_col(position = "dodge") +
    ylim(0,.5) +
    theme_bw() + 
    labs(x = "Rally Length",
         fill = "ID",
         title = "Observed vs Expected Men's Grass Rally Lengths") +
    scale_fill_manual(values = c("springgreen4","azure4")))
  
}



## -----------------------------------------------------------------------------------------------------

## read in the data 
rally_data <- read.csv("C:/Users/jared/OneDrive/Desktop/Tennis 2 Data/all_data_surfaces.csv",
                       header = TRUE) 

## Get rally counts for men on each surface ##

## All Serves ##

men_hard_rally_counts <- rally_data %>% filter(sex == 1 & Surface == "Hard") %>% select(new_rally_count)
men_clay_rally_counts <- rally_data %>% filter(sex == 1 & Surface == "Clay") %>% select(new_rally_count)
men_grass_rally_counts <- rally_data %>% filter(sex == 1 & Surface == "Grass") %>% select(new_rally_count)

## First Serves ##

men_hard_first <- rally_data %>% filter(sex == 1 & Surface == "Hard" & X1stIn == 1) %>% 
  select(new_rally_count)
men_clay_first <- rally_data %>% filter(sex == 1 & Surface == "Clay" & X1stIn == 1) %>% 
  select(new_rally_count)
men_grass_first <- rally_data %>% filter(sex == 1 & Surface == "Grass" & X1stIn == 1) %>% 
  select(new_rally_count)

## Second Serves ##

men_hard_second <- rally_data %>% filter(sex == 1 & Surface == "Hard" & X1stIn == 0) %>% 
  select(new_rally_count)
men_clay_second <- rally_data %>% filter(sex == 1 & Surface == "Clay" & X1stIn == 0) %>% 
  select(new_rally_count)
men_grass_second <- rally_data %>% filter(sex == 1 & Surface == "Grass" & X1stIn == 0) %>% 
  select(new_rally_count)



## -----------------------------------------------------------------------------------------------------

## Get MLE estimates for men on each surface 

## Note
## All Serves: serve == 0
## First Serves: serve == 1
## Second Serves: serve == 2

## All Serves ##

## Z1
men_hard_parms <- mle(men_hard_rally_counts, 3, 0) ## data, number of parameters, serve
men_clay_parms <- mle(men_clay_rally_counts, 3, 0)
men_grass_parms <- mle(men_grass_rally_counts, 3, 0)

## Z123
men_hard_parms3 <- mle(men_hard_rally_counts, 5, 0)
men_clay_parms3 <- mle(men_clay_rally_counts, 5, 0)
men_grass_parms3 <- mle(men_grass_rally_counts, 5, 0)

## Z1234
men_hard_parms4 <- mle(men_hard_rally_counts, 6, 0)
men_clay_parms4 <- mle(men_clay_rally_counts, 6, 0)
men_grass_parms4 <- mle(men_grass_rally_counts, 6, 0)

## First Serves ##

## ZT1
men_hard_parms_first <- mle(men_hard_first, 2, 1)
men_clay_parms_first <- mle(men_clay_first, 2, 1)
men_grass_parms_first <- mle(men_grass_first, 2, 1)

## ZT123
men_hard_parms3_first <- mle(men_hard_first, 4, 1)
men_clay_parms3_first <- mle(men_clay_first, 4, 1)
men_grass_parms3_first <- mle(men_grass_first, 4, 1)

## ZT1234
men_hard_parms4_first <- mle(men_hard_first, 5, 1)
men_clay_parms4_first <- mle(men_clay_first, 5, 1)
men_grass_parms4_first <- mle(men_grass_first, 5, 1)

## Second Serves ##

## Z1
men_hard_parms_second <- mle(men_hard_second, 3, 2)
men_clay_parms_second <- mle(men_clay_second, 3, 2)
men_grass_parms_second <- mle(men_grass_second, 3, 2)

## Z123
men_hard_parms3_second <- mle(men_hard_second, 5, 2)
men_clay_parms3_second <- mle(men_clay_second, 5, 2)
men_grass_parms3_second <- mle(men_grass_second, 5, 2)

## Z1234
men_hard_parms4_second <- mle(men_hard_second, 6, 2)
men_clay_parms4_second <- mle(men_clay_second, 6, 2)
men_grass_parms4_second <- mle(men_grass_second, 6, 2)



## -----------------------------------------------------------------------------------------------------

## Make data for visualization for men on all surfaces

## All Serves ##

## Z1
men_hard_data <- make_visualization_data(men_hard_rally_counts, men_hard_parms, 3, 0) ## data, MLE estimates, number of parameters, ## serve
men_clay_data <- make_visualization_data(men_clay_rally_counts, men_clay_parms, 3, 0)
men_grass_data <- make_visualization_data(men_grass_rally_counts, men_grass_parms, 3, 0)

## Z123
men_hard_data3 <- make_visualization_data(men_hard_rally_counts, men_hard_parms3, 5, 0)
men_clay_data3 <- make_visualization_data(men_clay_rally_counts, men_clay_parms3, 5, 0)
men_grass_data3 <- make_visualization_data(men_grass_rally_counts, men_grass_parms3, 5, 0)

## Z1234
men_hard_data4 <- make_visualization_data(men_hard_rally_counts, men_hard_parms4, 6, 0)
men_clay_data4 <- make_visualization_data(men_clay_rally_counts, men_clay_parms4, 6, 0)
men_grass_data4 <- make_visualization_data(men_grass_rally_counts, men_grass_parms4, 6, 0)

## First Serves ##

## ZT1
men_hard_data_first <- make_visualization_data(men_hard_first, men_hard_parms_first, 2, 1)
men_clay_data_first <- make_visualization_data(men_clay_first, men_clay_parms_first, 2, 1)
men_grass_data_first <- make_visualization_data(men_grass_first, men_grass_parms_first, 2, 1)

## ZT123
men_hard_data3_first <- make_visualization_data(men_hard_first, men_hard_parms3_first, 4, 1)
men_clay_data3_first <- make_visualization_data(men_clay_first, men_clay_parms3_first, 4, 1)
men_grass_data3_first <- make_visualization_data(men_grass_first, men_grass_parms3_first, 4, 1)

## ZT1234
men_hard_data4_first <- make_visualization_data(men_hard_first, men_hard_parms4_first, 5, 1)
men_clay_data4_first <- make_visualization_data(men_clay_first, men_clay_parms4_first, 5, 1)
men_grass_data4_first <- make_visualization_data(men_grass_first, men_grass_parms4_first, 5, 1)

## Second Serves ##

## Z1
men_hard_data_second <- make_visualization_data(men_hard_second, men_hard_parms_second, 3, 2)
men_clay_data_second <- make_visualization_data(men_clay_second, men_clay_parms_second, 3, 2)
men_grass_data_second <- make_visualization_data(men_grass_second, men_grass_parms_second, 3, 2)

## Z123
men_hard_data3_second <- make_visualization_data(men_hard_second, men_hard_parms3_second, 5, 2)
men_clay_data3_second <- make_visualization_data(men_clay_second, men_clay_parms3_second, 5, 2)
men_grass_data3_second <- make_visualization_data(men_grass_second, men_grass_parms3_second, 5, 2)

## Z1234
men_hard_data4_second <- make_visualization_data(men_hard_second, men_hard_parms4_second, 6, 2)
men_clay_data4_second <- make_visualization_data(men_clay_second, men_clay_parms4_second, 6, 2)
men_grass_data4_second <- make_visualization_data(men_grass_second, men_grass_parms4_second, 6, 2)



## -----------------------------------------------------------------------------------------------------

## Make Plots

## All Serves ##################################################################

## Z1 Estimation
make_mle_plots(men_hard_data, men_clay_data, men_grass_data) 

## Z123 Estimation
make_mle_plots(men_hard_data3, men_clay_data3, men_grass_data3)

## Z124 Estimation
make_mle_plots(men_hard_data4, men_clay_data4, men_grass_data4)

## For all serves it appears that the Z1234 fits best across surfaces ##

## First Serves ################################################################

## ZT1
make_mle_plots(men_hard_data_first, men_clay_data_first, men_grass_data_first)

## ZT123
make_mle_plots(men_hard_data3_first, men_clay_data3_first, men_grass_data3_first)

## ZT1234
make_mle_plots(men_hard_data4_first, men_clay_data4_first, men_grass_data4_first)

## For first serves it appear that the ZT1234 fits best across surfaces ##

## Second Serves ###############################################################

## Z1 Estimation
make_mle_plots(men_hard_data_second, men_clay_data_second, men_grass_data_second)

## Z123 Estimation
make_mle_plots(men_hard_data3_second, men_clay_data3_second, men_grass_data3_second)

## Z124 Estimation
make_mle_plots(men_hard_data4_second, men_clay_data4_second, men_grass_data4_second)
