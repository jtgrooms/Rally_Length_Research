## Nimble WAIC Comparisons for Bayesian Rally Length Model
## Written by Jared Grooms
## 4/15/2024


## load in libraries ###########################################################
library(nimble)
library(tidyverse)
library(ggplot2)
library(coda)
library(kableExtra)

## set working directory
setwd("C:/Users/jared/OneDrive/Desktop/Tennis3")

################################################################################

## Prepare the data ############################################################

rally_data <- read.csv("all_data_surfaces.csv", header = TRUE)

rally_data <- rally_data %>% 
  mutate(Tiebreaker = case_when(
    tiebreaker == "Set Point" ~ 1,
    tiebreaker == "Tiebreaker Point" ~ 2
  ))

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

## First Serves ##
men_hard_first <- rally_data %>% 
  filter(sex == 1 & Surface == "Hard" & X1stIn == 1) %>% 
  select(new_rally_count)
men_clay_first <- rally_data %>% 
  filter(sex == 1 & Surface == "Clay" & X1stIn == 1) %>% 
  select(new_rally_count)
men_grass_first <- rally_data %>% 
  filter(sex == 1 & Surface == "Grass" & X1stIn == 1) %>% 
  select(new_rally_count)

## Second Serves ##
men_hard_second <- rally_data %>% 
  filter(sex == 1 & Surface == "Hard" & X1stIn == 0) %>% 
  select(new_rally_count)
men_clay_second <- rally_data %>% 
  filter(sex == 1 & Surface == "Clay" & X1stIn == 0) %>% 
  select(new_rally_count)
men_grass_second <- rally_data %>% 
  filter(sex == 1 & Surface == "Grass" & X1stIn == 0) %>% 
  select(new_rally_count)

## source functions
source("rally_nimble_comparison_functions.R", echo = FALSE)

################################################################################

## Compare waic for different likelihoods across surfaces for all serves #######

# ## Grass Courts
# 
# nimble_data_grass <- make_nimble_data(men_grass_rally_counts, 
#                                       like_code = 4, num_params = 3)
# nimble_data_grass123 <- make_nimble_data(men_grass_rally_counts, 
#                                          like_code = 5, num_params = 5)
# nimble_data_grass1234 <- make_nimble_data(men_grass_rally_counts, 
#                                           like_code = 6, num_params = 6)
# 
# rally_heirarchical_grass <- run_nimble_model(data = nimble_data_grass,
#                                              monitors = c('p0','p1','p'),
#                                              burn = 1000, iter = 11000, 
#                                              chains = 1, thin = 2)
# 
# rally_heirarchical123_grass <- run_nimble_model(data = nimble_data_grass123,
#                                                 monitors = c('p0','p1', 'p2', 
#                                                              'p3', 'p'),
#                                                 burn = 1000, iter = 11000, 
#                                                 chains = 1, thin = 2)
# 
# rally_heirarchical1234_grass <- run_nimble_model(data = nimble_data_grass1234,
#                                                  monitors = c('p0','p1', 'p2', 
#                                                               'p3', 'p4', 'p'),
#                                                  burn = 1000, iter = 11000, 
#                                                  chains = 1, thin = 2)
# 
# grass_waic <- c(rally_heirarchical_grass$WAIC$WAIC, 
#                 rally_heirarchical123_grass$WAIC$WAIC, 
#                 rally_heirarchical1234_grass$WAIC$WAIC)
# 
# grass_df <- data.frame(grass_waic, row.names = c("Z1", "Z123", "Z1234"))
# colnames(grass_df) <- c("Grass")
# 
# ## Clay Courts ##
# 
# nimble_data_clay <- make_nimble_data(men_clay_rally_counts, 
#                                      like_code = 4, num_params = 3)
# nimble_data_clay123 <- make_nimble_data(men_clay_rally_counts, 
#                                         like_code = 5, num_params = 5)
# nimble_data_clay1234 <- make_nimble_data(men_clay_rally_counts, 
#                                          like_code = 6, num_params = 6)
# 
# rally_heirarchical_clay <- run_nimble_model(data = nimble_data_clay,
#                                             monitors = c('p0','p1','p'),
#                                             burn = 1000, iter = 11000, 
#                                             chains = 1, thin = 2)
# 
# rally_heirarchical123_clay <- run_nimble_model(data = nimble_data_clay123,
#                                                monitors = c('p0','p1', 'p2', 
#                                                             'p3', 'p'),
#                                                burn = 1000, iter = 11000, 
#                                                chains = 1, thin = 2)
# 
# rally_heirarchical1234_clay <- run_nimble_model(data = nimble_data_clay1234,
#                                                 monitors = c('p0','p1', 'p2', 
#                                                              'p3', 'p4', 'p'),
#                                                 burn = 1000, iter = 11000, 
#                                                 chains = 1, thin = 2)
# 
# clay_waic <- c(rally_heirarchical_clay$WAIC$WAIC, 
#                rally_heirarchical123_clay$WAIC$WAIC, 
#                rally_heirarchical1234_clay$WAIC$WAIC)
# 
# clay_df <- data.frame(clay_waic, row.names = c("Z1", "Z123", "Z1234"))
# colnames(clay_df) <- c("Clay")
# 
# ## Hard Courts ##
# 
# nimble_data_hard <- make_nimble_data(men_hard_rally_counts, 
#                                      like_code = 4, num_params = 3)
# nimble_data_hard123 <- make_nimble_data(men_hard_rally_counts, 
#                                         like_code = 5, num_params = 5)
# nimble_data_hard1234 <- make_nimble_data(men_hard_rally_counts, 
#                                          like_code = 6, num_params = 6)
# 
# rally_heirarchical_hard <- run_nimble_model(data = nimble_data_hard,
#                                             monitors = c('p0','p1','p'),
#                                             burn = 1000, iter = 11000, 
#                                             chains = 1, thin = 2)
# 
# rally_heirarchical123_hard <- run_nimble_model(data = nimble_data_hard123,
#                                                monitors = c('p0','p1', 
#                                                             'p2', 'p3', 'p'),
#                                                burn = 1000, iter = 11000, 
#                                                chains = 1, thin = 2)
# 
# rally_heirarchical1234_hard <- run_nimble_model(data = nimble_data_hard1234,
#                                                 monitors = c('p0','p1', 'p2', 
#                                                              'p3', 'p4', 'p'),
#                                                 burn = 1000, iter = 11000, 
#                                                 chains = 1, thin = 2)
# 
# hard_waic <- c(rally_heirarchical_hard$WAIC$WAIC, 
#                rally_heirarchical123_hard$WAIC$WAIC, 
#                rally_heirarchical1234_hard$WAIC$WAIC)
# 
# hard_df <- data.frame(hard_waic, row.names = c("Z1", "Z123", "Z1234"))
# colnames(hard_df) <- c("Hard")
# 
# all_df <- cbind(grass_df, clay_df, hard_df)
# 
# write.table(all_df, file = "results/all_df.txt", sep = "\t", quote = FALSE)

################################################################################

## Compare waic for different likelihoods across surfaces for second serves #######

## Grass Courts

nimble_data_grass_second <- make_nimble_data(men_grass_second, 
                                      like_code = 4, num_params = 3)
nimble_data_grass12_second <- make_nimble_data(men_grass_second, 
                                             like_code = 10, num_params = 4)
nimble_data_grass123_second <- make_nimble_data(men_grass_second, 
                                         like_code = 5, num_params = 5)
nimble_data_grass1234_second <- make_nimble_data(men_grass_second, 
                                          like_code = 6, num_params = 6)

rally_heirarchical_grass_second <- run_nimble_model(data = nimble_data_grass_second,
                                             monitors = c('p0','p1','p'),
                                             burn = 1000, iter = 11000, 
                                             chains = 1, thin = 2)

rally_heirarchical12_grass_second <- run_nimble_model(data = nimble_data_grass12_second,
                                                       monitors = c('p0','p1', 
                                                                    'p2', 'p'),
                                                       burn = 1000, iter = 11000, 
                                                       chains = 1, thin = 2)

rally_heirarchical123_grass_second <- run_nimble_model(data = nimble_data_grass123_second,
                                                monitors = c('p0','p1', 'p2', 
                                                             'p3', 'p'),
                                                burn = 1000, iter = 11000, 
                                                chains = 1, thin = 2)

rally_heirarchical1234_grass_second <- run_nimble_model(data = nimble_data_grass1234_second,
                                                 monitors = c('p0','p1', 'p2', 
                                                              'p3', 'p4', 'p'),
                                                 burn = 1000, iter = 11000, 
                                                 chains = 1, thin = 2)

grass_waic_second <- c(rally_heirarchical_grass_second$WAIC$WAIC, 
                       rally_heirarchical12_grass_second$WAIC$WAIC,
                       rally_heirarchical123_grass_second$WAIC$WAIC, 
                       rally_heirarchical1234_grass_second$WAIC$WAIC)

grass_df_second <- data.frame(grass_waic_second, row.names = c("Z1", "Z12", "Z123", "Z1234"))
colnames(grass_df_second) <- c("Grass")

## Clay Courts

nimble_data_clay_second <- make_nimble_data(men_clay_second, 
                                             like_code = 4, num_params = 3)
nimble_data_clay12_second <- make_nimble_data(men_clay_second, 
                                              like_code = 10, num_params = 4)
nimble_data_clay123_second <- make_nimble_data(men_clay_second, 
                                                like_code = 5, num_params = 5)
nimble_data_clay1234_second <- make_nimble_data(men_clay_second, 
                                                 like_code = 6, num_params = 6)

rally_heirarchical_clay_second <- run_nimble_model(data = nimble_data_clay_second,
                                                    monitors = c('p0','p1','p'),
                                                    burn = 1000, iter = 11000, 
                                                    chains = 1, thin = 2)

rally_heirarchical12_clay_second <- run_nimble_model(data = nimble_data_clay12_second,
                                                      monitors = c('p0','p1', 
                                                                   'p2', 'p'),
                                                      burn = 1000, iter = 11000, 
                                                      chains = 1, thin = 2)

rally_heirarchical123_clay_second <- run_nimble_model(data = nimble_data_clay123_second,
                                                       monitors = c('p0','p1', 'p2', 
                                                                    'p3', 'p'),
                                                       burn = 1000, iter = 11000, 
                                                       chains = 1, thin = 2)

rally_heirarchical1234_clay_second <- run_nimble_model(data = nimble_data_clay1234_second,
                                                        monitors = c('p0','p1', 'p2', 
                                                                     'p3', 'p4', 'p'),
                                                        burn = 1000, iter = 11000, 
                                                        chains = 1, thin = 2)

clay_waic_second <- c(rally_heirarchical_clay_second$WAIC$WAIC, 
                      rally_heirarchical12_clay_second$WAIC$WAIC,
                      rally_heirarchical123_clay_second$WAIC$WAIC, 
                      rally_heirarchical1234_clay_second$WAIC$WAIC)

clay_df_second <- data.frame(clay_waic_second, row.names = c("Z1", "Z12", "Z123", "Z1234"))
colnames(clay_df_second) <- c("Clay")

## Hard Courts

nimble_data_hard_second <- make_nimble_data(men_hard_second, 
                                            like_code = 4, num_params = 3)
nimble_data_hard12_second <- make_nimble_data(men_hard_second, 
                                              like_code = 10, num_params = 4)
nimble_data_hard123_second <- make_nimble_data(men_hard_second, 
                                               like_code = 5, num_params = 5)
nimble_data_hard1234_second <- make_nimble_data(men_hard_second, 
                                                like_code = 6, num_params = 6)

rally_heirarchical_hard_second <- run_nimble_model(data = nimble_data_hard_second,
                                                   monitors = c('p0','p1','p'),
                                                   burn = 1000, iter = 11000, 
                                                   chains = 1, thin = 2)

rally_heirarchical12_hard_second <- run_nimble_model(data = nimble_data_hard12_second,
                                                      monitors = c('p0','p1', 
                                                                   'p2', 'p'),
                                                      burn = 1000, iter = 11000, 
                                                      chains = 1, thin = 2)

rally_heirarchical123_hard_second <- run_nimble_model(data = nimble_data_hard123_second,
                                                      monitors = c('p0','p1', 'p2', 
                                                                   'p3', 'p'),
                                                      burn = 1000, iter = 11000, 
                                                      chains = 1, thin = 2)

rally_heirarchical1234_hard_second <- run_nimble_model(data = nimble_data_hard1234_second,
                                                       monitors = c('p0','p1', 'p2', 
                                                                    'p3', 'p4', 'p'),
                                                       burn = 1000, iter = 11000, 
                                                       chains = 1, thin = 2)

hard_waic_second <- c(rally_heirarchical_hard_second$WAIC$WAIC,
                      rally_heirarchical12_hard_second$WAIC$WAIC,
                      rally_heirarchical123_hard_second$WAIC$WAIC, 
                      rally_heirarchical1234_hard_second$WAIC$WAIC)

hard_df_second <- data.frame(hard_waic_second, row.names = c("Z1", "Z12", "Z123", "Z1234"))
colnames(hard_df_second) <- c("Hard")

all_df_second <- cbind(grass_df_second, clay_df_second, hard_df_second)
all_df_second$Serve <- "Second"

################################################################################

## Compare waic for different likelihoods across surfaces for first serves #######

## Grass Courts

nimble_data_grass_first <- make_nimble_data(men_grass_first, 
                                             like_code = 7, num_params = 2)
nimble_data_grass12_first <- make_nimble_data(men_grass_first, 
                                               like_code = 11, num_params = 3)
nimble_data_grass123_first <- make_nimble_data(men_grass_first, 
                                                like_code = 8, num_params = 4)
nimble_data_grass1234_first <- make_nimble_data(men_grass_first, 
                                                 like_code = 9, num_params = 5)

rally_heirarchical_grass_first <- run_nimble_model(data = nimble_data_grass_first,
                                                    monitors = c('p1','p'),
                                                    burn = 1000, iter = 11000, 
                                                    chains = 1, thin = 2)

rally_heirarchical12_grass_first <- run_nimble_model(data = nimble_data_grass12_first,
                                                      monitors = c('p1', 'p2', 
                                                                   'p'),
                                                      burn = 1000, iter = 11000, 
                                                      chains = 1, thin = 2)

rally_heirarchical123_grass_first <- run_nimble_model(data = nimble_data_grass123_first,
                                                       monitors = c('p1', 'p2', 
                                                                    'p3', 'p'),
                                                       burn = 1000, iter = 11000, 
                                                       chains = 1, thin = 2)

rally_heirarchical1234_grass_first <- run_nimble_model(data = nimble_data_grass1234_first,
                                                        monitors = c('p1', 'p2', 
                                                                     'p3', 'p4', 'p'),
                                                        burn = 1000, iter = 11000, 
                                                        chains = 1, thin = 2)

grass_waic_first <- c(rally_heirarchical_grass_first$WAIC$WAIC, 
                      rally_heirarchical12_grass_first$WAIC$WAIC,
                      rally_heirarchical123_grass_first$WAIC$WAIC, 
                      rally_heirarchical1234_grass_first$WAIC$WAIC)

grass_df_first <- data.frame(grass_waic_first, row.names = c("ZT1", "ZT12", "ZT123", "ZT1234"))
colnames(grass_df_first) <- c("Grass")

## Clay Courts

nimble_data_clay_first <- make_nimble_data(men_clay_first, 
                                            like_code = 7, num_params = 2)
nimble_data_clay12_first <- make_nimble_data(men_clay_first, 
                                             like_code = 11, num_params = 3)
nimble_data_clay123_first <- make_nimble_data(men_clay_first, 
                                               like_code = 8, num_params = 4)
nimble_data_clay1234_first <- make_nimble_data(men_clay_first, 
                                                like_code = 9, num_params = 5)

rally_heirarchical_clay_first <- run_nimble_model(data = nimble_data_clay_first,
                                                   monitors = c('p1','p'),
                                                   burn = 1000, iter = 11000, 
                                                   chains = 1, thin = 2)

rally_heirarchical12_clay_first <- run_nimble_model(data = nimble_data_clay12_first,
                                                     monitors = c('p1', 'p2', 
                                                                  'p'),
                                                     burn = 1000, iter = 11000, 
                                                     chains = 1, thin = 2)

rally_heirarchical123_clay_first <- run_nimble_model(data = nimble_data_clay123_first,
                                                      monitors = c('p1', 'p2', 
                                                                   'p3', 'p'),
                                                      burn = 1000, iter = 11000, 
                                                      chains = 1, thin = 2)

rally_heirarchical1234_clay_first <- run_nimble_model(data = nimble_data_clay1234_first,
                                                       monitors = c('p1', 'p2', 
                                                                    'p3', 'p4', 'p'),
                                                       burn = 1000, iter = 11000, 
                                                       chains = 1, thin = 2)

clay_waic_first <- c(rally_heirarchical_clay_first$WAIC$WAIC,
                     rally_heirarchical12_clay_first$WAIC$WAIC,
                     rally_heirarchical123_clay_first$WAIC$WAIC, 
                     rally_heirarchical1234_clay_first$WAIC$WAIC)

clay_df_first <- data.frame(clay_waic_first, row.names = c("ZT1", "ZT12", "ZT123", "ZT1234"))
colnames(clay_df_first) <- c("Clay")

## Hard Courts

nimble_data_hard_first <- make_nimble_data(men_hard_first, 
                                           like_code = 7, num_params = 2)
nimble_data_hard12_first <- make_nimble_data(men_hard_first, 
                                              like_code = 11, num_params = 3)
nimble_data_hard123_first <- make_nimble_data(men_hard_first, 
                                              like_code = 8, num_params = 4)
nimble_data_hard1234_first <- make_nimble_data(men_hard_first, 
                                               like_code = 9, num_params = 5)

rally_heirarchical_hard_first <- run_nimble_model(data = nimble_data_hard_first,
                                                  monitors = c('p1','p'),
                                                  burn = 1000, iter = 11000, 
                                                  chains = 1, thin = 2)

rally_heirarchical12_hard_first <- run_nimble_model(data = nimble_data_hard12_first,
                                                     monitors = c('p1', 'p2', 
                                                                  'p'),
                                                     burn = 1000, iter = 11000, 
                                                     chains = 1, thin = 2)

rally_heirarchical123_hard_first <- run_nimble_model(data = nimble_data_hard123_first,
                                                     monitors = c('p1', 'p2', 
                                                                  'p3', 'p'),
                                                     burn = 1000, iter = 11000, 
                                                     chains = 1, thin = 2)

rally_heirarchical1234_hard_first <- run_nimble_model(data = nimble_data_hard1234_first,
                                                      monitors = c('p1', 'p2', 
                                                                   'p3', 'p4', 'p'),
                                                      burn = 1000, iter = 11000, 
                                                      chains = 1, thin = 2)

hard_waic_first <- c(rally_heirarchical_hard_first$WAIC$WAIC, 
                     rally_heirarchical12_hard_first$WAIC$WAIC,
                     rally_heirarchical123_hard_first$WAIC$WAIC, 
                     rally_heirarchical1234_hard_first$WAIC$WAIC)

hard_df_first <- data.frame(hard_waic_first, row.names = c("ZT1", "ZT12", "ZT123", "ZT1234"))
colnames(hard_df_first) <- c("Hard")

all_df_first <- cbind(grass_df_first, clay_df_first, hard_df_first)
all_df_first$Serve <- "First"

final_serve_waic <- rbind(all_df_first, all_df_second)





