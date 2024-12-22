rm(list = ls(all = TRUE))

library(tidyverse)
library(runjags)
library(rjags)

# ~~~~~~~~~~~~~~~~~~~~~~~

# Prepare data ####

nsharks <- 7

# Mean speed from this file: "Spencer et al energetics calculations_no aus.xlsx"
# (in m/s so convert to km/h)
speed_mean <- c(0.62, 0.74, 0.86, 0.63, 0.60, 0.67, 0.83)*3.6
# Speed standard deviations from Erin:
speed_tau <- 1/c(0.1, 0.1, 0.2, 0.3, 0.2, 0.2, 0.2)^2*3.6

# 2 m radius = 4 m diameter = 0.004 km
perception_diameter <- 0.004

# ~~~~~~~~~~~~~~~~~~~~~~~

# Compose model ####

# Define the model

hammerheads <- "model {

  for (i in 1:nsharks) {
  
    # Encounter rates: Teleosts
    teleost_sonar[i] ~ dnorm(mu_teleost_sonar, tau_teleost_sonar) T(0, )
    t_encounter_sonar[i] ~ dpois(teleost_sonar[i])
    
    teleost_video[i] ~ dnorm(mu_teleost_video, tau_teleost_video) T(0, )
    t_encounter_video[i] ~ dpois(teleost_video[i])
    
    # Foraging success rates: Teleosts
    foraging_success_sonar_40[i] <- t_encounter_sonar[i] * 0.4
    foraging_success_sonar_30[i] <- t_encounter_sonar[i] * 0.3
    foraging_success_sonar_50[i] <- t_encounter_sonar[i] * 0.5
    
    foraging_success_video_40[i] <- t_encounter_video[i] * 0.4
    foraging_success_video_30[i] <- t_encounter_video[i] * 0.3
    foraging_success_video_50[i] <- t_encounter_video[i] * 0.5
    
    # Encounter rates: Black-tips
    cruise_speed[i] ~ dnorm(speed_mean[i], speed_tau[i]) T(0, )
    
    btdensity_summer[i] ~ dnorm(mu_bt_summer, tau_bt_summer) T(0, )
    bts_summer[i] <- cruise_speed[i] * btdensity_summer[i] * perception_diameter
    bt_encounter_summer[i] ~ dpois(bts_summer[i])
    
    btdensity_winter[i] ~ dnorm(mu_bt_winter, tau_bt_winter) T(0, )
    bts_winter[i] <- cruise_speed[i] * btdensity_winter[i] * perception_diameter
    bt_encounter_winter[i] ~ dpois(bts_winter[i])
    
    # Foraging success rates: Blacktips
    foraging_success_bt_s_15[i] <- bt_encounter_summer[i] * 0.15
    foraging_success_bt_s_10[i] <- bt_encounter_summer[i] * 0.1
    foraging_success_bt_s_5[i] <- bt_encounter_summer[i] * 0.05
    
    foraging_success_bt_w_15[i] <- bt_encounter_winter[i] * 0.15
    foraging_success_bt_w_10[i] <- bt_encounter_winter[i] * 0.1
    foraging_success_bt_w_5[i] <- bt_encounter_winter[i] * 0.05
  
  }
  
  # Calculating mean across all individuals
  mean_t_encounter_sonar <- mean(t_encounter_sonar[])
  mean_t_encounter_video <- mean(t_encounter_video[])
  
  mean_foraging_success_sonar_40 <- mean(foraging_success_sonar_40[])
  mean_foraging_success_sonar_30 <- mean(foraging_success_sonar_30[])
  mean_foraging_success_sonar_50 <- mean(foraging_success_sonar_50[])
  
  mean_foraging_success_video_40 <- mean(foraging_success_video_40[])
  mean_foraging_success_video_30 <- mean(foraging_success_video_30[])
  mean_foraging_success_video_50 <- mean(foraging_success_video_50[])
  
  mean_bt_encounter_summer <- mean(bt_encounter_summer[])
  mean_bt_encounter_winter <- mean(bt_encounter_winter[])
  
  mean_foraging_success_bt_s_15 <- mean(foraging_success_bt_s_15[])
  mean_foraging_success_bt_s_10 <- mean(foraging_success_bt_s_10[])
  mean_foraging_success_bt_s_5 <- mean(foraging_success_bt_s_5[])
  
  mean_foraging_success_bt_w_15 <- mean(foraging_success_bt_w_15[])
  mean_foraging_success_bt_w_10 <- mean(foraging_success_bt_w_10[])
  mean_foraging_success_bt_w_5 <- mean(foraging_success_bt_w_5[])
  
  # Priors
  mu_teleost_sonar ~ dgamma(1.14795918, 0.07653061)
  tau_teleost_sonar ~ dgamma(2.777778, 555.555556)
  
  mu_teleost_video ~ dgamma(4, 0.6666667)
  tau_teleost_video ~ dgamma(0.7303675, 6.5739645)
  
  mu_bt_winter ~ dgamma(1.096608119, 0.003798719)
  tau_bt_winter ~ dgamma(1, 100)
  
  mu_bt_summer ~ dgamma(0.8814669, 0.1707211)
  tau_bt_summer ~ dgamma(1, 900)
  
  # Data and monitoring nodes
  #data# nsharks, speed_mean, speed_tau, perception_diameter
  #monitor# mean_t_encounter_sonar, mean_t_encounter_video, mean_foraging_success_sonar_50, mean_foraging_success_sonar_40, mean_foraging_success_sonar_30, mean_foraging_success_video_50, mean_foraging_success_video_40, mean_foraging_success_video_30, mean_bt_encounter_summer, mean_bt_encounter_winter, mean_foraging_success_bt_s_15, mean_foraging_success_bt_s_10, mean_foraging_success_bt_s_5, mean_foraging_success_bt_w_15, mean_foraging_success_bt_w_10, mean_foraging_success_bt_w_5
  
}"

# ~~~~~~~~~~~~~~~~~~~~~~~

# Run model ####

results <- run.jags(hammerheads, n.chains = 3,
                    method = "int", burnin = 2000, sample = 10000)

# failed.jags()

# ~~~~~~~~~~~~~~~~~~~~~~~

# Extract results ####

sumr<-summary(results)
sumr

write.csv(sumr, "JAGS_scripts/Model_output.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~

# Plot Priors/Posteriors ####

# encounter rates of teleosts from sonar
plot(results, vars=c("mean_t_encounter_sonar"), c("hist", "trace"))
# encounter rates of teleosts from video
plot(results, vars=c("mean_t_encounter_video"), c("hist", "trace"))
# encounter rates of blacktips in summer
plot(results, vars=c("mean_bt_encounter_summer"), c("hist", "trace"))
# encounter rates of blacktips in winter
plot(results, vars=c("mean_bt_encounter_winter"), c("hist", "trace"))

# foraging success
plot(results, vars=c("mean_foraging_success_sonar_50"), c("hist", "trace"))
plot(results, vars=c("mean_foraging_success_sonar_40"), c("hist", "trace"))
plot(results, vars=c("mean_foraging_success_sonar_30"), c("hist", "trace"))

plot(results, vars=c("mean_foraging_success_video_50"), c("hist", "trace"))
plot(results, vars=c("mean_foraging_success_video_40"), c("hist", "trace"))
plot(results, vars=c("mean_foraging_success_video_30"), c("hist", "trace"))

plot(results, vars=c("mean_foraging_success_bt_s_15"), c("hist", "trace"))
plot(results, vars=c("mean_foraging_success_bt_s_10"), c("hist", "trace"))
plot(results, vars=c("mean_foraging_success_bt_s_5"), c("hist", "trace"))

plot(results, vars=c("mean_foraging_success_bt_w_15"), c("hist", "trace"))
plot(results, vars=c("mean_foraging_success_bt_w_10"), c("hist", "trace"))
plot(results, vars=c("mean_foraging_success_bt_w_5"), c("hist", "trace"))
