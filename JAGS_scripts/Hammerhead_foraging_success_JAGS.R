rm(list = ls(all = TRUE))

library(tidyverse)
library(runjags)
library(rjags)

# Load data ####

setwd("C:/Users/Ruth/Dropbox/Collaborations and friends/Yannis Papastamatiou/from_Erin/")

# Sonar tags to estimate encounter rates of teleost prey
# Each echogram represents 30 s of a deployment
# teleosts_sonar <- read_csv("Spencer et al echogram by hour.csv") %>%
  # dplyr::select(-"Hour")
# So each row here is an hour of 30 s echograms and the sum of teleosts seen over each hour?
# Hourly prey encounter rate (x 35) for 2 sharks

# Video deployments to estimate encounter rates of teloest prey
# Encounter defined as the presence of a teleost on a video within a 30 s window
# teleosts_video <- read_csv("Spencer et al video encounter by hour.csv") %>%
  # dplyr::select(-"Hour")
# So each row here is an hour of 30 s echograms and the sum of teleosts seen over each hour?
# Hourly prey encounter rate (x 10) for 3 sharks

# ~~~~~~~~~~~~~~~~~~~~~~~

# Prepare data ####

nsharks <- 7

# Mean speed from this file: "Spencer et al energetics calculations_no aus.xlsx"
cruise_speed <- c(0.62, 0.74, 0.86, 0.63, 0.60, 0.67, 0.83)

rm(teleosts_sonar, teleosts_video)

# ~~~~~~~~~~~~~~~~~~~~~~~

# Compose model ####

# Define the model

hammerheads <- "model {

  for (i in 1:nsharks) {
  
    # Process model for teleost prey encounter rate (sonar):
    teleost_sonar[i] ~ dnorm(mu_teleost_sonar, tau_teleost_sonar)
    
    # Process model for teleost prey encounter rate (sonar):
    teleost_video[i] ~ dnorm(mu_teleost_video, tau_teleost_video)
    
    foraging_success[i] <- (teleost_sonar[i] + teleost_video[i]) * cruise_speed[i]
  
  }
  
  #data# nsharks, cruise_speed
  #monitor# foraging_success
  
  #### Priors ####
  mu_teleost_sonar ~ dgamma(2.25, 0.15)
  tau_teleost_sonar ~ dgamma(14.0625, 187.5)
  
  mu_teleost_video ~ dgamma(1, 0.333)
  tau_teleost_video ~ dgamma(16, 40)
  
}"

# ~~~~~~~~~~~~~~~~~~~~~~~

# Run model ####

results<-run.jags(hammerheads, n.chains = 3, method = "int", burnin = 1000, sample = 5000)

# ~~~~~~~~~~~~~~~~~~~~~~~

# Extract results ####

sumr<-summary(results)
head(sumr)

test <- add.summary(results, confidence = c(0.5, 0.75, 0.95))
test_save <- summary(test)

write.csv(test.save, "ModelOutput_01.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~

# DIC ####

plot(results, vars="deviance", "trace")
extract(results, "dic")

# Extract separate DIC value for each chain:
dev <- extract(results, "full.deviance")
mean(dev[[1]])
mean(dev[[2]])
deviance

# ~~~~~~~~~~~~~~~~~~~~~~~

# Plot Priors/Posteriors ####

# rsd
# rsd~dnorm(100, 0.001)
# 500, 0.0001
hist(rnorm(30000, mean = 500, sd = 1/sqrt(0.0001)))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_rsd.png", width = 600, height = 450)
plot(results, vars=c("rsd"), c("hist", "trace"))
dev.off()

# mpr
# mpr~dgamma(2.351111 51.111111)
# 3.3856, 18.4000
hist(rgamma(3000, shape = 2.351111, rate = 51.111111))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_mpr.png", width = 600, height = 450)
plot(results, vars=c("mpr"), c("hist", "trace"))
dev.off()

# mcv
# mcv~dgamma(5184, 72000)
hist(rgamma(3000, shape = 5184, rate = 72000))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_mcv.png", width = 600, height = 450)
plot(results, vars=c("mcv"), c("hist", "trace"))
dev.off()

# r0
# r0~dgamma(4.7353732, 0.9450644)
hist(rgamma(30000, shape = 4.7353732, rate = 0.9450644))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_r0.png", width = 600, height = 450)
plot(results, vars=c("r0"), c("hist", "trace"))
dev.off()

# DIC
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_dic.png", width = 600, height = 450)
plot(results, vars = "deviance", c("hist", "trace"))
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~

# r1~dnorm(0, 0.01)
hist(rnorm(30000, mean = 0, sd = 10))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_r1.png", width = 600, height = 450)
plot(results, vars=c("r1"), c("hist", "trace"))
dev.off()

# r2~dnorm(0, 0.01)
hist(rnorm(30000, mean = 0, sd = 10))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_r2.png", width = 600, height = 450)
plot(results, vars=c("r2"), c("hist", "trace"))
dev.off()

# r3~dnorm(0, 0.01)
hist(rnorm(30000, mean = 0, sd = 10))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_r3.png", width = 600, height = 450)
plot(results, vars=c("r3"), c("hist", "trace"))
dev.off()

# r4~dnorm(0, 0.01)
hist(rnorm(30000, mean = 0, sd = 10))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_r4.png", width = 600, height = 450)
plot(results, vars=c("r4"), c("hist", "trace"))
dev.off()

# r5~dnorm(0, 0.01)
hist(rnorm(30000, mean = 0, sd = 10))
png(filename="Plots/Model_outputs/11_10_burnin10000_sample20000_1min_full/posterior_r5.png", width = 600, height = 450)
plot(results, vars=c("r5"), c("hist", "trace"))
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~
