library(tidyverse)

###### Load functions ####
### Beta distribution specification function ###
betaPr<-function(mu, sd)
{
  al<-mu*(mu*(1-mu)/sd^2-1)
  be<-(1-mu)*(mu*(1-mu)/sd^2-1)
  x<-seq(0,1, 0.01)
  plot(x, dbeta(x, al, be),
       main=paste("Prior distribution with mean ",mu," and sd ", sd), type="l")
  print(paste("The two parameters of the Beta distribution are alpha=",al," and beta=",be))
  return(c(al,be))
}

### Gamma distribution specification function ###
gammaPr<-function(mu, sd)
{
  shape<-mu^2/sd^2
  rate<-mu/sd^2
  x<-seq(max(0, mu-4*sd),mu+4*sd, length.out=100)
  plot(x, dgamma(x, shape=shape, rate=rate),
       main=paste("Prior distribution with mean ",mu," and sd ", sd), type="l")
  print(paste("The two parameters of the Gamma distribution are shape=",shape," and rate=",rate))
  return(c(shape,rate))
}

### Normal distribution specification function ###
normPr<-function(mu, sd)
{
  tau<-1/sd^2
  x<-seq(mu-(sd*4), mu+(sd*4), length=100)
  plot(x, dnorm(x, mean=mu, sd=sd), main=paste("Prior distribution with mean ",mu," and tau ", tau), type="l")
  print(paste("The two parameters of the Normal distribution are mu=",mu," and tau=",tau))
  return(c(mu,tau))
}

# ~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)

# Priors: Teloests via sonar ####

# Sonar tags to estimate encounter rates of teleost prey
# Each echogram represents 30 s of a deployment
teleosts_sonar <- read_csv("from_Erin/Spencer et al echogram by hour.csv") %>%
  dplyr::select(-"Hour")
# So each row here is an hour of 30 s echograms and the sum of teleosts seen over each hour?
# Hourly prey encounter rate (x 35) for 2 sharks

# Mu:
mean(rowMeans(read_csv("from_Erin/Spencer et al echogram by hour.csv") %>%
                dplyr::select(-"Hour")))
# Mean = 15
sd(rowMeans(read_csv("from_Erin/Spencer et al echogram by hour.csv") %>%
                dplyr::select(-"Hour")))
# SD = 14
hist(rowMeans(read_csv("from_Erin/Spencer et al echogram by hour.csv") %>%
                dplyr::select(-"Hour")))
gammaPr(15,14)

# Tau:
hist(1/apply(read_csv("from_Erin/Spencer et al echogram by hour.csv") %>%
        dplyr::select(-"Hour"), 2, sd)^2)
1/14^2
gammaPr(0.005, 0.003) # gamma because tau must be positive

# ~~~~~~~~~~~~~~~~~~~~~~~

# Priors: Teloests via video ####

# Video deployments to estimate encounter rates of teloest prey
# Encounter defined as the presence of a teleost on a video within a 30 s window
teleosts_video <- read_csv("from_Erin/Spencer et al video encounter by hour.csv") %>%
  dplyr::select(-"Hour")
# So each row here is an hour of 30 s echograms and the sum of teleosts seen over each hour?
# Hourly prey encounter rate (x 10) for 3 sharks

# Mu:
mean(rowMeans(read_csv("from_Erin/Spencer et al video encounter by hour.csv") %>%
                dplyr::select(-"Hour")) %>%
       na.omit())
# Mean = 6
sd(rowMeans(read_csv("from_Erin/Spencer et al video encounter by hour.csv") %>%
                dplyr::select(-"Hour")) %>%
       na.omit())
# SD = 3
hist(rowMeans(read_csv("from_Erin/Spencer et al video encounter by hour.csv") %>%
                dplyr::select(-"Hour")))
gammaPr(6,3)

# Tau:
hist(1/apply(read_csv("from_Erin/Spencer et al video encounter by hour.csv") %>%
               dplyr::select(-"Hour"), 2, sd, na.rm = T)^2)
1/3^2
gammaPr(0.1111, 0.13) # gamma because tau must be positive

# ~~~~~~~~~~~~~~~~~~~~~~~

# Priors: Blacktips in winter ####

# winter; January – March; 0.28 ± 0.27 sd sharks m-2

winter_bt <- (read.csv("Blacktips/Blacktip_abundances.csv") %>%
  filter(Period == "Winter") %>%
  dplyr::select(Sharks)) %>%
  mutate(Sharks = as.numeric(Sharks))

hist((winter_bt$Sharks)/15.1)
mean(winter_bt$Sharks/15.1)
sd(winter_bt$Sharks/15.1)

gammaPr(288.6784, 275.6694)

# tau
hist(winter_bt$Sharks)
# Likely to be an error of 30ish sharks?
1/30^2
1/0.0011111

# ~~~~~~~~~~~~~~~~~~~~~~~

# Priors: Blacktips in summer ####

# summer; May – December; 0.005 ± 0.0052 sd sharks m-2

summer_bt <- (read.csv("Blacktips/Blacktip_abundances.csv")) %>%
  filter(Period == "Summer") %>%
  filter(Clarity..1.excellent..5.poor. != 5) %>%
  dplyr::select(Sharks) %>%
  mutate(Sharks = as.numeric(Sharks))

hist((summer_bt$Sharks)/15.1)
mean(summer_bt$Sharks/15.1)
sd(summer_bt$Sharks/15.1)

gammaPr(5.163198, 5.499406)

# tau
hist(summer_bt$Sharks)
# Likely to be an error of 10ish sharks?
1/10^2
1/0.01

# Plot these two quickly:

# Create a sequence of x values
x_values <- seq(0, 0.05, length.out = 1000)

# Calculate the gamma density values
density_winter <- dgamma(x_values, shape = 1, rate = 100)
density_summer <- dgamma(x_values, shape = 1, rate = 900)

# Create a data frame for plotting
df <- data.frame(x = x_values, winter = density_winter, summer = density_summer)

# Plot the curves
ggplot(df, aes(x)) + 
  geom_line(aes(y = winter), color = "blue", size = 1) + 
  geom_line(aes(y = summer), color = "red", size = 1) + 
  labs(title = "Gamma Distribution Curves",
       x = "x",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = c("winter" = "blue", "summer" = "red"),
                     labels = c("Winter (0.1)", "Summer (0.00111)")) +
  theme_minimal()
