

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

# Teloests via sonar
# Mu:
hist(rowMeans(read_csv("Spencer et al echogram by hour.csv") %>%
                dplyr::select(-"Hour")))
gammaPr(15,10)

# Tau:
hist(1/apply(read_csv("Spencer et al echogram by hour.csv") %>%
        dplyr::select(-"Hour"), 2, sd))
gammaPr(0.075, 0.02)

# Teloests via video
# Mu:
hist(rowMeans(read_csv("Spencer et al video encounter by hour.csv") %>%
                dplyr::select(-"Hour")))
gammaPr(3,3)

# Tau:
hist(1/apply(read_csv("Spencer et al video encounter by hour.csv") %>%
               dplyr::select(-"Hour"), 2, sd, na.rm = T))
gammaPr(0.4,0.1)
