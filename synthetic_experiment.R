library(SynthETIC)

# simulate observed data --------------------------------------------------

# actual values 
freq = 0.06
shape = 10
rate = 0.5

frequency_vector = claim_frequency(I=10,freq=freq) # 10 periods of development
claim_size_list = claim_size(frequency_vector,simfun=stats::rgamma,shape=shape,rate=rate)
no_payments_list = claim_payment_no(frequency_vector,claim_size_list)
payment_size_list = claim_payment_size(frequency_vector,claim_size_list,no_payments_list)
occurrence_list = claim_occurrence(frequency_vector)
notification_list = claim_notification(frequency_vector,claim_size_list)
settlement_list = claim_closure(frequency_vector,claim_size_list)
payment_delay_list = claim_payment_delay(frequency_vector,claim_size_list,no_payments_list, settlement_list)
payment_time_list = claim_payment_time(frequency_vector,occurrence_list,notification_list,payment_delay_list)

observed = claim_output(frequency_vector,payment_time_list,payment_size_list, adjust=TRUE, future=FALSE)
d = observed %>% matrix(c(10,10))
d[is.na(d)] <- 0 # future 

# ABC ---------------------------------------------------------------------

# assumed priors
# freq is Poisson(lambda)
# lambda is Uniform(0.02,0.06)
# size is Gamma(a,b)
# a is Uniform(5,20)
# b is Uniform(0,1)

n <- 1000 # number of simulations
lambda <- runif(n,0.01,0.1)
a <- runif(n,1,50)
b <- runif(n,0.1,1)
dist <- rep(NA,n)

for (i in 1:1000)
{
  print(i)
  li <- lambda[i]
  ai <- a[i]
  bi <- b[i]
  frequency_vector = claim_frequency(I=10,freq=li)
  claim_size_list = claim_size(frequency_vector,simfun=stats::rgamma,shape=ai,rate=bi)
  no_payments_list = claim_payment_no(frequency_vector,claim_size_list)
  payment_size_list = claim_payment_size(frequency_vector,claim_size_list,no_payments_list)
  occurrence_list = claim_occurrence(frequency_vector)
  notification_list = claim_notification(frequency_vector,claim_size_list)
  settlement_list = claim_closure(frequency_vector,claim_size_list)
  payment_delay_list = claim_payment_delay(frequency_vector,claim_size_list,no_payments_list, settlement_list)
  payment_time_list = claim_payment_time(frequency_vector,occurrence_list,notification_list,payment_delay_list)
  
  # need try except block
  # because synthetic 'helpfully' gives a warning, stopping the for loop
  # for any triangles that are too 'unrealistic'
  # which of course we'd chuck out anyway in the next step
  # so we need tryCatch to stop this behaviour
  simulated = tryCatch(claim_output(frequency_vector,payment_time_list,payment_size_list, adjust=TRUE, future=FALSE),error=0)
  if (sum(simulated[!is.na(simulated)])>0)
    {
      d1 = simulated %>% matrix(c(10,10))
      d1[is.na(d1)] <- 0 # future payments
      
      dist[i] <- abs(d-d1) %>% sum() / 100 
      # many options for distance metric e.g. square
      # divide by 100 just to make the numbers smaller. irrelevant really.
      # it's the relative distances that matter (e.g. distances in smallest 10%, which dividing by 100 doesn't change)
    }
}

# analysis / data viz -----------------------------------------------------

par(mfrow = c(1,2))

summary(dist)

length(dist[dist<100])
length(dist[dist<55])

# 100 approx = quantile(dist,0.2)
mean(lambda)
lambda[dist<100] %>% mean()
hist(lambda)
hist(lambda[dist<100])

mean(a)
a[dist<100] %>% mean()
hist(a)
hist(a[dist<100])

mean(b)
b[dist<100] %>% mean()
hist(b)
hist(b[dist<100])

# 55 approx = quantile(dist,0.05)
median(lambda) # worth looking at robust measures as many outliers will exist from simulation process
lambda[dist<55] %>% median()
hist(lambda)
hist(lambda[dist<55])

median(a)
a[dist<55] %>% median()
hist(a)
hist(a[dist<55])

median(b)
b[dist<55] %>% median()
hist(b)
hist(b[dist<55])

df = data.frame(l = lambda[dist<55], a =a[dist<55], b=  b[dist<55])

