# Question 1
# Uniform distribion
# Let X ~ U(0, 0.5). We can plot the pdf and the cdf of the X in R
x <- seq(0,0.5,0.01)
curve(dunif(x, min = 0, max = 0.5),from=0,to=1,main="The pdf of U(0,0.5)")
curve(punif(x, min = 0, max = 0.5),from=0,to=1,main="The cdf of U(0.3,0.5)")

simulations<-runif(10000, min = 0, max = 0.5)
par(mfrow=c(1,2))
hist(simulations,freq=FALSE,xlim=c(0,1),ylim=c(0,5),breaks=1000, main="Simulated")
curve(dunif(x, min = 0, max = 0.5),from=0,to=1,main="Theoretical")

# Assume that the time T between job submissions to a busy computer centre is 
# uniformly distributed in the range [0, 0.5] min.

# The memory less property holds if:
# P(T > x + a | T > a) = P(T > x + a) / P(T > a) = P(T > x)
# P(T > 5 | T > 2) = P(T > 5) / P(T > 2) = P(T > 3)

# Let suppose 


checkMemoryless <- function(LHS, RHS)
{
  if(LHS == RHS)
  {
    print("Memory less property valid")
  }
  else
  {
    print("Memory less property is not valid")
  }
}

LHS = (1 - punif(0.44, min = 0, max = 0.5)) / (1 - punif(0.25, min = 0, max = 0.5)) 
RHS = 1 - punif(0.19, min = 0, max = 0.5)
checkMemoryless(LHS, RHS)


# Exponential distribution
LHS = (1 - pexp(0.44, 4)) / (1 - pexp(0.25, 4)) 
RHS = 1 - pexp(0.19, 4)


# P(T < 5 | T > 2) = (P(T < 5) - P(T > 2) ) / P(T > 2) = P(T > 3)


#P(T < 0.44 | T > 0.25) = (P(T < 0.44) - P(T > 0.25)) / P(T > 0.25) = P(T > 0.19)
# uniform function

LHS = (punif(0.44, min = 0, max = 0.5) - punif(0.25, min = 0, max = 0.5)) / (1 - punif(0.25, min = 0, max = 0.5)) 
RHS = punif(0.19, min = 0, max = 0.5)

submissions25<-subset(simulations,simulations>0.25)
length(submissions25)

y<-submissions25-0.25 
mean(y)
var(y)

mean(submissions)
var(submissions)

par(mfrow=c(1,2))
hist(y,freq=F,xlab="t-0.25|t>0.25",main="Subsample t-0.25|t>0.25")
hist(simulations,freq=F,xlab="t",main="Sample")

#P(T > 0.44 | T > 0.25) = P(T > 0.44) / P(T > 0.25) = P(T > 0.19)
LHS = (1- punif(0.44, min = 0, max = 0.5)) / (1 - punif(0.25, min = 0, max = 0.5)) 
RHS = 1- punif(0.19, min = 0, max = 0.5)

# using exponential function
LHS = (pexp(0.44, 4) - pexp(0.25, 4)) / (1 - pexp(0.25, 4)) 
RHS = pexp(0.19, 4)

LHS = (1- pexp(0.44, 4)) / (1 - pexp(0.25, 4)) 
RHS = 1- pexp(0.19, 4)

## Question 2

x = sequence(500)
mean(x)
var(x)
N=200
x=c(0,1,2,3,4,5)
obs=c(109,65,22,3,1,0)

curve(dpois(x, 40),from=0,to=10,main="The pdf of U(0,0.5)")
?dpois()
x = rpois(2000, 24)
mean(x)
var(x)
curve(dpois(x, 40),from=0,to=100,main="The pdf of Poissons ditribution")

x = 0:4
curve(dpois(x, 20),from=0,to=100,main="The pdf of Poissons ditribution")

?rnorm()
simulations = rnorm(2000, mean = 20, sd = 4.48)
hist(simulations,freq=FALSE,xlim=c(0,40),ylim=c(0,0.22),breaks=1000, main="Simulated")
curve(dnorm(x),from=-100,to=100,main="The pdf of Poissons ditribution")
sqrt(20)


expect0_4=N*dpois(0:4,0.61)
expect5=N-sum(expect0_4)
expect=c(expect0_4,expect5)
expect
sum(expect)


x<-0:10



# Question 2 

get_non_negative_discrete_values <- function(x_continous)
{
  x_continous[x_continous < 0] <- 0
  x_discrete = round(x_continous)
  return(x_discrete)
}

normal_poisson <- function(lambda_list)
{
  par(mfrow = c(2,3))
  lambda_value = c()
  p_val_list = c()
  for(lambda in lambda_list)
  {
    cat("\n\nFor lambda = ", lambda)
    x_continous = rnorm(20, mean = lambda, sd = sqrt(lambda))
    x_discrete = get_non_negative_discrete_values(x_continous)
    
    frequency_table = as.data.frame(table(x_discrete))
    observed_value = frequency_table$Freq
    N = sum(obs)
    cat("\n The observed value:", obs)
    # N = length(x_discrete)
    x = unique(x_discrete)
    min_val = min(unique(x_discrete))
    max_val = max(unique(x_discrete))
    
    expected_value=N*dpois(x, lambda)
    
   
    # plot(x_val,dnorm(x_val, mu, sqrt(sigma2)),type="l",col="red",xlab="x",ylab="P(X=x)", xlim = c(0,200))
    # lines(x,dpois(x, lambda),type="h",col="black")  
    
    cat("\nThe expected value:\n", expect)
    sum(expect)
    
    chi_sq=sum((observed_value-expected_value)^2/expected_value)
    cat("\nThe observed chi-square value:", chi_sq)
    # degree of freedom = no. of category - k - 1
    # k = number of parameters
    
    k = 1
    df = length(unique(x_discrete)) -k - 1
    cat("\nThe test chi square value:", qchisq(0.95,df))
    p_val = 1-pchisq(chi_sq, df)
    cat("\nThe probability score:", p_val)
    options("digits"=4)
    
    mu = lambda
    sigma2 = lambda
    x_val = min_val:max_val
    p <- p_val
    title = paste("P-val:", p)
    x_label = paste("x", " lambda : ", lambda)
    plot(x_val,dnorm(x_val, mu, sqrt(sigma2)),type="l",col="red",xlab=x_label,ylab="P(X=x)", main = title)
    #, xlim = c(0,200))
    lines(x,dpois(x, lambda),type="h",col="black")  
    
    
    if(p_val > 0.05)
    {
      lambda_value <- c(lambda_value, lambda)
      p_val_list <- c(p_val_list, p_val)
    }
  }
  
  return(list("lambda_value" = lambda_value, "p_val_list" = p_val_list))
}

find_index <- function(p_val_list)
{
  index = 0
  minimum_val = min(p_val_list)
  for(p in p_val_list)
  {
    if(p == minimum_val)
    {
      index = index + 1
    }
  }
  return(index+1)
}

lambda_list = seq(0:1000)
a = normal_poisson(lambda_list)
i = find_index(a$p_val_list)
minimum_lambda = a$lambda_value[i]
minimum_p_value = a$p_val_list[i]
