# Normal Approximation to poissons distribution
get_non_negative_discrete_values <- function(x_continous)
{
  x_continous[x_continous < 0] <- 0
  x_discrete = round(x_continous)
  return(x_discrete)
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

normal_poisson <- function(n, lambda_list)
{
  
  par(mfrow = c(2,3))
  lambda_value = c()
  p_val_list = c()
  
  for(lambda in lambda_list)
  {
    cat("\n\nFor lambda = ", lambda)
    x_continous = rnorm(n, mean = lambda, sd = sqrt(lambda))
    x_discrete = get_non_negative_discrete_values(x_continous)
    
    frequency_table = as.data.frame(table(x_discrete))
    observed_value = frequency_table$Freq
    N = sum(observed_value)
    cat("\n The observed value:", observed_value)
    x = unique(x_discrete)
    expected_value=N*dpois(x, lambda)
    cat("\nThe expected value:\n", expected_value)
    sum(expected_value)
    
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
    min_val = min(unique(x_discrete))
    max_val = max(unique(x_discrete))
    x_val = min_val:max_val
    p <- p_val
    title = paste("P-val:", p)
    x_label = paste("x", " lambda : ", lambda)
    plot(x_val,dnorm(x_val, mu, sqrt(sigma2)),type="l",col="red",xlab=x_label,ylab="P(X=x)", main = title)
    lines(x,dpois(x, lambda),type="h",col="black")  
    
    if(p_val > 0.05)
    {
      lambda_value <- c(lambda_value, lambda)
      p_val_list <- c(p_val_list, p_val)
    }
  }
  
  return(list("lambda_value" = lambda_value, "p_val_list" = p_val_list))
}

main <- function()
{
  lambda_list = seq(0:100)
  n = 25
  a = normal_poisson(n, lambda_list)
  i = find_index(a$p_val_list)
  minimum_lambda = a$lambda_value[i]
  minimum_p_value = a$p_val_list[i]
  cat("\n\nThe minimum value of lambda : ", minimum_lambda)
  cat("\nThe minimum P-value : ", minimum_p_value)
}

main()


