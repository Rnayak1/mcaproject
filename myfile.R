calchoice <-0
################################# MOdule 2 functions ###############################################

# Corelation calculation function
correlation <- function()
{
  prompt1 <- "Enter the data for X : "
  x <- as.integer(strsplit(readline(prompt1)," ")[[1]])
  prompt2 <- "Enter the data for Y : "
  y <- as.integer(strsplit(readline(prompt2)," ")[[1]])
  if(length(x) == length(y))
    n <- length(x)
  else
  {
    cat("\n  length mismatch, calculator will exit in two secs")
    Sys.sleep(2)
    return()
  }
  u1 <- n * sum(x*y) - sum(x)* sum(y)
  u2 <- ((n*sum(x^2) - (sum(x)^2)) * (n*sum(y^2) - (sum(y)^2)))^0.5
  
  rcof <- u1/u2
  cat("\n the correlation cofficient is : ",rcof)
  
  cat("\n Statistical test for given correlation : ")
  test <- rcof*((n-2)^0.5)
  test<- test/((1-(rcof)^2)^0.5)
  cat (test)
}
# Multiple Regression
Multi_Linear_Regression<-function() 
{
  prompt1 <- "Enter the data for X1 : "
  x1 <- as.integer(strsplit(readline(prompt1)," ")[[1]])
  prompt2 <- "Enter the data for X2 : "
  x2 <- as.integer(strsplit(readline(prompt2)," ")[[1]])
  prompt3 <- "Enter the data for Y : "
  y <- as.integer(strsplit(readline(prompt3)," ")[[1]])
  xMatrix<-cbind(c(length(x1),sum(x1),sum(x2)) , c(sum(x1),sum(x1**2),sum(x1*x2)) , c(sum(x2),sum(x1*x2),sum(x2**2)))
  xyMatrix<-cbind(c(sum(y),sum(x1*y),sum(x2*y)))
  result<-solve(xMatrix,xyMatrix)    #matrix inverse , then multiply with inverse and save final result in result
  print(result)
  # paste0("y=%f+%f*x1+%f*x2" , result[1][1] ,result[2][1],  result[3][1]) #print in this format
}

################################# Module 3 functions ################################################

# factorial calculation
fact <- function(num,index = 0)
{
  if(index == 0)
    num <- as.double(readline(prompt = " Enter the number to calculate factorial : "))
  factorial=1
  if(num == 0)
    factorial=1
  else
  {
    for(i in 1:num)
    {
      factorial = factorial * i
    }
  }
  if(index == 0)
    cat("\n The result is : ",factorial)
  else
    return(factorial)
}
# permutation
permutation <- function(set,num,index=0)
{
  if(index == 0)
  {
    set <- as.double(readline(prompt = "Enter the total numbers (n) : "))
    num <- as.double(readline(prompt = " Enter the number of selections to made : "))
  }
  numerator = fact(set,1)
  denominator = fact(set-num,1)
  result <- numerator/denominator
  if(index == 0)
    cat("\n The result is",result)
  else
    return(result)
}
# combination
combination <- function(set,num,index = 0)
{
  if(index == 0)
  {
    set <- as.double(readline(prompt = " Enter the length of population : "))
    num <- as.double(readline(prompt = " Enter the selection to made : "))
  }
  numerator = permutation(set,num,1)
  denominator = fact(num,1)
  result <- numerator/denominator
  if(index == 0)
    cat("\n The result is ",result)
  else
    return(result)
}
# basic probability
basicprob <- function(outcome,event,index = 0)
{
  if(index == 0)
  { 
    prompt1 <- "Enter the elements in Sample Space (seperated by space) : \n"
    outcome <- as.integer(strsplit(readline(prompt1), " ")[[1]]) 
    
    prompt2 <- "Enter the elements of favourable Events (seperated by space) : \n"
    event <- as.integer(strsplit(readline(prompt2), " ")[[1]])
  }
  check <-0           # to check wether events exist in sample
  probability <-0     # to compute probailty 
  for (i in 1:length(event)) 
  {
    for (j in 1:length(outcome))
    {
      if(event[i] == outcome[j])
      {
        check<-check+1
        break
      }
    }
  }
  if(check == length(event))
  {
    for(i in 1:length(event))
    {
      probability <- probability+ (1/length(outcome))
    }
    if(index == 0)
      cat("\n probability is :",probability)
    else
      return(probability)
  }
  else
  {
    cat("\n Events does not exist in Sample Space... \n Calculator will exit in 2 sec")
    Sys.sleep(2)
    return()
  }
}
# intersection of two sets
intersection <- function(a,b)
{
  set1 <- a
  set2 <- b
  set3 <- c()
  for(i in 1:length(set2))
  {
    for(j in 1:length(set1))
    {
      if(set2[i] == set1[j])
        set3 <- c(set3,set1[j])
    }
  }
  return(set3)
}  
# conditional probabiltiy
conditionalProb <- function(outcome,event1,event2,index = 0)
{
  if(index == 0)
  {
    prompt1 <- "Enter the elements in Sample Space (seperated by space) : \n"
    outcome <- as.integer(strsplit(readline(prompt1), " ")[[1]]) 
    prompt2 <- "Enter the elements of favourable Events A (seperated by space) : \n"
    event1 <- as.integer(strsplit(readline(prompt2), " ")[[1]])
    prompt3 <- "Enter the elements of favourable Events B (seperated by space) : \n"
    event2 <- as.integer(strsplit(readline(prompt3), " ")[[1]])
  }
  choice <- as.double(readline(prompt = " calculate 1). p(A/B) \t 2). p(B/A) "))
  if(choice == 1)
    lowerevent <- event2
  else
    lowerevent <- event1
  check1 <-0
  check2 <- 0
  # to check all element of event1 exist in sample space
  for(i in 1:length(event1))
  {
    for (j in 1:length(outcome))
    {
      if(event1[i] == outcome[j])
        check1 <- check1 + 1
    }
  }
  # to check all element of event2 exist in sample space
  for(i in 1:length(event2))
  {
    for (j in 1:length(outcome))
    {
      if(event2[i] == outcome[j])
        check2 <- check2 + 1
    }
  }
  print(check2)
  print(check1)
  if(check1 != length(event1) || check2 != length(event2))
  {
    cat("\n One event does not falls in sample, calculator is exiting ...")
    Sys.sleep(2)
    return()
  }
  upperevent <- intersection(event1,event2)
  upper <- basicprob(outcome,upperevent,1)
  lower <- basicprob(outcome,lowerevent,1)
  result <- upper/lower
  if(is.na(result))
  {
    cat("\n syntax error occured, calcuator is exitting")
    Sys.sleep(2)
    return()
  }
  if(index == 0)
    cat("\n The conditional probability is : ",result)
  else
    return(result)
}
# bayes theorm
bayes <- function()
{
  outcome <- as.double(strsplit(readline(prompt = " Enter the set for Bayes theorm : ")," ")[[1]])
  event1 <- as.double(strsplit(readline(prompt = " Enter the event A outcomes : ")," ")[[1]]) 
  event2 <- as.double(strsplit(readline(prompt = " Enter the event B outcomes : ")," ")[[1]])
  choice <- as.double(readline(prompt = " What you want to calculate \n 1). p(A/B) \n 2). p(B/A)"))
  if(choice == 1)
    lower <- event2
  else
    lower <- event1
  upper <- intersection(event2,event1)
  print(upper)
  if(length(upper) == 0)
  {
    cat("\n The result is 0 \n")
    return()
  }
  numerator <- basicprob(outcome,upper,1)
  denominator <- basicprob(outcome,lower,1)
  result <- numerator/denominator
  cat("\n The result is : ",result)
}

################################### Module 4 functions ##############################################

#Descrete Uniform Distribution
UniformDis <- function()
{
  x <- as.numeric(strsplit(readline(prompt = "Enter the data for  X : ")," ")[[1]])
  x <- unique(x)
  x <- na.omit(x)
  udis <- 1/length(x)
  cat("\n Probability distribution is : ",udis  )
  
}
#Bernoulli Distribution
BernoulliDis <- function()
{
  p <- as.numeric(readline(prompt = "Enter the probability of success : "))
  x <- as.numeric(readline(prompt = "Enter the value of x for failure and succes i.e. (0 or 1) : "))
  if(x < 0 || x >1)
  {
    cat("\n Wrong input .. calculator is returning to main module in 2 secs")
    Sys.sleep(2)
    return()
  }
  success <- p^x
  failure <- (1-p)^(1-x)
  berdis <- success * failure
  cat("\n Probability distribution is : ",berdis  )
  
}
#Binomial Distribution
BinomialDis <- function(n,x,p,index = 0)
{
  if(index == 0)
  {
    n <- as.numeric(readline(prompt = "Enter the total no. of trials :  "))
    x <- as.numeric(readline(prompt = "Enter the no. of successes in n trials : "))
    p <- as.numeric(readline(prompt = 'Enter the probability of success : '))
  }
  if(is.na(n) || is.na(x) || is.na(p))
  {
    cat("\n Wrong parameter entered calculator will exit in 2 sec to main function")
    Sys.sleep(2)
    return()
  }
  c <- combination(n,x,1)
  success <- p^x
  failure <- (1-p)^(n-x)
  bdd <- c * success * failure
  if(index == 0)
    cat("\nProbability distribution is : ",bdd  )
  else
    return(bdd)
  
}
#Geometric Distribution
GeometricDis <- function()
{
  success <- as.numeric(readline(prompt = "Enter the probability of success for a single trial : "))
  x <- as.numeric(readline(prompt = "Enter the no. of failures before success : "))
  
  failure <- 1-success
  gd <- (failure^x)*success
  
  
  cat("\nProbability distribution is : ",gd )
  
  
}
#Hyper-Geometric Distribution
HgeoDis <- function()
{
  N <-as.numeric(readline(prompt = "Enter the total no. of the items in the population  :  "))
  k <- as.numeric(readline(prompt = "Enter the number of items in the population that are classified as successes : "))
  n <- as.numeric(readline(prompt = "Enter the number of items in the sample : "))
  x <- as.numeric(readline(prompt = "Enter the number of items in the sample that are classified as successes : "))
  
  a <- combination(k,x,1)
  b <- combination((N-k),(n-x),1)
  c <- combination(N,n,1)
  
  hd <- (a*b)/c
  
  cat("\nProbability distribution is : ",hd )
  
  
}
#Negative Binomial Distribution
NegBinomial <- function()
{
  x <- as.numeric(readline(prompt = "Enter the number of trials required to produce r successes : "))
  r <- as.numeric(readline(prompt = "Enter the number of successes : "))
  theta <- as.numeric(readline(prompt = "Enter the probability of success on an individual trial : "))
  
  c <- combination((x-1),(r-1),1)
  s <- theta^r
  f <- (1-theta)^(x-r)
  
  nbd <- c * s * f
  
  
  cat("\nAProbability distribution is : ",nbd )
  
  
}
#Poisson Distribution
Poisson <- function()
{
  lambda <- as.numeric(readline(prompt = "Enter the mean number of successes that occur in a specified region (lambda, which must be greater than zero) : "))
  x <- as.numeric(readline(prompt = "Enter the actual number of successes that occur in a specified region : "))
  
  a <- (lambda^x)*exp(-lambda)
  b <- fact(x,1)
  
  pd <- a/b
  
  cat("\nAProbability distribution is : ",pd )
  
  
}
#Multinomial Distribution
Multinomial <- function()
{
  n <- as.numeric(readline(prompt = "Enter the total no. of trials : "))
  x <- as.numeric(strsplit(readline(prompt = "Enter the discrete number of the possible outcomes according to the each trial (seperated by a space) : ")," ")[[1]])
  theta <- as.numeric(strsplit(readline(prompt ="Enter the probabilities of the possible outcomes (seperated by a space) : " )," ")[[1]])
  div <- 1
  mul <- 1
  fac <- fact(x,1)
  n <- length(fac)
  for(i in 1:n)
  {
    div <- (div*fac[i])
  }
  nfact <- fact(n,1)
  a <- nfact/div
  pow <- theta^x
  for(i in 1:n)
  {
    mul <- mul*pow[i]
  }
  
  md <- d *mul
  cat("\nAProbability distribution is : ",md )
  
}
#Multivariate Hypergeometric Distribution
Multivariate <- function()
{
  n <- as.numeric(readline(prompt = "Enter the total no. of trials (n): "))
  m <- as.numeric(strsplit(readline(prompt = "Enter the elements of kinds of possible outcomes(m):  " )))
  x <- as.numeric(strsplit(readline(prompt = "Enter the possible outcomes (x) : ")))
  
  
  c <-1
  N1 <-0
  C <- combination(m,x,1)
  for(i in 1:length(C))
  {
    a<-a*C[i]
  }
  for(i in 1:length(m))
  {
    N1<-N1+m[i]
  }
  TotalWays<-combination(N1,n,1)
  mhg <-a/TotalWays
  
  cat("\nAProbability distribution is : ",mhg )
  
}

#################################  Module 5 function  ##############################################

#Continous Uniform Distribution
UniformCDistribution <- function()
{
  a <- as.numeric(readline(prompt = "Enter the 1st parameter A (minimum) : "))
  b <- as.numeric(readline(prompt = "Enter the 2nd parameter B (maximum) : "))
  x <- as.numeric(readline(prompt = "Enter a random variable X : "))
  
  ucd <- 1/(b-a)
  if(x > a ||x < b)
  {
    pdensity <- ucd
  }
  else
  {
    pdensity <- 0
  }
  cat("\nProbability Density is : ",pdensity)
}
#Normal Distribution
NormalDistribution <- function()
{
  x <- as.numeric(readline(prompt = "Enter the value of random variable X : "))
  u <- as.numeric(readline(prompt = "Enter the mean value : "))
  sigma <- as.numeric(readline(prompt = 'Enter the value of standard deviation (must be a positive value) : '))
  
  power <- -0.5*((x-u)/sigma)^2
  denominator <- sigma*((2*3.14)^0.5)
  
  nd <- exp(power)/denominator
  cat("\nProbability Density is : ",nd)
}
#for gamma function(T)
T <- function(num)
{
  num <- fact(num-1,1)
  return(num)
}
#Gamma Distribution
GammaDistribution <- function()
{
  #  x <- as.numeric(readline(prompt = "Enter the value of random variable X : "))
  alpha <- as.numeric(readline(prompt = "Enter the value of alpha : "))
  beta <- as.numeric(readline(prompt = "Enter the value of beta : "))
  upper <- as.double(readline(prompt = " Enter the upper limit : "))
  lower <- as.double(readline(prompt = " Enter the lower limit : "))
  integrand <- function(x) {x}
  numerator <- integrand^(alpha-1)
  denominator <- beta^alpha
  power <- -x/beta
  t <- T(alpha)
  result <- (numerator * exp(power))/(denominator * t)
  
  if(lower > 0 && upper > 0)
  { 
    pdensity <- result  
  }
  else
  {
    pdensity <- 0
  }
  cat("\nProbability Density is : ",pdensity)
  val <- integrate(pdensity,upper,lower)
  print(val)
}

#################################  Module 6 functions ###############################################

# function to calculate chi square value for given dataset
chisq <- function()
{
  prompt1 <- " Enter the elements in observation (seperated by space) : "
  observed <- as.double(strsplit(readline(prompt1), " ")[[1]]) 
  
  expexted <- as.double(readline(prompt = " Enter the expected value : "))
  chival <- 0
  for(i in 1:length(observed))
  {
    value <- (observed[i] - expexted)^2
    value <- value/expexted
    chival <- chival + value
  }
  if(is.na(chival))
  {
    cat("\n syntax error ")
    Sys.sleep(2)
    return()
  }
  else
    cat(" \n The calculated chi square value of given data is : ",chival)
}
# function to calculate student t value for given datasets
ttest <- function()
{
  data1 <- as.double(strsplit(readline(prompt = " Enter the dataset 1 : ")," ")[[1]])
  data2 <- as.double(strsplit(readline(prompt = " Enter the dataset 2 : ")," ")[[1]])
  if(length(data1) != length(data2))
  {
    cat("\n data sets doesnot have same numbers of observation calculator is exiting..")
    Sys.sleep(2)
    return()
  }
  dif <- c()
  sqdif <- c()
  for(i in 1:length(data1))
  {
    dif <- c(dif,data1[i] - data2[i])
    sqdif <- c(sqdif,(dif[i])^2)
  }
  dif <- sum(dif)
  sqdif <- sum(sqdif)
  numerator <- dif/length(data1)
  denominator <- sqdif - (((dif)^2) / length(data1))
  denominator <- denominator/((length(data1) - 1) * length(data2))
  denominator <- denominator^0.5
  tvalue <- numerator/denominator
  
  if(is.na(tvalue))
  {
    cat("\n syntax error ..  calculator will exit in 2 sec ")
    Sys.sleep(2)
    return()
  }
  else
    cat("\n the t value calculated is : ",tvalue)
}
# function to calculate F value for given datasets
ftest <- function()
{
  data1 <- as.double(strsplit(readline(prompt = " Enter the dataset 1(seperated by space) : ")," ")[[1]])
  data2 <- as.double(strsplit(readline(prompt = " Enter the dataset 2(seperated by space) : ")," ")[[1]])
  mean1 <- sum(data1)/length(data1)
  var1 <- 0
  for(i in 1 : length(data1))
  {
    var1 <- var1 + (data1[i] - mean1)^2
  }
  var1 <- var1/length(data1)
  mean2 <- sum(data2)/length(data2)
  var2 <- 0
  for(i in 1 : length(data2))
  {
    var2 <- var2 + (data2[i] - mean2)^2
  }
  var2 <- var2/length(data2)
  
  fvalue <- var1/var2
  
  if(is.na(fvalue))
  {
    cat("\n syntax error .. invalid data entered calculator will eixt in two secs")
    Sys.sleep(2)
    return()
  }
  else
    cat("\n The calculated f-value is : ",fvalue)
  
}
# function to calculate Z value for given datasets
ztest <- function()
{
  sample1 <- as.double(readline(prompt = " Enter the length of sample 1, greater than 30 : ")) 
  select1 <- as.double(readline(prompt = " Enter the selection 1 from sample : ")) 
  sample2 <- as.double(readline(prompt = " Enter the length of sample 2, greater than 30 : ")) 
  select2 <- as.double(readline(prompt = " Enter the selection 2 from sample : "))
  if(is.na(sample1) || is.na(sample2) || is.na(select1) || is.na(select2) || sample1 < 30 || sample2 < 30)
  {
    cat("\n wrong parameter entered calculator is exiting")
    Sys.sleep(2)
    return()
  }
  probability1 <- select1/sample1
  probability2 <- select2/sample2
  comprobability <- (select1 + select2)/(sample2 + sample1)
  
  numerator <- probability1 - probability2
  denominator <- comprobability * (1 - comprobability)
  denominator <- denominator * ( sample2 + sample1)
  denominator <- denominator/(sample1 * sample2)
  denominator <- (denominator)^0.5
  
  zvalue <- numerator/denominator
  
  if(is.na(zvalue))
  {
    cat(" \n syntax error .. calculator will exit in 2 secs")
    Sys.sleep(2)
    return()
  }
  else
    cat(" \n The Z-value calculated is :",abs(zvalue))
}

###################################  Module 7 functions #########################################

# Estimation of mean
eom <- function(alpha)
{
  number <- as.double(readline(prompt = "Enter the number of sample population"))
  sdeviation <- as.double(readline(prompt = "Enter the standard variance for sample "))
  mean <- as.double(readline(prompt = "Enter the mean for sample : "))
  
  if(is.na(sdeviation))
  {
    cat("\n you have not entered standard deviation for sample")
    psdeviation <- as.double(readline(prompt = "Enter the standard deviation of population"))
    if(psdeviation == "")
    {
      print("\n Wrong value has been inserted .. calcualtor is exitting")
      Sys.sleep(2)
      return()
    }
    if(number > 30)
    {
      value <- abs(qnorm(alpha)) * psdeviation
      value <- value / (number)^0.5
      lvalue <- mean - value
      hvalue <- mean + value
    }
    
    if(number < 30)
    {
      value <- qt(alpha,number) * psdeviation
      value <- value / (number)^0.5
      lvalue <- mean - value
      hvalue <- mean + value
    }
  }
  else
  {
    value <- abs(qnorm(alpha)) * sdeviation
    value <- value / (number)^0.5
    lvalue <- mean - value
    hvalue <- mean + value
  }
  cat("\n The confidence interval lies between : ",lvalue," to ",hvalue)
}
# Estimation of propotion
eop <- function(alpha)
{
  number <- as.double(readline(prompt = "\n Enter the number in sample (n): ") )
  select <- as.double(readline(prompt = "\n Enter the number of selection (x) : "))
  if(number == "" || select == "")
  {
    cat("\n wrong input entered .. calculator exiting")
    Sys.sleep(2)
    return()
  }
  theta <- select/number
  value <- theta * (1 - theta)
  value <- (value/number)^0.5
  value <- value * abs(qnorm(alpha))
  lvalue <- theta - value
  hvalue <- theta + value
  
  cat("\n The confidence interval is : ",lvalue ," to ",hvalue)
}
# Estimation of variance
eov <- function(alpha)
{
  number <- as.double(readline(prompt = "\n Enter the number of sample : "))
  variance <- as.double(readline(prompt = "\n Enter the variance of sample : "))
  if(is.na(number) || is.na(variance) || number == 0)
  {
    cat ("\n wrong input parameters.. calculator is exiting now")
    Sys.sleep(2)
    return()
  }
  lvalue <- (number - 1) * variance
  lvalue <- lvalue / qchisq(alpha,number - 1)
  
  hvalue <- (number -1)*variance
  hvalue <- hvalue/qchisq(1 - alpha , number - 1)
  
  cat("\n the interval is",lavalue," to ",hvalue)
}
# Estimation of diffrence of mean
eodm <- function(alpha)
{
  number1 <- as.double(readline(prompt = " Enter the size of popualtion 1 : "))
  mean1 <- as.double(readline(prompt = " Enter the mean of population 1 : "))
  variance1 <- as.double(readline(prompt = " Enter the variance of population 1 :"))
  
  number2 <- as.double(readline(prompt = " Enter the size of population 2 : "))
  mean2 <- as.double(readline(prompt = " Enter the mean of population 2 : "))
  variance2 <- as.double(readline(prompt = " Enter the variance of population 2 : "))
  
  if(is.na(number1) || is.na(number2) || number1 == 0 || number2 == 0)
  {
    cat("\n Oops.. you entered wrong values. Calculator will exit in 2 sec")
    Sys.sleep(2)
    return()
  }
  if(is.na(variance2) || is.na(variance1))
  {
    cat("\n You have not entered variance of either of sample \n please enter variances for both population")
    pvariance1 <- as.double(readline(prompt = " Enter the variance of population 1 :"))
    pvariance2 <- as.double(readline(prompt = " Enter the variance of population 2 :"))
    
    if(is.na(pvariance2) || is.na(pvariance1))-
    {
      cat("\n wrong parameter entered.. calculator will exit in 2 secs")
      Sys.sleep(2)
      return()
    }
    if(number1 > 30 && number2 > 30)
    {
      value <- (number2 * pvariance1) + (number1 * pvariance2)
      value <- value/(number2*number1)
      value <- (value)^0.5
      value <- value * abs(qnorm(alpha))
      lvalue <- mean1 - mean2 - value
      hvalue <- mean1 - mean2 + value
    }
    else
    {
      sdpopulation <- ((number1 - 1)*pvariance1) + ((number2 - 1)*pvariance2)
      sdpopulation <- sdpopulation/(number1+number2-2)
      sdpopulation <- (sdpopulation)^0.5
      value <- number1 + number2
      value <- value / (number2 * number1)
      value <- (value)^0.5
      value <- value * sdpopulation
      value <- value * qt(alpha,number2+number1-2)
      lvalue <- mean1 - mean2 - value
      hvalue <- mean1 - mean2 + value
    }
  }
  else
  {
    value <- (variance1 * number2) + (variance2 * number1)
    value <- value/(number1 * number2)
    value <- (value)^0.5
    value <- value * abs(qnorm(alpha))
    lvalue <- mean1 - mean2 - value
    havlue <- mean1 - mean2 + value
  }
  
  cat("\n The range is from ",lvalue ," to ",hvalue)
}
# Estimation of diffrence of propotion
eodp <- function(alpha)
{
  number1 <- as.double(readline(prompt = " Enter the length of sample 1 : "))
  select1 <- as.double(readline(prompt = " Enter the selection from sample 1 : "))
  number2 <- as.double(readline(prompt = " Enter the length of sample 2 : "))
  select2 <- as.double(readline(prompt = " Enter the selection from sample 2 : "))
  theta1 <- select1/number1
  theta2 <- select2/number2
  value <- (theta1 * (1 - theta1))/number1
  value <- value + ((theta2 * (1 - theta2))/number2)
  value <- (value)^0.5
  value <- value * abs(qnorm(alpha))
  lvalue <- theta1 - theta2 - value
  hvalue <- theta1 - theta2 + value
  
  cat("\n The confidence interval lies between ",lvalue," to ",hvalue)
  
}
# Estimation for diffrence of variances
eodv <- function(alpha)
{
  number1 <- as.double(readline(prompt = " Enter the size of population 1 :"))
  variance1 <- as.double(readline(prompt = " Enter the variance of population 1 : "))
  number2 <- as.double(readline(prompt = " Enter the size of population 2 :"))
  variance2 <- as.double(readline(prompt = " Enter the variance of population 2 : "))
  lvalue <- variance1/(variance2 * qf(alpha,number1-1,number2-1))
  hvalue <- variance1 * qf(alpha,number2-1,number1-1)
  cat("\n The interval is between ",lvalue," to ",hvalue)
}
############################################## Module 8 function ###################################
UserSortKruskal <- function(input)
{
  temp=0
  for( i in 1:length(input))
  {
    for( j in i:length(input))
    {
      if(input[i] > input[j])
      {
        temp = input[i]
        input[i] = input[j]
        input[j] = temp
        
        temp <- names(input[i]) 
        names(input)[i]<-paste(names(input[j]))
        names(input)[j]<-paste(temp)
      }
    }
  }
  return(input)
}
# Sign test
UserSignT <- function(data,mu,alpha)
{
  countn <- 0
  countp <- 0
  theta <- 1/2
  for(i in 1:length(data))
  {
    if( (data[i]-mu) < 0){
      countn <- countn + 1
    }
    else if((data[i]-mu) > 0){
      countp <- countp + 1
      
    }
  }
  size <- countn+countp
  if(size < 30){
    result <- BinomialDis(size,countp,theta,1)
  }
  else {
    num <- countp - (size*theta)
    denom <-sqrt(size*theta*(1-theta))
    result <- num/denom
  }
  if(result < 0)
  {
    result <- abs(result)
  }
  if(result < alpha)
  {
    cat("Reject the Null Hypothesis ( Ho )")
    return()
  }
  else
  {
    cat("\n Accept the Null Hypothesis ( Ho )")
    return()
  }
}
# wilcoxon test
UserWilcoxonTest <- function(data,mu,alpha,case)
{
  tneg = 0
  tpos = 0
  count0 = 1
  data_minus_mu <- data - mu
  
  data_rank = sort(data_minus_mu)  
  
  for(i in 1:length(data_rank)){
    if( data_rank[i] == 0.0)
    {
      count0 = count0 + 1
    }
  }
  size <- length(data_rank)
  data_rank = data_rank[count0:size]
  
  for(i in 1:length(data_rank)){
    
    if(data_rank[i] < 0){
      tneg = tneg+i
    }
    else if (data_rank[i] > 0){
      tpos = tpos+i
    }
  }
  n = length(data_rank)
  t_min = min(tneg,tpos)
  if(case == 1)
  { 
    tvalue = qsignrank(alpha/2,n)
    
    if(t_min <= tvalue)
    {
      cat("\n Null Hypothesis (Ho) is rejected")
      return()
    }
    else
    {
      cat("\n Null Hypothesis (Ho) is accepted")
      return()
      
    }
  }
  else if(case == 2)
  {
    tvalue = qsignrank(alpha,n)
    
    if(tneg <= tvalue)
    {
      cat("\n Null Hypothesis (Ho) is rejected")
      return()
    }
    else
    {
      cat("\n Null Hypothesis (Ho) is accepted")
      return()
    }
  }
  else if(case == 3)
  {
    tvalue = qsignrank(alpha,n)
    
    if(tpos <= tvalue)
    {
      cat("\n Null Hypothesis (Ho) is rejected")
      return()
    }
    else
    {
      cat(" \n Null Hypothesis (Ho) is accepted")
      return()
    }
  }
  
}
# MannW Test
UserMannWTest <- function(data1,data2,alpha,case)
{
  total_data = as.numeric()
  
  for(i in 1:length(data1)){
    total_data = c(total_data,d1=data1[i])
  }
  for(i in 1:length(data2)){
    total_data = c(total_data,d2=data2[i])
  }
  
  total_data = UserSortKruskal(total_data)
  total_data_rank= c(1)
  
  for(i in 2:length(total_data)){
    if( total_data[i] == total_data[i-1] )
    {
      avg = (i+i-1)/2
      total_data_rank[i-1]=avg
      total_data_rank[i]=avg
    }
    else
    {
      total_data_rank[i]=i
    }
  }
  W1 <-0
  W2 <-0
  
  for(i in 1:length(total_data_rank))
  {
    if(names(total_data[i]) == "d1")
    {
      W1 <- W1 + total_data_rank[i]
    }
    else if(names(total_data[i]) == "d2")
    {
      W2 <- W2 + total_data_rank[i]
    }
  }
  n1 = length(data1)
  n2 = length(data2)
  U1 = W1 - (n1*(n1+1))/2
  U2 = W2 - (n2*(n2+1))/2
  U_cal = min(U1,U2)
  if(case == 1)
  {
    U = qwilcox(alpha/2,n1,n2)-1
    if(U_cal <= U)
    {
      cat("\n Reject Null Hypothesis (Ho) ")
      return()
    }
    else
    {
      cat("\n Accept Null Hypothesis (Ho) ")
      return()
    }
  }
  else if(case == 2)
  {
    U = qwilcox(alpha,n1,n2)-1
    if(U2 <= U)
    {
      cat("\n Reject Null Hypothesis (Ho) ")
      return()
    }
    else
    {
      cat("\n Accept Null Hypothesis (Ho) ")
      return()
    }  
  }
  else if( case == 3)
  {
    U = qwilcox(alpha,n1,n2)-1
    if(U1 <= U)
    {
      cat("Reject Null Hypothesis (Ho) ")
      return()
    }
    else
    {
      cat("Accept Null Hypothesis (Ho) ")
      return()
    }
  }
} 
# Kruskal Test
UserKruskalWTest <- function(data1,data2,data3=0,alpha)
{
  
  total_data = as.numeric()
  
  for(i in 1:length(data1)){
    total_data = c(total_data,d1=data1[i])
  }
  for(i in 1:length(data2)){
    total_data = c(total_data,d2=data2[i])
  }
  for(i in 1:length(data3)){
    total_data = c(total_data,d3=data3[i])
  }
  
  total_data = UserSortKruskal(total_data)
  total_data_rank= c(1)
  
  for(i in 2:length(total_data)){
    if( total_data[i] == total_data[i-1] )
    {
      avg = (i+i-1)/2
      total_data_rank[i-1]=avg
      total_data_rank[i]=avg
    }
    else
    {
      total_data_rank[i]=i
    }
  }
  R1 <-0
  R2 <-0
  R3 <-0
  
  for(i in 1:length(total_data_rank))
  {
    if(names(total_data[i]) == "d1")
    {
      R1 <- R1 + total_data_rank[i]
    }
    else if(names(total_data[i]) == "d2")
    {
      R2 <- R2 + total_data_rank[i]
    }
    else if(names(total_data[i]) == "d3")
    {
      R3 <- R3 + total_data_rank[i]
    }
    
  }
  R1 <- (R1*R1)/length(data1)
  R2 <- (R2*R2)/length(data2)
  R3 <- (R3*R3)/length(data3)
  n = length(total_data)
  H_calculated = (12/(n*(n+1)))*(R1+R2+R3) - 3*(n+1)
  H_observed = qchisq(1-alpha,n-1)
  if(H_calculated > H_observed)
  {
    cat("Reject Null Hypothesis (Ho) ")
    return()
  }
  else
  {
    cat("Accept Null Hypothesis (Ho) ")
    return()
  }
}

#################################################################################################
# module 1
DescAnalysis <- function()
{
  set <- c()
  print("Enter data and and press q to exit")
  
  # to take input from user till he wants to give(integer only)
  repeat 
  {
    element <- readline(prompt = "enter a number : ")
    if(element == "q")
      break
    is.letter <- function(x) grepl("[[:alpha:]]", x)        # to check given input is not other than character 
    
    if(element != "" && is.letter(element) == FALSE)
    {
      element <- as.integer(element)
      set <- c(set,element)
    }
  }
  cat("\n \n The final data set on which operations are being performed is : ",set,"\n")
  set<- sort(set)
  
  # to calculate mean
  sumset <- 0
  index <- 1
  repeat
  {
    if(index > length(set))
      break
    sumset<- sumset+set[index]
    index<-index+1
  }
  mean <- sumset/length(set)
  cat("\n mean is",mean,"\n")
  
  # to calculate median of given data
  median <- set[(length(set)/2)+1]
  cat(" median : ",median,"\n")
  
  # to calculate mode of given data
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  cat("\n mode : ",getmode(set),"\n")
  
  # to calculate variance
  varsum <- 0
  vari <- 1
  repeat
  {
    varsum <- varsum + (set[vari] - mean)^2
    vari <- vari+1
    if(vari > length(set))
      break
  }
  variance <- varsum/length(set)
  cat("variance : ",variance)
  
  # to calculate Standard Deviation
  SDeviation <- sqrt(variance)
  cat("\n standard deviation :",SDeviation,"\n")
  
  # to calculate absoute mean deviation
  absum <- 0
  abindex <-1
  repeat
  {
    if(abindex > length(set))
      break
    val <-abs(mean - set[abindex])
    absum <- absum+val
    abindex <- abindex+1
  }
  abdeviation <- absum/length(set)
  cat("\nAbsolute mean deviation : ",abdeviation,"\n")
  
  # to calculate range
  minimum <- set[1]
  maximum <- set[length(set)]
  range <- maximum - minimum
  
  cat("maximum of set : ",maximum,"\n")
  cat("minimum of set : ",minimum,"\n")
  cat("range of set :",range,"\n")
  
  # to calculate quartile
  q1 <- (set[length(set)])/4
  q2 <- (set[length(set)])/2
  q3 <- (3 * set[length(set)])/4
  cat("1st quartile :",q1,"\n")
  cat("2nd quartile :",q2,"\n")
  cat("3rd quartile :",q3,"\n")
  qdeviation <- (q3-q1)/2
  cat("quartile deviation",qdeviation,"\n")
  
  # to calculate skewness and krutosis
  skewness <- (mean - median)/SDeviation
  
}
# module 2
PredAnalysis <- function()
{
  cat("\n This module provides :\n 1). Corelation\n 2). Multiple Regression")
  selfun <- readline(prompt = " Enter your coice : ")
  if(selfun == 1)
    correlation()
  if(selfun == 2)
    Multi_Linear_Regression()
}
# module 3
ProbAnalysis <- function()
{
  cat("\n This module has following functions :\n 1). Factorial\n 2). Permutation\n 3). Combinations\n 4). Basic probabilty\n 5). Conditional probability\n 6). Bayes Theorm\n")
  selfun <- readline(prompt = "Enter your choice")
  if(selfun == 1)
    fact(,0)
  if(selfun == 2)
    permutation(,,0)
  if(selfun == 3)
    combination(,,0)
  if(selfun == 4)
    basicprob(,,0)
  if(selfun == 5)
    conditionalProb(,,,0)
  if(selfun == 6)
    bayes()
  else
    cat("\n wrong input.. calculator is returning to main menu")
}
# module 4
DescDistribution <- function()
{
  cat("\n This modules have follwing distribution : \n 1). Uniform \n 2). Bernouli \n 3). Binomial\n 4). Geometric\n 5). HyperGeometric\n 6.) NegativeBinomial\n 7).Poission\n 8). miltinomial\n 9).multivariate")
  selfun <- readline(prompt = " Enter your choice : ")
  if(selfun == 1)
    UniformDis()
  if(selfun == 2)
  BernoulliDis()
  if(selfun == 3)
  BinomialDis(,,,0)
  if(selfun == 4)
  GeometricDis()
  if(selfun == 5)
  HgeoDis()
  if(selfun == 6)
  NegBinomial()
  if(selfun == 7)
  Poisson()
  if(selfun == 8)
  Multinomial()
  if(selfun == 9)
  Multivariate()
  else
  {
    cat("\n wrong input provided.. calculator is returning to main menu ")
    Sys.sleep(2)
    return()
  }
}
# module 5
ContDisribution <- function()
{
  cat("\n This module have continous distribution : \n 1). Uniform continous Deviation\n 2).Normal Distribution\n 3). Gammma Distribution")
  selfun <- readline(prompt = " Enter your choice : ")
  if(selfun == 1)
    UniformCDistribution()
  if(selfun == 2)
    NormalDistribution()
  if(selfun == 3)
    GammaDistribution()
}
# module 6
SampleTest <- function()
{
  cat("\n Which test do you want to perform ? \n 1. chi square \n 2. student t-test \n 3. F-test \n 4. Z-test \n 5. Shapiro-wilk test")
  selfun <- readline(prompt = " Enter your choice : ")
  if(selfun == 1)
    chisq()
  if(selfun == 2)
    ttest()
  if(selfun == 3)
    ftest()
  if(selfun == 4)
    ztest()
  else
    cat("\n Wrong input .. calculator returning to main menu")
}
# module 7
IntEstimate <- function()
{
  cat("\n select your choice : \n 1. Estimation of means \n 2. Estimation Diffrences in mean \n 3. Estimation of propotions \n 4. Estimation Diffrence in propotion \n 5. Estimation of variance \n 6. Estimation ratio of variance")
  choice <- readline(prompt = "Enter your choice : ")
  cinterval <- readline(prompt = "Enter confidence interval(in percentage) : ")
  alpha <- (100 - cinterval)/200
  if(choice == 1)
    eom(alpha)
  if(choice == 2)
    eodm(alpha)
  if(choice == 3)
    eop(alpha)
  if(choice == 4)
    eodp(alpha)
  if(choice == 5)
    eov(alpha)
  if(choice == 6)
    eodv(alpha)
  else
    cat("\n wrong input ")
} 
# module 8
NonAnalysis <- function()
{
  cat("\n This module has folowwing Tests :\n 1). Sign Test\n 2). Wilcoxon Test\n 3). Mann Test\n 4). Kruskal Test")
  selfun <- as.integer(readline(prompt = " Enter your choice : "))
  if(selfun == 1)
  {
    hypothesis <- as.double(readline(prompt = " Enter the value for null hypothesis : "))
    alp <- as.double(readline(prompt = " Enter the alpha value : "))
    data <- as.double(strsplit(readline(prompt = " Enter the data for testing : ")," ")[[1]])
    UserSignT(data,hypothesis,alp)
  }
  if(selfun == 2)
  {
    hypothesis <- as.double(readline(prompt = " Enter the value for null hypothesis : "))
    alp <- as.double(readline(prompt = " Enter the alpha value : "))
    dat <- as.double(strsplit(readline(prompt = " Enter the data for testing : ")," ")[[1]])
    cat("\n tailed test \n 1). Two tailed\n 2). one tailed(left side)\n 3). one tailed(right side)")
    case <- as.integer(readline(prompt = " Enter your choice : "))
    UserWilcoxonTest(dat,hypothesis,alp,case)
  }
  if(selfun == 3)
  {
    dat1 <- as.double(strsplit(readline(prompt = " Enter the data1 for testing : ")," ")[[1]])
    dat2 <- as.double(strsplit(readline(prompt = " Enter the data2 for testing : ")," ")[[1]])
    alp <- as.double(readline(prompt = " Enter the alpha value : "))
    cat("\n tailed test \n 1). Two tailed\n 2). one tailed(left side)\n 3). one tailed(right side)")
    case <- as.integer(readline(prompt = " Enter your choice : "))
    UserMannWTest(dat1,dat2,alp,case)
  }
  if(selfun == 4)
  {
    dat1 <- as.double(strsplit(readline(prompt = " Enter the data1 for testing : ")," ")[[1]])
    dat2 <- as.double(strsplit(readline(prompt = " Enter the data2 for testing : ")," ")[[1]])
    alp <- as.double(readline(prompt = " Enter the alpha value : "))
    UserKruskalWTest(dat1,dat2,,alp)
  }
  
}
#module 9
visualize <- function()
{
  cat("\n In this module, inbuilt datasets are used to show the various visualizations")
  cat("\n Each graphs will disappears in 5 seconds")
  cat("\n 1). Histogram (dataset used is volcano")
  hist(volcano,density = 20,col = rainbow(10),border = "black",angle = 90,labels = TRUE,ylim = c(0,1500))
  Sys.sleep(5)
  
  cat("\n 2). Line Graph (dataset used is JohnsonJohnson")
  graph <- as.matrix(JohnsonJohnson)
  plot(graph,xlim = c(30,100))
  Sys.sleep(1)
  lines(graph)
  Sys.sleep(5)
  
  cat("\n 3). bar Graph (dataset used is Cars")
  barplot(cars$speed,cars$dist,col = rainbow(7),density = 80,xlab = "distance",ylab = "speed",border = "black")
  Sys.sleep(5)
  
  cat("\n 4). Pie chart (dataset used is trees) ")
  pie(trees$Height,col = rainbow(6),density = 100,radius = 1,clockwise = TRUE,border = "black")
  Sys.sleep(5)
  
  cat("\n 5). scatter plot (dataset used is swiss) ")
  education <- swiss$Education
  examination <- swiss$Examination
  plot(education,examination,xlim = c(1,40))
  Sys.sleep(5)
  
  cat("\n 6). Box plot (dataset used is swiss)")
  plot(education,examination,xlim = c(1,40))
  box(lty = "solid",col = "green")
  
  cat("\n 7). Qunatile Quantile plot (dataset used is faithful)")
  qqplot(faithful$eruptions,faithful$waiting,xlim=c(1.5,3.0),xlab = "Eruptions",ylab = "Waiting")
  Sys.sleep(5)  
  
  cat("\n 8). stem-leaf plot (dataset used is iris3) ")
  stem(iris3)
  Sys.sleep(5)
}
  
calculator <- function()
{
  cat("\n Menu of Calculator \n 1. Descriptive Analysis \n 2. Predictive Analysis \n 3. Probability Analysis \n 4. Descrete Distribution \n 5. Continous Distribution \n 6. Sample Test \n 7. Interval Estimation \n 8. Non-Parametric Analysis \n 9. Visualization")
  calchoice <<- readline(prompt = "Enter Your Choice : ")
  if(calchoice == 1)
  {
    DescAnalysis()
    again()
    return()
  }
  if(calchoice == 2)
  {
    PredAnalysis() 
    again()
    return()
  }
  if(calchoice == 3)
  {
    ProbAnalysis()
    again()
    return()
  }
  if(calchoice == 4)
  {
    DescDistribution()
    again()
    return()
  }
  if(calchoice == 5)
  {
    ContDisribution()
    again()
    return()
  }
  if(calchoice == 6)
  {  
    SampleTest()
    again()
    return()
  }
  if(calchoice == 7)
  {  
    IntEstimate()
    again()
    return()
  }
  if(calchoice == 8)
  {  
    NonAnalysis()
    again()
    return()
  }
  if(calchoice == 9)
  {  
    visualize()
    again()
    return()
  }
  else
  {
    print("wrong input.. exiting")
    return(0)
  }
}
again<- function()
{
  inp <- readline(prompt = "\n Do you want to run calculator again (y/n)")
  if(inp == "y")
    calculator()
  return()
}
calculator()
