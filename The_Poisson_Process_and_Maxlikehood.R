##Callum Simpson
##b6030326

## ----- Set up ----

library("tidyverse")
library("ggplot2")

url = "http://www.mas.ncl.ac.uk/~nseg4/teaching/MAS8380/practical2.dat"
arr.times = scan(url)

arrDF <- data.frame(scan(url))

##Create a colums of ID to the data
arrDF <- tibble::rowid_to_column(arrDF, "ID")



## ---- q1  -----
##This is the code for the first graph - Plot the arival time against the postion in sequence

  ggplot(data = arrDF, aes(x = ID, y = scan.url.)) + geom_point() + ggtitle("Arrival time against the patient postion in the sequence") +
    xlab("Patients place in the squence") + ylab("Arrival time starting from 9am") +
    theme(plot.title = element_text(hjust = 0.5)) 
    
  
  
## ---- q2  -----

##Vector of number of intervals sizes that will be used 
k <- c(10,25, 50, 75, 100, 150)


##create a data frame that will store the mean and variance for each interval
q2_df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("IntervalSize", "Mean", "Var")
colnames(q2_df) <- x

q2_df$IntervalSize <- as.character(q2$IntervalSize)

##loop through the vector of interval sizes
for(i in k){

##cut the table into i equal intervals (selected number of interval)

  x = table(cut(arr.times, breaks=i))
  
##work out the mean and variance of the interval counts add them to the vector
  q2_df[nrow(q2_df) + 1,] = c(i,mean(x),var(x))
  
  
  ##Print out rate
  print((mean(x) * i) / (12))
  
}

  ##Produce the graph for question 2
  ggplot(q2_df, aes(x=Mean, y=Var, color=IntervalSize)) + 
    geom_point(size=6) + 
    scale_color_discrete(breaks=c("10","25","50","75","100","150"), name = "Number of intervals") + ggtitle("Mean and Variance for diffrent number of intervals") +
    xlab("Mean") + ylab("Variance") +  
    theme(plot.title = element_text(hjust = 0.5)) 
  
  
  
  ##----- Question 3 --------
  
  ##Set lamda 
  lamda = 10
  
  ##Simulate a series of random quantities and cumulative sum all the values to 
  Csum <- cumsum(rexp(500 , lamda))
  
  ##turn that cumulative sum series into a data table to add in an id colum and remove all values that exceed 12
  arrCsum <- data.frame((Csum))
  arrCsum <- tibble::rowid_to_column(arrCsum, "ID")  %>% filter(X.Csum. <= 12)
  
  ##arrCsum
  
  ##A graph of results, a line has been added to represent roughly what 10 arrivals per hour should look like 
  ggplot(data = arrCsum, aes(x = ID, y = X.Csum.)) + geom_point()+ 
    geom_abline(intercept = 0, slope = 0.1, color="red", linetype="dashed", size=0.5) + ggtitle("Poisson process with rate 10 arrivals per hour") +
    xlab("Patients place in the squence") + ylab("Arrival time starting from 9am") +  
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_label(label="Red line repersents a 0.1 gradient", 
               x=30,
               y=7.5,
               label.padding = unit(0.55, "lines"), # Rectangle size around label
               label.size = 0.35,
               color = "black",
               fill =  "red"
    )
  
  
  ## ---- Question 4 ----
  
  ##create a uniform distribution 
  Uniform <- cumsum(runif(400 ,0, 0.1))
  ##set the  a uniform distribution as a data frame to add an id column and also trim off any values that are greater than 12
  df_CS_Uniform <- data.frame((Uniform))
  df_CS_Uniform <- tibble::rowid_to_column(df_CS_Uniform, "ID") %>% filter(X.Uniform. <= 12)
  df_CS_Uniform
  
  ##plot the graph made from uniform
  ##ggplot(data = df_CS_Uniform, aes(x = ID, y = X.Uniform.)) + geom_point()
  
  
  ##create a df to hold the results to complete the rest of the question
  df_q4 <- data.frame(matrix(ncol = 3, nrow = 0))
  x <- c("k", "Mean", "Var")
  colnames(df_q4) <- x
  df_q4$k <- as.character(df_q4$k)
  
  ##set up incremnt values
  k <- c(10,25, 50, 75, 100, 150)
  
  print("----------------")
  
  ##Loop through the increments
  for(i in k){
    
    ##cut the table into i equal intervals
    x = table(cut(df_CS_Uniform$X.Uniform, breaks=i))
    
    ##store mean and var in data frame
    df_q4[nrow(df_q4) + 1,] = c(i,mean(x),var(x))
    
    ##print((mean(x) * i )/ 12 )
    
  }
  
  ##df_q4
  
  ## The graph for question for that is similar to the one created for q2
  ggplot(df_q4, aes(x=Mean, y=Var, color=k)) + 
    geom_point(size=6)  + 
    scale_color_discrete(breaks=c("10","25","50","75","100","150"), name = "Number of intervals") + ggtitle("Mean and Variance for diffrent number of intervals of a uniform distribution ") +
    xlab("Mean") + ylab("Variance") +  
    theme(plot.title = element_text(hjust = 0.5)) 
  
  
## ---- Question 5 ----
  
  ##the log like function, it takes in a sample and a theta 
  loglike <- function(sample,theta){
    
    ##sum up all the pois values gotten and return the results
    llh <- sum(dpois(sample,theta,log=TRUE))
    return(llh)
    
  }
  
## ---- Question 6 -----  

  ##A sample of lambdas (from 1 to 20 in 0.1 increments)
  lambdas <- seq(1, 20 , 0.1)
  
  ##the sample needed
  sample <- c(8, 14, 8, 11, 6, 13, 9, 9, 10)
  
  ##calulate the log like using the lambas and the sample provided
  sample_ll <- sapply(lambdas,function(x){loglike(sample,x)})
  
  # save the lambdas and log-likelihoods in a data frame
  df_q6 <- data.frame(Log_Likelihood=sample_ll, Theta=lambdas)
  
  
  ##Plot the graph
  ggplot(data = df_q6, aes(x = Theta, y = Log_Likelihood)) + geom_point() +
    geom_vline(xintercept = lambdas[which.max(sample_ll)], color="red",size=1) +
    ggtitle("Log-likehood for the sample against a range of theta's") +
    xlab("Theta (1 to 20 in 0.1 increments)") + ylab("Log Likeihood") +  
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_label(label="Max Theta is 9.8", x=12, y=-100, label.padding = unit(0.55, "lines"), label.size = 0.35, color = "black")
  
  ##print off the maximun
  print(df_q6[which.max(df_q6$Log_Likelihood),])
  
  
## ---- Question 7 ----
  
  ##Similar to the one for question 6 instead it takes in an input of sample
  loglike2 <- function(sample){
    
    ##create a function that will be optimized, uses sample given
    Poisson.LL <- function(x) sum((dpois(sample, lambda=x, log=TRUE)))
    
    ##optimize to find the value of X that returns the highest value using values from 1 to 20
    max <- optimize(Poisson.LL, c(1,20), maximum=TRUE)
    
    return(max)
    
  }
  
  ##using the q6 sample, use the new function to find the optimal x value.
  sample <- c(8, 14, 8, 11, 6, 13, 9, 9, 10)
  loglike2(sample)
  
  
## ---- Question 8 ----
  
  ##A function that takes an input of sample size n and finds the maximun likehood of the n.
  generatePoss <- function(sampleSize){
    
    ##empty vector to hold all of the max likehoods
    vec_of_maxs <- numeric(0)
    
    ##for 1000 runs
    for ( i in 1:1000){
      ## generate an possions sample of size sampleSize using the lamda 10
      data <- rpois(n=sampleSize, lambda=10)
      ##get maximum likely hood
      vec_of_maxs[i] <- (loglike2(data))$maximum
    }
    
    ##work out the mean and var of the diffrent likely hoods
    retunlist <- list(mean(vec_of_maxs),var(vec_of_maxs))
    
    return(retunlist)
    ##print(retunlist)
    
  }
  
  ## ---- Question 9 ----
  
  ##Question 9
  
  ##vector of different sample sizes 
  question_9_value <- c(5, 10, 25, 50, 100, 1000)
  
  ##create a df to store the value
  q9_df <- data.frame(matrix(ncol = 3, nrow = 0))
  x <- c("k", "Mean", "Var")
  colnames(q9_df) <- x
  
  q9_df$k <- as.character(q9_df$k)
  
  
  ##loop through the vector of diffrent sample sizes and generate the list of maximum values.
  for(i in question_9_value){
    
    ##generate the possion for the interval
    q9_df[nrow(q9_df) + 1,] = c(i,generatePoss(i))
    
  }
  
  q9_df
  
  ##create the q9 graph
  ggplot(q9_df, aes(x=Mean, y=Var, color=k)) + geom_point(size=6) + 
    scale_color_discrete(breaks=c("5","10","25","50","100","1000"), name = "Sample Size") +
    ggtitle("Mean and variances for diffrent samples of size n from a P o(θ = 10) distribution") +
    xlab("Mean") + ylab("Variance") +  
    theme(plot.title = element_text(hjust = 0.5)) 
  
  
  