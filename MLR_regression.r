### (1) Reading & preparing the dataset

# csv data :
#df <- read_csv("C:/Users/Admin/Desktop/Applications.xlsx")

# xls data :
#df<- read_excel("file.xls")

# xlsx data:
library(readxl)
data <- read_excel("D:/FCDS/Spring2023/Regression/Applications.xlsx")
### Data Cleaning 

#to remove duplicated data
df <- unique(data)
#to remove null values
df <- na.omit(data)

#to show if exist outliers
boxplot(df)
title("Applications")

# define a function to remove outliers based on z-score
remove_outliers <- function(df, threshold = 3) {
  # calculate z-scores for each variable in the data frame
  z_scores <- apply(df, 2, function(x) abs(scale(x)))
  # identify rows with any z-score greater than the threshold
  outlier_rows <- apply(z_scores, 1, function(x) any(x > threshold))
  # return the data frame with outlier rows removed
  df[!outlier_rows, ]
}

# remove outliers from the mtcars data frame based on z-score
df <- remove_outliers(data)

# view the cleaned data frame
df

#input data from user

readUserData <- function() {
  col_num_x=as.integer(readline(prompt="number of features is: ")) 
  n=as.integer(readline(prompt="number values in any records: "))  
  df <- data.frame()
  for (i in 1:col_num_x) {
    xarr=c()
    for(j in 1:n){
      my.x <- as.integer(readline(prompt=paste("X",i," values:",sep="")))
      xarr<-append(xarr,my.x)
    }
    xname <-paste("X",i,sep="")
    if(ncol(df) == 0){
      df= data.frame( X1= xarr)
    }else{
      df[xname] <- xarr
    }
  }
  yarr=c()
  for(i in 1:n){
    my.y <- as.integer(readline(prompt="y values: "))
    yarr<-append(yarr,my.y)
  }
  df["Y"] <- yarr
  return(df)
}
#df = readUserData()


# choose which col is the dependent variable / leave empty to pick the 
# last col as the dependent variable (the default)
data_picking<-function(colnum_ToBe_Dependent){
  # Extract predictor (X) and response (Y) variables 
  if(missing(colnum_ToBe_Dependent)) {
    # below ncol return the number of columns
    y <<- as.matrix(df[ncol(df)])
    # df[-ncol(df)] makes a data frame of all data except last column 
    # then as.matrix cast it to a matrix  
    x <- as.matrix(df[-ncol(df)])
    # column full of one's to represent the B0 (intercept,the constant)
    onesarray <- rep(1, length(y))
    # Add the B0 (intercept) column to X
    x <<- cbind(onesarray, x)
  } else {
    y <<- as.matrix(df[colnum_ToBe_Dependent])
    x <- as.matrix(df[-colnum_ToBe_Dependent])
    onesarray <- rep(1, length(y))
    x <<- cbind(onesarray, x)
  }
  
}
data_picking()
y
x


#for a matrix n = nrow of x
n = nrow(x)
### (2) calculations

# calculate beta coefficients using matrix algebra
B <- solve(t(x) %*% x) %*% t(x) %*% y
#to calculate mean of y
y_mean <- mean(y)
y_hat <- x %*% B
#sum of square error
SSE <- (t(y) %*% y) - (t(B) %*% t(x) %*% y)
#sum of square total
SST <- (t(y) %*% y) - (n*(y_mean)^2)
#sum of square regression
SSR = SST - SSE


p=ncol(x)  # p= number of restriction
k = p-1 #number of independent variable.
k
#degree of freedom regression = d.o.f_reg
d.o.f_reg=k
d.o.f_reg
#degree of freedom error = d.o.f_error
d.o.f_error = n-p
#degree of freedom total = d.o.f_total
d.o.f_total = n - 1 # d.o.f_error+ d.o.f_reg

# Construct the ANOVA table and perform an F test
# Mean square Regression
MSR<-SSR/d.o.f_reg

# Mean square error
MSE<-SSE/d.o.f_error

#Calculate F test
F_stat <- MSR/MSE

#calculate the Pr(>F)
p_value <- pf(F_stat, d.o.f_reg, d.o.f_error,lower.tail = FALSE)
p_value
anova_table <- data.frame(
  Source = c(" Regression", "Residuals", "Total"),
  SumSq = c(SSR, SSE, SST),
  DF = c(d.o.f_reg, d.o.f_error, d.o.f_total),
  MeanSq = c(MSR, MSE, stringsAsFactors = " "),
  F = c(F_stat, stringsAsFactors = " ",stringsAsFactors = " "),
  P = c(p_value, stringsAsFactors = " ",stringsAsFactors = " ")
)
print(anova_table)

#the null hypothesis H0:B1=B2=....=Bk=0 and the alternative hypothesis Ha: at least B1???0
#the significant level
alpha <-0.05
if (p_value <= alpha){
  print("reject H0 ,accept Ha ")
}else if (p-value > alpha){
  print("fail to reject H0 ")
}

# calculate the variance-covariance matrix (MSE * solve(t(x) %*% x))

# Define a scalar and a matrix
MSE
c<- solve(t(x) %*% x)

# Create an empty matrix to store the results
vcov_mat <- MSE[1] * c

# Print the result
vcov_mat


### (4) Confidence intervals

# Confidence Interval when the standard deviation is UNKNOWN

# calculate t critical for specific significant level
t.critical <- function(sig_level) {
  return(qt(p=sig_level/2, df=d.o.f_error, lower.tail=FALSE))
}


#for Beta
CI_beta <- function(i, sig_level){
  
  second_term = t.critical(sig_level = sig_level) * sqrt(MSE*c[i+1,i+1])
  
  L.CI_beta_i = B[i+1,] - second_term
  U.CI_beta_i = B[i+1,] + second_term
  
  return(c(L.CI_beta_i , U.CI_beta_i))
}

for (i in 0:k){
  cat("CI of B" , i , " is: " , CI_beta(i, 0.05), "\n")
}


# calculate y hat at x = x0

est.y0 <- function(i){ 
  return(t(x[i,]) %*% B) 
}

 
# for (i in 1:20) {
#  cat("Estimated y for x" , i , " is: " , est.y0(i), "\n")
# }



# for mean response at x = x0 

CI_mean_response <- function( i ,sig_level ) {
  
  x0 = x[i,]
  
  est.mean_reponse = est.y0(i)
  
  SE_mean_response = sqrt(MSE * (t(x0)  %*% c  %*% x0))
  
  L.CI_mean_response = est.mean_reponse - t.critical(sig_level) * SE_mean_response
  U.CI_mean_responce = est.mean_reponse + t.critical(sig_level) * SE_mean_response
  
  CI = data.frame(L.CI_mean_response, U.CI_mean_responce)
  colnames(CI) = c('lower' , 'upper')
  return (CI)
}

CI_mean_response(1,0.05)

# for new observation at x = x0

CI_new_obs <- function(row_index , sig_level) {
  
  x0 = x[row_index,]
  est.new_obs = est.y0(row_index)
  
  SE_new_obs = sqrt(MSE * (1 + (t(x0)  %*% c  %*% x0) ))
  
  L.CI_new_obs = est.new_obs - t.critical(sig_level) * SE_new_obs
  U.CI_new_obs = est.new_obs + t.critical(sig_level) * SE_new_obs
  
  CI = data.frame(lower = L.CI_new_obs, Upper = U.CI_new_obs)
  colnames(CI) = c('lower' , 'upper')
  return (CI)
}

CI_new_obs(1,0.05)


# calculate studentized residuals / ri

H <- x %*% solve(t(x) %*% x) %*% t(x)
ei=y-y_hat

hi <- diag(H)

SE <- sqrt(MSE * (1 - hi))

student_resid <- ei / (SE)
plot(student_resid, pch = 20,cex = 1.5, main = "Studentized Residual Plot",
     xlab = "Observation Index", ylab = "Studentized Residual")
# Add a line at y = 0
abline(h = 0, lty = 2)
# Add a line at the  value of 3 threshold /(doctor said that)
abline(h = 3, lty = 2, col = "red")
# Add a line at the threshold value of -3
abline(h = -3, lty = 2, col = "red")

### Printing the results
cat("Beta values are : " , "\n")
B

cat("ANOVA Table is : " , "\n")
anova_table

cat("Variance-Covariance matrix is : " , "\n")
vcov_mat

for (i in 0:k){
  cat("CI of B" , i , " is: " , CI_beta(i, 0.05), "\n")
}

cat("CI of mean response is : " , "\n")
CI_mean_response(1, 0.05)

cat("CI of a new observation is : " , "\n")
CI_new_obs(1,0.05)
