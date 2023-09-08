library(readxl)
library(ggplot2)
library(readr)
library(ggpubr)

### (1) Reading & preparing the dataset

# csv data :
#df <- read_csv("file.csv")

# xls data :
#df<- read_excel("file.xls")

# xlsx data:
df<- read_excel("D:/FCDS/Spring2023/Regression/Applications.xlsx")

# input data from user:
# readUserData <- function() {
#   df <- data.frame(x = c(), y = c())
#   while(TRUE) {
#     my.x <- as.integer(readline(prompt="x values: "))
#     # stop reading if no x value was typed in
#     if (is.na(my.x))
#       break
#     my.y <- as.integer(readline(prompt="y values: "))
#     # stop reading if no y value was typed in
#     if (is.na(my.y))
#       break
#     # add the read data to the bottom of the dataframe
#     df <- rbind(df, data.frame(x = c(my.x), y = c(my.y)))
#   }
#   df
# }
# df<- readUserData()
#df


# Extract predictor (X) and response (Y) variables
x <- df$`Length of Membership`
y <- df$`Yearly Amount Spent`


### (2) calculations

# for a matrix n = nrow of x
n <- length(x)

# mean of x
x_mean <- mean(x)
sxx <- sum(x^2) - (n*(x_mean)^2)

# mean of y
y_mean <- mean(y)
syy <- sum(y^2) - (n*(y_mean)^2)

sxy <- sum(x * y) - (n*x_mean*y_mean)

# The least squares estimator for beta_1
est.beta_1 <- sxy / sxx

# The least squares estimator for beta_0
est.beta_0 <- y_mean - (est.beta_1 * x_mean)

# sum of square regression
SSR <- (est.beta_1^2) * sxx

# sum of square total = syy
SST <- syy 
# sum of square error
SSE <- SST - SSR


### (3) Create Anova table

# ncol() function used to calculate the number of columns
K= 1 # number of independent variable.
P= K+1

# degree of freedom regression = d.o.f_reg
d.o.f_reg= K 

# degree of freedom error = d.o.f_error
d.o.f_error = n-P # p = number of restriction

# degree of freedom total = d.o.f_total
d.o.f_total = n - 1 # d.o.f_error+ d.o.f_reg

# Construct the ANOVA table and perform an F test

# Mean square Regression
MSR<-SSR/d.o.f_reg

# Mean square error
MSE<-SSE/d.o.f_error

# Calculate F test
F_stat <- MSR/MSE

# calculate the Pr(>F)
p_value <- pf(F_stat, d.o.f_reg, d.o.f_error,lower.tail = FALSE)


anova_table <- data.frame(
  Source = c(" Regression", "Residuals", "Total"),
  SumSq = c(SSR, SSE, SST),
  DF = c(d.o.f_reg, d.o.f_error, d.o.f_total),
  MeanSq = c(MSR, MSE, stringsAsFactors = " "),
  F = c(F_stat, stringsAsFactors = " ",stringsAsFactors = " "),
  P = c(p_value, stringsAsFactors = " ",stringsAsFactors = " ")
)

### (3.1) Hypothesis test using p-value

# the null hypothesis H0:B1=0 and the alternative hypothesis Ha:B1???0
# the significant level
sig_level <-0.05
if (p_value <= sig_level){
  print("reject H0 ,accept Ha then B1!=0")
}else if (p-value > sig_level){
  print("fail to reject H0 then B1=0")
}

### (4) Confidence intervals

# Confidence Interval when the standard deviation is UNKNOWN

# calculate t critical for specific significant level

t.critical <- function(sig_level) {
  
  return(qt(p=sig_level/2, df=d.o.f_error, lower.tail=FALSE))
  
}


# for beta0
CI_beta0 <- function(sig_level){
  
  SE_beta0 = sqrt(MSE* ((1/n) + (x_mean^2) / sxx))
  
  L.CI_beta0 = est.beta_0 - t.critical(sig_level) * SE_beta0
  U.CI_beta0 = est.beta_0 +  t.critical(sig_level)* SE_beta0
  
  return(c(L.CI_beta0 , U.CI_beta0))
  
}


# for beta1
CI_beta1 <- function(sig_level) {
  
  SE_beta1 = sqrt(MSE / sxx)
  
  L.CI_beta1 = est.beta_1 - t.critical(sig_level) * SE_beta1
  U.CI_beta1 = est.beta_1 + t.critical(sig_level) * SE_beta1
  
  return(c(L.CI_beta1 , U.CI_beta1))
  
}

# calculate y hat at x = x0

est.y0 <- function(x0){ return(est.beta_0 + est.beta_1 * x0) }

# for mean response at x = x0

CI_mean_responce <- function(x0,sig_level) {
  
  est.mean_reponse = est.y0(x0)
  
  SE_mean_response = sqrt(MSE * ((1/n) + ((x0 - x_mean)^2) / sxx))
  
  L.CI_mean_response = est.mean_reponse - t.critical(sig_level) * SE_mean_response
  U.CI_mean_responce = est.mean_reponse + t.critical(sig_level) * SE_mean_response
  
  return (data.frame(lower = L.CI_mean_response, Upper = U.CI_mean_responce))
}

# for new observation at x = x0

CI_new_obs <- function(x0 , sig_level) {
  
  est.new_obs = est.y0(x0)
  
  SE_new_obs = sqrt(MSE * (1 + (1/n) + ((x0 - x_mean)^2) / sxx))
  
  L.CI_new_obs = est.new_obs - t.critical(sig_level) * SE_new_obs
  U.CI_new_obs = est.new_obs + t.critical(sig_level) * SE_new_obs
  
  return(data.frame(lower = L.CI_new_obs, Upper = U.CI_new_obs))
  
}


### (5) scatter plot

# plotting regression line 
fittedLine_and_meanResponse_plot = ggplot(data =df ,aes(x=x,y=y))+
  geom_point(color = "black")+
  geom_smooth(method="lm",level=.99)+    # mean response interval plot
  stat_function(fun=function(x){est.beta_1*x+est.beta_0},color="orange",lwd=1)


# prediction interval plot

model<-lm(y~x,data=df)


pre<-predict(model,interval="prediction")
data_new<-cbind(df,pre)
#  View(data_new)  #the data + prediction interval for each value

newObs._plot = ggplot(data_new,aes(x=df$`Length of Membership`))+
  
  geom_point(aes(y=df$`Yearly Amount Spent`),
             col="grey")+
  
  geom_line(aes(y=fit),
            col="orange",lwd=1)+  
  
  geom_line(aes(y=upr), # upper line
            col="black",
            linetype="dashed")+
  
  geom_line(aes(y=lwr),  #lower line
            col="black",
            linetype="dashed")

ggarrange(fittedLine_and_meanResponse_plot, newObs._plot,
          labels = c("fitted line & mean response CI", "new observation CI"), 
          ncol = 1, nrow = 2)




### Printing the results

cat("Sxx is: " , sxx, "\n")
cat("Syy is: " , syy, "\n")
cat("Sxy is: " , sxy, "\n")

cat("Estimator of Beta0 is: " , est.beta_0, "\n")
cat("Estimator of Beta1 is: " , est.beta_1, "\n")
cat("predicted value: y","=",est.beta_0,"+",est.beta_1,"x")

print(anova_table)

cat("Confidence Interval of Beta0: ", CI_beta0(0.05), "\n")
cat("Confidence Interval of Beta1: ", CI_beta1(0.05), "\n")
cat("Confidence Interval of mean response: ", CI_mean_responce(5.5, 0.05)$lower[1],CI_mean_responce(5.5, 0.05)$Upper[1], "\n")
cat("Confidence Interval of new observation: ", CI_new_obs(5.5, 0.05)$lower[1],CI_new_obs(5.5, 0.05)$Upper[1], "\n")

