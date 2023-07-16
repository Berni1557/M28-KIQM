
######### Descriptive statistics
######### Descriptive statistics
######### Descriptive statistics

#1- Data types 

# Create an example of a data frame
df <- data.frame(
  id = c(1, 2, 3, 4, 5),
  age = c(21, 35, 28, 45, 29),
  CAC_score = c(350.5, 40.05, 800, 5000, 0),
  gender = c("M", "F", "M", "F", "F"),
  severity1= c("moderate", "mild", "moderate", "severe", "mild")
)
str(df)
# Get summary statistics for the data frame
summary(df)
hist(df)
sd(df$CAC_score)
mean(df$CAC_score)
median(df$CAC_score)
##changing the type of gender & Severity in new variables 
df$sex<-as.factor(df$gender)

df$sex<-as.factor(df$gender) 
df$Severity2<-as.factor(df$severity1) 
str(df)
summary(df)

## if in same variable 

df$sex<-as.numeric(df$sex) 
df$Severity2<-as.numeric(df$Severity2) 

str(df)
summary(df)


library(ggplot2)

# Set seed for reproducibility
set.seed(123)


# Generate normally distributed variable
normal_variable <- rnorm(1000, mean = 0, sd = 1)

# Plot the distribution
ggplot(data.frame(normal_variable), aes(x = normal_variable)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(x = "Normal Variable", y = "Frequency") +
  ggtitle("Normally Distributed Variable")


# Generate normally distributed variable
normal_variable <- rnorm(1000, mean = 0, sd = 4)

# Plot the distribution
ggplot(data.frame(normal_variable), aes(x = normal_variable)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(x = "Normal Variable", y = "Frequency") +
  ggtitle("Normally Distributed Variable")



# Generate normally distributed variable
normal_variable <- rnorm(1000, mean = 0, sd = 1)

# Plot the distribution
ggplot(data.frame(normal_variable), aes(x = normal_variable)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(x = "Normal Variable", y = "Frequency") +
  ggtitle("Normally Distributed Variable")+ coord_cartesian(xlim = c(-10, 10))



# Generate normally distributed variable
normal_variable <- rnorm(1000, mean = 0, sd = 0.1)

# Plot the distribution
ggplot(data.frame(normal_variable), aes(x = normal_variable)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(x = "Normal Variable", y = "Frequency") +
  ggtitle("Normally Distributed Variable")+ coord_cartesian(xlim = c(-10, 10))



# Generate right-skewed variable
skewed_variable <- rgamma(1000, shape = 2, rate = 0.5)

# Plot the distribution
ggplot(data.frame(skewed_variable), aes(x = skewed_variable)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black")



# Generate right-skewed variable
skewed_variable <- rgamma(1000, shape = 4, rate = 0.5)

# Plot the distribution
ggplot(data.frame(skewed_variable), aes(x = skewed_variable)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black")




# Generate right-skewed variable (gama)
skewed_variable <- rgamma(1000, shape = 8, rate = 0.5)

# Plot the distribution
ggplot(data.frame(skewed_variable), aes(x = skewed_variable)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black")


##CLT simulation
#####Pseudo code
N # Number of trials (population size)
n # Number of simulations
standardized_sample_mean = rep(0,n)
EX #Expectation
VarX #Variance
for (i in 1:n){
  samp #Sample from any distribution
  sample_mean <- mean(samp) # Sample mean
  standardized_sample_mean[i] <- sqrt(N)*(sample_mean - EX)/sqrt(VarX)
  #Standardized Sample Mean
}
hist(standardized_sample_mean,prob=TRUE) #Histogram
qqnorm(standardized_sample_mean) #QQPlot

###Uniform ###Uniform ###Uniform ###Uniform ###Uniform ###Uniform ###Uniform ###Uniform ###Uniform ###Uniform ###Uniform ###Uniform 

N <- 2000 # Number of trials (population size)
n <- 1000 # Number of simulations
standardized_sample_mean = rep(0,n)
EX <- 0.5
VarX <- 0.25
for (i in 1:n
){
  samp <- runif( N, 0, 1)
  sample_mean <- mean(samp) # sample mean
  standardized_sample_mean[i] <- sqrt(N)*(sample_mean - EX)/sqrt(VarX)
}
par(mfrow=c(1,2))
hist(standardized_sample_mean,prob=TRUE)
qqnorm(standardized_sample_mean)

##### Bernouli 0.5

N <- 2000 # Number of trials (population size)
n <- 1000 # Number of simulations
standardized_sample_mean = rep(0,n)
EX <- 0.5
VarX <- 0.25
for (i in 1:n){
  samp <- rbinom(1, size = N, prob = 0.05)
  sample_mean <- mean(samp) # sample mean
  standardized_sample_mean[i] <- sqrt(N)*(sample_mean - EX)/sqrt(VarX)
}
par(mfrow=c(1,2))
hist(standardized_sample_mean,prob=TRUE)
qqnorm(standardized_sample_mean)



####Poission(1)

N <- 2000 # Number of trials (population size)
n <- 1000 # Number of simulations
standardized_sample_mean = rep(0,n)
EX <- 1
VarX <- 1
for (i in 1:n){
  samp <- rpois(N,1)
  sample_mean <- mean(samp) # sample mean
  standardized_sample_mean[i] <- sqrt(N)*(sample_mean - EX)/sqrt(VarX)
}
par(mfrow=c(1,2))
hist(standardized_sample_mean,prob=TRUE)
qqnorm(standardized_sample_mean)


####Exponential population Rate 

N <- 2000 # Number of trials (population size)
n <- 1000 # Number of simulations
standardized_sample_rate = rep(0,n)
EX <- 1
VarX <- 0.5
for (i in 1:n){
  samp <- rexp(2000)
  sample_rate<- 1/mean(samp) # sample mean
  standardized_sample_rate[i] <- sqrt(N)*(sample_rate - EX)/sqrt(VarX)
}
par(mfrow=c(1,2))
hist(standardized_sample_rate,prob=TRUE)
qqnorm(standardized_sample_rate)
############################################################ Do Nromal distribution yourself?  #################

################### linear regression 


# Set seed for reproducibility
set.seed(123)

# Generate simulated data
x1 <- rnorm(100)  # Continuous independent variable
x2 <- sample(c(0, 1), 100, replace = TRUE)  # Binary independent variable
x3 <- factor(sample(c("A", "B", "C"), 100, replace = TRUE))  # Factor independent variable
y <- 2 * x1 + 3 * x2 + rnorm(100)  # Dependent variable
# Combine the independent variables into a data frame
data <- data.frame(x1, x2, x3)
# Fit the multiple linear regression model

model <- lm(y ~ x1 + x2 + x3, data = data)


# Fit the multiple linear regression model
model <- lm(y ~ x1 + x2 + x3, data = data)

# Set seed for reproducibility
set.seed(123)

# Generate simulated data
x1 <- rnorm(100)  # Continuous independent variable
x2 <- sample(c(0, 1), 100, replace = TRUE)  # Binary independent variable
x3 <- factor(sample(c("A", "B", "C", "D"), 100, replace = TRUE))  # Factor independent variable
y <- 2 * x1 + 3 * x2 + rnorm(100)  # Dependent variable
# Combine the independent variables into a data frame
data <- data.frame(x1, x2, x3)
# Fit the multiple linear regression model

model <- lm(y ~ x1 + x2 + x3, data = data)


summary(model)
plot(y ~ x1, data = data)

abline(model)

model1 <- lm(y ~ x1 , data = data)


summary(model1)
plot(y ~ x1, data = data)

abline(model1 )
0.5382 /0.3058
####homoscedasticity (constant variance of residuals) 
#by plotting the residuals against the dependent variable (y). 
# Obtain the residuals
residuals <- resid(model)

# Plot the residuals
plot(y, residuals, xlab = "y", ylab = "Residuals")
abline(h = 0, col = "red")
#The red horizontal line represents the expected mean of zero for the residuals.


###normality of residuals
# Plot the histogram of residuals
hist(residuals, breaks = 15, main = "Histogram of Residuals")

# Plot a Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals)

### multi coliniarity
# Calculate the variance inflation factor (VIF)
vif <- car::vif(model)

# Print the VIF values
print(vif)


# Set seed for reproducibility
set.seed(123)

# Generate correlated independent variables
n <- 100  # Number of observations
x1 <- rnorm(n)  # Independent variable 1
x2 <- x1 + rnorm(n, mean = 0, sd = 0.5)  # Independent variable 2 with correlation
cor(x1, x2)  # Calculate correlation coefficient

# Generate other independent variables
x3 <- rnorm(n)  #ndependent variable 


# Combine the independent variables into a data frame
data <- data.frame(x1, x2, x3, x4)

# Fit the multiple regression model
model <- lm(x3 ~ x1 + x2 , data = data)

# Calculate the variance inflation factor (VIF)
vif <- car::vif(model)

# Print the VIF values
print(vif)

######################################### function to calculate the p value 
calculate_p_value <- function(f_statistic, sample_size, num_parameters) {
  df1 <- num_parameters
  df2 <- sample_size - num_parameters - 1
  
  p_value <- 1 - pf(f_statistic, df1, df2)
  
  return(p_value)
}


################################ logistic regression 

dataAI<- read.csv("C:/Users/mohammma/Downloads/dataAI.csv")


# Perform logistic regression
model1 <- glm(CT_ITD  ~  heart_rate, data = dataAI, family = binomial)

# View the model summary
summary(model1)


anova(model1, test = "Chisq")


# Perform logistic regression
model2 <- glm(CT_ITD  ~  heart_rate+ age+gender, data = dataAI, family = binomial)

# View the model summary
summary(model2)


anova(model2, test = "Chisq")
anova(model1, model2, test = "Chisq")

# Perform logistic regression
model3 <- glm(CT_ITD  ~  heart_rate+ age+gender, data = dataAI, family = binomial)

# View the model summary
summary(model3)


anova(model3, test = "Chisq")
anova(model1, model3, test = "Chisq")


# Examine coefficient interpretation
coefficients <- coef(model3)
exp_coefficients <- exp(coefficients)
odds<-exp_coefficients
odds

#######Disrimination 
predictions<- predict(model3, type="response")
library(reportROC)
reportROC(dataAI$CT_ITD, predictions)


################### calibaration 
library(rms)
lrm_model <- lrm(CT_ITD ~ heart_rate+ age+gender, data = dataAI, x = TRUE, y = TRUE)



# Perform calibration using the 'calibrate' function
calibration <- calibrate(lrm_model, method = "boot", B = 500)  # B: number of bootstraps

# Plot the calibration curve
plot(calibration)



######### on the COME CCT dataset and perform linear and logistic regression 

