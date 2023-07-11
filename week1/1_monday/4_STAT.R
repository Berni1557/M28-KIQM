

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

#### starting from zero or any sequence of numbers

# Define a mapping of character values to numeric values
mapping <- c("mild" = 5, "moderate" = 6, "severe" = 7)

# Convert the severity column to numeric based on the mapping
df$severity1 <- mapping[as.character(df$severity1)]

df


library(ggplot2)

# Set seed for reproducibility
set.seed(42)


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



# Generate right-skewed variable
skewed_variable <- rgamma(1000, shape = 100, rate = 0.2)

# Plot the distribution
ggplot(data.frame(skewed_variable), aes(x = skewed_variable)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black")

