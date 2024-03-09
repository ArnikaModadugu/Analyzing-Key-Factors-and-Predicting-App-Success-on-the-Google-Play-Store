# Load required libraries
library(dplyr)
library(tidyr)
library(stargazer)
library(randomForest)
library(MASS)
library(gbm)
library(car)
library(ISLR)

#Read the data into R
googleplaystore <- read.csv("googleplaystore.csv", header = TRUE, na.strings = c("NaN", "?", " ", "NA", "N/A"))
options(scipen=999)
fix(googleplaystore)

#Use dim() function
dim(googleplaystore)

#Find quantitative predictors and qualitative predictors
str(googleplaystore)

#Use summary() function. 
summary(googleplaystore)

# Convert 'Size' column from kilobytes to megabytes
kilobytes_indices <- grepl("k", googleplaystore$Size, ignore.case = TRUE)
googleplaystore$Size[kilobytes_indices] <- as.numeric(gsub("k", "", googleplaystore$Size[kilobytes_indices])) / 1024

# Convert 'Installs' column to numeric by removing non-numeric characters
googleplaystore$Installs <- as.integer(gsub("\\D", "", googleplaystore$Installs))

# Convert 'Price' column to numeric by removing non-numeric characters
googleplaystore$Price <- as.numeric(gsub("[^0-9.]", "", googleplaystore$Price))

# Convert 'Size' column to numeric by removing non-numeric characters
googleplaystore$Size <- as.numeric(gsub("[^0-9.]", "", googleplaystore$Size))

# Convert 'Reviews' column to numeric
googleplaystore$Reviews <- as.numeric(googleplaystore$Reviews)

# Remove rows with missing values
googleplaystore = na.omit(googleplaystore)

# Check for any missing values in the dataset
any(is.na(googleplaystore))

# Display unique values in the 'Category' column
unique(googleplaystore$Category)

# Remove rows where 'Category' is 1.9
googleplaystore <- googleplaystore[googleplaystore$Category != 1.9, ]
summary(googleplaystore)
stargazer(googleplaystore, type="text", title="Table 1: Summary statistics")

#correlation matrix 
# Select only numeric columns from the dataframe
numeric_columns <- sapply(googleplaystore, is.numeric)

# Calculate the correlation matrix for numeric columns
cor_matrix <- cor(googleplaystore[, numeric_columns])

# Display the correlation matrix
# Convert the correlation matrix into a data frame
cor_df <- as.data.frame(cor_matrix)

# Display the correlation matrix using stargazer
stargazer(correlation_matrix, header=FALSE, type="text", title="Table 2: Correlation Matrix")


# scatterplots

numeric_vars = googleplaystore[, c("Rating", "Reviews", "Size", "Installs","Price")]
pairs(numeric_vars, pch = 16, col = "blue")


# Now you can use the stargazer function

# histograms

par(mfrow = c(3, 3))
hist(googleplaystore$Rating, col = "skyblue", main = "Histogram of Rating", xlab = "Rating")
hist(googleplaystore$Reviews, col = "lightgreen", breaks = seq(0, 50000000, by = 1000000), main = "Histogram of Reviews", xlab = "Reviews")
hist(googleplaystore$Size, col = "lightcoral", main = "Histogram of Size", xlab = "Size")
hist(googleplaystore$Installs, breaks = seq(0, 1e9, by = 1e8), col = "lightyellow", main = "Histogram of Installs", xlab = "Installs")
hist(googleplaystore$Price, col = "pink",breaks = seq(0, 400, by = 20), main = "Histogram of Price", xlab = "Price")

#The lm() function to fit a simple linear regression model, with Rating as the response and Installs as the predictor
model <- lm(Rating ~ Installs , data = googleplaystore)
#For more detailed information, we use summary( ). We get p values  standard errors for the coefficients, as well as the R2 statistic and F-statistic
summary(model)


#We will now plot Rating and Installation along with the least squares regression line using the plot() and abline() functions.
plot(googleplaystore$Installs, googleplaystore$Rating, 
     xlab = "Number of Installations", ylab = "Application Rating",
     main = "Graph 1: Scatter Plot: Rating vs Installations")

# Add the regression line to the scatter plot
abline(model, col = "red")

#Use of stargazer to generate profession table of linear regression model
stargazer(googleplaystore, type="text",model,title="Table 4: Simple Linear Regression Results", align=TRUE, out="table1.txt")


# Rating as Success Indicator 

# Defining success as RATING greater than mean rating
mean_rating <- mean(googleplaystore$Rating, na.rm = TRUE)
print(mean_rating)

# Define a threshold for success
success_rating = 4.174

# Creating a binary variable 'Success' based on the threshold
googleplaystore$Success <- ifelse(googleplaystore$Rating >= success_rating, 1, 0)

# Now 'Success' is a binary variable where 1 indicates success and 0 indicates failure

# Set a seed
set.seed(1)

# Define selected columns for the model
selected_columns = c('Installs', 'Reviews', 'Size', 'Price', 'Success')

# Subset the dataframe to include only selected columns
data_for_model = googleplaystore[, selected_columns]

# Create a training set by randomly sampling half of the rows
train = sample(1:nrow(data_for_model), nrow(data_for_model) / 2)

# Extract the 'Success' column for testing
Success.test = data_for_model[-train, "Success"]

# Set seed
set.seed(1)

# Fit a random forest model for predicting 'Success'
rf.success = randomForest(Success ~ ., data = data_for_model, subset = train, mtry = 5/3, importance = TRUE)

# Make predictions on the testing set
yhat.rf = predict(rf.success, newdata = data_for_model[-train, ])

# Calculate the mean squared error of the predictions
mean_squared_error = mean((yhat.rf - Success.test)^2)

# Display the mean squared error
print(paste("Mean Squared Error:", mean_squared_error))

# Plot variable importance from the random forest model
varImpPlot(rf.success)


#According to this test, the most important variable for predicting success is the number of reviews.
#The least important variable is Price.


# Now, perform multiple linear regression. The syntax lm(y~x1+x2+x3) is used to fit a model with  predictors
lm.fit=lm(Success ~ ., data = data_for_model)
lm.fit
# The summary() function now outputs the regression coefficients for all the predictors
summary(lm.fit)

stargazer(googleplaystore, type = "text",lm.fit, title = "Table 5: Multiple Linear Regression Results", align = TRUE,out="table1.txt")

# Backward Selection
# we find that the p value for Installs is the largest one, we use backward selection method to 
#delete one variable with the largest p value
lm.fit1=lm(Success~Reviews + Size + Price,data=data_for_model)
summary(lm.fit1)

# we find that the p value for Installs is the largest one, we use backward selection method to 
#delete one variable with the largest p value
lm.fit2=lm(Success~Reviews + Size,data=data_for_model)
summary(lm.fit2)

stargazer(googleplaystore, type = "text",lm.fit2, title = "Table 6: Multiple Linear Regression Results.", align = TRUE,out="table2.txt")

#We check multicollinearity problem by using VIF.

# we found most of the vif is less than 5 or 10. So we did not find multicolinearity problems.
vif(lm.fit)

# run linear regression based on the training dataset
lm.fit2=lm(Success~Reviews + Size,data=data_for_model,subset=train)
summary(lm.fit2)

# Make predictions 
predictions = predict(lm.fit2, newdata = data_for_model[-train, ])

# Calculate the mean squared difference for the test set
mean_squared_error = mean((predictions - Success.test)^2)

# Print or use the mean squared difference
print(mean_squared_error)

#produce four diagnostic plots of the least squares regression fit.
par(mfrow=c(2,2))
plot(lm.fit)

#Clustering 

# Create a contingency table of counts for Category and Content.Rating
contingency_table <- googleplaystore %>%
count(Category, Content.Rating) %>%
spread(Content.Rating, n, fill = 0)

# Convert the contingency table to a data frame
contingency_table = data.frame(contingency_table)

# Set row names as Category values
rownames(contingency_table) = contingency_table$Category
contingency_table = contingency_table[,-1]

# Display the first few rows of the contingency table
head(contingency_table)

# Calculate Euclidean distance between rows of the contingency table
data.dist <- dist(contingency_table, method = "euclidean")

# Perform hierarchical clustering using complete linkage
hc <- hclust(d = data.dist, method = "complete")

# Plot the hierarchical clustering dendrogram
plot(hc, main="Complete Linkage", xlab="", sub="", ylab="")


