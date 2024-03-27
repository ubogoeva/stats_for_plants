#######################################################################
#  Linear regression example with reading in a .csv file ##############
#######################################################################

# Read in the data set and specify which columns contan factors resp. numeric values
# The file is Spinach1
# data <- read.csv("path/to/your/file/data.csv", header = TRUE, colClasses = c("factor", "numeric", "numeric"))
Spin.data <- read.csv("/home/tomerius/EPBA/Spinach1.csv", header = TRUE, sep = ";", colClasses = c("factor", "numeric", "numeric"))
head(Spin.data)
str(Spin.data)

# Plot the data
plot(Spin.data$Days, Spin.data$Yield)
# Plot it in a nicer way
plot(Spin.data$Days, Spin.data$Yield, pch = 16, cex = 1.3, col = "blue", 
     main = "Days to yield against yield in kg per plot", xlab = "Days", ylab = "Yield (kg)")

# Plotting using ggplot2-package
#Import libraries (install package, if needed, before loading)
library(ggplot2)

# Create basic ggplot
ggp <- ggplot(Spin.data, aes(x = Days, y = Yield)) +      
  geom_point()
ggp   # show plot

# add regression line to plot
ggp <- ggplot(Spin.data, aes(x = Days, y = Yield)) +      
  geom_point()+     
  geom_smooth(method = "lm", formula = y ~ x)
ggp   # show plot (regression line in blue, shadowed area indicates variance)

# create box plots to check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(Spin.data$Days, main="Days", sub=paste("Outlier rows: ", boxplot.stats(Spin.data$Days)$out))  # box plot for 'Days'
boxplot(Spin.data$Yield, main="Yield", sub=paste("Outlier rows: ", boxplot.stats(Spin.data$Yield)$out))  # box plot for 'Yield'

# create density plot to check if the response variable is close to normality
library(e1071)
plot(density(Spin.data$Yield), main="Density Plot: Yield", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Spin.data$Yield), 2)))  # density plot for 'Yield'
polygon(density(Spin.data$Yield), col="red")

### Calculate correlation between thw two traits
cor(Spin.data$Days, Spin.data$Yield) 

##### Regression analysis #####

# 1) Do the actual linear regression using lm() - example 1: simple regression
model <- lm(Yield ~ Days , data = Spin.data)   # Model: "yield is a function of days to harvest"
model                                          # display Intercept and slope
summary(model)                                 # display further results incl significance / p-value and R²
                                               # may only use our model for predicting it it is statistically significant!

# 2) Check underlying assumptions
#    Regression diagnostics plots can be created using the R base function plot()

par(mfrow = c(2, 2))  # creates a 2x2 grid for plotting
plot(model)           # need to run this line together with the one above

plot(model, 1)     # individual plots can be created by indicating the respective number

# 3) Using our model to predict unknown values
#    test quality of the model by splitting data into training set and test set (here 50:50)
#    Cross prediction -> e .g applied in Genomic selection   
#    Define training set and test set
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(Spin.data), 0.5*nrow(Spin.data))  # row indices for training data
trainingData <- Spin.data[trainingRowIndex, ]  # model training data
testData  <- Spin.data[-trainingRowIndex, ]   # test data
testData   # view one example of test data set

# Build the model on training data only
model2 <- lm(Yield ~ Days, data=trainingData)  # build the model
YieldPred <- predict(model2, testData)         # predict Yield
YieldPred                                      # see predicted Yield values

# Check if model is still significant (here: yes, but a little lesss than with the full data set)
summary(model2)

# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$Yield, predicteds=YieldPred))  # make actuals_predicteds dataframe
head(actuals_preds)   # see actual vs predicted values

correlation_accuracy <- cor(actuals_preds)  # calculate correlation between actual and predicted values
correlation_accuracy  # view correlation      

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  # calculate min-max-accuracy 
min_max_accuracy

# Predict the yield of test entries without Yield results
# Let's create some fake data
PredNew <- data.frame(Days = c(12.5, 18, 8, 7.5, 14, 17))   # create some fake data (6 entries)
PredNew                                                     # show the data

# Use our prediction model to predict their yield
YieldPred2 <- predict(model2, newdata = PredNew)            # newdata (explain by removing and typing in - mouse over)   
YieldPred2                                                  # see results

# Add a 95% confidence interval to the predicted values
YieldPred3 <- predict(model2, newdata = PredNew, interval = "confidence")

# Combine in a data frame with proper rows and columns 
YieldPred  <- data.frame(Days = c(12.5, 18, 8, 7.5, 14, 17),
                         Yield = YieldPred3)
YieldPred   # see result  with 95% confidence interval                                    


###############################################################################################

### sources used for this demo #####
# https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/
# http://r-statistics.co/Linear-Regression.html 
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/#regression-assumptions
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/#prediction-for-new-data-set
# check out for yourself - there is plenty of material on the internet!