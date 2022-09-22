library(haven)
library(readxl)
impairment <- read_excel("impairment.xlsx")
head(impairment)

# Prepare a copy for exercise
OLR_data <- impairment
# Ordering the dependent variable
OLR_data$Impairment <- factor(OLR_data$Impairment,levels=c(1,2,3,4),labels=c("well","mild","moderate","impaired"))
# Ordering the independent variable
OLR_data$ses <- factor(OLR_data$ses,levels=c(0,1),labels=c("low","high"))
str(OLR_data)

# Exploratory data analysis 
# Summarizing the data
summary(OLR_data)

# Making frequency table
table(OLR_data$Impairment, OLR_data$ses)

table(OLR_data$Impairment, OLR_data$lifeevents)

# Run the ordinal logistic Regression model using MASS package

# In order to run the ordinal logistic regression model, we need the polr function from the MASS package
library(MASS)
# Build ordinal logistic regression model
OLRmodel <- polr(Impairment ~ ses + lifeevents , data = OLR_data, Hess = TRUE)
summary(OLRmodel)

# Run a "only intercept" model
OIM <- polr(Impairment ~ 1, data = OLR_data)
summary(OIM)

# Compare the our test model with the "Only intercept" model
anova(OIM,OLRmodel)

# Check the predicted probability for each program
OLRmodel$fitted.values

# We can get the predicted result by using predict function
predict(OLRmodel)

# Test the goodness of fit
chisq.test(OLR_data$Impairment,predict(OLRmodel))

# Compute a confusion table and misclassification error (R exclusive)
#Compute confusion table and misclassification error
predictOLR <-  predict(OLRmodel,OLR_data)
cTab <- table(OLR_data$Impairment, predictOLR)
mean(as.character(OLR_data$Impairment) != as.character(predictOLR))

(CCR <- sum(diag(cTab)) / sum(cTab)) # Calculate the classification rate

# Measuring Strength of Association (Calculating the Pseudo R-Square)

# Load the DescTools package for calculate the R square
library("DescTools")
# Calculate the R Square
PseudoR2(OLRmodel, which = c("CoxSnell","Nagelkerke","McFadden"))


# Parameter Estimates
# We can use the coef() function to check the parameter estimates
(OLRestimates <- coef(summary(OLRmodel)))

# Add the p-value to our parameter estimates table
library(AER)
coeftest(OLRmodel)


# Calculating Expected Values
# Bulding the newpatient entry to test our model
newpatient <- data.frame("subject" = 1, "Impairment" = NA, "ses" = "high", "lifeevents"=3)
# Use predict function to predict the patient's situation
predict(OLRmodel,newpatient)
