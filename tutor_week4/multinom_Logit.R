#CTG Data


mydata <- read.csv("~/Documents/Cardiotocographic.csv", header = TRUE)
str(mydata)
head(mydata)

#NSP = 1 : normal patien
#NSP = 2 : suspect patient
#NSP = 3 : pathologic patient

#predict patient NSP category

mydata$NSP <- as.factor((mydata$NSP))
# mydata$NSP

#penamaan bisa menggunakan cara ini:
# levels(mydata$NSP) <- c("academic","general","vocational")
str(mydata)

#descriptive statistics
summary(mydata)
library(jmv)
descriptives(mydata, freq = TRUE)
descriptives(mydata, vars = vars(LB, AC, FM, UC, NSP), freq = TRUE)

# Run a "only intercept" model
library(nnet)
OIM <- multinom(NSP ~ 1, data = mydata)
summary(OIM)


# Run a multinomial model
multi_mo <- multinom(NSP ~ LB + AC + FM + UC, data = mydata,model=TRUE)
summary(multi_mo)
# Check the model fit information
# the anova function is confilcted with JMV's anova function, so we need to unlibrary the JMV function before we use the anova function.
detach("package:jmv", unload=TRUE)
# Compare the our test model with the "Only intercept" model (test overall significant of the model)
#Ho : all b's = 0
#ha : at least one b not = 0
anova(OIM,multi_mo, test = "Chisq")



#data Partition
set.seed(222)
ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.6, 0.4))
training <- mydata[ind==1,]
testing <- mydata[ind==2,]

#multinomial logistic regression
# library(nnet)
training$NSP <- relevel(training$NSP, ref = "1")
mymodel <- multinom(NSP ~ ., data = training)
summary(mymodel)



# 2-tailed Z-test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
z
p_val <- (1 - pnorm(abs(z), 0, 1))*2
p_val 
library(AER)
coeftest(mymodel)

#final model
mymodel <- multinom(NSP ~ . - MLTV - Width - Min - Max - Nmax - Nzeros - Tendency, data = training, model = TRUE)

# Test the goodness of fit
chisq.test(training$NSP,predict(mymodel))

# Calculate the Pseudo R-Square

# Load the DescTools package for calculate the R square
library("DescTools")
# Calculate the R Square
PseudoR2(mymodel, which = c("CoxSnell","Nagelkerke","McFadden"))

# Likelihood Ratio Tests
# Use the lmtest package to run Likelihood Ratio Tests
library(lmtest)
lrtest(mymodel, "LB") # Chi-Square=12.922,p=0.01166*
##ppsst: ini tidak dilanjutkan, lanjutan: https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html#parameter-estimates

#interpretation: (with equation)

# ln[P(NSP=2)/P(NSP=1)] = intercept + coenf*variable + ...
# ln[P(NSP=3)/P(NSP=1)] = intercept + coenf*variable + ...


#confusion matrix & miscslassification error - Training data
pred <- predict(mymodel, training)
head(pred)
head(training$NSP)
tab_mat <- table(pred, training$NSP)
tab_mat

#calculate accuracy rate
acc_rate <- sum(diag(tab_mat))/sum(tab_mat)
1 - acc_rate #missclasification for training data set

#confusion matrix & miscslassification error - Testing data
pred_test <- predict(mymodel, testing)
tab_mat2 <- table(pred_test, testing$NSP)
tab_mat2
acc_rate2 <- sum(diag(tab_mat2))/sum(tab_mat2)
1 - acc_rate2 #missclasification for testing data set

###Prediction and Model Assesment
#in training data
n1 <- table(training$NSP)
n1/sum(n1) #percentage NSP

tab_mat/colSums(tab_mat) #accuracy for each classification

#in testing data
n2 <- table(training$NSP)
n2/sum(n2) #percentage NSP

tab_mat2/colSums(tab_mat2) #accuracy for each classification


#confusion matrix all
library(caret)
confusionMatrix(pred, training$NSP)
confusionMatrix(pred_test, testing$NSP)
