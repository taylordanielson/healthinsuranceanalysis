insurance <- read.csv("insurance.csv", header = T)

##descriptive analysis

dim(insurance)
View(insurance)



dim(insurance) #view dimensionality of the dataset
head(insurance) #shows first few observations in the dataset
View(insurance) #views the data in spreadsheet form

##DESCRIPTIVE SUMMARY STATISTICS##
summary(insurance) #generates the "six number summary" statistics for each variable
cov(insurance$bmi, insurance$charges)#generates the variance-covariance matrix from column variables 5-8
cov(insurance$children, insurance$charges)
cov(insurance$age, insurance$charges)
cor(insurance$age, insurance$charges) #generates the correlation matrix for column variables 5-8
cor(insurance$bmi, insurance$charges)
cor(insurance$children, insurance$charges)


##GENERATING SOME BASIC VISUALIZATIONS
hist(insurance$charges, prob = TRUE) #generates a histogram for the Charges variable
#add calibrated normal density curve to histogram
curve(dnorm(x, mean = mean(insurance$charges), sd = sd(insurance$charges)), col = "darkblue", lwd = 2, add = TRUE)

#generates a nonparametric density estimate of the distribution of the Sales variable
plot(density(insurance$charges)) 
#generate pairs of scatter plots from the variables in columns 5-8
pairs(df[,5:8]) 
#generate a time series line plot of Sales over the index (day of the year)
plot(df$Sales, type='l') 
#generate scatter plot of Sales(X) vs. AdSpend(Y)
plot(insurance$charges~insurance$age) 
plot(insurance$charges~insurance$bmi) 
plot(insurance$charges~insurance$children) 


##Dummy Variable Creation

insurance$isFemale <- as.integer(insurance$sex == "female")
insurance$isSmoker <- as.integer(insurance$smoker == "yes")
insurance$isNorthwest <- as.integer(insurance$region == "northwest")
insurance$isNortheast <- as.integer(insurance$region == "northeast")
insurance$isSouthwest <- as.integer(insurance$region == "southwest")
insurance$isSoutheast <- as.integer(insurance$region == "southeast")

summary(insurance)
plot(density(insurance$charges))

cor(insurance$age, insurance$charges)
cor(insurance$bmi, insurance$charges)
cor(insurance$children, insurance$charges)
cor(insurance$isFemale, insurance$charges)
cor(insurance$isSmoker, insurance$charges)
cor(insurance$isSouthwest, insurance$charges)
cor(insurance$isSoutheast, insurance$charges)
cor(insurance$isNorthwest, insurance$charges)
cor(insurance$isNortheast, insurance$charges)

Model1 <- lm(charges~isSmoker, insurance)
summary(Model1)
plot(insurance$charges~insurance$isSmoker)
abline(Model1$coefficients[1],Model1$coefficients[2], col='red', lwd=3)

Model2 <- lm(charges~isSmoker+age, insurance)
summary(Model2)

Model3 <- lm(charges~isFemale+age+isSmoker, insurance)
summary(Model3)
plot(insurance$charges~insurance$isFemale)

Model4 <- lm(charges~isNorthwest+bmi+isSmoker, insurance)
summary(Model4)

Model5 <- lm(charges~isSmoker+age+bmi+children+isFemale, insurance)
summary(Model5)

confint(Model5)

Training<-subset(insurance, insurance$isSouthwest!='1') #generates training data
Testing<-subset(insurance, insurance$isSouthwest=='1') #generates testing data

dim(Training)
dim(Testing)
View(Testing)

#RE-BUILD MODEL M8 WITH ONLY THE TRAINING DATA PARTITION
M9<-lm(charges~isSmoker+age+bmi+children+isFemale, Training)
summary(M9)

#EVALUATE M9 ON THE TEST PARTITION TO COMPUTE THE OUT-OF-SAMPLE PREDICTIONS
predictions<-predict(M9, Testing)
View(predictions) # view predictions for Northwest

##CALCULATE ROOT MEAN SQUARE PREDICTION ERROR ON TEST DATA: THE OUT-OF-SAMPLE ERROR MEASURE
RMSE=sqrt(sum((predictions-Testing$charges)^2)/(length(Testing$charges)-4))
RMSE #report root mean squared error (E_out) using the out-of-sample testing data


#Classification 

set.seed(123)
inTrain <- createDataPartition(y=insurance$charges, p=0.7, list=FALSE)
Training <- insurance[inTrain,]
Testing <- insurance[-inTrain,]
M1Train <- lm(charges~isSmoker+age+bmi+children+isFemale, Training)
predictions <- predict(M1Train, Testing)
View(predictions)

RMSE=sqrt(sum((predictions-Testing$insurance)^2)/(length(Testing$insurance)-5))
RMSE

confint(M1Train)

#Logistic Regression

x <- mean(insurance$charges)
chargeb <- ifelse(insurance$charges >= x, 1, 0)
View(chargeb)
set.seed(123)
inTrain <- createDataPartition(y=$charges, p=0.7, list=FALSE)
Training <- insurance[inTrain,]
Testing <- insurance[-inTrain,]
LM1 <- glm(chargeb~isSmoker+age+bmi+children+isFemale, insurance, family="binomial") 
summary(LM1)

exp(cbind(LM1$coefficients, confint(LM1)))
confusionMatrix(table(predict(LM1, Testing, type="response") >= 0.5,
                      Testing$charges == 1))


signal <- predict(LM1, insurance)
pred_prob <- (1/(1 + exp(-signal)))
View(pred_prob)

##construct confidence intervals using one of two methods:
confint(LM1)  #using profiled log-likelihood
confint.default(M1.2) #using standard errors

point_conf_table<-cbind(LM1$coefficients, confint(LM1))
point_conf_table

##converting values to odds-ratio interpretation using base e
exp(point_conf_table)



############################
#####PARTITIONING DATA######
###WITH THE CARET PACKAGE###
############################

library(caret)  #calls the caret library to use createDataPartition()

##now we will split the data into training and test sets
##creates a vector of rows to randomly sample p=70% from the raw data for traning
set.seed(1234) #locks seed for random partitioning
inTrain <- createDataPartition(y=insurance$charges, p=.70, list = FALSE) 

##stores these rows in the training set
Training<-insurance[inTrain,]  

##stores all rows not in the training set in the test/validation set
Testing<-insurance[-inTrain,]  

#######################################
###PREDICTION W/ LOGISTIC REGRESSION###
#######################################


x <- mean(insurance$charges)
chargeb <- ifelse(insurance$charges >= x, 1, 0)
View(chargebin)
LM1 <- glm(chargeb~isSmoker+age+bmi+children, insurance, family="binomial") 
summary(LM1)
#takes the coefficients to the base e for odds-ratio interpretation
exp(cbind(LM1$coefficients, confint(LM1)))

#builds the confusion matrix to look at accuracy on training data
confusionMatrix(table(predict(LM1, Training, type=) >= 0.5,
                      Training$charges == 1))

#builds the confusion matrix to look at accuracy on testing data
confusionMatrix(table(predict(LM1, Testing, type="response") >= 0.5,
                      Testing$charges == 1))


## CART MODEL CLASSIFICATION

#rpart package implementation
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
M_CART <- train(chargeb ~ isSmoker+age+bmi+children+isFemale, data = Training, trControl=train_control, tuneLength=10, method = "rpart") #increasing tunelength increases regularization penalty
View(Training)
##the "cv", number = 10 refers to 10-fold cross validation on the training data
plot(M_CART) #produces plot of cross-validation results
M_CART$bestTune #returns optimal complexity parameter
confusionMatrix(predict(M_CART, Testing)
                , Testing$chargeb)
