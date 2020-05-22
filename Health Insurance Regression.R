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

set.seed(123)
inTrain <- createDataPartition(y=insurance$charges, p=0.7, list=FALSE)
Training <- insurance[inTrain,]
Testing <- insurance[-inTrain,]
M1Train <- lm(charges~isSmoker, Training)
predictions <- predict(M1Train, Testing)
View(predictions)

RMSE=sqrt(sum((predictions-Testing$insurance)^2)/(length(Testing$insurance)-2))
RMSE

x <- mean(insurance$charges)
chargebin <- ifelse(insurance$charges >= x, 1, 0)
View(chargebin)
LM1 <- glm(chargebin~isSmoker, insurance, family="binomial") 
summary(LM1)

signal <- predict(LM1, insurance)
pred_prob <- (1/(1 + exp(-signal)))
View(pred_prob)





