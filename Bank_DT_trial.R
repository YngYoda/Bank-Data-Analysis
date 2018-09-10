## This is Bank data analysis:keeping only desired variables.


# Read Data into table.
data <- read.csv("/Users/akan/Desktop/Stat_Training/train_x1.csv", header=T)

str(data)
newdata <- na.omit(data)

data$age <- as.numeric(data$age)
data$duration <- as.numeric(data$duration)
data$campaign <- as.numeric(data$campaign)
data$pdays <- as.numeric(data$pdays)
data$previous <- as.numeric(data$previous)


names(data)

##Creating linear models that fit the data set
mod2 <- lm(Outcome ~ age, data=data)
summary(mod2)

mod3 <- lm(Outcome ~ job, data=data)
summary(mod3)

relevel(data$marital,"single")
factor(data$marital <- relevel(data$marital , ref="single"))
mod4 <- glm(Outcome ~ factor(marital),family="binomial", data=data)
summary(mod4)

mod5 <- glm(Outcome ~ education, family="binomial",data=data)
summary(mod5)

mod6 <- glm(Outcome ~ default, family="binomial",data=data)
summary(mod6)

mod7 <- glm(Outcome ~ housing, family="binomial",data=data)
summary(mod7)

mod8 <- glm(Outcome ~ contact, family="binomial",data=data)
summary(mod8)

mod9 <- glm(Outcome ~ month, family="binomial",data=data)
summary(mod9)

mod10 <- glm(Outcome ~ day_of_week, family="binomial",data=data)
summary(mod10)

mod11 <- glm(Outcome ~ duration, family="binomial",data=data)
summary(mod11)

mod12 <- glm(Outcome ~ campaign, family="binomial",data=data)
summary(mod12)

mod13 <- glm(Outcome ~ pdays, family="binomial",data=data)
summary(mod13)

mod14 <- glm(Outcome ~ poutcome, family="binomial",data=data)
summary(mod14)

mod <- glm(Outcome ~ age + job + marital + education+ 
             default+housing+contact+duration+campaign+pdays+poutcome , family="binomial", data= data)
summary(mod)

or=exp(cbind(OR=coef(mod),confint(mod)))
or

##Perform Data splitting
newdata <- data[,c(1:16,22)]
str(newdata)


##Perform Data Binning
newdata$age <- cut(newdata$age, c(1,20,40,60,100))
newdata$is_divorced <- ifelse(newdata$marital == "divorced", 1, 0)
newdata$is_single <- ifelse(newdata$marital == "single" , 1, 0)
newdata$is_married <- ifelse(newdata$marital == "married" , 1, 0)
newdata$marital <- NULL
str(newdata)

## Splitting the Data
require(caTools)
set.seed(108)
sample <- sample.split(newdata$Outcome, SplitRatio = .75)
train <- subset(newdata, sample == TRUE)
test <- subset(newdata, sample == FALSE)

##Creating Decision Tree
library(rpart)
library(rpart.plot)
library(rattle)
model <- rpart(Outcome ~ ., data = newdata)
summary(model)

##Testing Decision Tree
pred <- predict(model, test)
table(test[,5], pred)
printcp(model)
plotcp(model)

##Check Pruning--Giving Errors
##Figure margins too large
pmodel <- prune(model,cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
plot(pmodel,uniform = TRUE)
text(pmodel, use.n=TRUE, all=TRUE,cex=0.5)
