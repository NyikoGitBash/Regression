library('caret')
library('ggplot2')
library('ggthemes')
names(getModelInfo())
library('data.table')
library('gridExtra')
library('corrplot')

library('pROC')

#set the working directory
setwd("~/MIT Big Data Science/MIT 801 - Introduction to Machine Learning and Statistical Learning/Assignment 03")

#Import dataset
dataset<-read.csv('insurance.csv',header = TRUE)
Check_dataset<-read.csv('check.csv',header = TRUE)

#check dimension of dataset
dim(dataset)
dim(Check_dataset)

sapply(dataset, function(x) sum(is.na(x)))

sapply(dataset, function(x) class(x))

#Encode categorical variables for insurance dataset
dataset$job <- as.factor(dataset$job)
levels(dataset$job) <- list('admin.'=1,'blue-collar'=2,'entrepreneur' =3,'housemaid'=4,'management'=5,
                            'retired'=6,'self-employed'=7,'services'=8,'student'=9,'technician'=10,
                            'unemployed'=11,'unknown'=12)
dataset$job <- as.numeric(dataset$job)

dataset$marital <- as.factor(dataset$marital)
levels(dataset$marital) <- list('divorced'=1,'married'=2,'single' =3)
dataset$marital <- as.numeric(dataset$marital)

dataset$education <- as.factor(dataset$education)
levels(dataset$education) <- list('primary'=1,'secondary'=2,'tertiary'=3,'unknown'=4)
dataset$education <- as.numeric(dataset$education)

dataset$default <- as.factor(dataset$default)
levels(dataset$default) <- list('no'=1,'yes'=2)
dataset$default <- as.numeric(dataset$default)

dataset$housing <- as.factor(dataset$housing)
levels(dataset$housing) <- list('no'=1,'yes'=2)
dataset$housing <- as.numeric(dataset$housing)

dataset$loan <- as.factor(dataset$loan)
levels(dataset$loan) <- list('no'=1,'yes'=2)
dataset$loan <- as.numeric(dataset$loan)

dataset$contact <- as.factor(dataset$contact)
levels(dataset$contact) <- list('cellular'=1,'telephone'=2,'unknown'=3)
dataset$contact <- as.numeric(dataset$contact)

dataset$month <- as.factor(dataset$month)
levels(dataset$month) <- list('apr'=1,'aug'=2,'dec' =3,'feb'=4,'jan'=5,
                            'jul'=6,'jun'=7,'mar'=8,'may'=9,'nov'=10,
                            'oct'=11,'sep'=12)
dataset$month <- as.numeric(dataset$month)

dataset$poutcome <- as.factor(dataset$poutcome)
levels(dataset$poutcome) <- list('failure'=1,'other'=2,'success' =3,'unknown'=4)
dataset$poutcome <- as.numeric(dataset$poutcome)

dataset$y <- as.factor(dataset$y)
levels(dataset$y) <- list('no'=1,'yes'=2)
dataset$y <- as.numeric(dataset$y)

dataset$age <- as.numeric(dataset$age)

dataset$balance <- as.numeric(dataset$balance)

dataset$day <- as.numeric(dataset$day)

dataset$duration <- as.numeric(dataset$duration)

dataset$campaign <- as.numeric(dataset$campaign)

dataset$pdays <- as.numeric(dataset$pdays)

dataset$previous <- as.numeric(dataset$previous)

table(dataset$poutcome)

#Encoding categorical variables for check dataset

Check_dataset$job <- as.factor(Check_dataset$job)
levels(Check_dataset$job) <- list('admin.'=1,'blue-collar'=2,'entrepreneur' =3,'housemaid'=4,'management'=5,
                            'retired'=6,'self-employed'=7,'services'=8,'student'=9,'technician'=10,
                            'unemployed'=11,'unknown'=12)
Check_dataset$job <- as.numeric(Check_dataset$job)

Check_dataset$marital <- as.factor(Check_dataset$marital)
levels(Check_dataset$marital) <- list('divorced'=1,'married'=2,'single' =3)
Check_dataset$marital <- as.numeric(Check_dataset$marital)

Check_dataset$education <- as.factor(Check_dataset$education)
levels(Check_dataset$education) <- list('primary'=1,'secondary'=2,'tertiary'=3,'unknown'=4)
Check_dataset$education <- as.numeric(Check_dataset$education)

Check_dataset$default <- as.factor(Check_dataset$default)
levels(Check_dataset$default) <- list('no'=1,'yes'=2)
Check_dataset$default <- as.numeric(Check_dataset$default)

Check_dataset$housing <- as.factor(Check_dataset$housing)
levels(Check_dataset$housing) <- list('no'=1,'yes'=2)
Check_dataset$housing <- as.numeric(Check_dataset$housing)

Check_dataset$loan <- as.factor(Check_dataset$loan)
levels(Check_dataset$loan) <- list('no'=1,'yes'=2)
Check_dataset$loan <- as.numeric(Check_dataset$loan)

Check_dataset$contact <- as.factor(Check_dataset$contact)
levels(Check_dataset$contact) <- list('cellular'=1,'telephone'=2,'unknown'=3)
Check_dataset$contact <- as.numeric(Check_dataset$contact)

Check_dataset$month <- as.factor(Check_dataset$month)
levels(Check_dataset$month) <- list('apr'=1,'aug'=2,'dec' =3,'feb'=4,'jan'=5,
                              'jul'=6,'jun'=7,'mar'=8,'may'=9,'nov'=10,
                              'oct'=11,'sep'=12)
Check_dataset$month <- as.numeric(Check_dataset$month)

Check_dataset$poutcome <- as.factor(Check_dataset$poutcome)
levels(Check_dataset$poutcome) <- list('failure'=1,'other'=2,'success' =3,'unknown'=4)
Check_dataset$poutcome <- as.numeric(Check_dataset$poutcome)


Check_dataset$age <- as.numeric(Check_dataset$age)

Check_dataset$balance <- as.numeric(Check_dataset$balance)

Check_dataset$day <- as.numeric(Check_dataset$day)

Check_dataset$duration <- as.numeric(Check_dataset$duration)

Check_dataset$campaign <- as.numeric(Check_dataset$campaign)

Check_dataset$pdays <- as.numeric(Check_dataset$pdays)

Check_dataset$previous <- as.numeric(Check_dataset$previous)
table(dataset$y)

#convert all dataset to numeric
dataset1<-dataset[,c(-1,-2)]

#Split data into train and test set
dataset$y<-as.factor(dataset$y)

train<-dataset1[1:36000,]
test<-dataset1[36001:40000,]

ggplot(train, aes(x = y, fill = factor(y))) +
    geom_bar(stat='count', position='dodge') + scale_x_discrete(limits=c(0,1)) +
ggtitle("y=client subscribed a term deposit?(yes=1,no=1)")+geom_text(stat='count',aes(label=..count..),vjust=1)+theme_few()

g1<- ggplot(train, aes(duration, color = factor(y), fill = factor(y))) + geom_density(alpha = 0.2)
g2<- ggplot(train, aes(housing, color = factor(y), fill = factor(y))) + geom_density(alpha = 0.2)
g3<- ggplot(train, aes(contact, color = factor(y), fill = factor(y))) + geom_density(alpha = 0.2)
g4<- ggplot(train, aes(pdays, color = factor(y), fill = factor(y))) + geom_density(alpha = 0.2)
grid.arrange(g1, g2,g3,g4, ncol=2,nrow=2)

g5<- ggplot(train, aes(x=duration, y = y)) + geom_point(shape=1) 
g6<- ggplot(train, aes(x=housing, y=y)) + geom_point(shape=1) 
g7<- ggplot(train, aes(x=contact, y=y)) + geom_point(shape=1) 
g8<- ggplot(train, aes(x=pdays, y=y)) + geom_point(shape=1) 
grid.arrange(g5, g6,g7,g8, ncol=2,nrow=2)

maxs <- apply(train[1:17], 2, max)
mins <- apply(train[1:17], 2, min)
train[1:17]<-scale(train[1:17], center = mins, scale = maxs - mins)

maxs <- apply(test[1:17], 2, max)
mins <- apply(test[1:17], 2, min)
test[1:17]<-scale(test[1:17], center = mins, scale = maxs - mins)

train$y<-as.factor(train$y)
test$y<-as.factor(test$y)
fitControl<-trainControl(method='cv',number=10,classProbs = FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = y ~.,data = train)
summary(regressor)
summary(as.data.frame(y_pred))
y_pred <- predict(regressor, newdata = test[,-17])
y_pred1<-as.data.frame(y_pred)
threshold <- 0.5
pred      <- factor( ifelse(y_pred1 > threshold, 1, 0) )
pred      <- relevel(pred, 1)   
pred1<-as.data.frame(pred)
confusionMatrix(test$y,as.matrix(pred1))

ggplot(y_pred1, aes(x = y, fill = factor(y))) +
    geom_bar(stat='count', position='dodge') + scale_x_discrete(limits=c(0,5)) +
    ggtitle("predictions for linear",aes(label=..count..),vjust=1)+theme_few()

boxplot(as.data.frame(y_pred),ylab="predictions",title("linear regression predictions on test set"))

# Fitting logistic Regression to the Training set

Logit_model<-glm(formula = y~., family = binomial, data = train)
summary(Logit_model)
prob_pred <- predict(Logit_model, newdata = test[,-17],type="response")
#y_pred3<-as.data.frame(y_pred2)
threshold <- 0.5
pred2      <- factor( ifelse(prob_pred > threshold, 1, 0) )
pred2      <- relevel(pred2, 1)   
y_pred1<-as.data.frame(pred2)
confusionMatrix(test$y,pred2)

#Predictions of logistic regression model on the check dataset
Check<-Check_dataset[,c(-1,-2)]
maxs <- apply(Check[1:16], 2, max)
mins <- apply(Check[1:16], 2, min)
Check[1:16]<-scale(Check[1:16], center = mins, scale = maxs - mins)

probs_pred <- predict(Logit_model, newdata = Check,type="response")
#y_pred3<-as.data.frame(y_pred2)
threshold <- 0.5
pred6      <- factor( ifelse(probs_pred > threshold, 1, 0) )
pred6      <- relevel(pred6, 1)   
y_predictions<-as.data.frame(pred6)
solution <- data.frame(Client = Check_dataset$Seq.nr, y = y_predictions)
# Write the solution to file
write.csv(solution, file = 'check_Solution1.csv', row.names = F)

ggplot(data=solution,aes(x=pred6)) + geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=1)+ggtitle(label = "logistic regression predictions on check dataset")+theme_few()


#glm_scores <- predict(Logit_model, test, type="link")

#glm_response_scores <- predict(Logit_model, test, type="response")



Logit_model1<-train(factor(y)~.-job,data=train, method="glm")
predictions <- predict(Logit_model1, newdata = test[,-17])
confusionMatrix(test$y,predictions)



