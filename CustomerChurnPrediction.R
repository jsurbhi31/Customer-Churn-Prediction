#Predict Customer Churn.Also known as Customer Attrition/Loss of Clients.This is a typical problem which is being 
#faced by many companies especially telecom/Internet/Insurance Providers.This problem occurs because in todays scenario customers
#have many options available to choose with a location.
#Task is to identify the patterns in the data that will help in identifying the proportion of churners. 
#Dataset has been taken from IBM sample datasets. This is a telecom industry dataset.
#Importing Libraries
library(plyr)
library(corrplot)
library(ggplot2)
library(caret)
library(randomForest)
library(MASS)
library(party)
library(gridExtra)
library(ggthemes)
library(car)
library(ROSE)


#Setting working driectory
setwd('E:\\Surbhi\\Customer_Churn')
#Reading Data
churn <- read.csv("CustomerChurn.csv")
#There are 21 features and 7043 datapoints
#Structure of dataframe
str(churn)
#Churn is the target variable with two levels
#Dataframe has factors, numeric and integer datatypes available.

#Check for any  missing values in the dataset
sapply(churn,function(x) sum(is.na(x)))

#There are 11 missing values in Total Charges column
#hence, only taking the rows with no missing data
churn<-churn[complete.cases(churn),]

#Summary of dataframe
summary(churn)
# There are various columns which require preprocessing and this has been explained below
# Out of 7043 observations, 5174 did not churn whereas 1869 customers are reported to be churned. This is a case of biased dataset.
table(churn$Churn)
prop.table(table(churn$Churn))
#Average churn rate is close to 26%. That means out of every 100 customers,
#26 are likely to churn

#Data Preprcoessing/Feature Engineering
#Before moving to EDA, I would like to preprocess few columns such as TechSupport, Device Protection etc to replace values
# Values as No internet service with No.

for(i in 1:ncol(churn[,10:15])) {
  churn[,10:15][,i] <- as.factor(mapvalues
                                          (churn[,10:15][,i], ("No internet service"),("No")))
}

#Binning tenure feature into groups
tenure_g <- function(tenure) {
  if(tenure>=0 & tenure <= 12){
    return ("0-12 months")
  }
  else if (tenure > 12 & tenure <=24){
    return("12-24 months")
  }
  else if (tenure > 24 & tenure <=48){
    return("within 48 months")
  }
  else if (tenure > 48 & tenure <= 60){
    return ("within 60 months")
  }
  else if(tenure >60){
    return("More than 60 months")
  }
}

churn$tenure_g <- sapply(churn$tenure,tenure_g)
# Since we created new variable tenure group using tenure, hence removing tenure from our further analysis
churn <- subset( churn, select = -tenure )

# Changing senior citizen value for 1 is yes and for 0 is No
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

#Exploratory Data Analysis

#For categorical columns, I have created bar charts 

#Gender
p_gender =ggplot(data=data.frame(churn$gender),aes(x=churn$gender))+
    geom_bar(aes(y = (..count..)),fill="orange")+ xlab("Gender")+ylab("Count")+
    geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black",vjust = -.5)+coord_flip() 
 
p_gender

#Visualizing the relationship between churn and gender
ggplot(churn, aes(x=gender, fill=Churn)) +
  geom_bar()
# From the visualization,both Male and female are equally likely to churn. Hence, I
#will keep this variable in my model to study its behavior in predicting churn

#Senior Citizen

p_seniorcitizen =ggplot(data=data.frame(churn$SeniorCitizen),aes(x=churn$SeniorCitizen))+
  geom_bar(aes(y = (..count..)),fill="black")+ xlab("Senior Citizen")+ylab("Count")+
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="red",vjust = -.5)+coord_flip() 

p_seniorcitizen

#Visualizing the relationship between churn and Senior Citizen
ggplot(churn, aes(x=SeniorCitizen, fill=Churn)) +
  geom_bar()
# From the dataset, we can see that senior citizens are more likely to churn

#Partner
p_partner =ggplot(data=data.frame(churn$Partner),aes(x=churn$Partner))+
  geom_bar(aes(y = (..count..)),fill="black")+ xlab("Partner")+ylab("Count")+
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="red",vjust = -.5)+coord_flip() 
p_partner
#Visualizing the relationship between churn and Partner
ggplot(churn, aes(x=Partner, fill=Churn)) +
  geom_bar()

#Dependents
p_dependents =ggplot(data=data.frame(churn$Dependents),aes(x=churn$Dependents))+
  geom_bar(aes(y = (..count..)),fill="black")+ xlab("Dependents")+ylab("Count")+
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="red",vjust = -.5)+coord_flip() 
p_dependents

#Visualizing the relationship between churn and Dependents
ggplot(churn, aes(x=Dependents, fill=Churn)) +
  geom_bar()

#PhoneService
p_phoneservice =ggplot(data=data.frame(churn$PhoneService),aes(x=churn$PhoneService))+
  geom_bar(aes(y = (..count..)),fill="black")+ xlab("Phone Service")+ylab("Count")+
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="red",vjust = -.5)+coord_flip() 

#Multiple Lines
p_multiplelines =ggplot(data=data.frame(churn$MultipleLines),aes(x=churn$MultipleLines))+
  geom_bar(aes(y = (..count..)),fill="orange")+ xlab("Multiple Lines")+ylab("Count")+
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="red",vjust = -.5)+coord_flip() 

#Internet Service
p_internetservice =ggplot(data=data.frame(churn$InternetService),aes(x=churn$InternetService))+
  geom_bar(aes(y = (..count..)),fill="orange")+ xlab("Internet Service")+ylab("Count")+
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="red",vjust = -.5)+coord_flip() 
p_internetservice

#Visualizing internet service vs churn

ggplot(churn, aes(x=InternetService, fill=Churn)) +
  geom_bar()

#Online Scurity
p_onlinesecurity =ggplot(data=data.frame(churn$OnlineSecurity),aes(x=churn$OnlineSecurity))+
  geom_bar(aes(y = (..count..)),fill="black")+ xlab("Online Security")+ylab("Count")+
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="red",vjust = -.5)+coord_flip() 
p_onlinesecurity

#Visualizing  online security vs churn
ggplot(churn, aes(x=OnlineSecurity, fill=Churn)) +
  geom_bar()
grid.arrange(p_gender,p_seniorcitizen,p_onlinesecurity,p_internetservice,p_multiplelines,p_phoneservice)

#Plotting histogram for Numerical column

hist(churn$MonthlyCharges,
     xlim = c(17,120),
     col = "lightblue",
     ylab = "Count",
     xlab = "Customer Monthly Charges",
     main = "Histogram of Monthly Charges")

#correlation between numeric variables

numeric.var<-sapply(churn,is.numeric)
corr.matrix<-cor(churn[,numeric.var])
corr.matrix
corrplot(corr.matrix,main="\n\n Corr plot",method="number")

#Removing customer id and total charges from the dataset as total charges is highly 
#correlated with monthly charges and customer id is not a menaingful variable
#for further analysis
churn$TotalCharges<-NULL
churn$customerID<-NULL

#Model Building

#splitting dataset into training and test
DP<-createDataPartition(churn$Churn,p=0.8,list=FALSE)
set.seed(1578)

churntrain<-churn[DP,]
churntest<-churn[-DP,]
dim(churntrain)
dim(churntest)



#Random Forest
fit = randomForest(Churn ~ ., data = churntrain,importance = TRUE)

#After running above line, I recieved below error
#"In data.matrix(x) : NAs introduced by coercion". This is due to
#some character variables in dataframe. Hence, converting all character
#into factor and then running above statement again
library(dplyr)
churntrain1=churntrain %>% mutate_if(is.character, as.factor)
fit = randomForest(Churn ~ ., data = churntrain1,importance = TRUE)

# This is the extractor function for variable importance measures
importance(fit)

# Dotchart of variable importance as measured by a Random Forest
varImpPlot (fit)
summary(fit)

# Top features include Contract, Paperless billing, Monthly Charges

#Predict Output
churntest1=churntest %>% mutate_if(is.character, as.factor)
predicted= predict(fit,churntest1)
misclassificationError = mean(predicted != churntest1$Churn)
misclassificationError
Accuracy = 1 - misclassificationError
Accuracy

#Accuracy of random forest is 83%

# function to plot confusion matrix. 

plotConfusionMatrix = function(actual,predicted,l)
{
  Actual = factor(c(0,0,1,1))
  Predicted = factor(c(0,1,0,1))
  p = as.vector(table(actual,predicted))
  df = data.frame(Actual,Predicted,p)
  a = ggplot(data =  df, mapping = aes(x = Predicted, y = Actual)) +
    ggtitle(l) +
    geom_tile(aes(fill = p), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", p)), vjust = 1) +
    scale_fill_gradient(low = "blue", high = "red") +
    theme_bw() + theme(legend.position = "none")
  return(a)
}

# plotting confusion matrix
plotConfusionMatrix(churntest1$Churn,predicted,"Random Forest")
randomFCMatrix = table(churntest1$Churn,predicted)
randomFCMatrix

# AUC for Random Forest
roc.curve(churntest1$Churn,predicted)
#Area under the curve (AUC): 0.716

# Logistic Regression
fit = glm(Churn~.,data = churntrain, family = binomial(link = "logit"))
summary(fit)

# Significant variables as per Logistic Regression are Contract, tenure group and 
#Paperless billing

anova(fit, test="Chisq")

#Predict Output
churntest$Churn <- as.character(churntest$Churn)
churntest$Churn[churntest$Churn=="No"] <- "0"
churntest$Churn[churntest$Churn=="Yes"] <- "1"
predicted= predict(fit,churntest,type = "response")
predicted = ifelse(predicted>0.5,1,0)
predicted
misclassificationError = mean(predicted != churntest$Churn)
misclassificationError
Accuracy = 1 - misclassificationError
Accuracy

#Accuracy of LR is 78.8%

#Confusion Matrix
plotConfusionMatrix(churntest$Churn,predicted,"Logistic Regression")
logCMatrix = table(churntest$Churn,predicted)
logCMatrix

# AUC for Logistic Regression
roc.curve(churntest$Churn,predicted)

#Area under the curve (AUC): 0.692
library(rpart.plot)

# Decision Tree
churntrain$tenure_g<-as.factor(churntrain$tenure_g)
churntest$tenure_g<-as.factor(churntest$tenure_g)
binary.model <- rpart(Churn~Contract+tenure_g+PaperlessBilling, data=churntrain)
rpart.plot(binary.model)

#From decision tree plot we saw that,Contract is the root variable to decide
#customer churn.
# If the contract is of 1 or 2 year then irrespective of paperless billing, that customer
#is more likely to churn.
# For month to month contracts, if the customer's tenure group is 0-12 months
#and they use paperless billing then that customer is more likely to churn
# Predict using DT
tree <- ctree(Churn~Contract+tenure_g+PaperlessBilling, churntrain)
pred_tree <- predict(tree, churntest)

#Confusion Matrix for DT
plotConfusionMatrix(churntest$Churn,pred_tree,"Decision Tree")
DTMatrix = table(churntest$Churn,pred_tree)
DTMatrix

# AUC for DT
roc.curve(churntest$Churn,pred_tree)

#Area under the curve (AUC): 0.635

#Model Selection
#Since random Forest has more accuracy and AUC is also more, we will chose Random Forest.
# Work to be done for further analysis
#Since,This dataset is an example of imbalanced dataset so accuracy and ROC curve is not a proper
#measure to assess the model. I will go for Pricsion Recall curve as it is more 
#informative for imbalanced dataset
