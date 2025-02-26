library(tidyverse)
library(skimr)
library(caret)
library(ROCR)


Haven_data <- read.csv(file.choose(), header = T)
head(Haven_data)
skim(Haven_data)

#CHECKING FOR NULL VALUES
anyNA(Haven_data)
colSums(is.na(Haven_data))

#INCOME
#There were 15 missing values in the dataset for the Income column.The team decided to replace the missing values with the median 
median_income <- median(Haven_data$Income,na.rm = T)
Haven_data$Income[is.na(Haven_data$Income)] <- median_income
#Haven_data <- drop_na(Haven_data)
anyNA(Haven_data)



#CHECKING FOR DUPLICATES
sum(duplicated(Haven_data))
#111 duplicates were found in the data. The team decided to drop duplicate rows to ensure the data had unique rows
Haven_data <- Haven_data[!duplicated(Haven_data),]

sum(duplicated(Haven_data))

#checking for the data types
sapply(Haven_data,class)

Haven_data<- Haven_data %>%
  mutate(Marital_Status = case_when(
    Marital_Status == "Single" ~ "single",
    Marital_Status == "Married" ~ "married",
    Marital_Status == "Divorced" ~ "single",
    Marital_Status == "Alone" ~ "single",
    Marital_Status == "Widow" ~ "single",
    Marital_Status == "YOLO" ~ "single",
    Marital_Status == "Together" ~ "married",
    Marital_Status == "Absurd" ~ "married"
    
  ))
#anyNA(Haven_data)

#Converting the categorical variables to factors 
Haven_data <- Haven_data %>% mutate_at(c("Education","Marital_Status","Response","AcceptedCmp1",
                                         "AcceptedCmp2","AcceptedCmp3","AcceptedCmp4","AcceptedCmp5","Complain"),factor)
sapply(Haven_data, class)


#checking outliers in numerical variables
boxplot(Haven_data[,4:6])
boxplot(Haven_data$Recency)
boxplot(Haven_data[,9:14])
boxplot(Haven_data[,15:19])


#table(Haven_data$Education)
table(Haven_data$Response)
#relationships among variables
ggplot(Haven_data,aes(x=Response))+geom_bar(fill="skyblue")+labs(title = "Frequency of the response variable")
ggplot(Haven_data,aes(x=Response,y=NumStorePurchases))+geom_boxplot()+labs(title = "Relationship between NumStorepurchases and the response variable",x="Response",y="NumStorepurchases")
ggplot(Haven_data,aes(x=Response,y=NumWebPurchases))+geom_boxplot()+labs(title = "Relationship between Numwebpurchases and the response variable",x="Response",y="Numwebpurchases")
ggplot(Haven_data,aes(x=Response,y=Recency))+geom_boxplot()+labs(title = "Relationship between Recency and the response variable",x="Response",y="Recency")
#ggplot(Haven_data,aes(y=Dt_Cus_Year,x=Recency))+geom_point()+labs(title = "Relationship between Recency and the response variable",x="Response",y="Recency")

#Converting Dt_Customer column to date format
#Haven_data %>% select(Dt_Customer)
Haven_data$Dt_Customer <- mdy(Haven_data$Dt_Customer)
sapply(Haven_data,class)
#EXTRACTING DAY,MONTH,YEAR COLUMNS FROM DT_CUSTOMER COLUMN
Haven_data$Dt_Cus_Year <- as.numeric(format(Haven_data$Dt_Customer, "%Y"))
Haven_data$Dt_Cus_Month <- as.factor(format(Haven_data$Dt_Customer, "%m"))
#Haven_data$Dt_Cus_Day <- as.factor(format(Haven_data$Dt_Customer, "%d"))
#DROPPING DT_CUSTOMER COLUMN
Haven_data <- Haven_data[, !(names(Haven_data) %in% "Dt_Customer")]


Haven_data_predictors <- select(Haven_data,-Response)

#CREATING DUMMY VARIABLES
#create dummy variables expect for the response
dummies_model <- dummyVars(~ ., data = Haven_data_predictors)

#provide only predictors that are now converted to dummy variables
predictors_dummy<- data.frame(predict(dummies_model, newdata = Haven_data_predictors)) 

#recombine predictors with dummy variables with response
Haven_data <- cbind(Response=Haven_data$Response, predictors_dummy) 


#Factor recoding of response variable
Haven_data$Response<-fct_recode(Haven_data$Response, Yes = "1",No = "0")

#relevel response
Haven_data$Response<- relevel(Haven_data$Response, ref = "Yes")
levels(Haven_data$Response)



#write.csv(Haven_data,"Cleaned_Haven_dataset.csv",row.names = FALSE)
#DATA PARTITIONING
set.seed(234) #set random seed

index <- createDataPartition(Haven_data$Response, p = .8,list = FALSE)

assign_train <- Haven_data[index,]

assign_test <- Haven_data[-index,]

#FORWARD SELECTION
set.seed(234)
Forward_selection_model <- train(Response~.,data = assign_train,method="glmStepAIC",
                                 direction="forward",trControl=trainControl(method="cv",number = 5,classProbs = TRUE,summaryFunction = twoClassSummary),
                                 metric="ROC")



holdout_data <- read.csv("Gourmet Haven _holdout.csv",header=T)
#write.csv(holdout_data,"holdout_noresponse.csv",row.names = F)
#holdout_data<- read.csv("holdout_noresponse.csv", header=T)
#convert the same columns to factor
#holdout_data %>% mutate_at(c("Education","Marital_Status","Response","AcceptedCmp1",
#                                            "AcceptedCmp2","AcceptedCmp3","AcceptedCmp4","AcceptedCmp5","Complain"),factor)

#str(holdout_data)
#str(dummies_model)
#Convert to dummy variables using the same dummies model of the training data 
#CHECKING FOR NULL VALUES
anyNA(holdout_data)
colSums(is.na(holdout_data))

#INCOME
#There were 9 missing values in the dataset for the Income column.The team decided to replace the missing values with the median 
median_income <- median(holdout_data$Income,na.rm = T)
holdout_data$Income[is.na(holdout_data$Income)] <- median_income
anyNA(holdout_data)

holdout_data <- holdout_data[!duplicated(holdout_data),]
sum(duplicated(holdout_data))


holdout_data<- holdout_data %>%
  mutate(Marital_Status = case_when(
    Marital_Status == "Single" ~ "single",
    Marital_Status == "Married" ~ "married",
    Marital_Status == "Divorced" ~ "single",
    Marital_Status == "Alone" ~ "single",
    Marital_Status == "Widow" ~ "single",
    Marital_Status == "YOLO" ~ "single",
    Marital_Status == "Together" ~ "married",
    Marital_Status == "Absurd" ~ "married"
    
  ))

#Converting the categorical variables to factors 
holdout_data <- holdout_data %>% mutate_at(c("Education","Marital_Status","AcceptedCmp1",
                                             "AcceptedCmp2","AcceptedCmp3","AcceptedCmp4","AcceptedCmp5","Complain"),factor)

holdout_data$Dt_Customer <- mdy(holdout_data$Dt_Customer)
sapply(holdout_data,class)
#EXTRACTING DAY,MONTH,YEAR COLUMNS FROM DT_CUSTOMER COLUMN
holdout_data$Dt_Cus_Year <- as.numeric(format(holdout_data$Dt_Customer, "%Y"))
holdout_data$Dt_Cus_Month <- as.factor(format(holdout_data$Dt_Customer, "%m"))
#Haven_data$Dt_Cus_Day <- as.factor(format(Haven_data$Dt_Customer, "%d"))
#DROPPING DT_CUSTOMER COLUMN
holdout_data <- holdout_data[, !(names(holdout_data) %in% "Dt_Customer")]


holdout_data<- data.frame(predict(dummies_model, newdata = holdout_data)) 


case_holdoutprob<- predict(Forward_selection_model, holdout_data, type="prob")

case_holdout_scored<- cbind(holdout_data, case_holdoutprob$Yes)
case_holdout_scored[1:3,]


write.csv(case_holdout_scored,"Holdout_dataset.csv",row.names = F)

