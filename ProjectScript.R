##########################################################
# Download data and import to R
##########################################################

# install packages if they are not already installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# set the number of digits to print
options(digits=3)

# data source: the "Estimation of obesity levels based on eating habits and physical condition Data Set" from the UCI Machine Learning Repository
# download url: https://archive.ics.uci.edu/ml/machine-learning-databases/00544/ObesityDataSet_raw_and_data_sinthetic (2).zip
# download the data to a temp file, unzip it and read into mydata
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00544/ObesityDataSet_raw_and_data_sinthetic (2).zip"
tmp <- tempfile()
download.file(url, tmp)
mydata <- fread(unzip(tmp, files="ObesityDataSet_raw_and_data_sinthetic.csv"))

##########################################################
# Create tables for presentation, not for data processing
##########################################################

# table to present the defintion of the obesity level
tab1 <- tribble(
~Obesity_Level, ~Definition,
"Underweight", "Less than 18.5",
"Normal", "18.5 to 24.9",
"Overweight", "25.0 to 29.9",
"Obesity I", "30.0 to 34.9",
"Obesity II", "35.0 to 39.9",
"Obesity III", "Higher than 40"
)
tab1 %>% knitr::kable()

# table to present the defintions of variables related to prior history, eating habits and physical condition
tab2 <- tribble(
~Variable, ~Definition,
"family_history_with_overweight","Has a family member suffered or suffers from overweight?",
"FAVC","Frequent consumption of high caloric food",
"FCVC","Frequency of consumption of vegetables",
"NCP","Number of main meals",
"CAEC","Consumption of food between meals",
"SMOKE","Do you smoke?",
"CH2O","Consumption of water daily",
"CALC","Consumption of alcohol",
"SCC","Calories consumption monitoring",
"FAF","Physical activity frequency",
"TUE","Time using technology devices",
"MTRANS","Transportation used"
)
tab2 %>% knitr::kable()

# table to present the defintions of variables converted to binary variables
tab3 <- tribble(
~Original_Variable, ~New_Variable, ~Definition,
"Gender", "female", "1=female",
"family_history_ with_overweight","familyhistory", "1=have a family member who suffered or suffers from overweight",
"FAVC","FAVC2", "1=eat high caloric food frequently",
"FCVC","N/A","N/A",
"NCP","N/A","N/A",
"CAEC","CAEC2", "1=eat food between meals",
"SMOKE","smoking", "1=smoke",
"CH2O","N/A","N/A",
"CALC","alcohol", "1=drink alcohol",
"SCC","SCC2", "1=monitor the calories",
"FAF","N/A","N/A",
"TUE","N/A","N/A",
"MTRANS","walkbike", "1=use bike or walk"
)
tab3 %>% knitr::kable()

##########################################################
# Data exploration
##########################################################

# Figure 1.1 Distribution of Obesity Level
mydata %>% group_by(NObeyesdad) %>% summarize(count=n()) %>% ggplot(aes(NObeyesdad, count)) + geom_bar(stat="identity", fill="skyblue") + xlab("") + theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))

# identify the dimension of the dataset
dim(mydata)

# Figure 1.2 Snapshot of the dataset
head(mydata)

# Figure 1.3 Gender Distribution
mydata %>% group_by(Gender) %>% summarize(count=n()) %>% ggplot(aes(Gender, count)) + geom_bar(stat="identity", fill="skyblue")

# % of male
mean(mydata$Gender=="Male")

# Figure 1.4 Age Distribution
hist(mydata$Age, xlab="Age", col="skyblue")

# min and max of age
min(mydata$Age)
max(mydata$Age)

# Height Distrubution
hist(mydata$Height, main="Height Distrubution", xlab="Height", col="skyblue")

# Height Distrubution by Gender
ggplot(mydata, aes(Height)) + geom_histogram(aes(color=Gender, fill=Gender), position="identity", bins = 30, alpha=0.4) + ggtitle("Height Distribution by Gender")

# Weight Distrubution
hist(mydata$Weight, main="Weight Distrubution", xlab="Weight", col="skyblue")

# Weight Distrubution by Gender
ggplot(mydata, aes(Weight)) + geom_histogram(aes(color=Gender, fill=Gender), position="identity", bins = 30, alpha=0.4) + ggtitle("Weight Distribution by Gender")

##########################################################
# Data processing
##########################################################

# convert categorical or ordinal variables to binary variables
newdata <- mydata %>%
mutate(female=ifelse(Gender=="Female",1,0),
familyhistory=ifelse(family_history_with_overweight=="yes",1,0),
FAVC2=ifelse(FAVC=="yes",1,0),
CAEC2=ifelse(CAEC=="no",0,1),
smoking=ifelse(SMOKE=="yes",1,0),
SCC2=ifelse(SCC=="yes",1,0),
alcohol=ifelse(CALC=="no",0,1),
walkbike=ifelse(MTRANS %in% c("Bike","Walking"),1,0))

# convert obesity level to binary and remove height and weight
newdata <- newdata %>% mutate(overweight=factor(ifelse(NObeyesdad %in% c("Normal_Weight","Insufficient_Weight"),0,1))) %>% 
select(female, Age, familyhistory, FAVC2, FCVC, NCP, CAEC2, smoking, CH2O, SCC2, FAF, TUE, alcohol, walkbike, overweight)

##########################################################
# Create training, test sets and final test set
##########################################################

# split the original dataset into "traindata" and "finaltest" sets, with "finaltest" 20% of the original dataset
set.seed(100, sample.kind="Rounding") # if using R 3.5 or earlier, use set.seed(100)
test_index <- createDataPartition(y=newdata$overweight, times=1, p=0.2, list=FALSE)
traindata <- newdata[-test_index,]
finaltest <- newdata[test_index,]

# split "traindata" into "train_set" and "test_set", with "test_set" 20% of the "traindata" dataset
set.seed(200, sample.kind="Rounding") # if using R 3.5 or earlier, use set.seed(200)
test_index <- createDataPartition(y=traindata$overweight, times=1, p=0.2, list=FALSE)
train_set <- traindata[-test_index,]
test_set <- traindata[test_index,]

##########################################################
# Model development
##########################################################

# Model 1: predict everyone to be overweight

# % of overweight in train_set
mean(train_set$overweight==1)
# accuracy rate to predict everyone to be overweight
r1 <- mean(test_set$overweight==1)
# store accuracy rate of Model 1 and report it in a table
models <- tibble(Model="Model 1", Name="Baseline Model", Accuracy_Rate=r1)
models %>% knitr::kable()

# Model 2: Logistic Regression with Demographic Data

# 2.1 Gender Factor
# Figure 2.1 Proportion of Overweight by Gender
train_set %>% group_by(female, overweight) %>% summarize(count=n()) %>% ggplot(aes(x=female, y=count, fill=overweight)) + geom_bar(position="fill", stat="identity") + ylab("Proportion") + xlab("Female or not")
# chi-square test for gender
chisq.test(table(train_set$female, train_set$overweight))

# 2.2 Age Factor
# Figure 2.2 Proportion of Overweight by Age
train_set %>% group_by(Age) %>% filter(n() >= 2) %>% summarize(prop=mean(overweight==1)) %>% ggplot(aes(Age, prop)) + geom_point() + geom_smooth(method = "loess") + ylab("Proportion")

# logistic regression with gender and age
train_glm1 <- train(overweight ~ female + Age, method="glm", data=train_set)
glm_p1 <- predict(train_glm1, test_set)
r2 <- mean(glm_p1==test_set$overweight)
# store accuracy rate of Model 2 and report it in a table
models <- bind_rows(models, tibble(Model="Model 2", Name="Logistic Regression with Demographic Data", Accuracy_Rate=r2))
models %>% knitr::kable()

# Model 3: Logistic Regression with Demographic Data and Family History

# Figure 2.3 Proportion of Overweight by Family History
train_set %>% group_by(familyhistory, overweight) %>% summarize(count=n()) %>% ggplot(aes(x=familyhistory, y=count, fill=overweight)) + geom_bar(position="fill", stat="identity") + ylab("Proportion") + xlab("Family History")

# logistic regression with gender, age and family history
train_glm2 <- train(overweight ~ female + Age + familyhistory, method="glm", data=train_set)
glm_p2 <- predict(train_glm2, test_set)
r3 <- mean(glm_p2==test_set$overweight)
# store accuracy rate of Model 3 and report it in a table
models <- bind_rows(models, tibble(Model="Model 3", Name="Logistic Regression with Demographic Data and Family History", Accuracy_Rate=r3))
models %>% knitr::kable()

# Model 4: Logistic Regression with Demographic Data, Family History, Habits and Physical Condition

# Figure 2.4 Proportion of Overweight by Frequent Consumption of High Caloric Food
train_set %>% group_by(FAVC2, overweight) %>% summarize(count=n()) %>% ggplot(aes(x=FAVC2, y=count, fill=overweight)) + geom_bar(position="fill", stat="identity") + ylab("Proportion") + xlab("Frequent consumption of high caloric food")

# Figure 2.5 Proportion of Overweight by Transportation Used
train_set %>% group_by(walkbike, overweight) %>% summarize(count=n()) %>% ggplot(aes(x=walkbike, y=count, fill=overweight)) + geom_bar(position="fill", stat="identity") + ylab("Proportion") + xlab("Walk or Bike")

# check correlation among variables
tab4 <- cor(train_set[,4:14])
tab4 %>% knitr::kable()

# logistic regression with all variables
train_glm3 <- train(overweight ~ ., method="glm", data=train_set)
glm_p3 <- predict(train_glm3, test_set)
r4 <- mean(glm_p3==test_set$overweight)
# store accuracy rate of Model 4 and report it in a table
models <- bind_rows(models, tibble(Model="Model 4", Name="Logistic Regression with Demographic Data, Family History, Habits and Physical Condition", Accuracy_Rate=r4))
models %>% knitr::kable()

# Model 5: KNN Model

# model training and tune K
train_knn <- train(overweight ~ ., method="knn", data=train_set, tuneGrid=data.frame(k=seq(3, 37, 2)))
train_knn$bestTune
knn_p <- predict(train_knn, test_set)
r5 <- mean(knn_p==test_set$overweight)
# store accuracy rate of Model 5 and report it in a table
models <- bind_rows(models, tibble(Model="Model 5", Name="KNN Model", Accuracy_Rate=r5))
models %>% knitr::kable()

# Model 6: Random Forest Model

# model training and tune mtry
train_rf <- train(overweight ~ ., data=train_set, method="rf", tuneGrid=data.frame(mtry=seq(1:7)))
train_rf$bestTune
rf_p <- predict(train_rf, test_set)
r6 <- mean(rf_p==test_set$overweight)
# store accuracy rate of Model 6 and report it in a table
models <- bind_rows(models, tibble(Model="Model 6", Name="Random Forest Model", Accuracy_Rate=r6))
models %>% knitr::kable()

# Model 7: Ensemble Model

# model training
ensemble <- cbind(glm=glm_p3==1, knn=knn_p==1, rf=rf_p==1)
ensemble_p <- ifelse(rowMeans(ensemble)>0.5, 1, 0) %>% factor()
r7 <- mean(ensemble_p==test_set$overweight)
# store accuracy rate of Model 7 and report it in a table
models <- bind_rows(models, tibble(Model="Model 7", Name="Ensemble Model", Accuracy_Rate=r7))
models %>% knitr::kable()

##########################################################
# Evaluate models and report model performance
##########################################################

# Table 1. Model Performance by Overall Accuracy Rate
models %>% knitr::kable()

# calculate sensitivity and specificity of each model and store in a table
baseline_p <- ifelse(test_set$overweight==0,1,1)
s1 <- sum(test_set$overweight==1 & baseline_p==1)/sum(test_set$overweight==1)
sp1 <- sum(test_set$overweight==0 & baseline_p==0)/sum(test_set$overweight==0)

s2 <- confusionMatrix(glm_p1, test_set$overweight, positive = "1")$byClass[["Sensitivity"]]
sp2 <- confusionMatrix(glm_p1, test_set$overweight, positive = "1")$byClass[["Specificity"]]

s3 <- confusionMatrix(glm_p2, test_set$overweight, positive = "1")$byClass[["Sensitivity"]]
sp3 <- confusionMatrix(glm_p2, test_set$overweight, positive = "1")$byClass[["Specificity"]]

s4 <- confusionMatrix(glm_p3, test_set$overweight, positive = "1")$byClass[["Sensitivity"]]
sp4 <- confusionMatrix(glm_p3, test_set$overweight, positive = "1")$byClass[["Specificity"]]

s5 <- confusionMatrix(knn_p, test_set$overweight, positive = "1")$byClass[["Sensitivity"]]
sp5 <- confusionMatrix(knn_p, test_set$overweight, positive = "1")$byClass[["Specificity"]]

s6 <- confusionMatrix(rf_p, test_set$overweight, positive = "1")$byClass[["Sensitivity"]]
sp6 <- confusionMatrix(rf_p, test_set$overweight, positive = "1")$byClass[["Specificity"]]

s7 <- confusionMatrix(ensemble_p, test_set$overweight, positive = "1")$byClass[["Sensitivity"]]
sp7 <- confusionMatrix(ensemble_p, test_set$overweight, positive = "1")$byClass[["Specificity"]]

models2 <- tribble(
~Model, ~Name, ~Sensitivity, ~Specificity,
"Model 1", "Baseline Model", s1, sp1,
"Model 2", "Logistic Regression with Demographic Data",s2,sp2,
"Model 3", "Logistic Regression with Demographic Data and Family History", s3, sp3,
"Model 4", "Logistic Regression with Demographic Data, Family History, Habits and Physical Condition", s4, sp4,
"Model 5", "KNN Model", s5, sp5,
"Model 6", "Random Forest Model", s6, sp6,
"Model 7", "Ensemble Model", s7, sp7
)

# Table 2. Model Performance by Sensitivity and Specificity
models2 %>% knitr::kable()

##########################################################
# Report final model performance on the "finaltest" set
##########################################################

# train model on the "traindata" set and predict overweight on the "finaltest" set
finalmodel <- train(overweight ~ ., data=traindata, method="rf", tuneGrid=data.frame(mtry=seq(1:7)))
finalmodel$bestTune
finalmodel_p <- predict(finalmodel, finaltest)

# calculate overall accuracy rate, sensitivity and specificity
finalmodel_a <- confusionMatrix(finalmodel_p, finaltest$overweight, positive = "1")$overall[["Accuracy"]]
finalmodel_s <- confusionMatrix(finalmodel_p, finaltest$overweight, positive = "1")$byClass[["Sensitivity"]]
finalmodel_sp <- confusionMatrix(finalmodel_p, finaltest$overweight, positive = "1")$byClass[["Specificity"]]

results <- tribble(
~Final_Model, ~Accuracy_Rate, ~Sensitivity, ~Specificity,
"Random Forest Model", finalmodel_a, finalmodel_s, finalmodel_sp
)

# report final model performance
results %>% knitr::kable()