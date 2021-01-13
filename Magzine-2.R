library(tidyverse)
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
magzine<- marketing
head(magzine)
map(magzine,class)
magzine<- magzine%>%
  mutate(`Education`= as.character(`Education`),
         `Marital_Status`= as.character(`Marital_Status`),
         `Dt_Customer`= as.numeric(`Dt_Customer`))
map(magzine,class)
ymd(magzine$Dt_Customer)
#checking for outliers
summary(magzine)
boxplot(magzine$Income,
        ylab="Income")
#find outliers value
OutVals= boxplot(magzine$Income)$out
OutVals

#Remove outliers from Income
outliers <- boxplot(magzine$Income, plot=FALSE)$out
print(outliers)
magzine[which(magzine$Income %in% outliers),]
magzine <- magzine[-which(magzine$Income %in% outliers),]
boxplot(magzine$Income)

#Checking for missing values
summary<- magzine%>%
  summarise(mean(Income,na.rm = TRUE))
summary_2<- magzine%>%
  drop_na(Income)%>%
  summarize(income_avg= mean(Income, na.rm = TRUE))
na_count<- colSums(is.na(magzine))
View(na_count)
magzine1<- magzine%>%
  mutate(`Income`= replace_na(`Income`,51633.64))%>%
  summarise(mean(Income))
magzine2<- magzine%>%
  drop_na(Income)%>%
  summarise(mean(Income))
# We can drop or replace as both have same value of average

#Replacing all the NA with average values
magzine<- magzine%>%
  mutate(`Income`= replace_na(`Income`,51633.64))

#Replacing numbers for character variables
summary_by_Education <- magzine%>% 
  group_by(Education)%>%
  summarise(mean(Income))
 
summary_by_Marital_Status <- magzine%>% 
  group_by(Marital_Status)%>%
  summarise(mean(Income))
df <- magzine %>% 
  mutate(Education = factor(Education,
                         levels = c("2n Cycle","Basic","Graduation","Master","PhD"),
                         labels = c(1,2,3,4,5)))%>%
  mutate(Marital_Status = factor(Marital_Status,
                            levels = c("Absurd","Alone","Divorced","Married","Single","Together","Widow","YOLO"),
                            labels = c(1,2,3,4,5,6,7,8)))
df1<- df%>%
  mutate(`Education`= as.numeric(`Education`),
         `Marital_Status`= as.numeric(`Marital_Status`),
         `Dt_Customer`= as.numeric(`Dt_Customer`))
#Correlation Matrix
magzine4<- df%>%
  select(-Dt_Customer)
cor(df1)

#linear model
magzine_lm_model<- lm(Response ~ MntWines + MntMeatProducts+ NumCatalogPurchases + AcceptedCmp3 + AcceptedCmp5 +AcceptedCmp1, data = magzine)
summary(magzine_lm_model)

#KNN model
set.seed(1)
library(caret)
train_indices <- createDataPartition(y = magzine[["Response"]],
                                     p = 0.7,
                                     list = FALSE)
train_listings <- magzine[train_indices,]
test_listings <- magzine[-train_indices,]
knn_grid <- expand.grid(k = 1:20)
train_control <- trainControl(method = "cv",number = 5)
knn_model <- train(Response~ MntWines + MntMeatProducts+ NumCatalogPurchases + AcceptedCmp3 + AcceptedCmp5 +AcceptedCmp1,
                   data = train_listings,
                   method = "knn",
                   trControl = train_control,
                   preProcess = c("center", "scale"),
                   tuneGrid= knn_grid)
              
plot(knn_model)
knn_model

#GLMmodel
library(plotROC)
ind <- sample(2, nrow(magzine), replace = T, prob = c(0.8, 0.2))
train1 <- magzine[ind==1,]
test1 <- magzine[ind==2,]
debt.glm <- glm(Response ~ MntWines + MntMeatProducts+ NumCatalogPurchases + AcceptedCmp3 + AcceptedCmp5 +AcceptedCmp1, family = binomial, data = train1)

step(debt.glm, direction="both")
step.model <- debt.glm %>% stepAIC(trace = FALSE)
coef(step.model)
probabilities <- predict(step.model, test1, type = "response")
vif(debt.glm)
optCutOff <- optimalCutoff(test1$Response, probabilities)[1] 
optCutOff
misClassError(test1$Response, probabilities, threshold = optCutOff)
plotROC(test1$Response, probabilities)
Concordance(test1$Response, probabilities)
confusionMatrix(test1$Response, probabilities, threshold = optCutOff)

model.null <- glm(`Response` ~ 1, family = "binomial" ,data = train1)
step.BIC <- step(model.null, scope = list(lower = model.null, upper = debt.glm), direction = "both", k = log(nrow(train1)))
summary(step.BIC)
pred <- predict(step.BIC, test1, type = "response")
optCutOff1 <- optimalCutoff(test1$Response, pred)[1]
confusionMatrix(test1$Response, pred, threshold = optCutOff1)
data.frame(
  RMSE = RMSE(pred, test1$Response),
  R2 = R2(pred, test1$Response)
)


