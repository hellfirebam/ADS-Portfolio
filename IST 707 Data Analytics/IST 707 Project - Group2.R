
# goal: predict whether the index will go up or down on a given 1d-return 

# Loading libraries
library (ISLR)
library(quantmod)
library(tidyquant)
library(class)
library(tidyr)
library(TTR)
library(ggplot2)
library (caret)
library(dplyr)
library(lubridate)
library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(ggthemes)
library(smotefamily)
library(arules)
library(arulesViz)
library(arulesCBA)
library(factoextra)
library(cluster)
library(mclust)

set.seed (1)
library(doParallel)
cl <- makePSOCKcluster(9)
registerDoParallel(cl)

# Data pre-processing

## Pull SPY data from Yahoo finance
symbol <- "SPY"
data <- getSymbols(symbol, from ="2001-01-01",to ="2021-01-01", auto.assign = FALSE)
colnames(data) <- c("Open","High","Low","Close","Volume","Adjusted")
data_df<- data.frame(data)
data_df$date <- date (rownames(data_df))

# Visualization 


## Plot line chart of daily close  
data_df %>% 
  ggplot(aes(x = date, y = Close,color=Volume)) + 
  geom_line() + 
  labs(title = "SPY Daily Close", y = "Close", x = "")+ 
  theme_few() +
  theme(legend.position = "none") 


## plot volume
data_df %>%
  ggplot(aes(x = date, y = Volume)) +
  geom_segment(aes(xend = date, yend = 0, color = Volume)) + 
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "SPY Daily Volume", 
       y = "Volume", x = "") +
  theme_few() +
  theme(legend.position = "none") 




## Calculate lagged % returns
delta<-Delt(data$Close,k=1:5)
#delta <- na.omit(delta)
delta_lag <- data.frame(lag(as.matrix(delta), 1))
df_SPY <- data.frame(cbind(delta_lag, delta$Delt.1.arithmetic))


## Create a direction and a date column 
#Direction <- ifelse(coredata(df_SPY$Delt.1.arithmetic) >= 0,"Up","Down") #Return_1d and direction can be used interchangably 
Return <- round(dailyReturn(data$Close, type='arithmetic'),2)
class <- ifelse(coredata(Return) >= 0,"Up","Down")
Date<-lubridate::ymd(rownames(df_SPY))


## Combine direction with the data frame 
df<- cbind(Date, df_SPY,class)
df <- na.omit(df)
SPY<-df[-1,]
colnames(SPY)<- c('Date','Lag_1d','Lag_2d','Lag_3d','Lag_4d','Lag_5d', 'Return_1d', 'Class')
str(SPY)

df1<-data.frame(data)
df1$Date<-date (rownames(df1))
combined<-merge(SPY, df1, by="Date")
combined$dayOfWeek<-lubridate::wday(combined$Date,label = TRUE, abbr = TRUE)
combined$dayOfMonth<-ordered(lubridate::mday(combined$Date))
combined$monthOfYear<-ordered(lubridate::month(combined$Date))
combined$weekOfYear<-ordered(lubridate::week(combined$Date))

combined$Class<-as.factor(combined$Class)

# Data visualization

g1<-ggplot(combined,aes(x=dayOfWeek, fill=Class)) + geom_bar(position="dodge", stat="count") + theme_few() +theme(legend.position='none') 
g1
g2<-ggplot(combined,aes(x=dayOfMonth, fill=Class)) + geom_bar(position="dodge", stat="count") + theme_few()+ theme(legend.position='none')
g2
g3<-ggplot(combined,aes(x=monthOfYear, fill=Class)) + geom_bar(position="dodge", stat="count") + theme_few()+ theme(legend.position='none')
g3
g4<-ggplot(combined,aes(x=weekOfYear, fill=Class)) + geom_bar(position="dodge", stat="count") + theme_few() + theme(legend.position='none')
g4

library(ggalt)


library(ggpubr)
ggarrange(g1,g2,g3,g4)


## Bar plot the count of each direction 
counta <- function (n){
  length((which(SPY$Class==n)))
}

class_count<-aggregate(x = SPY$Class, by = list(SPY$Class), FUN = counta)

ggplot(SPY,aes(x=Class,fill=Class))+ geom_bar(stat="count") + theme_few()
#barplot(class_count$x,names.arg=class_count$Group.1,ylim=c(0,3000),xlab='Class',ylab='Count')
## slightly more ups than downs - rather balanced data set 


## Scatter plot matrix
pairs(~ Lag_1d + Lag_2d + Lag_3d+ Lag_4d + Lag_5d + Return_1d, data = SPY,
      col = "purple",pch=18   # Change color by group
      #pch = c(8, 18)[SPY$Class]                          # Change points by group
      )
## positive relationships between lagged returns; no clear relationship between lagged returns and 1-day return  


pairs(~ dayOfWeek+dayOfMonth+monthOfYear+weekOfYear+ Return_1d+Lag_1d + Lag_2d + Lag_3d+ Lag_4d + Lag_5d, data = combined,col = "purple",pch=18)


plotConfusionMatrix<- function(predicted,actual){
  table<-(table(Predicted=predicted, Market=actual))
  print(table)
  cm<-confusionMatrix(table)
  print(cm$overall)
  #table <- table / rowSums(table)
  confusion_matrix <- as.data.frame(table)
  plot<-ggplot(data = confusion_matrix,
               aes(x = Predicted,y = Market)) +
    geom_tile(aes(fill = Freq)) +theme(legend.position='none')+
    #geom_text(aes(label = sprintf("%.2f", (Freq), vjust = 1))) +
    geom_text(aes(label = sprintf("%.2f", (Freq/sum(Freq)*100))), vjust = 1) +
    #geom_text(aes(label = scales::percent(Freq,accuracy = 2.2))) +
    scale_fill_gradient(low = "white",
                        high = "purple",
                        trans = "log")  +theme_gdocs()
  print(plot)
  return(cm)
}

# Decision tree
plotTree<-function(treeModel){
  #summary(treeModel)
  #fancyRpartPlot(treeModel,type=1)
  plot(treeModel)
  #plot number of splits
  rsq.rpart(treeModel)
  plotcp(treeModel)
  #printcp(ptree)
  #confusion matrix to find correct and incorrect predictions
}
predictClassModel<-function(model,testDF, type){
  summary(model)
  predicted= predict(model, subset(testDF, select=-c(Class)), type=type) 
  cm<-plotConfusionMatrix(predicted,testDF$Class)
  return(cm)
}

df1<-select(combined,c(Date,Lag_1d,Lag_2d,Lag_3d,Lag_4d,Lag_5d,Class,dayOfWeek,dayOfMonth,monthOfYear,weekOfYear))
#df1<-select(combined,c(Date,Lag_1d,Lag_2d,Lag_3d,Lag_4d,Lag_5d,Return_1d,Class))
## Split the data into training and testing data sets 
df1$dayOfWeek<-wday(df1$Date)
df1$dayOfMonth<-mday(df1$Date)
df1$monthOfYear<-month(df1$Date)
df1$weekOfYear<-week(df1$Date)

trainDF1 <-df1%>% filter(Date <= "2017-12-31")  #take until 2017 data 
trainDF1<-na.omit(trainDF1)
trainDF1<- select(trainDF1,-c(Date))
testDF1<- df1%>% filter(Date > "2017-12-31")  # take after 2017
testDF1<-na.omit(testDF1)

df_data1<-subset(trainDF1, select=-c(Class))

df_class1<-subset(trainDF1, select=c(Class))


trainDF_balanced1<- ADAS(df_data1,df_class1)

trainDF_balanced1<- trainDF_balanced1$data
trainDF_balanced1<-trainDF_balanced1 %>% 
  rename(
    Class = class
  )

trainDF1<-trainDF_balanced1
## the new dataset is now balanced 
ggplot(trainDF_balanced1,aes(x=Class,fill=Class)) + geom_bar() + theme_few()


edaDF1<-subset(df1, select=-(Date))
edaDF1$Lag_1d <- discretizeDF.supervised(Class ~ ., edaDF1[, c('Class', 'Lag_1d')], method='mdlp')$Lag_1d
edaDF1$Lag_2d <- discretizeDF.supervised(Class ~ ., edaDF1[, c('Class', 'Lag_2d')], method='mdlp')$Lag_2d
edaDF1$Lag_3d <- discretizeDF.supervised(Class ~ ., edaDF1[, c('Class', 'Lag_3d')], method='mdlp')$Lag_3d
edaDF1$Lag_4d <- discretizeDF.supervised(Class ~ ., edaDF1[, c('Class', 'Lag_4d')], method='mdlp')$Lag_4d
edaDF1$Lag_5d <- discretizeDF.supervised(Class ~ ., edaDF1[, c('Class', 'Lag_5d')], method='mdlp')$Lag_5d
edaDF1$dayOfWeek<-lubridate::wday(df1$Date,label = TRUE, abbr = TRUE)
edaDF1$monthOfYear<-lubridate::month(df1$Date,label = TRUE, abbr = TRUE)

transactionData1<- as(edaDF1,"transactions")
inspect(transactionData1)
itemFrequencyPlot(transactionData1,support=0.5)
arules_model1<-apriori(transactionData1,parameter = list(support=0.01,confidence=0.2))
summary(arules_model1)
quality(arules_model1)
#inspect(arules_model1)
plot(arules_model1,method = "grouped")
goodrules1 <- arules_model1[quality(arules_model1)$lift > 3]
plot(goodrules1,method="graph",engine="interactive")
#plot(goodrules, method = "grouped", interactive = TRUE)
plot(goodrules1, method = "grouped")
inspect(goodrules1)


kmeansDF1<-df1
kmeansDF1$dayOfWeek<-lubridate::wday(combined$Date)
kmeansDF1$dayOfMonth<-(lubridate::mday(combined$Date))
kmeansDF1$monthOfYear<-(lubridate::month(combined$Date))
kmeansDF1$weekOfYear<-(lubridate::week(combined$Date))

fviz_nbclust(select(kmeansDF1,-c(Date,Class)),FUN=hcut, method="silhouette")

kmeans1 <- kmeans(select(kmeansDF1,-c(Date,Class)),centers = 2,nstart=25,iter.max = 20)
summary(kmeans1)
plot(kmeansDF1$Lag1d,kmeansDF1$Lag5d,
     xlab = "Lagged 1 day",
     ylab = "Lagged 5 day",
     col = kmeans1$cluster)
points(kmeans1$centers,col= 1:200 , pch=8,cex=2)

#Tree model
train.control <- trainControl(method = "cv", number=5,allowParallel = T)
tree_model1<-train(Class~., 
                  data=trainDF1, 
                  method="rpart", 
                  trControl = train.control,
                  tuneGrid = expand.grid(cp = seq(0, 1, length = 10)))
plot(tree_model1)
print(tree_model1$finalModel)
plotTree(tree_model1$finalModel)
fancyRpartPlot(tree_model1$finalModel,type=1)
cm_tree1<-predictClassModel(tree_model1,testDF1,"raw")
cm_tree1$byClass
summary(tree_model1$finalModel)


nb_model1 =  train(Class~., data = trainDF1, method = "nb",
                   trControl = train.control,  
                   tuneLength=10)
#summary(model_nb1)
plot(nb_model1)
cm_nb1<-predictClassModel(nb_model1,testDF1,"raw")


knn_model1<- train(Class~., data = trainDF1, method = "knn",
                  trControl = train.control,  preProcess = c("center","scale"),
                  tuneLength=10)
plot(knn_model1)
print(knn_model1)
knnPredict1 <- predict(knn_model1,newdata = subset(testDF1, select=-c(Class,Date)) )
cm_knn1<-plotConfusionMatrix(knnPredict1,testDF1$Class)
cm_knn1$byClass



svm_model_linear1 <- train(Class~., data = trainDF1, method = "svmLinear",
                         trControl = train.control,  preProcess = c("center","scale"),
                         tuneGrid = expand.grid(C = seq(0, 2, length = 20)))
plot(svm_model_linear1)
print(svm_model_linear1)
summary(svm_model_linear1)
cm_svm1<-predictClassModel(svm_model_linear1,testDF1,"raw")

svm_model_ploy1 <- train(Class~., data = trainDF1, method = "svmPoly",
                        trControl = train.control,  preProcess = c("center","scale"),
                        tuneGrid = expand.grid(C = seq(0, 2, length = 20),scale=1,degree=3))
plot(svm_model_ploy1)
print(svm_model_ploy1)
cm_ploy1<-predictClassModel(svm_model_ploy1,testDF1,"raw")


svm_model_radial1 <- train(Class~., data = trainDF1, method = "svmRadial",
                          trControl = train.control,  preProcess = c("center","scale"),
                          tuneGrid = expand.grid(C = seq(0, 2, length = 10),sigma=seq(0, 1, length = 10)))
plot(svm_model_radial1)
print(svm_model_radial1)
cm_radial1<-predictClassModel(svm_model_radial1,testDF1,"raw")




rf_model1<- train(Class~., data = trainDF1, method = "rf",
                 trControl = train.control,  preProcess = c("center","scale"),
                 tuneLength=10)

plot(rf_model1)
print(rf_model1)
cm_rf1<-predictClassModel(rf_model1,testDF1,"raw")
cm_rf1$byClass
# Test a different approach 

# Calculate lagged technical indicators  
#RSI, SMA, LMA and ADX (Welles Wilder's Directional Movement Index)
closeprice <- data$Close

rsi <- round(RSI(closeprice, n = 14, maType="WMA"),1)
rsi <- c(NA,head(rsi,-1))   #lagged data 

SMA<- 5
LMA <- 50

sma <- round(SMA(closeprice, SMA),1)
sma <- c(NA,head(sma,-1))

lma <- round(SMA(closeprice, LMA),1)
lma <- c(NA,head(lma,-1))


data_ADX <- data.frame (ADX(data[,c("High","Low","Close")]))
adx <- round(data_ADX$ADX,1)
adx <- c(NA,head(adx,-1))


# Create a class for direction  
Return <- round(dailyReturn(data$Close, type='arithmetic'),2)
#colnames(data) <- c("Open","High","Low","Close","Volume","Adjusted","Return")

#class <- character(nrow(data))
class <- ifelse(coredata(Return) >= 0,"Up","Down")

#Correlation
dfADX<-data.frame(Return,rsi,sma,lma,adx)
colnames(dfADX)<-c('Return_1d','RSI','SMA','LMA','ADX')
dfADX<-na.omit(dfADX)
pairs(~ RSI + SMA + LMA+ ADX + Return_1d, data =dfADX ,
      col = "purple",pch=18   # Change color by group
      #pch = c(8, 18)[SPY$Class]                          # Change points by group
)


# Create a dataframe
df2<-data.frame(rownames(data_df),class,rsi,sma,lma,adx)
colnames(df2)<-c('Date','Class','RSI','SMA','LMA','ADX')
str(df2)



df2$dayOfWeek<-lubridate::wday(df2$Date,label = TRUE, abbr = TRUE)
df2$dayOfMonth<-ordered(lubridate::mday(df2$Date))
df2$monthOfYear<-ordered(lubridate::month(df2$Date))
df2$weekOfYear<-ordered(lubridate::week(df2$Date))
df2$Class<-as.factor(df2$Class)
# viz 
ggplot(data=df2, aes(x=Class, fill=Class)) + geom_bar() + theme_few()

## significantly more ups than downs - imbalanced data set


ggplot(data=df2, aes(x=ADX, y=RSI, color=Class)) + geom_point(alpha=0.6) + theme_few()
ggplot(data=df2, aes(x=Class,color=Class, y=ADX)) + geom_boxplot() + theme_few()
## not clear separation/classification

tempDF<-df2
tempDF$dayOfWeek<-wday(tempDF$Date)
tempDF$dayOfMonth<-mday(tempDF$Date)
tempDF$monthOfYear<-month(tempDF$Date)
tempDF$weekOfYear<-week(tempDF$Date)
# create a balanced sample data set 
trainDF2 <-tempDF%>% filter(Date <= "2017-12-31")  #take until 2017 data 
trainDF2<-na.omit(trainDF2)
trainDF2<- select(trainDF2,-c(Date))
testDF2<- tempDF%>% filter(Date > "2017-12-31")  # take after 2017
testDF2<-na.omit(testDF2)



df_data2<-subset(trainDF2, select=-c(Class))

df_class2<-subset(trainDF2, select=c(Class))


trainDF_balanced2<- ADAS(df_data2,df_class2)

trainDF_balanced2<- trainDF_balanced2$data
trainDF_balanced2<-trainDF_balanced2 %>% 
  rename(
    Class = class
  )
## the new dataset is now balanced 
ggplot(trainDF_balanced2,aes(x=Class,fill=Class)) + geom_bar() + theme_few()


train.control <- trainControl(method = "cv", number=5,allowParallel = T)


fviz_nbclust(select(df2,-c(Date,Class)),FUN=hcut, method="silhouette")


kmeansDF2<-df2
kmeansDF2$dayOfWeek<-lubridate::wday(df2$Date)
kmeansDF2$dayOfMonth<-(lubridate::mday(df2$Date))
kmeansDF2$monthOfYear<-(lubridate::month(df2$Date))
kmeansDF2$weekOfYear<-(lubridate::week(df2$Date))
kmeansDF2<-na.omit(kmeansDF2)
kmeans2 <- kmeans(select(kmeansDF2,-c(Date,Class)),centers = 2,nstart=25,iter.max = 20)
summary(kmeans2)

kmeansDF2$kmeans<- as.factor(kmeans2$cluster)
ggplot(kmeansDF2, aes(x=LMA,fill=kmeans,group=kmeans,color=kmeans))+geom_bar(stat="count") + theme_few()

plot(kmeansDF2$LMA,kmeansDF2$ADX,
     xlab = "LMA",
     ylab = "ADX",
     col = kmeans2$cluster)
points(kmeans2$centers,col= 1:2 , pch=8,cex=2)

fviz_cluster(kmeans2,data=select(kmeansDF2,-c(kmeans,Class,Date)),geom="point")
#clusplot(kmeans2,select(kmeansDF2,c(kmeans())))

nb_model2 =  train(Class~., data = trainDF_balanced2, method = "nb",
                   trControl = train.control,  
                   tuneLength=10)
#summary(model_nb1)
plot(nb_model2)
cm_nb2<-predictClassModel(nb_model2,testDF2,"raw")



#HC
hclustDF2<-df2
hclustDF2$dayOfWeek<-lubridate::wday(df2$Date)
hclustDF2$dayOfMonth<-(lubridate::mday(df2$Date))
hclustDF2$monthOfYear<-(lubridate::month(df2$Date))
hclustDF2$weekOfYear<-(lubridate::week(df2$Date))
hclustDF2<-na.omit(hclustDF2)

hclustDF2_train<-select(hclustDF2,-c(Date,Class))

distMatrix_E <- dist(hclustDF2_train, method="euclidean")
distMatrix_M <- dist(hclustDF2_train, method="manhattan")
distMatrix_Mi <- dist(hclustDF2_train, method="minkowski", p=5)

groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1)
rect.hclust(groups_E, k=2)
summary(groups_E)


##  Manhattan
groups_M_n <- hclust(distMatrix_M,method="ward.D")
plot(groups_M_n)
rect.hclust(groups_M_n, k=2)
summary(groups_M_n)

##  Minkowski
groups_Mi <- hclust(distMatrix_Mi,method="ward.D")
plot(groups_Mi_n, cex=0.9, hang=-1)
rect.hclust(groups_Mi_n, k=2)



tree_model2<-train(Class~., 
      data=trainDF_balanced2, 
      method="rpart", 
      trControl = train.control,
      tuneGrid = expand.grid(cp = seq(0, 1, length = 10)))

plot(tree_model2)
print(tree_model2)
#tree_model_1<- rpart (Class~., data=trainDF_balanced1, control=rpart.control(cp=.0001))
plotTree(tree_model2$finalModel)
summary(tree_model2$finalModel$terms)
tree_model2$finalModel$cptable
rpart.rules(tree_model2$finalModel)
#fancyRpartPlot(tree_model$finalModel)
tree_cf_unpruned<-predictClassModel(tree_model2$finalModel,testDF,"class")
tree_finalModel2<-tree_model2$finalModel
ptree2<- prune(tree_finalModel2, 
               cp=tree_finalModel2$cptable[which.max(tree_finalModel2$cptable[,"rel error"]),"CP"])


plotTree(tree_finalModel2)
cm_tree2<-predictClassModel(ptree2,testDF2,"class")
cm_tree2$byClass
#tree_finalModel$cptable[,"xerror"]

fancyRpartPlot(tree_finalModel2)


#Just for presentation , to shop top splits
tree_model_temp<- rpart (Class~., data=trainDF_balanced2)
fancyRpartPlot(tree_model_temp,type=4,clip.right.labs = FALSE,branch=.3)


# find the best splits - more 0s indicate better split
bestSplits <- apply(trainDF[-1], 2, function(col){
  t <- table(trainDF$Class,col)
  sum(t == 0)
})
order <- order(bestSplits,decreasing = TRUE)
bestSplits <- bestSplits[order]
par(mar=c(10,2,2,2))
barplot(bestSplits,
        main="Best splits vs Variables",
        xlab="Variables",ylab="",las=2,col="gold")

#KNN
knn_model2<- train(Class~., data = trainDF_balanced2, method = "knn",
                   trControl = train.control,  preProcess = c("center","scale"),
                   tuneLength=10)

plot(knn_model2)
print(knn_model2)
knnPredict2 <- predict(knn_model2,newdata = subset(testDF2, select=-c(Class,Date)) )
cm_knn2<-plotConfusionMatrix(knnPredict2,testDF2$Class)
cm_knn2$byClass
#predictClassModel(knn_model,"raw")


#SVM

# Train the model


svm_model_linear2 <- train(Class~., data = trainDF_balanced2, method = "svmLinear",
                          trControl = train.control,  preProcess = c("center","scale"),
                          tuneGrid = expand.grid(C = seq(0, 2, length = 20)))

plot(svm_model_linear2)
print(svm_model_linear2)
cm_svm2<-predictClassModel(svm_model_linear2,testDF2,"raw")
cm_svm2$byClass

#plot(svm_model_linear2$finalModel, trainDF_balanced2)

svm_model_ploy2 <- train(Class~., data = trainDF_balanced2, method = "svmPoly",
                        trControl = train.control,  preProcess = c("center","scale"),
                        tuneGrid = expand.grid(C = seq(0, 2, length = 20),scale=1,degree=3))
plot(svm_model_ploy2)
print(svm_model_ploy2)
cm_ploy2<-predictClassModel(svm_model_ploy2,testDF2,"raw")


svm_model_radial2 <- train(Class~., data = trainDF_balanced2, method = "svmRadial",
                          trControl = train.control,  preProcess = c("center","scale"),
                          tuneGrid = expand.grid(C = seq(0, 2, length = 10),sigma=seq(0, 1, length = 10)))
plot(svm_model_radial2)
print(svm_model_radial2)
cm_radial2<-predictClassModel(svm_model_radial2,testDF2,"raw")




rf_model2<- train(Class~., data = trainDF_balanced2, method = "rf",
                   trControl = train.control,  preProcess = c("center","scale"),
                   tuneLength=10)

plot(rf_model2)
print(rf_model2)
cm_rf2<-predictClassModel(rf_model2,testDF2,"raw")
cm_rf2$byClass





## Final conclusion: tried svm and random forest which don't significantly improve accuaracy - in line with Random Walk theory 

#EDA
edaDF<-subset(tempDF, select=-(Date))
edaDF$RSI <- discretizeDF.supervised(Class ~ ., edaDF[, c('Class', 'RSI')], method='mdlp')$RSI
edaDF$SMA <- discretizeDF.supervised(Class ~ ., edaDF[, c('Class', 'SMA')], method='mdlp')$SMA
edaDF$LMA <- discretizeDF.supervised(Class ~ ., edaDF[, c('Class', 'LMA')], method='mdlp')$LMA
edaDF$ADX <- discretizeDF.supervised(Class ~ ., edaDF[, c('Class', 'ADX')], method='mdlp')$ADX
edaDF$dayOfWeek<-lubridate::wday(tempDF$Date,label = TRUE, abbr = TRUE)
edaDF$monthOfYear<-lubridate::month(tempDF$Date,label = TRUE, abbr = TRUE)

transactionData<- as(edaDF,"transactions")
inspect(transactionData)
itemFrequencyPlot(transactionData,support=0.5)

arules_model<-apriori(transactionData,parameter = list(support=0.01,confidence=0.5))

summary(arules_model)

quality(arules_model)
#inspect(arules_model)

plot(arules_model,method = "grouped")

goodrules <- arules_model[quality(arules_model)$lift > 2.5]

plot(goodrules,method="graph",engine="interactive")

#plot(goodrules, method = "grouped", interactive = TRUE)

plot(goodrules, method = "grouped")

inspect(goodrules)



#Comparison

result1<-list(cm_nb1,cm_tree1,cm_knn1,cm_svm1,cm_ploy1,cm_radial1,cm_rf1)
result2<-list(cm_nb2,cm_tree2,cm_knn2,cm_svm2,cm_ploy2,cm_radial2,cm_rf2)
models1<-c("nb1","dtree1","knn1","svm_linear1","svm_poly1","svm_radial1","rf1")
models2<-c("nb2","dtree2","knn2","svm_linear2","svm_poly2","svm_radial2","rf2")

#initalize empty df
overall<-t(data.frame(cm_nb1$overall))
overall<-overall[FALSE,]
for(i in result1){
  overall<-rbind(overall,t(data.frame(i$overall)))
}
for(i in result2){
  overall<-rbind(overall,t(data.frame(i$overall)))
}

rownames(overall)<-c(models1,models2)
overall<-data.frame(overall)
overall$model<-rownames(overall)

byClass<-t(data.frame(cm_nb1$byClass))
byClass<-byClass[FALSE,]
for(i in result1){
  byClass<-rbind(byClass,t(data.frame(i$byClass)))
}
for(i in result2){
  byClass<-rbind(byClass,t(data.frame(i$byClass)))
}

rownames(byClass)<-c(models1,models2)
byClass<-data.frame(byClass)
byClass$model<-rownames(byClass)



k_fold<-list()
#training accuracy
m1<-list(nb_model1,tree_model1,knn_model1,svm_model_linear1,svm_model_ploy1,svm_model_radial1,rf_model1)
m2<-list(nb_model2,tree_model2,knn_model2,svm_model_linear2,svm_model_ploy2,svm_model_radial2,rf_model2)
j<-1
for(i in m1){
  l<-c(i$results$Accuracy)
  print(max(l[!is.nan(l)]))
  k_fold[[j]]<-(max(l[!is.nan(l)]))
  j<-j+1
}


for(i in m2){
  l<-c(i$results$Accuracy)
  print(max(l[!is.nan(l)]))
  k_fold[[j]]<-(max(l[!is.nan(l)]))
  j<-j+1
}

byClass$k_fold<-unlist(k_fold)

total<- merge(overall,byClass,by="model")

write.csv(total,row.names = FALSE)

stopCluster(cl)