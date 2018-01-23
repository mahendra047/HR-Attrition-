
################################# 1. import the dataset and packages ##################################
# install.packages("Amelia")
# install.packages("corrplot")
# install.packages("catools")
# install.packages("car")
# install.packages("caret")
data_set = read.csv('HR_Employee_Attrition_Data.csv', header = T)
#View(data_set)

###############################  2. Perform Exploratory Data Analysis ###############################
##############################   3. Interpret the output of Exploratory Data Analysis###############
str(data_set)
#summary(data_set)
#head(data_set)


########### check missing value using Amelia package ###########
library(Amelia)
missmap(data_set)
any(is.na(data_set))
#### No missing value #####
############# dataset has 35 variable now check for no of the  the numeric and factor variables in the dataset
dim(data_set)
table(sapply(data_set,class))
prop.table(table(data_set$Attrition))
table(data_set$Attrition)
#install.packages("ROSE")
library(ROSE)
data_ovun<-ovun.sample(Attrition~.,data = data_set,N=3400,method="over")$data
table(data_ovun$Attrition)
prop.table(table(data_ovun$Attrition))
### Among 35 variables 9 factor class and 26 integer class store them in different variables for analysis

factor_ID=which(sapply(data_ovun, class) == "factor")

integer_ID=which(sapply(data_ovun, class) == "integer")



#################################### 4.	Compute Information Value (IV) ###############################

## Let us find the variables Information Value
#install.packages("devtools")
library(devtools)
#install_github("riv","tomasgreif")
library(woe)

iv.plot.summary(iv.mult(data_ovun,
                        "Attrition",TRUE))

iv <- iv.mult(data_ovun,
              "Attrition",TRUE)


##############  variable-> 'Over18','EmployeeCount','StandardHours','Employeenumber'
##############             has 0.0 iv value remove it




################################### 5.	Create train_data and Hold-Out Sample ####################

############# Now split the dataset ############

library(caTools)
set.seed(123)

split= sample.split(data_ovun$Attrition,SplitRatio = 0.7)

train_data=subset(data_ovun, split==T)

HoldOut_data=subset(data_ovun, split==F)

train_set = train_data

HoldOut_set = HoldOut_data

dim(train_data)
dim(HoldOut_data)
dim(data_set)
################################ 6.	Create Visualization for all the variables #################

############## Visual representation of each variable contribution to Attrition
#install.packages("ggplot2")
require(ggplot2)
############# bar plots for all the factor variables
for( i in factor_ID){
  name = paste(names(train_set[i]),'.png', sep = '')
  png(filename =paste0('plots/',name), width = 800, height = 600, units = 'px')
  print(ggplot( train_set,
                aes(x=train_set[,i],
                    fill =train_set$Attrition))+
          xlab(names(train_set[i]))+
          ylab('Frequency')+
          geom_bar(position = 'dodge')+
          guides(fill = guide_legend('Attrition')))
  dev.off()
  print.noquote(names(train_set[i]))
  print.noquote(summary(train_set[,i]))
}


####################################### Remove the unsed varibles from the dataset ###############################

############# single category variable in categorical type variables ##############

for(i in factor_ID){
  if(length(levels(train_set[,i]))==1){
    print.noquote(paste0(names(train_set[i]),' has only 1 category and its field id is ',i))
  }
}
### Over18 has only 1 category and its field id is 22 so remove it is not contributing any 
### significant information about the attrition becuse it has only one level
################  single valued variable in interger type ##########################
for( i in integer_ID){
  if(max(train_set[,i])==min(train_set[,i])){
    print.noquote(paste0('All the values of ',names(train_set[i]),' are : ',
                         max(train_set[,i]),' and its ID is : ',i ))
  }
}
# All the values of EmployeeCount are : 1 and its ID is : 9
# All the values of StandardHours are : 80 and its ID is : 27
# remove the Employeecount and StandardHours becuse they have same value so SD=0 so not significant

##########
# we also remove Employeenumber becuse no significant
### Removing 'Over18','EmployeeCount','StandardHours','Employeenumber' 

train_set = train_set[,-c(9,10,22,27)] 
HoldOut_set = HoldOut_set[,-c(9,10,22,27)]

#View(train_set)

#################  correlation plots for all interger variables ############
#################Dropping dependent variable for calculating Multicollinearity
train_a = subset(train_set, select = -c(Attrition))

# #Identifying numeric variables
numericData <- train_a[sapply(train_a, is.numeric)]
#View(numericData)
####### Calculating Correlation
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
# Visualize Correlation Matrix
#install.packages("corplot")
library(corrplot)

corrplot(descrCor, order = "FPC", method = "circle", type = "lower",tl.cex = 0.8, tl.col = rgb(0, 0, 0))

########## Checking Variables that are highly correlated
library(car)
library(caret)
highlyCorrelated = findCorrelation(descrCor, cutoff=0.7)

#Identifying Variable Names of Highly Correlated Variables
highlyCorCol = colnames(numericData)[highlyCorrelated]

#Print highly correlated attributes
highlyCorCol

#Remove highly correlated variables and create a new dataset
#dat3 = train_set[, -which(colnames(train_set) %in% highlyCorCol)]
# dim(dat3)


#########################  8.	Build Random Forest Model on train_set Sample #####################


####################################################################
############ Random Forset on Holdout data & Tunning ################
library(randomForest)
set.seed(123)
RF_model = randomForest(x = train_set[-2],
                        y = train_set$Attrition,
                        data_set = train_set,
                        ntree =  2000,  
                        mtry =  6,
                        nodesize = 30)
RF_model
plot(RF_model, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest train_set")


# importance(RF_model)
# varImpPlot(RF_model)


########### Adjusted Random forest based on previous oob plot #################

set.seed(123)
RF_model = randomForest(x = train_set[-2],
                        y = train_set$Attrition,
                        data_set = train_set,
                        ntree =  500,  
                        mtry =  6,
                        nodesize = 30)
RF_model
plot(RF_model, main = "Error Rates Random Forest train_set")

####
############################  Adjusting mtry using tuneRF ########
set.seed(321)
tuned_RF_model = tuneRF(x = train_set[-2],
                        y = train_set$Attrition,
                        data_set = train_set,
                        mtryStart = 2,
                        stepFactor = 1.5,
                        improve = 0.001,
                        ntreeTry = 500,
                        nodesize = 40,
                        doBest = T, trace = T, plot = T,importance = T)
tuned_RF_model
#tuned_RF_model$importance
#Evaluate variable importance
importance(tuned_RF_model)
varImpPlot(tuned_RF_model)

############## tune_RF gives the best mtry 9 ##########################

############## Adjust the model parameter ##############################

set.seed(123)
RF_model = randomForest(x = train_set[-2],
                        y = train_set$Attrition,
                        data_set = train_set,
                        ntree =  500,  
                        mtry =  9,
                        nodesize = 30)
RF_model
plot(RF_model, main = "Error Rates Random Forest train_set")

#View(train_set)
dim(train_set)
############ now remove the variable "performanceRating" ,"gender",
###########  based on meandecreaseacc and meandecreasegini
train_set<-train_set[,-c(5,10)]
#View(train_set)
dim(train_set)


set.seed(123)
RF_model = randomForest(x = train_set[-2],
                        y = train_set$Attrition,
                        data_set = train_set,
                        ntree =  500,  
                        mtry =  9,
                        nodesize = 30)
RF_model
plot(RF_model, main = "High number of trees vs OOB error")

##### After removing gender and PerformanceRating OOB slightly decrease to 8.61 so remove these
###### variable

#################### finally tune the nodesize ################

temp_train_acc = numeric()
temp_nodesize = numeric()
temp_hold_acc = numeric()
j = 1
for (i in seq(from = 1, to = 800 , by = 5)){
  set.seed(123)
  RF_model = randomForest(x = train_set[-2],
                          y = train_set$Attrition,
                          data_set = train_set,
                          ntree = 500,
                          mtry =  9,
                          nodesize = i)
  if(RF_model$confusion[1]!=2058 & RF_model$confusion[2]!=2058){
    temp_train_acc[j] = (RF_model$confusion[1]+RF_model$confusion[4])/2058
    temp_nodesize[j] = i
    holdout_prediction = predict(RF_model, newdata = HoldOut_set, type ='class')
    table = table(HoldOut_set$Attrition,holdout_prediction)
    temp_hold_acc[j] = (table[1]+table[4])/882
    j = j+1
  }
}
nodesize_df = data.frame(x=temp_nodesize,y1=temp_train_acc,y2=temp_hold_acc)
############### use visualization for node size
library(ggplot2)
ggplot() +
  geom_line(data = nodesize_df,
            aes(nodesize_df$x,nodesize_df$y1),col = 'red')+
   geom_line(data = nodesize_df,
             aes(nodesize_df$x,nodesize_df$y2),col = 'green')+
  xlab('node size')+
  ylab('accuracy')+
  ggtitle('ntree is 500 & mtry is 9')

################# nodesize 9 gives the best accuracy 
################ so we choose the nodesize=400


########################### Final Random Forest Model  ####################

set.seed(123)
RF_model = randomForest(x = train_set[-2],
                        y = train_set$Attrition,
                        data_set = train_set,
                        ntree =  500,
                        mtry =  9,
                        nodesize =60)
RF_model
plot(RF_model)

train_data$RF_Prob = predict(RF_model,newdata =train_set, type='prob')
train_data$RF_Prob =train_data$RF_Prob[,2]

Holdout_prediction_Prob = predict(RF_model,newdata = HoldOut_set, type='prob')
Holdout_prediction_class =predict(RF_model,newdata = HoldOut_set, type='class')
HoldOut_set$RF_prob =Holdout_prediction_Prob[,2]
HoldOut_set$RF_class = Holdout_prediction_class
HoldOut_data$RF_prob = HoldOut_set$RF_prob
HoldOut_data$RF_Class = Holdout_prediction_class

# Classification 
Confusion_Matrix_RF=addmargins(table(actual = HoldOut_set$Attrition, Prediction = HoldOut_set$RF_class))
Confusion_Matrix_RF
Accuracy_RF=(Confusion_Matrix_RF[1]+Confusion_Matrix_RF[5])/Confusion_Matrix_RF[9]*100
Accuracy_RF
################ final model accuracy

############ GINI Index ##############
#install.packages("ineq")
library(ineq)
gini = ineq(Holdout_prediction_Prob, type="Gini")
gini



### Calculating AUC using ROC Curve and KS for the model
#install.packages("ROCR")
library(ROCR)
pred <- prediction(Holdout_prediction_Prob[,2], HoldOut_set$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="green", lwd=2, main="ROC Curve")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
KS
auc

############################### 11.	Finally test the Rank Ordering on Hold-Out Sample ###################

######################## KS Ranking ##########################################
decile <- function(x){
  deciles = vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10 ))))))))))
}
HoldOut_set$decile = decile(Holdout_prediction_Prob[,2])
require(data.table)
require(scales)
HoldOut_set$Attrition_Numeric = ifelse(HoldOut_set$Attrition=="No",0,1)

Ranking <-function(tmp_DT){
  rank = tmp_DT[, list(
    cnt = length(Attrition_Numeric), 
    cnt_resp = sum(Attrition_Numeric), 
    cnt_non_resp = sum(Attrition_Numeric == 0)) , 
    by=decile][order(-decile)]
  rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
  rank$cum_resp <- cumsum(rank$cnt_resp)
  rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
  rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
  rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
  rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
  rank$rrate <- percent(rank$rrate)
  rank$cum_rel_resp <- percent(rank$cum_rel_resp)
  rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)
  
  return(rank)
}
View(rank)

Holdout_RF_Ranking = Ranking(data.table(HoldOut_set))

train_set$Attrition_Numeric = ifelse(train_set$Attrition=="No",0,1)
train_set_Prob = predict(RF_model,newdata =train_set, type='prob')
train_set_class =predict(RF_model,newdata = train_set, type='class')
train_set$RF_prob =train_set_Prob[,2]
train_set$decile = decile(train_set_Prob[,2])
train_set$RF_class = train_set_class
train_set_RF_Ranking = Ranking(data.table((train_set)))

View(Holdout_RF_Ranking)

View(train_set_RF_Ranking)
#check ks in top 3 decile
## decile   Holdout_RF_Ranking  train_set_RF_Ranking
##  1               63%               64%
##  2               87%               95%
##  3               80%               83%

### first and third decile have slightly difference and second have 8% diff in ks 
### so we also check the class imbalances and impove it 
#########



