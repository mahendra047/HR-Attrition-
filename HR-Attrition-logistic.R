###################### 1. import the data set and packages ##################################
# install.packages("Amelia")
# install.packages("corrplot")
# install.packages("catools")
# install.packages("car")
# install.packages("caret")
LR_DF <- read.table("HR_Employee_Attrition_Data.csv",sep = ",", header = T)
View(LR_DF)

##################### 2.	Perform Exploratory Data Analysis #####################

str(LR_DF)
summary(LR_DF)

########### a. check missing value using Amelia package ###########
library(Amelia)
missmap(LR_DF)
any(is.na(LR_DF))
#### No missing value #####
############# dataset has 35 variable now check for no of the  the numeric and factor variables
############## in the dataset
dim(LR_DF)
table(sapply(LR_DF,class))
prop.table(table(LR_DF$Attrition))
table(LR_DF$Attrition,LR_DF$Age)
# Employee ranges from age 18 to 60 which is pretty common
# We can see that employees between the age group 25-35 have a high chance of attrition

table(LR_DF$Attrition,LR_DF$TotalWorkingYears)
# Also,Employess who gain 1 year of experience have atleast 50% chances of attrition
# and after 15 years of experience the churn rate is very low

table(LR_DF$Attrition,LR_DF$OverTime)
# Employees who have to do over time have high chances of attrition

table(LR_DF$Attrition,LR_DF$MaritalStatus)
# Single employees often tend to change their jobs frequently



### Among 35 variables 9 factor class and 26 integer class store them in different variables for analysis

factor_ID=which(sapply(LR_DF, class) == "factor")

integer_ID=which(sapply(LR_DF, class) == "integer")

################# Visual representation of each variable contribution to Attrition ######
#install.packages("ggplot2")
require(ggplot2)
# bar plots for all the factor variables
for( i in factor_ID){
  name = paste(names(LR_DF[i]),'.png', sep = '')
  png(filename =paste0('plots/',name), width = 800, height = 600, units = 'px')
  print(ggplot( LR_DF,
                aes(x=LR_DF[,i],
                    fill =LR_DF$Attrition))+
          xlab(names(LR_DF[i]))+
          ylab('Frequency')+
          geom_bar(position = 'dodge')+
          guides(fill = guide_legend('Attrition')))
  dev.off()
  print.noquote(names(LR_DF[i]))
  print.noquote(summary(LR_DF[,i]))
}

####################### Remove the unsed varibles from the dataset ###############################

############# single category variable in categorical type variables ##############

for(i in factor_ID){
  if(length(levels(LR_DF[,i]))==1){
    print.noquote(paste0(names(LR_DF[i]),' has only 1 category and its field id is ',i))
  }
}
### Over18 has only 1 category and its field id is 22 so remove it is not contributing any 
### significant information about the attrition becuse it has only one level

################  single valued variable in interger type ##########################
for( i in integer_ID){
  if(max(LR_DF[,i])==min(LR_DF[,i])){
    print.noquote(paste0('All the values of ',names(LR_DF[i]),' are : ',
                         max(LR_DF[,i]),' and its ID is : ',i ))
  }
}
# All the values of EmployeeCount are : 1 and its ID is : 9
# All the values of StandardHours are : 80 and its ID is : 27
# remove the Employeecount and StandardHours becuse they have same value so SD=0 so not significant
# we also remove Employeenumber becuse no significant 
### Removing 'Over18','EmployeeCount','StandardHours','Employeenumber' 

LR_DF = LR_DF[,-c(9,10,22,27)] 

# View(LR_DF)
dim(LR_DF)


## What if I want the percentile distribution for all the fields
apply(LR_DF[,sapply(LR_DF, is.numeric)], 
      2, quantile, 
      probs=c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
      na.rm=T)
boxplot(LR_DF$MonthlyIncome , 
        main= "Monthly Income Box Plot" ,
        xlab = "Overall Base"
)

boxplot(LR_DF$MonthlyRate , 
        main= "MonthlyRate Box Plot" ,
        xlab = "Overall Base"
)
boxplot(LR_DF$YearsAtCompany , 
        main= "YearsAtCompany Box Plot" ,
        xlab = "Overall Base"
)
## Typically we floor and cap the variables at P1 and P99. 
## Let us cap the Balance variable at P99.
# LR_DF$YearsAtCompanyCap <- 
#   ifelse(LR_DF$YearsAtCompany > 31, 31, LR_DF$YearsAtCompany)
# 
# summary(LR_DF$YearsAtCompanyCap)
# sd(LR_DF$YearsAtCompanyCap)
# 
# quantile(LR_DF$YearsAtCompanyCap, 
#          c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))
# boxplot(LR_DF$YearsAtCompanyCap , 
#         main= "YearsAtCompany Box Plot" ,
#         xlab = "Overall Base"
# )
train_a = subset(LR_DF, select = -c(Attrition))

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

# #Remove highly correlated variables and create a new dataset
# LR_DF = LR_DF[, -which(colnames(LR_DF) %in% highlyCorCol)]
# dim(LR_DF)

#install.packages("devtools")
library(devtools)
#install_github("riv","tomasgreif")
library(woe)

iv.plot.summary(iv.mult(LR_DF,
                        "Attrition",TRUE))

iv <- iv.mult(LR_DF,"Attrition",TRUE)

iv
## Pattern Detection

LR_DF$Attrition<-as.numeric(LR_DF$Attrition)

LR_DF$Attrition<-ifelse(LR_DF$Attrition==2,1,0)

source("C:/Users/Mahendra Thakur/Documents/Visualization/Visualization.R")
output_folder = "C:/Users/Mahendra Thakur/Documents/Visualization/"
Target_var_name = "Attrition"
col_list = colnames(LR_DF)[
  lapply(LR_DF, class) %in% c("numeric", "integer")
  ]
col_list
for (i in 1 : length(col_list)) {
  fn_biz_viz(df = LR_DF, target = Target_var_name, var = col_list[i])
}


######################### Now split the dataset ############
mydata <- LR_DF

mydata$random <- runif(nrow(mydata), 0, 1)
mydata.dev <- mydata[which(mydata$random <= 0.5),]
mydata.val <- mydata[which(mydata$random > 0.5 
                           & mydata$random <= 0.8 ),]
mydata.hold <- mydata[which(mydata$random > 0.8),]
nrow(mydata)
nrow(mydata.dev)
nrow(mydata.val)
nrow(mydata.hold)

sum(mydata$Attrition) / nrow(mydata)
sum(mydata.dev$Attrition)/ nrow(mydata.dev)
sum(mydata.val$Attrition)/ nrow(mydata.val)
sum(mydata.hold$Attrition)/ nrow(mydata.hold)

##install.packages("aod")
##install.packages("ggplot2")
##library(aod)
##library(ggplot2)

mylogit1 <- glm(
  Attrition ~., 
  data = mydata.dev, family = "binomial"
)
summary(mylogit1)
##########################
library(MASS)
fit=stepAIC(mylogit1,direction = "both")
summary(fit)
############################ After stepAIC ###########

mylogit<-glm(formula = Attrition ~ Age + BusinessTravel + DailyRate + 
      DistanceFromHome +  
      HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + 
      MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
      TotalWorkingYears + TrainingTimesLastYear +
      YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
      YearsWithCurrManager, family = "binomial", data = mydata.dev)

summary(mylogit)

############## remove the variable IV<.1

##install.packages("car")
library(car)
vif(mylogit)

mylogit<-glm(formula = Attrition ~ Age + BusinessTravel + DailyRate + 
               DistanceFromHome + EducationField +
               HourlyRate + JobInvolvement + JobRole + JobSatisfaction +
               MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
               TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance  
                + YearsInCurrentRole + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = mydata.dev)

summary(mylogit)

library(car)
vif(mylogit)

################


## Rank Ordering Test
## Calculating the probabilities and create deciles
mydata.dev$prob <- predict(mylogit, mydata.dev, type="response")
mydata.dev$deciles <- decile(mydata.dev$prob)
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)

tmp_DT = data.table(mydata.dev)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,3);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),3);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),3);
rank$ks <- percent(abs(rank$cum_rel_resp - rank$cum_rel_non_resp));
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)


#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE***#

concordance=function(y, yhat)
{
  Con_Dis_Data = cbind(y, yhat) 
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}
#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE ENDS***#


concordance_output = concordance(mydata.dev$Attrition, mydata.dev$prob)
concordance_output

############ GINI Index ##############
#install.packages("ineq")
library(ineq)
gini = ineq(mydata.dev$prob, type="Gini")
gini
####################### Calculating AUC using ROC Curve and KS for the model
#install.packages("ROCR")
library(ROCR)
pred <- prediction(mydata.dev$prob, mydata.dev$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="green", lwd=2, main="ROC Curve")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
KS
auc
################################# Validation of Model ####################

mylogit_val<-glm(formula = Attrition ~ Age + BusinessTravel + DailyRate + 
               DistanceFromHome + EducationField +
               HourlyRate + JobInvolvement + JobRole + JobSatisfaction +
               MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
               TotalWorkingYears + TrainingTimesLastYear  
             + YearsInCurrentRole + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = mydata.val)

summary(mylogit_val)

################


## Rank Ordering Test
## Calculating the probabilities and create deciles
mydata.val$prob <- predict(mylogit_val, mydata.val, type="response")
mydata.val$deciles <- decile(mydata.val$prob)
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)

tmp_DT = data.table(mydata.val)
rank.val <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
rank.val$rrate <- round (rank.val$cnt_resp / rank.val$cnt,3);
rank.val$cum_resp <- cumsum(rank.val$cnt_resp)
rank.val$cum_non_resp <- cumsum(rank.val$cnt_non_resp)
rank.val$cum_rel_resp <- round(rank.val$cum_resp / sum(rank.val$cnt_resp),3);
rank.val$cum_rel_non_resp <- round(rank.val$cum_non_resp / sum(rank.val$cnt_non_resp),3);
rank.val$ks <- percent(abs(rank.val$cum_rel_resp - rank.val$cum_rel_non_resp));
rank.val$rrate <- percent(rank.val$rrate)
rank.val$cum_rel_resp <- percent(rank.val$cum_rel_resp)
rank.val$cum_rel_non_resp <- percent(rank.val$cum_rel_non_resp)

View(rank.val)



concordance_output.val = concordance(mydata.val$Attrition, mydata.val$prob)
concordance_output.val

############ GINI Index ##############
#install.packages("ineq")
library(ineq)
gini.val = ineq(mydata.val$prob, type="Gini")
gini.val
####################### Calculating AUC using ROC Curve and KS for the model
#install.packages("ROCR")
library(ROCR)
pred <- prediction(mydata.val$prob, mydata.val$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="green", lwd=2, main="ROC Curve")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
KS
auc

# df<-data.frame(mylogit$coefficients,mylogit_val$coefficients)
# summary(mylogit)

################## 11.	Finally test the Rank Ordering on Hold-Out Sample ##########
## Scoring syntax
mydata.hold$prob <- predict(mylogit, mydata.hold, type="response")
mydata.hold$deciles <- decile(mydata.hold$prob)

tmp_DT = data.table(mydata.hold)
h_rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);
library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)


