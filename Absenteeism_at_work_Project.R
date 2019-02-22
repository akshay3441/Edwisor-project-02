#remove all the objects stored

rm(list=ls(all=T))

#Check current working directory
getwd() 

#Set current working directory

setwd("C:/Users/on846/Downloads/Edwisor notes/Project_2/project details")





#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','xlsx')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

## we will load the data

df = read.xlsx('Absenteeism_at_work_Project.xls', sheetIndex = 1)

## Now we will segragate continuous and catagorical variable #####################################

cont_var = c('Transportation.expense','Distance.from.Residence.to.Work','Service.time','Age','Work.load.Average.day.','Hit.target','Weight','Height','Body.mass.index','Absenteeism.time.in.hours')
cat_var = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')


##################################Missing Values Analysis###############################################
# we will create a different dataset which contains missing value counts and it will be a column level operation

missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))

#convert row names into columns 
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL

#Now rename 1st variable name

names(missing_val)[1]= "Missing_percentage"

#Calculate percentage 

missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df)) * 100
# we will consider all the missing values as the values are way less then 30% in dataaset..

#Arrange in descending order

missing_val = missing_val[order(-missing_val$Missing_percentage),]

#Rearranging columns

missing_val = missing_val[,c(2,1)]

#Write output results back in to disk

write.csv(missing_val, "Miising_perc.csv", row.names = F)

##Now we can also plot the graphs of missing values using below line of code


library(ggplot2)
ggplot(data = missing_val[1:19,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
ggtitle("Missing data percentage (Absenteesism)") + theme_bw()

# Now we will impute the missing values and to do that we will deliberately create a missing value to check which method(Mean,median,knn imputation works better)

##### comparision of values for body mass index at 22nd row##

# Actual value = 31
# mean value = 26.7
#Median value = 25
#knn imputation = 31

# Mean method :


#df$ID[is.na(df$ID)] = mean(df$ID, na.rm = T)

#Median Method

#df$ID[is.na(df$ID)] = median(df$ID, na.rm = T)

# kNN Imputation

df = knnImputation(df, k = 3)

sum(is.na(df))

## So we have applied KNN imputation method to fill our missing values


############################ Outlier Analysyis#########################################################

# Boxplot for continuous variables
for (i in 1:length(cont_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cont_var[i]), x = "Absenteeism.time.in.hours"), data = subset(df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cont_var[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of absenteeism",cont_var[i])))
}

#now we will plot and extract the plots together..

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,ncol=5)
gridExtra::grid.arrange(gn6,gn7,gn8,gn9,gn10,ncol=5)

# #Remove outliers using boxplot method

# #loop to remove from all variables
for(i in cont_var)
{
  print(i)
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  df = df[which(!df[,i] %in% val),]
}

#Replace all outliers with NA and impute
for(i in cont_var)
{
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  df[,i][df[,i] %in% val] = NA
}
## Imputing missing vaues using KNN
df = knnImputation(df,k=3)
## We will run KNN imputaiod method to fill our outliers..


################################## Feature Selection #########################################################

## As the dataset contains all continuous variable so we will apply correlation plot to extract relevant features out of it

## Correlation Plot 
corrgram(df[,cont_var], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")



##Anova test 

summary(aov(formula = Absenteeism.time.in.hours~ID,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Reason.for.absence,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Month.of.absence,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Day.of.the.week,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Seasons,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Disciplinary.failure,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Education,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Social.drinker,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Social.smoker,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Son,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Pet,data = df))

## Dimension Reduction
df = subset(df, select = -c(Hit.target,Weight,Age))


## Now our feature selection is done and now we have 18 variables left..

############################## Feature Scaling #############################################################

#Normality check

cont_var = c('Transportation.expense','Distance.from.Residence.to.Work','Service.time','Work.load.Average.day.','Height','Body.mass.index','Absenteeism.time.in.hours')
cat_var = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')


# Normalization
for(i in cont_var)
{
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/(max(df[,i])-min(df[,i]))
}

# #Standardisation
 #for(i in cnames1){
  # print(i)
   #df[,i] = (df[,i] - mean(df[,i]))/
                                  #sd(df[,i])
 #} 

###################################Model Development(Machine learning algorithm#######################################
## decision tree:

#Divide the data into train and test
#set.seed(123)
train_index = sample(1:nrow(df), 0.8 * nrow(df))
train = df[train_index,]
test = df[-train_index,]


#Load Libraries
library(rpart)

# ##rpart for regression
fit = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")


#Predict for new test cases
predictions_DT = predict(fit, test[,-18])


predictions_DT

## Now we will calculate RMSE 

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- test[,18] - predictions_DT

rmse(test[,18])     

### RMSE = 0.14 (overall)
##RMSE(train)= 0.34
# RMSE (test)= 0.30


#Linear Regression
#check multicollearity
library(usdm)
vifcor(df[,-10], th = 0.9)

#run regression model
lm_model = lm(Absenteeism.time.in.hours ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,-18])

summary(lm_model)


## now calculate RMSE for linear regression model
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- test[,18] - predictions_LR

rmse(train[,18])     

##RMSE = 0.15
## RMSE (train) = 0.34
##RMSE(test)= 0.31



### Random Forest:

# ##rpart for regression
fit_RF = randomForest(Absenteeism.time.in.hours ~ ., data = train)


#Predict for new test cases
predictions_RF = predict(fit_RF, test[,-18])




## Now we will calculate RMSE 

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- test[,18] - predictions_RF

rmse(train[,18])   

##RMSE = 0.13
##RMSE (test)= 0.31
##RMSE (train)= 0.34

#################################### Visualisation in order to help the company to find what parameters are actually responsible for Absenteeism in a graphically manner##############################



#load libraries
library("ggplot2")
library("scales")
library("psych")
library("gplots")

#Univariate 
#Bar plot(categorical data)
#If you want count then stat="bin"
ggplot(df, aes_string(x = df$Pet, y = df$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('pet') + ylab('Absenteeism') + scale_y_continuous(breaks=pretty_breaks(n=10)) + scale_x_continuous(breaks = pretty_breaks(n=9))+
  ggtitle("absenteeism against pet") +  theme(text=element_text(size=15))

range(df$Reason.for.absence)
## we will draw the bar plots keeping our target variable as y - axis and our categorical variable as x - axis in order to find the relationship between them graphically..


range(df$pet)




range((df$Pet))
