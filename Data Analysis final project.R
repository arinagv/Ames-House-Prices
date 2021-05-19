library(magrittr)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(xgboost)
library(rpart)
options(scipen = 999)
#Set working directory and import data
train <- read_csv("~/Downloads/train.csv")

test <- read_csv("~/Downloads/test.csv")

train_labels <- train$Id
test_labels <- test$Id
 
# Remove the target variable not found in test set
test$SalePrice <- NA


# combine the datasets
combined <- rbind(train,test)




####IMPUTATION, MISSING VALUES####

#List variables with missing variables in decreasing order
sort(sapply(combined, function(x) sum(length(which(is.na(x))))), decreasing = TRUE)
sum(is.na(combined))-sum(is.na(combined$SalePrice))

#change variable name formatting
names(combined)[names(combined) == "1stFlrSF"] <- "Flr1SF"
names(combined)[names(combined)== "2ndFlrSF"] <- "Flr2SF"
names(combined)[names(combined)== "3SsnPorch"] <- "Porch3Szn"


str(combined)


#MiscFeature
table(is.na(combined$MiscFeature))
str(combined$MiscFeature)
combined$MiscFeature = as.character(combined$MiscFeature)
combined$MiscFeature[is.na(combined$MiscFeature)] = 'None'
table(combined$MiscFeature)

#Pool
#PoolQC
#Reasonable to assume houses with NA’s for PoolQC don’t have a pool, however, 
#we verify by searching for any of the houses that have NA for PoolQC but a recorded PoolArea > 0.
which((combined$PoolArea > 0) & is.na(combined$PoolQC))
#Of the 2909 NA’s for PoolQC only 3 recorded a PoolArea greater than 0. 
#To fill the NAs for these houses we can see what the median pool area is for each quality of pool. 
#Then we assign the quality with closest pool area to the 3 stragglers.
combined[,c('PoolQC','PoolArea')] %>%
  group_by(PoolQC) %>%
  summarise(median = median(PoolArea), counts=n())
combined[2421,'PoolQC'] = 'Ex'
combined[2504,'PoolQC'] = 'Ex'
combined[2600,'PoolQC'] = 'Fa'
combined$PoolQC = as.character(combined$PoolQC)
combined$PoolQC[is.na(combined$PoolQC)] = 'None'


#Alley
#Going into the None function, NA likely means they don't have an alley space.
combined$Alley[is.na(combined$Alley)] = 'None'


#Fence
#Going to None Function, NA likely means they don't have this feature.
combined$Fence[is.na(combined$Fence)] = 'None'

#FireplaceQu
which((combined$Fireplaces > 0) & (is.na(combined$FireplaceQu)))
  #All the houses that have missing values did not record having any fireplaces.
combined$FireplaceQu[is.na(combined$FireplaceQu)] = 'None'
#Fireplaces
combined$Fireplaces[is.na(combined$Fireplaces)] = 0


#GarageYrBlt
  #Are there any missing values in GarageYrBlt because there is no garage?
which((combined$GarageArea = 0) & is.na(combined$GarageYrBlt))  
length(which(combined$GarageYrBlt == combined$YearBuilt))
  #No, so we assume for NAs that the garage was built the same year as the house.
combined$GarageYrBlt[is.na(combined$GarageYrBlt)] <- combined$YearBuilt[is.na(combined$GarageYrBlt)]
  #are there any houses built later than 2010 (the year this data goes to)
which(combined$GarageYrBlt > 2010)
combined$GarageYrBlt[2593] <- 2007


#GarageCond
#combined$GarageCond[is.na(combined$GarageCond)] = 'None'
table(combined$GarageCond)
any(is.na(combined$GarageCond))
str(combined$GarageCond)


#if-else statement to fill numeric vs factor Garage variable NAs
garage_vars <- names(combined)[sapply(names(combined), function(x) str_detect(x, 'Garage'))]
for (i in garage_vars){
  if (sapply(combined[i], is.numeric) == TRUE){
    combined[sapply(combined[,i], is.na),i] = 0
  }
  else{
    combined[sapply(combined[,i],is.na),i] = 'None'
  }
}
rm(garage_vars)


#Exterior
which(is.na(combined$Exterior1st) & is.na(combined$Exterior2nd))
  #There is only 1 missing value for Exterior1st and Exterior2nd and it's the same house,
  #and there aren’t any other features that can help us predict what variable should be filled 
  #so we can fill this with ‘Other’, i.e. the exterior cover used is not listed.
combined$Exterior1st[is.na(combined$Exterior1st)] = 'Other'
combined$Exterior2nd[is.na(combined$Exterior2nd)] = 'Other'


#11 basement characteristics that seem to have at least one missing value
#We can take a look at the subset of just these columns from our data.
bsmt_vars <- names(combined)[sapply(names(combined), function(x) str_detect(x, 'Bsmt'))]

which(is.na(combined$BsmtExposure) & combined$TotalBsmtSF > 0)
#Rows 949, 1488 and 2349 are the only NAs from BsmtExposure/TotalBsmtSF>0
#So they have a basement, but we will assume that the basement has no exposure
combined[c(949, 1488, 2349), 'BsmtExposure'] = 'No' 
#We fill in the rest of the BsmtExposure values with ‘None’ since these houses certainly don’t have basements.

#if-else statement for filling numeric vs factor Bsmt columns
for (i in bsmt_vars){
  if (sapply(combined[i], is.numeric) == TRUE){
    combined[sapply(combined[,i], is.na),i] = 0
  }
  else{
    combined[sapply(combined[,i],is.na),i] = 'None'
  }
}
rm(bsmt_vars)


  #There are 23 missing values for MasVnrArea and 24 for MasVnrType. We can see if both missing values 
  #come from the same houses
combined[(is.na(combined$MasVnrType)) & (is.na(combined$MasVnrArea)), c('MasVnrType', 'MasVnrArea','Id')]
  #All but one house has missing values for both columns
  #We can obtain the median of the veneer area to see which type an area of 198 is closest to.
combined[,c('MasVnrType','MasVnrArea')] %>%
  group_by(MasVnrType) %>%
  summarise(median_vnr_area = median(MasVnrArea,na.rm = TRUE),counts = n()) %>%
  arrange(median_vnr_area)
which((is.na(combined$MasVnrType)) & !is.na(combined$MasVnrArea))
combined[2611, 'MasVnrType'] = 'Stone'
  #For houses with NA’s in both columns we can fill 0 for the area and None for the type, 
  #since it's probable that they don't have masonry veneer.
combined$MasVnrType[is.na(combined$MasVnrType)] = 'None'
combined$MasVnrArea[is.na(combined$MasVnrArea)] = 0


#LotFrontage will be filled with median value by lot configuration
table(is.na(combined$LotFrontage), combined$LotConfig)
table(combined$LotConfig)
combined %>%
  group_by(LotConfig) %>%
  summarise(median_frntg_area = median(LotFrontage,na.rm = TRUE),.groups="keep", counts = n()) %>%
  arrange(median_frntg_area)

combined$LotFrontage[is.na(combined$LotFrontage & combined$LotConfig=="CulDSac")] = 50
combined$LotFrontage[is.na(combined$LotFrontage & combined$LotConfig=="FR2")] = 62
combined$LotFrontage[is.na(combined$LotFrontage & combined$LotConfig=="FR3")] = 66
combined$LotFrontage[is.na(combined$LotFrontage & combined$LotConfig=="Inside")] = 66
combined$LotFrontage[is.na(combined$LotFrontage & combined$LotConfig=="Corner")] = 80


#MSZoning
#Only 4 missing values in MSZoning
combined[is.na(combined$MSZoning),c('MSZoning','MSSubClass', 'Id')]
#we make frequency table to see where MSZoning and MSSubClass overlap most to decide how to fill NAs
table(combined$MSZoning, combined$MSSubClass)
#subclass 20 has the biggest frequency in RL zoning, subclasses 30 and 70 both have biggest frequency in RM
combined$MSZoning[c(1916, 2251)] = 'RM'
combined$MSZoning[c(2217, 2905)] = 'RL'


  #With only 1 NA for both KitchenQual and Electrical we can fill in the missing value with the 
  #most frequent value from each column.
get_mode <- function(x){
  return(names(which.max(table(x))))
}
#KitchenQual
get_mode(combined$KitchenQual)
combined$KitchenQual[is.na(combined$KitchenQual)] = 'TA'

#Electrical (1 NA)
combined$Electrical[is.na(combined$Electrical)] = get_mode(combined$Electrical)


#SaleType
#Going to the mode function
combined$SaleType[is.na(combined$SaleType)] = get_mode(combined$SaleType)

#Functional (2 NAs)
combined$Functional[is.na(combined$Functional)] = get_mode(combined$Functional)


sort(sapply(combined, function(x) sum(length(which(is.na(x))))), decreasing = TRUE)


#find any infinite values so they don't cause problems for regressions
sort(sapply(combined, function(x) sum(length(which(is.infinite(x))))), decreasing = TRUE)
train <- train %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
test <- test %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
combined <- combined %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))



rm(get_mode)


####CONVERT VARIABLE CLASS####
str(lapply(combined, class))
combined <- combined %>% mutate_if(is.character,as.factor)
str(lapply(combined, class))
#MMSubClass
combined$MSSubClass <- as.factor(combined$MSSubClass)


table(combined$MiscFeature)
str(combined$BsmtFinType1)



library(sjlabelled)
combined[["OverallQual"]] <- ordered(combined$OverallQual, levels=c(1,2,3,4,5,6,7,8,9,10))
combined[["OverallQual"]] <- as.integer(as_numeric(combined[["OverallQual"]]))
combined[["OverallCond"]] <- ordered(combined[["OverallCond"]], levels=c(1,2,3,4,5,6,7,8,9,10))
combined[["OverallCond"]] <- as.integer(as_numeric(combined[["OverallCond"]]))
combined[["ExterQual"]] <- ordered(combined$ExterQual, levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["ExterQual"]] <- as.integer(as_numeric(combined[["ExterQual"]], start.at = 0))
combined[["PoolQC"]] <- ordered(combined[["PoolQC"]], levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["PoolQC"]] <- as.integer(as_numeric(combined[["PoolQC"]],start.at=0))
combined[["ExterCond"]] <- ordered(combined$ExterCond, levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["ExterCond"]] <- as.integer(as_numeric(combined[["ExterCond"]], start.at=0))
combined[["BsmtQual"]] <- ordered(combined[["BsmtQual"]], levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["BsmtQual"]] <- as.integer(as_numeric(combined[["BsmtQual"]],start.at=0))
combined[["BsmtCond"]] <- ordered(combined[["BsmtCond"]], levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["BsmtCond"]] <- as.integer(as_numeric(combined[["BsmtCond"]], start.at=0))
combined[["HeatingQC"]] <- ordered(combined[["HeatingQC"]], levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["HeatingQC"]] <- as.integer(as_numeric(combined[["HeatingQC"]], start.at=0))
combined[["KitchenQual"]] <- ordered(combined$KitchenQual, levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["KitchenQual"]] <- as.integer(as_numeric(combined[["KitchenQual"]],start.at=0))
combined[["FireplaceQu"]] <- ordered(combined$FireplaceQu,levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["FireplaceQu"]] <- as.integer(as_numeric(combined[["FireplaceQu"]], start.at = 0))
combined[["GarageQual"]] <-  ordered(combined$GarageQual, levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["GarageQual"]] <- as.integer(as_numeric(combined[["GarageQual"]], start.at=0))
combined[["GarageCond"]] <- ordered(combined[["GarageCond"]], levels=c('None', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
combined[["GarageCond"]] <- as.integer(as_numeric(combined[["GarageCond"]], start.at = 0))
combined[["CentralAir"]] <- ordered(combined[["CentralAir"]], levels=c('N','Y'))
combined[["CentralAir"]] <- as.integer(as_numeric(combined[["CentralAir"]],start.at = 0))
combined[["BsmtFinType1"]] <- ordered(combined$BsmtFinType1,levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
combined[["BsmtFinType1"]] <- as.integer(as_numeric(combined[["BsmtFinType1"]], start.at=0))
combined[["BsmtFinType2"]] <- ordered(combined$BsmtFinType2, levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
combined[["BsmtFinType2"]] <- as.integer(as_numeric(combined[["BsmtFinType2"]], start.at=0))

str(lapply(combined, class))
sort(sapply(combined, function(x) sum(length(which(is.na(x))))), decreasing = TRUE)
table(combined$MiscVal)
combined=combined%>% # Remove useless variables
  dplyr::select(-MiscFeature,-MiscVal,-Utilities)


#combined$SalePrice <- log(combined[['SalePrice']] + 1)

####Exploratory Data Analysis####
'
par(mfrow=c(2,2))
qqnorm(combined$SalePrice, main = "Price Q-Q Plot")
qqline(combined$SalePrice)
qqnorm(train[[log(SalePrice)]], main = "log(Price) Q-Q Plot")
qqline(train[[log(SalePrice)]])
qqnorm(train$LotFrontage, main = "Lot Frontage Q-Q Plot")
qqline(train$LotFrontage)
qqnorm(combined$LotFrontage, main = "log(Lot Frontage) Q-Q Plot")
qqline(combined$LotFrontage)
'
#Sale price normality
par(mfrow=c(1,2))
hist(combined$SalePrice,
     xlab='Sale Price',ylab='',
     main="Distribution of Sale Price",
     mgp=c(1.5,.5,0),
     col='dodgerblue3', cex.main=.9,cex.lab=.7,cex.axis=.6)
hist(log(combined$SalePrice),
     xlab='Log(Sale Price)',ylab='',
     main="Distribution of Log Sale Price",
     mgp=c(1.5,.5,0),
     col='dodgerblue3', cex.main=.9,cex.lab=.7,cex.axis=.6) 



#Outliers
#GrLivArea
ggplot(data=combined[!is.na(combined$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000)) +
  geom_text_repel(aes(label = ifelse(combined$GrLivArea[!is.na(combined$SalePrice)]>4500, rownames(combined), '')))

which((combined$GrLivArea > 4500) & (combined$SalePrice < 200000))



'
#Correlation to SalePrice?
# Set a table with "SalePrice" correlation
cors = cor(train[ , sapply(train, is.numeric)])
cors = as.data.frame(cors)
corrtable <- data.frame(corrVars = rownames(cors), 
                        corr = cors[, "SalePrice"])
# Order it by correlation
corrtable <- corrtable[order(corrtable$corr, decreasing = T),]
# Pick only values that have strong positive and negative correlation
corrtable <- corrtable[which(corrtable$corr > 0.5 | corrtable$corr < -0.5),]
rownames(corrtable) <- c()
corrtable
'

#Correlation Matrix
library(corrplot)
numericVars <- which(sapply(combined, is.numeric)) #index numeric variables
numVarNames <- combined[ ,numericVars]
cor_numVar <- cor(numVarNames, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[ ,'SalePrice'], decreasing = TRUE))
#select only high correlations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.9,cl.cex = .9, number.cex=.9)




#MULTICOLLINEARITY
cor(combined$GrLivArea, (combined$Flr1SF + combined$Flr2SF + combined$LowQualFinSF))

cor(combined$TotalBsmtSF, (combined$BsmtFinSF2 + combined$BsmtFinSF1 + combined$BsmtUnfSF))

cor(combined$OverallQual,combined$ExterQual)

cor(combined$GarageArea,!is.na(combined$SalePrice)) #not correlated



cor(train$SalePrice, train$TotalBsmtSF)




#skewed?
#determining skew of each numeric variable
numeric <- numeric <-names(numVarNames)
numeric
skewed <- sapply(numeric,function(x){skewness(combined[[x]],na.rm = T)})
skewed <- skewed[(skewed > 0.75) | (skewed < -0.75)]
skewed <- skewed[!is.na(skewed)]
skewed

skew(combined$SalePrice)
combined$SalePrice <- log(combined$SalePrice)


####FEATURE ENGINEERING####

#make a house age variable
combined = combined %>% mutate(HouseAge = YrSold-YearBuilt)

ggplot(data=combined[!is.na(combined$SalePrice),], aes(x=HouseAge, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))

combined[-c(524,1299), ] #delete two outliers


  #then make the year and month variables into integers
combined$YrSold <- as.integer(combined$YrSold)
table(combined$MoSold)
combined$MoSold <- as.integer(combined$MoSold)
combined$YearRemodAdd <- as.integer(combined$YearRemodAdd)

#make a number of bathrooms variable
combined$TotalBathrooms <- combined$FullBath + (combined$HalfBath*0.5) + combined$BsmtFullBath + (combined$BsmtHalfBath*0.5)


#make a total area variable
combined$TotalArea <- combined$GrLivArea + combined$TotalBsmtSF
cor(combined$FullBath,!is.na(combined$SalePrice))


####MODEL PREPPING####

numericVars <- which(sapply(combined, is.numeric)) #index numeric variables
numVarNames <- combined[ ,numericVars]
not_num <- c('MSSubClass', 
             'MoSold', 
             'YrSold', 
             'SalePrice', 
             'OverallQual', 
             'OverallCond',
             'YearRemodAdd',
             'ExterQual',
             'ExterCond',
             'BsmtQual',
             'BsmtCond',
             'BsmtFinType1',
             'BsmtFinType2',
             'HeatingQC',
             'CentralAir',
             'KitchenQual',
             'FireplaceQu',
             'GarageQual',
             'GarageCond',
             'PoolQC')
numVarNames <- numVarNames[,!(names(numVarNames) %in% not_num)] #'numeric' var names that aren't actually continuous


numeric <- numeric <-names(numVarNames)
numeric
rm(factors)
factors <- combined[,!(names(combined) %in% names(numVarNames))]
factors <- factors[,!names(factors) %in% ('SalePrice')]

#skewed?
#determining skew of each numeric variable
skewed <- sapply(numeric,function(x){skewness(combined[[x]],na.rm = T)})
skewed <- skewed[(skewed > 0.75) | (skewed < -0.75)]
skewed <- skewed[!is.na(skewed)]
skewed

for(x in names(skewed)) {
  combined[[x]] = log(combined[[x]] + 1)
}



combined=combined%>% # Remove useless variables
  dplyr::select(-Id)






#SCALING/NORMALIZING
#we must scale the continuous numeric variables for the linear model to work
PreNum <- preProcess(numVarNames, method=c("center", "scale"))
print(PreNum)
DFnorm <- predict(PreNum, numVarNames)
dim(DFnorm)




#ONE-HOT ENCODING VARIABLES AS DUMMIES
DFdummies <- as.data.frame(model.matrix(~.-1, factors))
dim(DFdummies)

#removing factor levels in test set with absent values
ZerocolTest <- which(colSums(DFdummies[(nrow(combined[!is.na(combined$SalePrice),])+1):nrow(combined),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest] #removing predictors


#check if some values are absent in the train set
ZerocolTrain <- which(colSums(DFdummies[1:nrow(combined[!is.na(combined$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain]

#Find and delete variables with less than 10 obs in them bc they have close to zero variance
fewOnes <- which(colSums(DFdummies[1:nrow(combined[!is.na(combined$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] #removing predictors


#Finally splitting the data, HOT ENCODED
combined_hot <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe 
combined_hot <- cbind(combined_hot, combined[,'SalePrice'])
combined_hot$GarageArea =NULL
train1 <- combined_hot[!is.na(combined$SalePrice),]
test1 <- combined_hot[is.na(combined$SalePrice),]

#Splitting for linear model: HOT ENCODED
#we need to drop variables with multicollinearity for the Linear model
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 
              'GarageArea', 'GarageCond', 
              'TotalBsmtSF', 'HalfBath', 'FullBath',
              'BsmtHalfBath','BsmtFullBath',
              'PoolArea','PoolQC','TotRmsAbvGrd')

train_linear <- train1[,!(names(train1) %in% dropVars)]
test_linear <- test1[,!(names(test1) %in% dropVars)]
train_linear <- train_linear[,!(names(combined) %in% ('SalePrice'))]
all_linear <- rbind(train_linear,test_linear)
all_linear <- cbind(all_linear,combined[,'SalePrice'])
train_linear <- all_linear[!is.na(all_linear$SalePrice),]
test_linear <- all_linear[is.na(all_linear$SalePrice),]
test_linear$SalePrice =NULL


trash <- c('CorHigh','i','numeric','numericVars','dropVars','PreNum',
           'cor_numVar','cor_sorted','x', 'fewOnes','ZerocolTest','ZerocolTrain',
           'skewed','test','train','all_linear')
rm(list = trash)
rm(all_linear)




rm(DFnorm)





####MODELING####


#initial model that includes every predictor
library(MASS)
library(olsrr)
library(forecast)

#kitchensink <- lm(log(SalePrice)~.-Id, data=combined)
#varImp(kitchensink)


#step <- ols_step_backward_aic(kitchensink, progress = T)




set.seed(77)
fitControl <- trainControl(method = "cv", number = 5)



#Linear 
set.seed(77)
fit.lm <- train(x=train_linear, y=combined$SalePrice[!is.na(combined$SalePrice)], 
                method="lm", 
                scale=TRUE, 
                metric="RMSE", 
                trControl=fitControl)
fit.lm

lm.pred <- exp(predict(fit.lm, newdata=test_linear))
head(lm.pred)

#accuracy(lm.pred,test_linear$SalePrice)




# --------------------------------------------
# GBM
# ---------------------------------------------

gbmGrid <-  expand.grid(interaction.depth = 6,
                        n.trees = 500, 
                        shrinkage = 0.03,
                        n.minobsinnode = 10)

warnings()
fit.gbm <- train(SalePrice~.,data=train1,
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 trControl= fitControl,
                 verbose = FALSE)
fit.gbm

gbm.pred <- exp(predict(fit.gbm, newdata = test1))
head(gbm.pred)
#accuracy(gbm.pred, combined$SalePrice[is.na(combined$SalePrice)])




# --------------------------------------------
# XGB
# --------------------------------------------
label_train <- combined$SalePrice[!is.na(combined$SalePrice)]
train2 <- train1
train2$SalePrice=NULL
test2 <- test1
test2$SalePrice=NULL
dtrain <- xgb.DMatrix(data = as.matrix(train2), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test2))


params <-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.03, #default = 0.3
  gamma=0,
  max_depth=5, #default=6
  min_child_weight=1, #default=1
  subsample=1,
  colsample_bytree=1)

fit.xgb <- xgb.train(data = dtrain, params=params, nrounds = 500, metric="RMSE")
fit.xgb
xgb.pred <- exp(predict(fit.xgb, dtest))
head(xgb.pred)

# --------------------------------------------
# PCR
# ---------------------------------------------

fit.pcr <- train(
  SalePrice~.,data=train_linear,
  method = "pcr",
  trControl = trainControl("cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 100
)

fit.pcr$results %>% filter(ncomp==pull(fit.pcr$bestTune))
pcr.pred <- exp(predict(fit.pcr, newdata = test_linear))
head(pcr.pred)
#accuracy(pcr.pred, test_linear$SalePrice)

#RMSE
# summarize accuracy of models
results <- resamples(list(lm=fit.lm, gbm=fit.gbm, pcr=fit.pcr))
summary(results)
dotplot(results)

results <- resamples(list(lm=fit.lm, gbm=gbm, pcr=fit.pcr))
summary(results)

results <- list(fit.lm, fit.gbm, fit.xgb, fit.pcr)
dotplot(fit.lm)





pav3=(1/3)*(pgbm+pelastic+pridge)

df.submit=data.frame(1461:2919,fit.gbm)

write.csv(df.sub,'housingprice1.csv',row.names=FALSE)


submitGBM <- data.frame(test_labels,SalePrice=gbm.pred)
colnames(submitGBM)<-c('Id','SalePrice')
write.csv(submitGBM,file="submitGBM.csv",row.names=F)

submitPCR <- data.frame(test_labels,SalePrice=pcr.pred)
colnames(submitPCR)<-c('Id','SalePrice')
write.csv(submitPCR,file="submitPCR.csv",row.names=F)

submitLM <- data.frame(test_labels,SalePrice=lm.pred)
colnames(submitLM)<-c('Id','SalePrice')
write.csv(submitLM,file="submitLM.csv",row.names=F)

submitXGB <- data.frame(test_labels,SalePrice=xgb.pred)
colnames(submitXGB)<-c('Id','SalePrice')
write.csv(submitXGB,file="submitXGB.csv",row.names=F)

