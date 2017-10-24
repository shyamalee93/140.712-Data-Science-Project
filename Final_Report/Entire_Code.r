## Load all libraries
packages<-c("dplyr", "Hmisc", "ggplot2","data.table", "tidyr", "caret", "mice", "randomForest", "magrittr", "lattice", "lubridate", "tidyverse", "corrplot", "leaflet", "htmltools", "DT", "gridextra", "doParallel", "xgboost", "doSNOW")

for (i in packages){
  if(!require(i,character.only = T,quietly=T,warn.conflicts = F)){
    install.packages(i, repos = "http://cran.us.r-project.org")
  }
  require(i,character.only = T,quietly=T,warn.conflicts = F)
}

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(Hmisc)
library(caret)
library(xgboost)
library(glmnet)

currentYear <- 2016   

#Read in the property file with features
#zillow_property <- fread("../properties_2016.csv", header=TRUE, na.strings=c("", " ", "NA", "N/A", "null"),blank.lines.skip = TRUE, showProgress = FALSE, colClasses=list(character=50))  

#dim(zillow_property)

## Read the training file
#train <- fread("../train_2016_v2.csv")

## Merge both
#train = zillow_property %>% right_join(., train, by = 'parcelid')
train <- train <- readRDS("train.rds")

## Function for mode - impute mode values
mode_ <- function(x) {
  names(which.max(table(x)))
}

## Convert variables to numbers - taxdeliquency flas, 0 if NA, else 1 if Y
## Hashottuborspa - if na, 0, if true, 1
## Fireplaceflag - if NA, 0, if true, 1
## Firelace count, if NA, 0
##Pool count, if NA = 0
train <- train %>%  
  mutate(taxdelinquencyflag = ifelse(is.na(taxdelinquencyflag), 0, ifelse(taxdelinquencyflag =="Y", 1, taxdelinquencyflag)),
         hashottuborspa = ifelse(is.na(hashottuborspa), 0, ifelse(hashottuborspa =="true", 1, hashottuborspa)),
         fireplaceflag = ifelse(is.na(fireplaceflag), 0, ifelse(fireplaceflag =="true", 1, fireplaceflag)),
         fireplacecnt = ifelse(is.na(fireplacecnt), 0, fireplacecnt),
         poolcnt = ifelse(is.na(poolcnt), 0, poolcnt)
  )

train <- train %>%  
  mutate(taxdelinquencyflag = ifelse(!is.na(taxdelinquencyyear) & taxdelinquencyflag==0, 1, taxdelinquencyflag),
         fireplaceflag = ifelse(fireplacecnt > 0, 1, fireplaceflag),
         fireplacecnt = ifelse(fireplaceflag==1 & fireplacecnt == 0, -1, fireplacecnt) )  

train <- train %>% mutate(taxdelinquencyyear4d = ifelse(is.na(taxdelinquencyyear), 0,ifelse(taxdelinquencyyear < 17, taxdelinquencyyear+2000, taxdelinquencyyear+1900)))

train <- train %>%  
  mutate(taxdelinquencyNoYrs = ifelse(taxdelinquencyyear4d == 0, 0, currentYear - taxdelinquencyyear4d + 1))

train <- train %>% mutate(latitude = latitude/1e6, longitude = longitude/1e6)   

mode1 = mode_(train$numberofstories)

train <- train %>%  
  mutate(numberofstories = ifelse(is.na(numberofstories), mode1, numberofstories),
         fips = ifelse(is.na(fips), mode_(fips), fips),
         latitude = ifelse(is.na(latitude), mode_(latitude), latitude),
         longitude = ifelse(is.na(longitude), mode_(longitude), longitude)         
  )  

set.seed(0)
train$lotsizesqft_imputed <- Hmisc::impute(train$lotsizesquarefeet, "random")

train$poolsizesum <- NULL
train$pooltypeid10 <- NULL
train$pooltypeid2 <- NULL
train$pooltypeid7 <- NULL
train$lotsizesquarefeet <- NULL    # field replaced
train$taxdelinquencyyear4d <- NULL # interim field
train$taxdelinquencyyear <- NULL # field replaced

train$airconditioningtypeid[is.na(train$airconditioningtypeid)] = 6

train$airconditioningtypeid[train$airconditioningtypeid == 13] = 6

train <- train %>% select(-architecturalstyletypeid)

train$basementsqft[is.na(train$basementsqft)] = 0

train$basementsqft[train$basementsqft != 0] = 1

train <- train %>% select(-bathroomcnt)

data.frame(table(train$bedroomcnt))

train$bedroomcnt[is.na(train$bedroomcnt)] = 3 # assigning to the mode 

train <- train %>% select(-buildingclasstypeid)

Zillow_mode_quality <- train %>%
  group_by(yearbuilt, buildingqualitytypeid) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# grouping by yearbuilt and buildingqualitytypeid, summarising with respect to n then
# adding in n/sum(n)

Zillow_mode_quality2 <- Zillow_mode_quality %>% 
  filter(!is.na(buildingqualitytypeid)) %>%
  filter(!is.na(yearbuilt)) %>%
  filter(n == max(n))
Zillow_mode_quality2 <- Zillow_mode_quality2[-28, ]

Zillow_mode_quality_nas = Zillow_mode_quality %>%
  filter(is.na(buildingqualitytypeid)) %>%
  filter(!is.na(yearbuilt)) %>%
  filter(freq != 1)

ids_to_repeat <- Zillow_mode_quality2$buildingqualitytypeid[which(Zillow_mode_quality_nas$yearbuilt %in% Zillow_mode_quality2$yearbuilt)]
# this code is generating the mode ids that I want to repeat

#impute_quality <- rep(ids_to_repeat, Zillow_mode_quality_nas$n)
# this is creating a vector of repeated mode ids to impute

#train$buildingqualitytypeid[!train$yearbuilt %in% c(1807, 1808, 1821, 1823, 1825, 1831, 1874) & !is.na(train$yearbuilt) & is.na(train$buildingqualitytypeid)] -> impute_quality

# Now I only need to deal with the cases where Year is also NA, and the very few ids
# that only had NA- the years listed above. For both of these cases, I am just going 
# to use the overall mode (from prior to the imputation)

buildingqualitytypeid <- table(train$buildingqualitytypeid)
# mode is 7
train$buildingqualitytypeid[is.na(train$buildingqualitytypeid)] = 7

data.frame(table(train$calculatedbathnbr))

train$calculatedbathnbr[is.na(train$calculatedbathnbr)] = 2

train$decktypeid[is.na(train$decktypeid)] = 0

train$decktypeid[train$decktypeid == 66] = 1

train <- train %>% select(-threequarterbathnbr)

train <- train %>% 
  mutate(heatingorsystemtypeid = as.factor(ifelse(is.na(heatingorsystemtypeid), 
                                                  "13", heatingorsystemtypeid)))
train <- train %>% 
  mutate(hashottuborspa = as.factor(hashottuborspa))

train <- train %>% 
  mutate(fullbathcnt = as.factor(ifelse(is.na(fullbathcnt),0,fullbathcnt)))



#An imputation for taxamount from taxvaluedollarcnt (may include landvaluedollarcnt later)
lm1 = lm(formula = taxamount ~ taxvaluedollarcnt, data = train)


train$taxamount = ifelse(!is.na(train$taxamount), train$taxamount, ifelse(is.na(train$taxvaluedollarcnt), NA, lm1$coefficients[1]+lm1$coefficients[2]*train$taxvaluedollarcnt)) 

train$calculatedfinishedsquarefeet[is.na(train$calculatedfinishedsquarefeet)] <- mean(train$calculatedfinishedsquarefeet, na.rm = T)

col_rm = c("finishedfloor1squarefeet", "finishedsquarefeet12", "finishedsquarefeet13", "finishedsquarefeet15", "finishedsquarefeet50","finishedsquarefeet6", "taxvaluedollarcnt", 'landtaxvaluedollarcnt', 'structuretaxvaluedollarcnt')

train <- train[, setdiff(names(train), col_rm)]
train = train %>% select(-storytypeid)
train$yardbuildingsqft17[is.na(train$yardbuildingsqft17)] = 0
train$yardbuildingsqft26[is.na(train$yardbuildingsqft26)] = 0
train = train %>% select(-typeconstructiontypeid)
train$roomcnt <- Hmisc::impute(train$roomcnt, "random") 
train$unitcnt = Hmisc::impute(train$unitcnt, "random") 
train$calculatedfinishedsquarefeet <- Hmisc::impute(train$calculatedfinishedsquarefeet, "random") 
train = train %>% select(-rawcensustractandblock)
train$censustractandblock <- Hmisc::impute(train$censustractandblock, "random")
train$propertylandusetypeid = Hmisc::impute(train$propertylandusetypeid, "random")
train$regionidcounty = Hmisc::impute(train$regionidcounty, "random")
train$regionidcity = Hmisc::impute(train$regionidcity, "random")
train$regionidzip = Hmisc::impute(train$regionidzip, "random")
train$yearbuilt = Hmisc::impute(train$yearbuilt, "random")
train$garagecarcnt[is.na(train$garagecarcnt)] = 0
train$garagetotalsqft[is.na(train$garagetotalsqft)] = 0
train = train %>% select(-assessmentyear)
train = train %>% select(-regionidneighborhood)
train = train %>% select(-propertyzoningdesc)
train$propertycountylandusecode = Hmisc::impute(train$propertycountylandusecode, "random")

fwrite(train, 'clean_output1.csv')
total_na = as.data.frame(sapply(train, function(x) round((sum(is.na(x))/length(x)) * 100,2)))
colnames(total_na) <- "Percent_NA"
total_na <- cbind(Features = rownames(total_na), total_na)
total_na <- total_na %>% arrange(desc(Percent_NA))
View(total_na)

total_na %>% 
  filter(Percent_NA > 0) %>% 
  ggplot(aes(x=reorder(Features, -Percent_NA), y=Percent_NA)) + 
  geom_bar(stat='identity', fill='blue') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + coord_flip()
  
 ## Try Linear Regression
 linear <- lm(logerror ~ 
     calculatedfinishedsquarefeet +
     calculatedbathnbr + 
     airconditioningtypeid +
     buildingqualitytypeid +
     garagetotalsqft + 
     yearbuilt + 
     fireplacecnt + 
     unitcnt + 
     regionidcity +
     regionidzip + 
     regionidcounty +
     airconditioningtypeid +
     bedroomcnt + 
     lotsizesqft_imputed +
     poolcnt +
     decktypeid +
     numberofstories +
     basementsqft +
     yardbuildingsqft17 + 
     yardbuildingsqft26 +
     garagecarcnt + 
     taxdelinquencyflag +
     heatingorsystemtypeid +
     fullbathcnt +
     taxamount,
   data = subTrain)
summary(linear)
predictions <- data.frame(predict(linear, subTest))
View(predictions)
subTest$predictions <- predictions$predict.linear..subTest
names(subTest)
sum(abs(subTest$logerror - subTest$predictions))/ nrow(subTest)

### Random Forest with Ranger
###Try RF
train$fips=as.factor(train$fips)
train$latitude = as.numeric(train$latitude)
train$longitude = as.numeric(train$longitude)
train$numberofstories = as.factor(train$numberofstories)
train$propertycountylandusecode = as.factor(train$propertycountylandusecode)

train$lotsizesqft_imputed = as.numeric(train$lotsizesqft_imputed)
train$yearbuilt = as.numeric(train$yearbuilt)
train$fullbathcnt = as.numeric(train$fullbathcnt)
train$roomcnt = as.numeric(train$roomcnt)
train$regionidcounty = as.numeric(train$regionidcounty)
train$propertylandusetypeid = as.numeric(train$propertylandusetypeid)
train$heatingorsystemtypeid = as.numeric(train$heatingorsystemtypeid)
train$unitcnt = as.numeric(train$unitcnt)
train$regionidzip = as.numeric(train$regionidzip)
train$regionidcity = as.numeric(train$regionidcity)
train$calculatedfinishedsquarefeet = as.numeric(train$calculatedfinishedsquarefeet)
train$fips = as.numeric(train$fips)
train$numberofstories = as.numeric(train$numberofstories)
train$propertycountylandusecode = as.numeric(train$propertycountylandusecode)

train_1 <- train
train_1$month <- lubridate::month(train_1$transactiondate)

maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

set.seed(0)
trainIndex <- sample(1:nrow(train_1), nrow(train_1)*0.75)

# training set
subTrain <- train_1[ trainIndex,-1]

## testing set
subTest  <- train_1[-trainIndex,-1]

tunegrid <- expand.grid(mtry = 2 ) #mtry can be lowered. 6 is the ideal mtry for 36ish columns. (sqrt(36 = 6))

## Using caret
rf_model1 <-train(logerror ~ . -transactiondate -censustractandblock, 
                  data=subTrain,
                  method="ranger",
                  trControl=trainControl(method="cv", summaryFunction = maeSummary,
                                         number=5, verboseIter = TRUE, returnData = FALSE,allowParallel=TRUE), 
                  #tuneGrid = tunegrid,
                  metric = c("MAE"),
                  preProc = c("center", "scale"),
                  importance = "permutation",
                  maximize = FALSE,
                  verbose = T)

##Random Forest
#try.rf <- randomForest(logerror ~. -propertycountylandusecode-numberofstories -censustractandblock -transactiondate, data = train_1, subset = trainIndex)

#rf_model1 <-train(logerror ~ . -numberofstories -censustractandblock -transactiondate, 
 #                 data=subTrain,
 #                  method="rf",
 #                 trControl=trainControl(method="cv", summaryFunction = maeSummary,
 #                                         number=10, verboseIter = TRUE ), 
 #                  tuneGrid = tunegrid,
 #                  metric = c("MAE"),
 #                  maximize = FALSE,
 #                  prox=TRUE,allowParallel=TRUE,
 #                 na.action=na.exclude,
 #                  verbose = T)
rf_model1$bestTune
plot(rf_model1)

tunegrid <- expand.grid(mtry = 2, splitrule = "extratrees")

##Ideal mtry = 2
bestrf <-train(logerror ~ . -transactiondate -censustractandblock, 
               data=subTrain,
               method="ranger",
               #mtry = 2,
               #ntree = 100,
               tuneGrid = tunegrid,
               metric = c("MAE"),
               preProc = c("center", "scale"),
               importance = "permutation",
               maximize = FALSE,
               na.action=na.exclude,
               verbose = T)
plot(bestrf)
importance(bestrf)
varImpPlot(bestrf)

rf.pred = predict(bestrf, data = subTest, type = "raw")

sum(abs(subTest$logerror - rf.pred)) / nrow(subTest)

cor(rf.pred)

fullTrain = train_1[,-1]

rf_full <- train(logerror ~ . -transactiondate -censustractandblock, 
                 data=fullTrain,
                 method="ranger",
                 #mtry = 2,
                 #ntree = 100,
                 tuneGrid = tunegrid,
                 metric = c("MAE"),
                 preProc = c("center", "scale"),
                 importance = "permutation",
                 maximize = FALSE,
                 na.action=na.exclude,
                 verbose = T)

pred <- predict(rf_full, data = train_1[, train_1$month == 10])
pred_11 <- predict(rf_full, data = train_1[, train_1$month == 11])
pred_12 <- predict(rf_full, data = train_1[, train_1$month == 12])

results <- data.table(parcelid=train_1$parcelid, 
                      '201610'=pred, 
                      '201611'=pred_11, 
                      '201612'=pred_12, 
                      '201710'=pred,
                      '201711'=pred_11,
                      '201712'=pred_12
)

##XG Boost
## Drop the columns with high missing values
cols_drop <- c("buildingclasstypeid", "finishedsquarefeet13", "basementsqft","storytypeid", "yardbuildingsqft26", "architecturalstyletypeid", "typeconstructiontypeid", "finishedsquarefeet6", "poolsizesum", "pooltypeid10", "pooltypeid2", "taxdelinquencyyear", "yardbuildingsqft17", "finishedsquarefeet15", "finishedfloor1squarefeet", "finishedsquarefeet50", "threequarterbathnbr", "pooltypeid7","numberofstories","garagetotalsqft", "regionidneighborhood", "finishedsquarefeet12", "regionidcity",  "calculatedbathnbr","fullbathcnt","yearbuilt","censustractandblock", "fips", "garagecnt", "rawcensustractandblock")

train_3 <- train[ , !(names(train) %in% cols_drop)]
train_3 <- train_3 %>%  mutate(month = month(transactiondate), year = year(transactiondate))

#Partition data

set.seed(0)
trainIndex <- createDataPartition(train_4$logerror, p = .75, list = FALSE,times = 1)

# Training
subTrain <- train_4[trainIndex,]

## Test
subTest  <- train_4[-trainIndex,]

# Whole thing
fullTrain = train_4[,-1]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

###New Xg boost
prop <- train_3[, !names(train_3) %in% c("logerror", "transactiondate")]
prop_oct <- prop %>%  filter(month == 10)
prop_nov <- prop %>%  filter(month == 11)
prop_dec <- prop %>%  filter(month == 12)

target <- train_3$logerror 
#target <- subTrain$logerror
dtrain <- train_3[, !names(train_3) %in% c('logerror', 'parcelid', 'year')]
#dtrain <- subTrain[, !names(subTrain) %in% c('logerror', 'parcelid', 'year', 'month')]
feature_names <- names(dtrain)
cv.train <- train_3[, c( 'parcelid','year')]
dtrain <- data.matrix(dtrain)
dtrain <- xgb.DMatrix(data=dtrain,label=target)


# Set up cross-validation scheme (3-fold)
foldsCV <- createFolds(target, k=10, list=TRUE, returnTrain=FALSE)

# Set xgboost parameters. These are not necessarily the optimal parameters.
# Further grid tuning is needed. 
param <- list(
  objective="reg:linear",
  eval_metric = "mae",
  eta = .005,
  max_depth = 2,
  min_child_weight = 10,
  subsample = 0.7,
  colsample_bytree = 0.5
)

# Perform xgboost cross-validation

xgb_cv <- xgb.cv(data=dtrain,
                 params=param,
                 nrounds=3000,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 early_stopping_rounds = 100,
                 print_every_n = 5
)
ggplot(xgb_cv$evaluation_log,aes(x=xgb_cv$evaluation_log$iter,y=xgb_cv$evaluation_log$test_mae_mean)) + geom_line() +
  labs(x = 'Num_iteration', y = 'test-MAE') + theme_bw()

print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
nrounds <- xgb_cv$best_iteration

xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     preProcess = c("center", "scale"),
                     nrounds=nrounds,
                     #nrounds=1500,
                     #verbose=1,
                     print_every_n = 5)

importance_matrix <- xgb.importance(feature_names,model=xgb_mod)

ggplot(importance_matrix,aes(x=reorder(Feature,Gain),y=Gain,fill=Gain)) +
  geom_bar(stat='identity',fill='#009E73') +coord_flip() + theme_bw() + ggtitle('Variable Importance of XGBoost') +xlab(NULL) +
  theme(plot.title = element_text(hjust = 0.5))

dtest <- xgb.DMatrix(data=data.matrix(prop[,names(prop) %in% feature_names]))
dtest_oct <- xgb.DMatrix(data=data.matrix( prop_oct[, names(prop_oct) %in% feature_names]))
dtest_nov <- xgb.DMatrix(data=data.matrix( prop_nov[, names(prop_nov) %in% feature_names]))
dtest_dec <- xgb.DMatrix(data=data.matrix( prop_dec[, names(prop_dec) %in% feature_names]))

preds <- predict(xgb_mod,dtest)
preds10 <- predict(xgb_mod,dtest_oct)
preds11 <- predict(xgb_mod,dtest_nov)
preds12 <- predict(xgb_mod,dtest_dec)

results <- data.table(parcelid=prop$parcelid, 
                      '201610'=preds10, 
                      '201611'=preds11, 
                      '201612'=preds12, 
                      '201710'=preds10,
                      '201711'=preds11,
                      '201712'=preds12
)

## Lasso Regression
lasso = select_if(fullTrain,is.numeric)
#lasso_train = select_if(subTrain,is.numeric)

x = model.matrix(logerror ~ ., lasso)  
y = lasso$logerror

#Creating training and test sets
set.seed(0)
train = sample(1:nrow(x), 7.5*nrow(x)/10)
test = (-train)
y.test = y[test]

#Values of lambda over which to check.
grid = 10^seq(-7, 2, length = 100)


#Fitting the Lasso regression
lasso.models = glmnet(x, y, alpha = 1, lambda=grid, standardize=TRUE, intercept=FALSE)

dim(coef(lasso.models))
coef_lasso=coef(lasso.models)
coef_lasso

#plotting the lasso model
plot(lasso.models, xvar = "lambda",  label = TRUE, main = "Lasso Regression")



#Perform 10-fold cross-validation
set.seed(0)
lasso_cv = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid, intercept=FALSE)
plot(lasso_cv, main = "Lasso Regression\n")
bestlambda.lasso = lasso_cv$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
coeff_lasso=coef(lasso_cv,s='lambda.min',exact=TRUE)
min(lasso_cv$cvm)

# predict on test data
set.seed(0)
lasso_fit = glmnet(x[train, ],y[train],alpha = 1,lambda = lasso_cv$lambda.min, intercept=FALSE)
y_pred = predict(lasso_fit,x[test, ])
mse_lasso = sum((y_pred-y.test)^2)/length(y.test)
print(mse_lasso)
mae_lasso = sum(abs(y_pred-y.test))/length(y.test)
print(mae_lasso)

plot(y.test,y_pred,xlab="Test logerror",ylab="Predicted logerror",
     main="Prediction using Lasso regression")
text(-1,3,substitute(r^2 == r2,list(r2=cor(y.test,y_pred))),adj=0)
text(-1,2.7,substitute(MSE == r2,list(r2=mse_lasso)),adj=0)
abline(0,1)


xn = model.matrix(~.,lasso)
pred_10_lasso <- predict(lasso.models, data = whole_data_1[, whole_data_1$month == 10])
pred_11_lasso <- predict(lasso.models, data = whole_data_1[, whole_data_1$month == 11])
pred_12_lasso <- predict(lasso.models, data = whole_data_1[, whole_data_1$month == 12])

## Ridge Regression
###New Ridge
# Ridge Regression
library(glmnet)
set.seed(1)
grid = 10^seq(5, -2, length = 100)
sub_train = subTrain %>% select(-propertycountylandusecode)
sub_test = subTest %>% select(-propertycountylandusecode)
sub_train = sub_train %>% select(-transactiondate)
sub_test = sub_test %>% select(-transactiondate)
sub_train$propertylandusetypeid = as.numeric(sub_train$propertylandusetypeid)
sub_test$propertylandusetypeid = as.numeric(sub_test$propertylandusetypeid)
sub_train$heatingorsystemtypeid = as.numeric(sub_train$heatingorsystemtypeid)
sub_test$heatingorsystemtypeid = as.numeric(sub_test$heatingorsystemtypeid)
sub_train$unitcnt = as.numeric(sub_train$unitcnt)
sub_test$unitcnt = as.numeric(sub_test$unitcnt)
sub_train$regionidzip = as.numeric(sub_train$regionidzip)
sub_test$regionidzip = as.numeric(sub_test$regionidzip)
sub_train$regionidcity = as.numeric(sub_train$regionidcity)
sub_test$regionidcity = as.numeric(sub_test$regionidcity)
sub_train$calculatedfinishedsquarefeet = as.numeric(sub_train$calculatedfinishedsquarefeet)
sub_test$calculatedfinishedsquarefeet = as.numeric(sub_test$calculatedfinishedsquarefeet)
sub_train$regionidcounty = as.numeric(sub_train$regionidcounty)
sub_test$regionidcounty = as.numeric(sub_test$regionidcounty)
sub_train$fips = as.numeric(sub_train$fips)
sub_test$fips = as.numeric(sub_test$fips)
sub_train$numberofstories = as.numeric(sub_train$numberofstories)
sub_test$numberofstories = as.numeric(sub_test$numberofstories)
sub_train = sub_train %>% select(-censustractandblock)
sub_test = sub_test %>% select(-censustractandblock)

grid2 = 10^seq(0.5, -3, length = 100)
x_rln = model.matrix(logerror ~ ., sub_train)[, -1] #Dropping the intercept column.
y_rln = sub_train$logerror
x_test = model.matrix(logerror ~ ., sub_test)[, -1] #Dropping the intercept column.
y_test = sub_test$logerror
ridge_train = glmnet(x_rln, y_rln, alpha = 0, lambda = grid)
cv.ridge_train = cv.glmnet(x_rln, y_rln,
                           lambda = grid, alpha = 0, nfolds = 10)
bestlambda.ridge = cv.ridge_train$lambda.min

ridge_best_train = predict.cv.glmnet(cv.ridge_train, s ="lambda.min", newx = x_test)

sum(abs(ridge_best_train-sub_test$logerror))/nrow(sub_test)

subtest_ridge = sub_test %>% select(-propertycountylandusecode, -transactiondate)
sub_test$lotsizesqft_imputed = as.numeric(sub_test$lotsizesqft_imputed)
sub_test$yearbuilt = as.numeric(sub_test$yearbuilt)
sub_test$fullbathcnt = as.numeric(sub_test$fullbathcnt)
sub_test$roomcnt = as.numeric(sub_test$roomcnt)
sub_test$regionidcounty = as.numeric(sub_test$regionidcounty)
sub_test$propertylandusetypeid = as.numeric(sub_test$propertylandusetypeid)
sub_test$heatingorsystemtypeid = as.numeric(sub_test$heatingorsystemtypeid)
sub_test$unitcnt = as.numeric(sub_test$unitcnt)
sub_test$regionidzip = as.numeric(sub_test$regionidzip)
sub_test$regionidcity = as.numeric(sub_test$regionidcity)
sub_test$calculatedfinishedsquarefeet = as.numeric(sub_test$calculatedfinishedsquarefeet)
sub_test$fips = as.numeric(sub_test$fips)
sub_test$numberofstories = as.numeric(sub_test$numberofstories)
x_ridge = model.matrix(logerror ~ ., sub_test)[, -1]

ridge_best_train_final = predict.cv.glmnet(cv.ridge_train, s ="lambda.min", newx = x_ridge)
ridge_best_train_final = as.data.frame(ridge_best_train_final)

sub_test$pred_ridge <- ridge_best_train_final$`1`
sum(abs(sub_test$logerror - sub_test$pred_ridge))/ nrow(subTest)

