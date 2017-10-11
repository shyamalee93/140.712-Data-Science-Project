## List of required packages
required.packages <- c("dplyr", "Hmisc", "ggplot2","data.table", "tidyr", "caret", 
						"mice", "randomForest", "magrittr", "lattice", "lubridate", 
						"tidyverse", "corrplot", "leaflet", "htmltools", "DT", 
						"gridextra", "doParallel", "xgboost", "doSNOW", "h20", 
						"kableExtra", "knitr")

## Check which ones need to be installed and install them
packages.to.install <- setdiff(required.packages, installed.packages())
if(length(packages.to.install) > 0) install.packages(packages.to.install)

## Load all the packages
library(dplyr)
library(Hmisc)
library(ggplot2)
library(data.table)
library(tidyr)
library(caret)
library(mice)
library(randomForest)
library(magrittr)
library(lattice)
library(lubridate)
library(tidyverse)
library(corrplot)
library(leaflet)
library(doParallel)
library(xgboost)
library(doSNOW)
library(h2o)
library(kableExtra)

currentYear <- 2016   

#Read in the property file with features
zillow_property <- fread("../properties_2016.csv", header=TRUE, na.strings=c("", " ", "NA", "N/A", "null"),blank.lines.skip = TRUE, colClasses=list(character=50))  

dim(zillow_property)

## Read the training file
train <- fread("../train_2016_v2.csv")

## Merge both
train = zillow_property %>% right_join(., train, by = 'parcelid')

## Function for mode - impute mode values
mode_ <- function(x) {
  names(which.max(table(x)))
}

### Exploratory Data Analysis

#Get percentage of NA's across features
total_na = as.data.frame(sapply(train, function(x) round((sum(is.na(x))/length(x)) * 100,2)))
colnames(total_na) <- "Percent_NA"
total_na <- cbind(Features = rownames(total_na), total_na)
total_na <- total_na %>% arrange(desc(Percent_NA))

#If some feature has only one unique value, we could ignore them
rapply(train, function(x)length(unique(x))<3)
unique(train$assessmentyear)
#Assessment Year has only value 2015. Take this off.
train$assessmentyear <- NULL

#Plot missing data by feature
total_na %>% 
    filter(Percent_NA > 0) %>% 
    ggplot(aes(x=reorder(Features, -Percent_NA), y=Percent_NA)) + 
    geom_bar(stat='identity', fill='red') +
    labs(x='', y='Percentage missing', title='Percent missing data by feature') +
    theme(axis.text.x=element_text(angle=90, hjust=1)) + coord_flip() + theme_bw()

## Distribution of what you have to predict - the logerror
train %>%
    ggplot(aes(x=logerror)) + 
    geom_density(fill='steelblue', color='steelblue') + 
    ggtitle('Distribution of logerror') + theme_bw()

## Distribution of error over the time

# Change transaction date to date object from integer
train$transactiondate = as.Date(train$transactiondate)

train %>%
    group_by(transactiondate) %>% 
    summarise(mean_abs_logerror = mean(abs(logerror))) %>%
    ggplot(aes(x=transactiondate, y = mean_abs_logerror)) + 
    geom_bar(stat='identity', fill='steelblue') + 
    labs(x='', y='Mean log error', title='Absolute mean log error over time') + theme_bw()

## Count transactions by day
train %>% 
  group_by(transactiondate) %>% 
  tally() %>% 
  ggplot(aes(x=transactiondate, y=n)) + 
    geom_bar(stat='identity', color='steelblue') + 
    labs(x='', y='Number of transactions', title='Total transactions by day') + theme(bw)

## Use Leaflet to plot where the houses are
train %>%
     leaflet() %>%
     addTiles() %>%
     addCircleMarkers(
         lat =  ~ latitude / 10e5,
         lng =  ~ longitude / 10e5,
         label = ~ as.character(c(bedroomcnt, bathroomcnt)),
         clusterOptions = markerClusterOptions()
     )

# Check total counties
unique(train$regionidcounty)

## There are 3 - LA, Orange, Ventura
train %>% 
  group_by(regionidcounty) %>%  
  summarise(count = n(), mean_error = mean(logerror), std_dev = sd(logerror)) %>% 
  round(3) %>% arrange()

# Looking at correlation plot between continuous variables and log error

continuous_vars <- c('bathroomcnt', 'bedroomcnt', 'calculatedbathnbr',     'finishedfloor1squarefeet', 'calculatedfinishedsquarefeet', 'finishedsquarefeet6', 'finishedsquarefeet12', 'finishedsquarefeet13','finishedsquarefeet15', 'finishedsquarefeet50', 'fireplacecnt','fullbathcnt', 'garagecarcnt', 'garagetotalsqft', 'poolcnt', 'roomcnt', 'threequarterbathnbr', 'unitcnt', 'numberofstories', 'logerror')

corrplot(cor(train[, continuous_vars], use='pairwise.complete.obs'), type='lower')

## Don't see any big correlation with logerror with any of the numeric variables

## Check when the houses were built? 
train %>% 
  ggplot(aes(x=yearbuilt))+geom_line(stat="density", color="blue")
+theme_bw()
  
# How does logerror change with the year built
train %>% 
  group_by(yearbuilt) %>% 
  summarise(MeanAbsLE = mean(abs(logerror)), n()) %>% 
  ggplot(aes(x=yearbuilt,y=MeanAbsLE))+
  geom_smooth(color = "grey") +
  geom_point(color="blue")+coord_cartesian(ylim=c(0,0.25))+theme_bw()

## Plot a map with absolute log error
map <- train %>% 
  sample_n(2000) %>% 
  select(parcelid,longitude,latitude, abs(logerror)) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6, abs_error = abs(logerror)) %>% 
  select(parcelid,lat,lon, abs_error)

palette <- colorQuantile("YlOrRd", map$abs_error, n = 7)

leaflet(map) %>% 
  addTiles() %>% 
  addCircleMarkers(stroke=FALSE, color=~palette(abs_error),fillOpacity = 1) %>% 
  addLegend("bottomleft", pal = palette, values = ~abs_error,title = "Absolute logerror",opacity = 1) %>% 
    addMiniMap()
	
## Missing Values Imputation

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

fwrite(train, 'clean_output.csv')

## Check if any NA's - None
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

## Simple Multiple Linear Regression
linear <- lm(logerror ~ . -transactiondate, data = subTrain)
summary(linear)  

###Try RF

## Convert to Factors

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

## Summary function to calculate Mean Absolute Error
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

set.seed(0)

## Generate an Index
trainIndex <- sample(1:nrow(train_1), nrow(train_1)*0.75)

# Training set
subTrain <- train_1[ trainIndex,-1]

## Test set
subTest  <- train_1[-trainIndex,-1]

## Generate a tuning grid
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

##Random Forest best tune values
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

results_rf <- data.table(parcelid=train_1$parcelid, 
                      '201610'=pred, 
                      '201611'=pred_11, 
                      '201612'=pred_12, 
                      '201710'=pred,
                      '201711'=pred_11,
                      '201712'=pred_12
)


######
####
###
## Lasso Regression
lasso1 = select_if(fullTrain,is.numeric)
#lasso1_train = select_if(subTrain,is.numeric)

x = model.matrix(logerror ~ ., lasso)  
y = lasso1$logerror

#Creating training and test sets
set.seed(0)
train = sample(1:nrow(x), 7.5*nrow(x)/10)
test = (-train)
y.test = y[test]

#Values of lambda over which to check.
grid = 10^seq(-7, 2, length = 100)


#Fitting the Lasso regression
lasso_models = glmnet(x, y, alpha = 0.05, lambda=grid, standardize=TRUE, intercept=FALSE)

dim(coef(lasso_models))
coef_lasso1=coef(lasso_models)
coef_lasso1

#plotting the lasso model
plot(lasso_models, xvar = "lambda",  label = TRUE, main = "Lasso Regression")



#Perform 10-fold cross-validation
set.seed(0)
lasso_cv_1 = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid, intercept=FALSE)
plot(lasso_cv_1, main = "Lasso Regression\n")
bestlambda_lasso = lasso_cv_1$lambda.min
bestlambda_lasso
log(bestlambda_lasso)
coeff_lasso_1=coef(lasso_cv_1,s='lambda.min',exact=TRUE)
min(lasso_cv_1$cvm)

# predict on test data
set.seed(0)
lasso_fit_1 = glmnet(x[train, ],y[train],alpha = 1,lambda = lasso_cv_1$lambda.min, intercept=FALSE)
y_pred = predict(lasso_fit_1,x[test, ])
mse_lasso_1 = sum((y_pred-y.test)^2)/length(y.test)
print(mse_lasso_1)
mae_lasso_1 = sum(abs(y_pred-y.test))/length(y.test)
print(mae_lasso_1)

plot(y.test,y_pred,xlab="Test logerror",ylab="Predicted logerror",
     main="Prediction using Lasso regression")
text(-1,3,substitute(r^2 == r2,list(r2=cor(y.test,y_pred))),adj=0)
text(-1,2.7,substitute(MSE == r2,list(r2=mse_lasso)),adj=0)
abline(0,1)


xn = model.matrix(~.,lasso)
pred_10_lasso <- predict(lasso.models, data = train_1[, train_1$month == 10])
pred_11_lasso <- predict(lasso.models, data = train_1[, train_1$month == 11])
pred_12_lasso <- predict(lasso.models, data = train_1[, train_1$month == 12])

results_lasso <- data.table(parcelid=train_1$parcelid, 
                      '201610'=pred_10_lasso, 
                      '201611'=pred_11_lasso, 
                      '201612'=pred_12_lasso, 
                      '201710'=pred_10_lasso,
                      '201711'=pred_11_lasso,
                      '201712'=pred_12_lasso
)

####
###
##
# XGBoost
library(xgboost)

prop_new <- train_1[, !names(train_1) %in% c("logerror", "transactiondate")]
prop_oct_new <- prop_new %>%  filter(month == 10)
prop_nov_new <- prop_new %>%  filter(month == 11)
prop_dec_new <- prop_new %>%  filter(month == 12)

target <- train_1$logerror 

dtrain <- train_1[, !names(train_1) %in% c('logerror', 'parcelid', 'year')]

feature_names <- names(dtrain)
#cv.train <- train_4[, c( 'parcelid','year')]
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

saveRDS(xgb_cv, file = "XGB_CV.rds")

xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     preProcess = c("center", "scale"),
                     nrounds=nrounds,
                     #nrounds=1500,
                     #verbose=1,
                     print_every_n = 5)

saveRDS(xgb_mod, file = "XGBTrain.rds")

importance_matrix <- xgb.importance(feature_names,model=xgb_mod)
saveRDS(importance_matrix, file = "XGBoost_Importancematrix.rds")

ggplot(importance_matrix,aes(x=reorder(Feature,Gain),y=Gain,fill=Gain)) +
  geom_bar(stat='identity',fill='turquoise4') +coord_flip() + theme_bw() + ggtitle('Variable Importance using XGBoost') +xlab(NULL) +
  theme(plot.title = element_text(hjust = 0.5))

dtest <- xgb.DMatrix(data=data.matrix(prop_new[,names(prop_new) %in% feature_names]))
dtest_oct <- xgb.DMatrix(data=data.matrix( prop_oct_new[, names(prop_oct_new) %in% feature_names]))
dtest_nov <- xgb.DMatrix(data=data.matrix( prop_nov_new[, names(prop_nov_new) %in% feature_names]))
dtest_dec <- xgb.DMatrix(data=data.matrix( prop_dec_new[, names(prop_dec_new) %in% feature_names]))

preds <- predict(xgb_mod,dtest)
preds10 <- predict(xgb_mod,dtest_oct)
preds11 <- predict(xgb_mod,dtest_nov)
preds12 <- predict(xgb_mod,dtest_dec)

results_xgboost <- data.table(parcelid=prop_new$parcelid, 
                      '201610'=preds10, 
                      '201611'=preds11, 
                      '201612'=preds12, 
                      '201710'=preds10,
                      '201711'=preds11,
                      '201712'=preds12
)