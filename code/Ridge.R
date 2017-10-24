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