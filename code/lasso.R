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