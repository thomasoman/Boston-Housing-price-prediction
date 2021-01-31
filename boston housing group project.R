install.packages("nclSLR", repos="http://R-Forge.R-project.org")
library(nclSLR)
## Load the data 
data(Boston, package="nclSLR")
## Check size
dim(Boston)
head(Boston)
summary(Boston)
help(Boston)
pairs(Boston)
X1_raw = as.matrix(Boston[,2:14])
class(X1_raw)
X1 = scale(X1_raw)
head(X1)
y = Boston[,1]
BostonData = data.frame(y,X1)
scale_data = BostonData
scale_data$y
install.packages("leaps")
library(leaps)
train_set = sample(1:nrow(scale_data),round(0.7*nrow(scale_data),0))
train = scale_data[train_set,]
test_set=setdiff(1:nrow(scale_data),train_set)
test = scale_data[test_set,]
dim(train)
dim(test)
bss = regsubsets(train$y~.,data=train,method="exhaustive", nvmax = 13)    
bss_summary = summary(bss)
bss_summary
bss_summary$adjr2
bss_summary$cp
bss_summary$bic
best_adjr2 = which.max(bss_summary$adjr2)
best_cp = which.min(bss_summary$cp)
best_bic = which.min(bss_summary$bic)
par(mfrow =c(1,1))
plot(1:13,bss_summary$adjr2,xlab = "Number of predictors", ylab ="Adjusted Rsq",type = 'b' )
points(best_adjr2,bss_summary$adjr2[best_adjr2],col ="red",pch = 16)
plot(1:13, bss_summary$cp, xlab="Number of predictors", ylab="Cp", type="b")
points(best_cp, bss_summary$cp[best_cp], col="red", pch=16)
plot(1:13, bss_summary$bic, xlab="Number of predictors", ylab="BIC", type="b")
points(best_bic, bss_summary$bic[best_bic], col="red", pch=16)
coef(bss,5)
data = train$zn+train$nox+train$disf+train$rad+train$lstat
crimeratetraininglm = lm(train$y~train$zn+train$nox+train$disf+train$rad+train$lstat)
summary(crimeratetraininglm)
predictions = predict.lm(crimeratetraininglm,newdata = data.frame(data = test$zn+test$nox+test$disf+test$rad+test$lstat))
boxplot(test$y,ylim=c(-5,5),ylab = "Actual values")
boxplot(predictions,ylim=c(-5,5),ylab = "predicted values")
summary(predictions)
summary(test$y)               
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  return(mat[,xvars] %*% coefi) 
  }
p = 13
fold_index = sample(nfolds, 506, replace=TRUE) 
nfolds = 10
cv_bss_errors = matrix(NA, p, nfolds)

for(k in 1:nfolds) {
  # Fit models M_1,...,M_p by best-subset selection using all but the k-th fold
  bss_tmp_fit = regsubsets(y ~ ., data=scale_data[fold_index!=k,], method="exhaustive", nvmax=p) 
  # For each model M_m where m=1,...,p: 
  for(m in 1:p) { 
    # Compute fitted values for the k-th fold 
    bss_tmp_predict = predict(bss_tmp_fit, scale_data[fold_index==k,], m)
    # Work out MSE for the k-th fold
    cv_bss_errors[m, k] = mean((scale_data[fold_index==k,]$y - bss_tmp_predict)^2) 
    } 
  } 
cv_bss_errors
fold_sizes = numeric(nfolds)
for(k in 1:nfolds) fold_sizes[k] = length(which(fold_index==k))
fold_sizes
bss_mse = numeric(p)
# For models M_1,...,M_p:
for(m in 1:p) {
  bss_mse[m] = weighted.mean(cv_bss_errors[m,], w=fold_sizes)
  }
bss_mse
best_cv = which.min(bss_mse)







