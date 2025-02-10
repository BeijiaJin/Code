install.packages("ISLR")
install.packages("dplyr")
library(ISLR)
library(dplyr)
data("Hitters")
head(Hitters)
Hitters=select(Hitters,-'League',-'Division',-'NewLeague')
d=na.omit(Hitters)
insample_fraction = 0.5
seed = 1
set.seed(1)
target_name = 'Salary'
preds_names = setdiff(names(Hitters),target_name)

names(d)
summary(d)
cor(d)

n = dim(d)[1]
idx_iis = sample(n,n*insample_fraction)
idx_oos = setdiff(1:n,idx_iis)
X_iis = as.matrix(d[idx_iis,preds_names])
y_iis = d[idx_iis,target_name]
X_oos = as.matrix(d[idx_oos,preds_names])
y_oos = d[idx_oos,target_name]
p = dim(X_iis)[2]

MyPlot = function(X,y,b,mytitle=''){
  yhat= X%*%b
  R2 = cor(y,yhat)^2
  MSE = mean(sum((y-yhat)^2))
  plot(y,yhat,cex=0.1)
  abline(a=0,b=1,col='red')
  title(paste(mytitle,'\nR2',round(R2,2),'\nMSE',round(MSE)))
}

fit = lm.fit(X_iis,y_iis)
b_fit = fit$coefficients
MyPlot(X_iis,y_iis,b_fit,'In sample')
MyPlot(X_oos,y_oos,b_fit,'Out of Sample')

f_RSS = function(b){
  sum((y_iis-X_iis %*% b)^2 + lambda*sum(b^2))
}
b_init = matrix(0,p,1)
lambda = 0
fit_optim = optim(b_init, f_RSS)
b_optim = fit_optim$par

MyPlot(X_iis,y_iis,b_optim,'In Sample')
MyPlot(X_oos,y_oos,b_optim,'Out of Sample')

X = X_iis
y = y_iis
Xt = t(X)
lambdas = 10^seq(-2,2,length.out = 10)
nLambdas = length(lambdas)
MSE_vec = vector('numeric',nLambdas)
for(i in 1:nLambdas){
  lambda = lambdas[i]
  fit_optim = optim(b_init,f_RSS)
  b_optim = fit_optim$par
  ypred = X_oos%*%b_optim
  MSE_vec[i] = mean(sum((y_oos-ypred)^2))
}
plot(log(lambdas),MSE_vec)
lambdas[which.min(MSE_vec)]