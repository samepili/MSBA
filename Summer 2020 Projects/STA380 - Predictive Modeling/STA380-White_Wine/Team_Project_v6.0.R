set.seed(12345) #Seed to guarantee same results
library(readr)
library(stringr)
library(corrplot)
library(visdat)
library(class) ## a library with lots of classification tools
library(kknn) ## knn library
library(glmnet) ## Lasso +Ridge
library(car) ##VIF analysis
library(tree)
library(rpart)
library(randomForest)
White_wine <- read_delim("winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(White_wine)
attach(White_wine, warn.conflicts = FALSE)
colnames(White_wine) # Check variables

summary(White_wine) # Summarize data

# Rename variables
fixed.acidity <- `fixed acidity`
volatile.acidity <- `volatile acidity`
citric.acid <- `citric acid`
residual.sugar <- `residual sugar`
free.sulfur.dioxide <- `free sulfur dioxide`
total.sulfur.dioxide <- `total sulfur dioxide`

names(White_wine)<-str_replace_all(names(White_wine), c(" " = "."))
# This part of the code is about getting rid of duplicates
# we can decide later if we want to do this - for now
# we might as well use all the data. I'm thinking the same thing
# for outliers. Might as well include them to make sure we're not
# just making a better fit at the expense of the true relationship
# b/w the predictors and the quality of the wine. After we run thru
# with all the data we can start taking out outliers and dups.

#         Duplicate values
#nrow(White_wine)
#!duplicated(White_wine)
#     there are 1000 duplicated values

#       Remove duplicate values from the data
#White_wine <- White_wine[!duplicated(White_wine), ]
#dim(White_wine)

# Check for missing values
dev.off() # run if getting "Error: invalid graphics state"
vis_miss(White_wine) # no missing values, :)

table(quality)
# Most rankings 5,6,7. Very few scored 3 or 9
# There are no quality scores for 0, 1, 2, 10

# Exploratrory Analysis
par(mfrow=c(3,4))
#barplot(table(quality),ylab = "Frequency", xlab = "Quality",col = c("yellow4","orange4","orange3","orange2","orange4","orange"))
hist(fixed.acidity,breaks = 20, col="orange2", main = NULL)
hist(volatile.acidity,breaks = 20, col="orange2", main = NULL)
hist(citric.acid,breaks = 20, col="orange2", main = NULL)
hist(residual.sugar,breaks = 20, col="orange2", main = NULL)
hist(chlorides,breaks = 20, col="orange2", main = NULL)
hist(free.sulfur.dioxide,breaks = 20, col="orange2", main = NULL)
hist(total.sulfur.dioxide, breaks = 20, col="orange2", main = NULL)
hist(density,breaks = 20, col="orange2", main = NULL)
hist(pH,breaks = 20, col="orange2", main = NULL)
hist(sulphates,breaks = 20, col="orange2", main = NULL)
hist(alcohol,breaks = 20, col="orange2", main = NULL)
hist(quality,breaks = 20, col="orange2", main = NULL)

#Boxplots
par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)

boxplot(fixed.acidity, col="orange2", pch=20)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(volatile.acidity, col="orange2", pch=20)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)
boxplot(citric.acid, col="orange2", pch=20)
mtext("Citric Acid", cex=0.8, side=1, line=2)
boxplot(residual.sugar, col="orange2", pch=20)
mtext("Residual Sugar", cex=0.8, side=1, line=2)
boxplot(chlorides, col="orange2", pch=20)
mtext("Chlorides", cex=0.8, side=1, line=2)
par(mfrow=c(1,6), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(alcohol, col="orange2", pch=20)
mtext("Alcohol", cex=0.8, side=1, line=2)
boxplot(density, col="orange2", pch=20)
mtext("Density", cex=0.8, side=1, line=2)
boxplot(free.sulfur.dioxide, col="orange2", pch=20)
mtext("Free Sulfur Dioxide", cex=0.8, side=1, line=2)
boxplot(pH, col="orange2", pch=20)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(sulphates, col="orange2", pch=20)
mtext("Sulphates", cex=0.8, side=1, line=2)
boxplot(total.sulfur.dioxide, col="orange2", pch=20)
mtext("Total Sulfur Dioxide", cex=0.8, side=1, line=2)

# The histogram of each predictor tells us its distribution. Some of the predictors
# like fixed acidity, total sulfur dioxide and pH appear to be normally distributed.

## SUMMARY STATS + CORRELATIONS
library(psych)
describe(White_wine)

# Bivariate Analysis
pairs(White_wine) #Pairwise scatterplot
cor(White_wine[,-12])
cor(White_wine[,-12], method="spearman")
par(mfrow=c(1,1))
corrplot(cor(White_wine),type = 'lower')
# Pearson correlation evaluates the linear relationship between two continuous variables
# Spearman correlation evaluates the monotonic relationship between two continuous or ordinal variables

#MULTIVARIATE ANALYSIS
base_model <- lm(quality~., data = White_wine) #simple linear regression, no interactions
vif(base_model) #throw out density because VIF is >> 10

data_df = data.frame(White_wine[-8]) # Get rid of density
new_model <- lm(quality~., data = data_df)
vif(new_model) #VIFs without density

# KNN K-Fold
#Data frame of the data
# Model for KNN

n = dim(White_wine)[1]

#Define the number of folds
kcv = 10 #In this case, 10-fold cross validation
#This k has nothing to do with the k from knn

#Size of the fold (which is the number of elements in the test matrix)
n0 = round(n/kcv, #Number of observations in the fold
           0) #Rounding with 0 decimals

#Number of neighbors for different models
kk <- 1:100

#MSE matrix
out_MSE = matrix(0, #matrix filled with zeroes
                 nrow = kcv, #number of rows
                 ncol = length(kk)) #number of columns

#Vector of indices that have already been used inside the for
used = NULL

#The set of indices not used (will be updated removing the used)
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){ #If the set of 'not used' is > than the size of the fold
    val = sample(set, size = n0) #then sample indices from the set
  }
  
  if(n0>=length(set)){ #If the set of 'not used' is <= than the size of the fold
    val=set #then use all of the remaining indices as the sample
  }
  
  #Create the train and test matrices
  train_i = data_df[-val,] #Every observation except the ones whose indices were sampled
  test_i = data_df[val,] #The observations whose indices sampled
  
  for(i in kk){
    
    #The current model
    near = kknn(quality~fixed.acidity+volatile.acidity+
    citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
    pH+sulphates+alcohol, #The formula
                train = train_i, #The train matrix/df
                test = test_i, #The test matrix/df
                k=i, #Number of neighbors
                kernel = "rectangular") #Type of kernel (see help for more)
    
    #Calculating the MSE of current model
    aux = mean((test_i[,11]-near$fitted)^2)
    
    #Store the current MSE
    out_MSE[j,i] = aux
  }
  
  #The union of the indices used currently and previously
  used = union(used,val)
  
  #The set of indices not used is updated
  set = (1:n)[-used]
  
  #Printing on the console the information that you want
  #Useful to keep track of the progress of your loop
  cat(j,"folds out of",kcv,'\n')
}

out_MSE<-out_MSE[1:9,]
#Calculate the mean of MSE for each k
mMSE = apply(out_MSE, #Receive a matrix
             2, #Takes its columns (it would take its rows if this argument was 1)
             mean) #And for each column, calculate the mean

dev.off()
#Complexity x RMSE graph
plot(log(1/kk),sqrt(mMSE),
     xlab="Complexity (log(1/k))",
     ylab="out-of-sample RMSE",
     col=4, #Color of line
     lwd=2, #Line width
     type="l", #Type of graph = line
     cex.lab=1.2, #Size of labs
     main=paste("kfold(",kcv,")")) #Title of the graph

#Find the index of the minimum value of mMSE
best = which.min(mMSE)

sqrt(min(mMSE))

#Inclusing text at specific coordinates of the graph
# Manually plug in "best"
text(log(1/kk[best]),sqrt(mMSE[best]), #Coordinates
     paste("k=",kk[best]), #The actual text
     col=2, #Color of the text
     cex=1.2) #Size of the text
text(log(1/2),sqrt(mMSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)


## REGRESSION
# Stepwise Regression
White_wine_no_density<-White_wine[,-8]

XXWhite_wine <- model.matrix(~.*alcohol*pH, data=data.frame(scale(White_wine_no_density[1:10])))[,-1]
colnames(XXWhite_wine)

DF_XX_White_wine = data.frame(quality,XXWhite_wine)
View(DF_XX_White_wine)
n = dim(DF_XX_White_wine)[1] #Sample size

tr = sample(1:n, #Sample indices do be used in training
            size = 2500, #Sample will have 5000 observation
            replace = FALSE)

train = DF_XX_White_wine[tr,]
test = DF_XX_White_wine[-tr,]

#4 models
#Two models initially
null_log = lm(log(quality)~1, data=train) #only has an intercept
null = lm(quality~1, data=train)
full = glm(quality~., data=train) #Has all the selected variables
full_log = glm(log(quality)~., data=train)

#Let us select models by stepwise
regForward = step(null, #The most simple model
                  scope=formula(full),#Let us analyze all models until the full model
                  direction="forward", #Adding variables
                  k=log(length(train))) #This is BIC
regForward_log = step(null_log, #The most simple model
                  scope=formula(full),#Let us analyze all models until the full model
                  direction="forward", #Adding variables
                  k=log(length(train))) #This is BIC
regBack = step(full, #Starting with the full model
               direction="backward", #And deleting variables
               k=log(length(train))) #This is BIC
regBack_log = step(full_log, #Starting with the full model
               direction="backward", #And deleting variables
               k=log(length(train))) #This is BIC
regForward_w_delete = step(null, #The most simple model
                            scope=formula(full), #The most complicated model
                            direction="both", #Add or delete variables
                            k=log(length(train))) #This is BIC
regForward_w_delete_log = step(null_log, #The most simple model
                           scope=formula(full), #The most complicated model
                           direction="both", #Add or delete variables
                           k=log(length(train))) #This is BIC

#Stepwise Model Outcomes:
#Stepwise forwards predicting quality
RF_model <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + residual.sugar.pH + 
                 volatile.acidity.alcohol + fixed.acidity + free.sulfur.dioxide.alcohol + 
                 free.sulfur.dioxide + chlorides.alcohol + citric.acid.pH + 
                 free.sulfur.dioxide.pH + volatile.acidity.pH.alcohol + fixed.acidity.alcohol + 
                 sulphates + total.sulfur.dioxide + fixed.acidity.pH.alcohol + 
                 chlorides + citric.acid.pH.alcohol + total.sulfur.dioxide.pH.alcohol + 
                 pH.alcohol + pH + fixed.acidity.pH, data = DF_XX_White_wine)
#Stepwise forwards predicting log(quality) 
RF_model_log <- lm(log(quality) ~ alcohol + volatile.acidity + residual.sugar + 
                     volatile.acidity.alcohol + fixed.acidity + residual.sugar.pH + 
                     citric.acid.pH + free.sulfur.dioxide.pH + free.sulfur.dioxide.alcohol + 
                     free.sulfur.dioxide + total.sulfur.dioxide + sulphates + 
                     volatile.acidity.pH.alcohol + chlorides.alcohol + fixed.acidity.alcohol + 
                     fixed.acidity.pH.alcohol + chlorides + citric.acid.pH.alcohol + 
                     total.sulfur.dioxide.pH.alcohol + pH.alcohol + pH + fixed.acidity.pH, data = DF_XX_White_wine)
#Stepwise backwards predicting quality 
RB_model <- lm(quality ~ volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + 
                 total.sulfur.dioxide + pH + sulphates + alcohol + fixed.acidity.alcohol + 
                 volatile.acidity.alcohol + chlorides.alcohol + free.sulfur.dioxide.alcohol + 
                 pH.alcohol + fixed.acidity.pH + free.sulfur.dioxide.pH + 
                 pH.sulphates + fixed.acidity.pH.alcohol + volatile.acidity.pH.alcohol + 
                 citric.acid.pH.alcohol + total.sulfur.dioxide.pH.alcohol, data = DF_XX_White_wine)

#Stepwise backwards predicting log(quality) 
RB_model_log <- lm(log(quality) ~ volatile.acidity + residual.sugar + chlorides + 
                     free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + 
                     alcohol + volatile.acidity.alcohol + chlorides.alcohol + 
                     free.sulfur.dioxide.alcohol + pH.alcohol + fixed.acidity.pH + 
                     free.sulfur.dioxide.pH + fixed.acidity.pH.alcohol + volatile.acidity.pH.alcohol + 
                     citric.acid.pH.alcohol + total.sulfur.dioxide.pH.alcohol,data = DF_XX_White_wine)

#Stepwise forward with deletion predicting log(quality) 
RFS_model <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + volatile.acidity.alcohol + 
                  free.sulfur.dioxide.alcohol + free.sulfur.dioxide + chlorides.alcohol + 
                  free.sulfur.dioxide.pH + volatile.acidity.pH.alcohol + sulphates + 
                  total.sulfur.dioxide + fixed.acidity.pH.alcohol + chlorides + 
                  citric.acid.pH.alcohol + total.sulfur.dioxide.pH.alcohol + 
                  pH.alcohol + pH + fixed.acidity.pH, data = DF_XX_White_wine)

#Stepwise forward with deletion predicting quality
RFS_model_log <- lm(log(quality) ~ alcohol + volatile.acidity + residual.sugar + 
                      volatile.acidity.alcohol + free.sulfur.dioxide.pH + free.sulfur.dioxide.alcohol + 
                      free.sulfur.dioxide + total.sulfur.dioxide + sulphates + 
                      volatile.acidity.pH.alcohol + chlorides.alcohol + fixed.acidity.pH.alcohol + 
                      chlorides + citric.acid.pH.alcohol + total.sulfur.dioxide.pH.alcohol + 
                      pH.alcohol + pH + fixed.acidity.pH, data=DF_XX_White_wine)

# Summary Statistics from Stepwise models (We tried including fixed acidity, volatile acidity
# and residual sugar, using only alcohol and pH gave use better SE's and R^2's)
summary(RF_model) #SE 0.7357, AjR^2 0.3099
summary(RB_model) #SE 0.7357, AjR^2 0.3100
summary(RFS_model) #SE 0.7361, AjR^2 0.3091

# Summary Statistics from Stepwise models Log(Quality models)
summary(RF_model_log) #SE 0.1287, AjR^2 0.3011
summary(RB_model_log) #SE 0.1287, AjR^2 0.3005
summary(RFS_model_log) #SE 0.1287, AjR^2 0.3005 

#Plot Training sets
#RF_model PLOT
attach(DF_XX_White_wine[])


plot(train$alcohol + train$volatile.acidity + train$residual.sugar + train$total.sulfur.dioxide.pH + 
       train$fixed.acidity + train$free.sulfur.dioxide + train$free.sulfur.dioxide.alcohol + 
       train$volatile.acidity.alcohol + train$sulphates + train$fixed.acidity.pH + 
       train$residual.sugar.alcohol + train$free.sulfur.dioxide.pH + train$residual.sugar.pH + 
       train$chlorides + train$chlorides.alcohol + train$citric.acid.pH.alcohol + 
       train$volatile.acidity.pH.alcohol + train$fixed.acidity.alcohol + train$chlorides.pH, train$quality)

plot(RF_model)
#RB_model_log(QUALITY) PLOT
plot(train$fixed.acidity + train$volatile.acidity + train$residual.sugar + 
       train$chlorides + train$free.sulfur.dioxide + train$sulphates + train$alcohol + train$fixed.acidity.alcohol + 
       train$volatile.acidity.alcohol + train$residual.sugar.alcohol + train$chlorides.alcohol + 
       train$free.sulfur.dioxide.alcohol + train$total.sulfur.dioxide.alcohol + 
       train$fixed.acidity.pH + train$residual.sugar.pH + train$free.sulfur.dioxide.pH + 
       train$volatile.acidity.pH.alcohol + train$citric.acid.pH.alcohol + train$total.sulfur.dioxide.pH.alcohol, log(train$quality))

# Plot Residuals of Train
hist(residuals(RB_model))
hist(residuals(RF_model_log))

# Regression with model from stepwise forwards predicting quality
testmodel <- lm(quality ~ volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + 
                  total.sulfur.dioxide + pH + sulphates + alcohol + fixed.acidity.alcohol + 
                  volatile.acidity.alcohol + chlorides.alcohol + free.sulfur.dioxide.alcohol + 
                  pH.alcohol + fixed.acidity.pH + free.sulfur.dioxide.pH + 
                  pH.sulphates + fixed.acidity.pH.alcohol + volatile.acidity.pH.alcohol + 
                  citric.acid.pH.alcohol + total.sulfur.dioxide.pH.alcohol, data=test)

testmodel_log <- lm(log(quality) ~ alcohol + volatile.acidity + residual.sugar + 
                      volatile.acidity.alcohol + fixed.acidity + residual.sugar.pH + 
                      citric.acid.pH + free.sulfur.dioxide.pH + free.sulfur.dioxide.alcohol + 
                      free.sulfur.dioxide + total.sulfur.dioxide + sulphates + 
                      volatile.acidity.pH.alcohol + chlorides.alcohol + fixed.acidity.alcohol + 
                      fixed.acidity.pH.alcohol + chlorides + citric.acid.pH.alcohol + 
                      total.sulfur.dioxide.pH.alcohol + pH.alcohol + pH + fixed.acidity.pH, data = test)

## Summary Stats for test data
summary(testmodel) #SE 0.7517, AjR^2 0.2931
summary(testmodel_log) #SE 0.1313, AjR^2 0.2847

# testmodel plot
plot(test$alcohol + test$volatile.acidity + test$residual.sugar + test$total.sulfur.dioxide.pH + 
       test$fixed.acidity + test$free.sulfur.dioxide + test$free.sulfur.dioxide.alcohol + 
       test$volatile.acidity.alcohol + test$sulphates + test$fixed.acidity.pH + 
       test$residual.sugar.alcohol + test$free.sulfur.dioxide.pH + test$residual.sugar.pH + 
       test$chlorides + test$chlorides.alcohol + test$citric.acid.pH.alcohol + 
       test$volatile.acidity.pH.alcohol + test$fixed.acidity.alcohol + test$chlorides.pH,test$quality, pch=21)
# testmodel_log plot
plot(test$fixed.acidity + test$volatile.acidity + test$residual.sugar + 
       test$chlorides + test$free.sulfur.dioxide + test$sulphates + test$alcohol + test$fixed.acidity.alcohol + 
       test$volatile.acidity.alcohol + test$residual.sugar.alcohol + test$chlorides.alcohol + 
       test$free.sulfur.dioxide.alcohol + test$total.sulfur.dioxide.alcohol + 
       test$fixed.acidity.pH + test$residual.sugar.pH + test$free.sulfur.dioxide.pH + 
       test$volatile.acidity.pH.alcohol + test$citric.acid.pH.alcohol + test$total.sulfur.dioxide.pH.alcohol, test$quality)

## RIDGE LASSO
## Ridge and Lasso on train data
#LASSO and RIDGE methods
tr_tr = sample(1:n, #Sample indices do be used in training
               size = 2500, #Sample will have 5000 observation
               replace = FALSE) #Without replacement

Lasso.Fit = glmnet(XXWhite_wine[tr_tr,],quality[tr_tr],alpha=1)
Ridge.Fit = glmnet(XXWhite_wine[tr_tr,],quality[tr_tr],alpha=0)

par(mfrow=c(1,2)) #Plot window: 1 row, 2 columns

#Evaluating LASSO and RIDGE
plot(Lasso.Fit, main='Lasso Fit')
plot(Ridge.Fit,main='Ridge Fit')

#Let us do a 10-fold cv for selecting our parameter lambda
CV.L = cv.glmnet(XXWhite_wine[tr_tr,], quality[tr_tr],alpha=1) #For Lasso
CV.R = cv.glmnet(XXWhite_wine[tr_tr,], quality[tr_tr],alpha=0) #For Ridge

#Values of lambda
LamR = CV.R$lambda.1se
LamL = CV.L$lambda.1se

par(mfrow=c(1,2))#Plot window: 1 row, 2 columns
#For Ridge
plot(log(CV.R$lambda),sqrt(CV.R$cvm),#Scatterplot
     main="Ridge CV (k=10)",
     xlab="log(lambda)",
     ylab = "RMSE",
     col=4,#Color of points
     cex.lab=1.2) #Size o lab
abline(v=log(LamR),lty=2,col=2,lwd=2) #selected lambda vs RMSE

#For LASSO
plot(log(CV.L$lambda),sqrt(CV.L$cvm),
     main="LASSO CV (k=10)",xlab="log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(LamL),lty=2,col=2,lwd=2)

#Now we can check the coefs of each selected model
coef.R = predict(CV.R,type="coefficients",s=LamR)
coef.L = predict(CV.L,type="coefficients",s=LamL)

par(mfrow=c(1,1))#Plot window: 1 row, 1 column
#Comparing the coefs of the two models
plot(abs(coef.R[2:20]),abs(coef.L[2:20]),
     ylim=c(0,1),xlim=c(0,1))
abline(0,1)

# Using lambdas on test data
prediction_lasso <- predict(Lasso.Fit, newx =XXWhite_wine[-tr_tr,], s = LamL, type = "response")
prediction_ridge <- predict(Ridge.Fit, newx =XXWhite_wine[-tr_tr,], s = LamR, type = "response")
ytrue_test <- quality[-tr_tr]

RMSE_lasso <- sqrt(mean((prediction_lasso - ytrue_test)^2))
RMSE_ridge <- sqrt(mean((prediction_ridge - ytrue_test)^2))

RMSE_lasso #RMSE = 0.758093
RMSE_ridge #RMSE = 0.76094

####################
## DECISION TREES ##
####################
tree.fit <- tree(quality ~ .,data=train, mindev =.0001)
cat('First create a big tree size: \n')
print(length(unique(tree.fit$where)))
RMSE_tree = NULL
for(i in 2:100){
  White_wine.tree=prune.tree(tree.fit, #The tree model
                             best=i)
  White_wine.fit = predict(White_wine.tree, newdata = test)
  
  #RMSE
  aux <- sqrt(mean((White_wine.fit - test$quality)^2))
  RMSE_tree[i] = aux
}

minRMSE_tree = min(RMSE_tree) # which.min(RMSE_tree) + 1 = number of branches
# 11 branches is optimal

cat('Pruned tree size: \n')
print(length(unique(White_wine.tree$where)))

dev.off()
plot(White_wine.tree)
text(White_wine.tree, pretty=0, cex=0.6)




#Plot fitted training data
plot(White_wine$alcohol,White_wine$quality, #Data
     cex=.5, #Size of points
     pch=16) #Type of point
p=order(White_wine$alcohol) #Order the indices of variable alcohol
lines(White_wine$alcohol[p],White_wine.fit[p], #Fitted values (step function)
      col='red', #Color of line
      lwd=3) #Line width
cutn=c(10.85,10.1167,11.7417,12.775,8.45,9.15) #Cutpoints from tree

for(i in 1:length(cutn)){
  abline(v=cutn[i],col='yellow',lty=5) #cutpoints
}


## using this method, created a plot of all the factors paired with quality individually to see each of their effects
#Reduce df to just quality and another factor
bdf = White_wine[,c(1,12)]

#Fit a big tree using rpart.control
big.tree = rpart(quality~fixed.acidity,
                 method="anova", #split maximizes the sum-of-squares between the new partitions
                 data=bdf, #data frame
                 control=rpart.control(minsplit=5, #the minimum number of obs in each leaf
                                       cp=.0005)) #complexity parameter (see rpart.control help)
nbig = length(unique(big.tree$where))
cat('Number of leaf nodes: ',nbig,'\n')

#plot best tree

oo=order(bdf$fixed.acidity) #Order the indices of the variable
bestcp=big.tree$cptable[which.min(big.tree$cptable[,"xerror"]),"CP"] #Find best CP
#Printing Best CP
cat('Bestcp: ',bestcp,'\n')

#Pruning the tree using the cp
best.tree = prune(big.tree,cp=bestcp)

par(mfrow=c(1,1))
pfit = predict(best.tree) #Predict of the train values
plot(bdf, #Plot the data
     pch=16, #Type of point
     col='blue', #Color of the points
     cex=.5) #Size of the points
lines(bdf$fixed.acidity[oo],pfit[oo], #Ordered data
      col='red', #Color of line
      lwd=2) #Line width


###################
###RANDOM FOREST###
###################
indexes = sample(1:nrow(White_wine), size=0.5*nrow(White_wine))
wine.train <- White_wine[indexes,]
wine.test <- White_wine[-indexes,]
wine.train$FactQ<-"Low"
wine.train[wine.train$quality>5,"FactQ"]<-"High"
wine.test$FactQ<-"Low"
wine.test[wine.test$quality>5,"FactQ"]<-"High"
wine.train$FactQ<-as.factor(wine.train$FactQ)
rf.fit <- randomForest(FactQ~.-quality , data=wine.train, ntree=200, importance=T, proximity=T)
rf.pred <- predict(rf.fit, wine.test, type="class")
#Completely unsupervised random forest method 
#on Training data with ntree = 200 
#leads to the following error plot:

plot(rf.fit, main="")
#Importance of predictors are 
#given in the following dotplot:
varImpPlot(rf.fit,  main="", cex=0.8)
table(rf.pred, wine.test$FactQ)
mean(rf.pred==wine.test$FactQ)

#Get rf fits for different number of trees
n = nrow(White_wine) #Sample size
ntreev = c(100,500,2000) #Different numbers of trees based on error seen in plot previous
nset = length(ntreev) #size of the for loop

fmat = matrix(0,n,nset) #Matrix of fits

for(i in 1:nset) {
  cat('Random Forest model: ',i,"; Number of Trees: ", ntreev[i],'\n')
  rffit = randomForest(quality~alcohol             , #Formula
                       data=White_wine, #Data frame
                       ntree=ntreev[i], #Number of trees in the forest
                       maxnodes=15) #Maximum number of nodes in each tree
  fmat[,i] = predict(rffit) #Predicted values for the fits
}

par(mfrow=c(1,1)) #Plot window: 1 row, 1 column
#plot oob error using last fitted rffit which has the largest ntree.
plot(rffit)

#Plot fits
par(mfrow=c(1,3))#Plot window: 1 row, 1 column
oo = order(White_wine$alcohol)
for(i in 1:nset) {
  plot(White_wine$alcohol,White_wine$quality, #Plot data
       xlab='alcohol',ylab='quality')
  lines(White_wine$alcohol[oo],fmat[oo,i], #Plot ordered fitted values
        col=(i+1), #Line color
        lwd=3) #Line width
  title(main=paste('Bagging ntrees = ',ntreev[i]))
}

######### fit best random forest on train, get fit on test and rmse

n = dim(White_wine)[1] #Sample size

tr = sample(1:n, #Sample indices do be used in training
            size = 2500, #Sample will have 5000 observation
            replace = FALSE)

train = White_wine[tr,]
test = White_wine[-tr,]

#Fit on train+val
set.seed(12345) #Seed to guarantee the same results

finrf = randomForest(quality~., #Formula
                     data=train, #Data frame
                     mtry=3, #Number of candidates variables for each split
                     ntree=500, #Number of trees
                     maxnodes = 15) #Maximum number of leaves
finrfpred=predict(finrf,newdata=test) #Get predictions of test set

#Plot y vs yhat for test data
finrfrmse = sqrt(sum((test$quality-finrfpred)^2)/nrow(test)) #Test set RMSE
cat('finrfrmse: ',finrfrmse,'\n')
par(mfrow = c(1,1)) #Plot window: 1 row, 1 column
plot(test$quality,finrfpred, #Scatterplot for quality of the fit
     xlab='test quality',ylab='rf pred')
abline(0,1,col='red',lwd=2)


################
### Boosting ###
################

#rm(list=ls()) #Removes every object from your environment

library(gbm) #boost package


#With attach you can call the columns of the df as objects
#Beware with this function! Be sure to NOT create objects with the same name
#as the name of the columns of the data frame (df).
attach(White_wine)


#Fit boosting for various number of trees
#set.seed(12345) #Seed to guarantee the same results
n = nrow(White_wine) #Sample size
ntreev = c(50,200,1000) #Number of trees on boosting
nset = length(ntreev) #size of the for loop

fmat = matrix(0,n,nset) #Matrix of fits

for(i in 1:nset) {
  cat('Boosting model: ',i,"; Number of Trees: ", ntreev[i],'\n')
  boostfit = gbm(quality~alcohol, #Formula
                 data=White_wine, #Data frame
                 distribution='gaussian',
                 interaction.depth=2, #Maximum depth of each tree
                 n.trees=ntreev[i], #Number of trees
                 shrinkage=.2) #Learning rate
  fmat[,i] = predict(boostfit,n.trees=ntreev[i]) #Predicted values for the fits
}


#Plot fits
par(mfrow=c(1,3))#Plot window: 1 row, 1 column
oo = order(White_wine$alcohol)
for(i in 1:nset) {
  plot(White_wine$alcohol,White_wine$quality, #Plot data
       xlab='alcohol',ylab='quality')
  lines(White_wine$alcohol[oo],fmat[oo,i], #Plot ordered fitted values
        col=(i+1), #Line color
        lwd=3) #Line width
  title(main=paste('Boosting ntrees = ',ntreev[i]))
}

######### fit best boosting on train, get fit on test and rmse

n = dim(White_wine)[1] #Sample size

tr = sample(1:n, #Sample indices do be used in training
            size = 2500, #Sample will have 5000 observation
            replace = FALSE)

train = White_wine[tr,]
test = White_wine[-tr,]

#Fit on train+val
set.seed(12345) #Seed to guarantee the same results
#trainval = rbind(train,caval) #Binding the two data frames
ntrees=200 #Number of trees
finb = gbm(quality~., #Formula
           data=train, #Data frame
           distribution='gaussian',
           interaction.depth=4, #Max tree depth
           n.trees=ntrees, #Number of trees
           shrinkage=.2) #Learning rate
finbpred=predict(finb,newdata=test,n.trees=ntrees) #Get predictions from test data

#Plot y vs yhat for test data and compute rmse on test.
finbrmse = sqrt(sum((test$quality-finbpred)^2)/nrow(test)) #Test RMSE
cat('finbrmse: ',finbrmse,'\n')
#plot(test$quality,finbpred, #Scatterplot for the quality of fits
#     xlab='test logMedVal',ylab='boost pred')
#abline(0,1,col='red',lwd=2)

#Plot variable importance
p=ncol(train)-1 #Get number of variables for later
vsum=summary(finb) #this object will have the variable importance info
row.names(vsum)=NULL #Drop varable names from rows.

#write variable importance table
print(vsum)

#Plot variable importance
plot(vsum$rel.inf,axes=F,pch=16,col='red')
axis(1,labels=vsum$var,at=1:p)
axis(2)
for(i in 1:p){
  lines(c(i,i),c(0,vsum$rel.inf[i]),lwd=4,col='blue')
}

#Partial dependence plots
par(mfrow=c(3,3)) #Plot window: 2 rows, 3 columns
nms = names(train)[1:9]
for(i in 1:9){
  plot(finb,i.var=nms[i]) #Plot pdps of gbm model
}



