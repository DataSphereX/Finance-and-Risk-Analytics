setwd(" ")
getwd()
## [1] " "
library(lmtest)
## Warning: package 'lmtest' was built under R version 3.4.4
## Loading required package: zoo
## Warning: package 'zoo' was built under R version 3.4.4
##
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
##
## as.Date, as.Date.numeric
library(pscl)
## Warning: package 'pscl' was built under R version 3.4.4
## Classes and Methods for R developed in the
## Political Science Computational Laboratory
## Department of Political Science
## Stanford University
## Simon Jackman
## hurdle and zeroinfl functions by Achim Zeileis
library(ROCR)
## Warning: package 'ROCR' was built under R version 3.4.4
## Loading required package: gplots
## Warning: package 'gplots' was built under R version 3.4.4
##
## Attaching package: 'gplots'
## The following object is masked from 'package:stats':
##
## lowess
library(pROC)
## Warning: package 'pROC' was built under R version 3.4.4
## Type 'citation("pROC")' for a citation.
##
## Attaching package: 'pROC'
## The following objects are masked from 'package:stats':
##
## cov, smooth, var
library(caret)
## Warning: package 'caret' was built under R version 3.4.4
## Loading required package: lattice
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 3.4.4
# install.packages("Deducer")
library(Deducer)
## Warning: package 'Deducer' was built under R version 3.4.4
## Loading required package: JGR
## Warning: package 'JGR' was built under R version 3.4.4
## Loading required package: rJava
## Warning: package 'rJava' was built under R version 3.4.4
## Loading required package: JavaGD
## Warning: package 'JavaGD' was built under R version 3.4.4
##
## Please type JGR() to launch console. Platform specific launchers (.exe and
.app) can also be obtained at http://www.rforge.net/JGR/files/.
## Loading required package: car
## Warning: package 'car' was built under R version 3.4.4
## Loading required package: carData
## Warning: package 'carData' was built under R version 3.4.4
## Loading required package: MASS
##
##
## Note Non-JGR console detected:
## Deducer is best used from within JGR (http://jgr.markushelbig.org/).
## To Bring up GUI dialogs, type deducer().
library(ggplot2)
#install.packages("rJava")
library(rJava)
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_181/bin')
# Read the training and test data trainData
<- read.csv("Training_Data.csv")
View(trainData)
testData <- read.csv("Test_Data.csv")
View(testData)
# Build logistic regression Model
logitModel <- glm(Prescription ~ Lagged_Returns, data = trainData, family =
                    binomial)
# anova() to analyze the devaiance
anova(logitModel, test="Chisq")
## Analysis of Deviance Table
##
## Model: binomial, link: logit
##
## Response: Prescription
##
## Terms added sequentially (first to last)
##
##
## Df Deviance Resid. Df Resid. Dev Pr(>Chi)
## NULL 125 174.67
## Lagged_Returns 1 4.3556 124 170.32 0.03689 *
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Overall significance of the Logistic Regression to test
Applicability lrtest(logitModel)
## Likelihood ratio test
##
## Model 1: Prescription ~ Lagged_Returns
## Model 2: Prescription ~ 1
## #Df LogLik Df Chisq Pr(>Chisq)
## 1 2 -85.159
## 2 1 -87.337 -1 4.3556 0.03689 *
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# McFadden R Square and Interpretation
pR2(logitModel)
## llh llhNull G2 McFadden r2ML
## -85.15875583 -87.33654475 4.35557784 0.02493560 0.03397743
## r2CU
## 0.04530324
# Summary of the model
summary(logitModel)
##
## Call:
## glm(formula = Prescription ~ Lagged_Returns, family = binomial,
## data = trainData)
##
## Deviance Residuals:
## Min 1Q Median 3Q Max
## -1.7985 -1.1366 0.1346 1.1483 1.4857
##
## Coefficients:
## Estimate Std. Error z value Pr(>|z|)
## (Intercept) -0.0008332 0.1812373 -0.005 0.9963
## Lagged_Returns 37.0917281 18.6910542 1.984 0.0472 *
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## (Dispersion parameter for binomial family taken to be 1)
##
## Null deviance: 174.67 on 125 degrees of freedom
## Residual deviance: 170.32 on 124 degrees of freedom
## AIC: 174.32
##
## Number of Fisher Scoring iterations: 4
# Odds Ratio
odds <- exp(coef(logitModel))
probability <- odds / (1+odds)
odds
## (Intercept) Lagged_Returns
## 9.991672e-01 1.284496e+16
probability
## (Intercept) Lagged_Returns
## 0.4997917 1.0000000
# Let us predict on the training data
trainData$predictions <- predict(logitModel, trainData, type = "response")
trainData$predictions <- ifelse(trainData$predictions > 0.5,1,0)
rocplot(logitModel)
ggplot(trainData, aes(trainData$predictions, color =
                        as.factor(trainData$Prescription)))+ geom_density()
# Confusion matrix
confusionMatrix(data = as.factor(trainData$predictions), reference
                = as.factor(trainData$Prescription))
## Confusion Matrix and Statistics
##
## Reference
## Prediction 0 1
## 0 42 27
## 1 21 36
##
## Accuracy : 0.619
## 95% CI : (0.5283, 0.7041)
## No Information Rate : 0.5
## P-Value [Acc > NIR] : 0.004752
##
## Kappa : 0.2381
## Mcnemar's Test P-Value : 0.470486
##
## Sensitivity : 0.6667
## Specificity : 0.5714
## Pos Pred Value : 0.6087
## Neg Pred Value : 0.6316
## Prevalence : 0.5000
## Detection Rate : 0.3333
## Detection Prevalence : 0.5476
## Balanced Accuracy : 0.6190
##
## 'Positive' Class : 0
##
tabValues <- table(trainData$predictions, trainData$Prescription)
sum(diag(tabValues)/sum(tabValues))
## [1] 0.6190476
1 - sum(diag(tabValues)/sum(tabValues))
## [1] 0.3809524
ROCRpred <- prediction(trainData$predictions, trainData$Prescription)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
# ROC & AUC Plots
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at =
       seq(0,1,0.1))
auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc
## [1] 0.6190476
# Write the predicted values in an output file
write.csv(trainData, "Logit_Train_Predictions.csv")
# Let us predict on the test data
testData$predictions <- predict(logitModel,newdata = testData,type
                                = 'response')
testData$predictions <- ifelse(testData$predictions > 0.5,1,0)
testData$predictions <- ifelse(testData$predictions > 0.5,1,0)
ggplot(testData, aes(testData$predictions, color =
                       as.factor(testData$Prescription)))+ geom_density()
# Confusion matrix
confusionMatrix(data=as.factor(testData$predictions),
                reference=as.factor(testData$Prescription))
## Confusion Matrix and Statistics
##
## Reference
## Prediction 0 1
## 0 29 34
## 1 29 32
##
## Accuracy : 0.4919
## 95% CI : (0.4011, 0.5832)
## No Information Rate : 0.5323
## P-Value [Acc > NIR] : 0.8389
##
## Kappa : -0.0151
## Mcnemar's Test P-Value : 0.6143
##
## Sensitivity : 0.5000
## Specificity : 0.4848
## Pos Pred Value : 0.4603
## Neg Pred Value : 0.5246
## Prevalence : 0.4677
## Detection Rate : 0.2339
## Detection Prevalence : 0.5081
## Balanced Accuracy : 0.4924
##
## 'Positive' Class : 0
##
tabValues <- table(testData$predictions, testData$Prescription)
sum(diag(tabValues)/sum(tabValues))
## [1] 0.4919355
1 - sum(diag(tabValues)/sum(tabValues))
## [1] 0.5080645
ROCRpred <- prediction(testData$predictions, testData$Prescription)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
# ROC & AUC Plots
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at =
       seq(0,1,0.1))
auc

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]

auc
## [1] 0.4924242
# Write the predicted values in an output file
write.csv(testData, "Logit_Test_Predictions.csv")
