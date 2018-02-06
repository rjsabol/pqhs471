Titanic
================
Rachel Sabol
February 5, 2018

Read in the data

``` r
path1="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/titanic_train.csv"
path2="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/titanic_test.csv"
titanic=read.csv(path1)
titanic_test=read.csv(path2)
fix(titanic)
```

Visualize and explore the data

``` r
dim(titanic)
```

    ## [1] 891  12

``` r
pairs(titanic)
```

![](titanic_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
summary(titanic)
```

    ##   PassengerId       Survived          Pclass     
    ##  Min.   :  1.0   Min.   :0.0000   Min.   :1.000  
    ##  1st Qu.:223.5   1st Qu.:0.0000   1st Qu.:2.000  
    ##  Median :446.0   Median :0.0000   Median :3.000  
    ##  Mean   :446.0   Mean   :0.3838   Mean   :2.309  
    ##  3rd Qu.:668.5   3rd Qu.:1.0000   3rd Qu.:3.000  
    ##  Max.   :891.0   Max.   :1.0000   Max.   :3.000  
    ##                                                  
    ##                                     Name         Sex           Age       
    ##  Abbing, Mr. Anthony                  :  1   female:314   Min.   : 0.42  
    ##  Abbott, Mr. Rossmore Edward          :  1   male  :577   1st Qu.:20.12  
    ##  Abbott, Mrs. Stanton (Rosa Hunt)     :  1                Median :28.00  
    ##  Abelson, Mr. Samuel                  :  1                Mean   :29.70  
    ##  Abelson, Mrs. Samuel (Hannah Wizosky):  1                3rd Qu.:38.00  
    ##  Adahl, Mr. Mauritz Nils Martin       :  1                Max.   :80.00  
    ##  (Other)                              :885                NA's   :177    
    ##      SibSp           Parch             Ticket         Fare       
    ##  Min.   :0.000   Min.   :0.0000   1601    :  7   Min.   :  0.00  
    ##  1st Qu.:0.000   1st Qu.:0.0000   347082  :  7   1st Qu.:  7.91  
    ##  Median :0.000   Median :0.0000   CA. 2343:  7   Median : 14.45  
    ##  Mean   :0.523   Mean   :0.3816   3101295 :  6   Mean   : 32.20  
    ##  3rd Qu.:1.000   3rd Qu.:0.0000   347088  :  6   3rd Qu.: 31.00  
    ##  Max.   :8.000   Max.   :6.0000   CA 2144 :  6   Max.   :512.33  
    ##                                   (Other) :852                   
    ##          Cabin     Embarked
    ##             :687    :  2   
    ##  B96 B98    :  4   C:168   
    ##  C23 C25 C27:  4   Q: 77   
    ##  G6         :  4   S:644   
    ##  C22 C26    :  3           
    ##  D          :  3           
    ##  (Other)    :186

Change some of the categorical variables to numeric using dummy variables:

``` r
titanic$Sex=factor(titanic$Sex, levels=c("male", "female"), labels=c(0,1)) #male 0, female 1
numeric_titanic=titanic[, sapply(titanic, is.numeric)]
cor(numeric_titanic, use="pairwise.complete.obs")
```

    ##              PassengerId     Survived      Pclass         Age       SibSp
    ## PassengerId  1.000000000 -0.005006661 -0.03514399  0.03684720 -0.05752683
    ## Survived    -0.005006661  1.000000000 -0.33848104 -0.07722109 -0.03532250
    ## Pclass      -0.035143994 -0.338481036  1.00000000 -0.36922602  0.08308136
    ## Age          0.036847198 -0.077221095 -0.36922602  1.00000000 -0.30824676
    ## SibSp       -0.057526834 -0.035322499  0.08308136 -0.30824676  1.00000000
    ## Parch       -0.001652012  0.081629407  0.01844267 -0.18911926  0.41483770
    ## Fare         0.012658219  0.257306522 -0.54949962  0.09606669  0.15965104
    ##                    Parch        Fare
    ## PassengerId -0.001652012  0.01265822
    ## Survived     0.081629407  0.25730652
    ## Pclass       0.018442671 -0.54949962
    ## Age         -0.189119263  0.09606669
    ## SibSp        0.414837699  0.15965104
    ## Parch        1.000000000  0.21622494
    ## Fare         0.216224945  1.00000000

``` r
sum(is.na(titanic$Age)) #missing about 20% of Age data
```

    ## [1] 177

Fare seems to be the most significantly correlated variable with survival. However, the missing age data might end up being a problem. I'll check and see what other variables are missing signficant amounts of information:

``` r
for (i in 1:ncol(titanic)){
  x=sum(is.na(titanic[,i]))
  cat(colnames(titanic)[i], x, "\n")
}
```

    ## PassengerId 0 
    ## Survived 0 
    ## Pclass 0 
    ## Name 0 
    ## Sex 0 
    ## Age 177 
    ## SibSp 0 
    ## Parch 0 
    ## Ticket 0 
    ## Fare 0 
    ## Cabin 0 
    ## Embarked 0

It looks like Age is the only variable with missing values so it needs to be determined what to do with those values. Losing 20% of the data is a lot and Age could add at least moderate predictive capability (at least based on the correlation matrix) so I do not want to remove it entirely. I will try multiiple imputation. I'm going to try the mice package, after a brief search of useful tools.

``` r
library(mice)
```

    ## Warning: package 'mice' was built under R version 3.4.3

    ## Loading required package: lattice

``` r
hist(titanic$Age)
```

![](titanic_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#imputed_data=mice(titanic)
```

Just kidding, the mice function takes forever to run so for right now we will just add in the median, even though it is not ideal.

``` r
med_age=median(titanic$Age, na.rm=TRUE)
titanic$Age[is.na(titanic$Age)]=med_age
hist(titanic$Age)
```

![](titanic_files/figure-markdown_github/unnamed-chunk-8-1.png) Clearly this skews the distribution, but we will leave it for now.

``` r
## 75% of the sample size
sample_size=floor(0.75 * nrow(titanic))
set.seed(17)
train_ind=sample(seq_len(nrow(titanic)), size = sample_size)
train=titanic[train_ind, ]
test=titanic[-train_ind, ]
```

Trying a logistic regression model (running with variables name, ticket, cabin didn't make sense):

``` r
glm.fit=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, family=binomial)
summary(glm.fit)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
    ##     Fare + Embarked, family = binomial, data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6151  -0.5897  -0.3885   0.5960   2.3687  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  1.340e+01  5.354e+02   0.025   0.9800    
    ## Pclass      -1.172e+00  1.651e-01  -7.100 1.25e-12 ***
    ## Sex1         2.851e+00  2.388e-01  11.939  < 2e-16 ***
    ## Age         -4.093e-02  9.376e-03  -4.366 1.27e-05 ***
    ## SibSp       -3.706e-01  1.257e-01  -2.948   0.0032 ** 
    ## Parch       -9.604e-02  1.436e-01  -0.669   0.5035    
    ## Fare         5.338e-04  2.423e-03   0.220   0.8257    
    ## EmbarkedC   -1.080e+01  5.354e+02  -0.020   0.9839    
    ## EmbarkedQ   -1.075e+01  5.354e+02  -0.020   0.9840    
    ## EmbarkedS   -1.113e+01  5.354e+02  -0.021   0.9834    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 882.30  on 667  degrees of freedom
    ## Residual deviance: 570.94  on 658  degrees of freedom
    ## AIC: 590.94
    ## 
    ## Number of Fisher Scoring iterations: 12

Try the test set with this model before modifying:

``` r
glm.probs=predict(glm.fit, newdata = test, type="response")
glm.pred=rep(0,223)
glm.pred[glm.probs>.5]=1
table(glm.pred, test$Survived)
```

    ##         
    ## glm.pred   0   1
    ##        0 116  32
    ##        1  14  61

``` r
mean(glm.pred==test$Survived)
```

    ## [1] 0.793722

Trying a logistic regression with only a subset of variables:

``` r
glm.fit=glm(Survived~Pclass+Sex+Age+SibSp, data=train, family=binomial)
summary(glm.fit)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Sex + Age + SibSp, family = binomial, 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6899  -0.5859  -0.3889   0.5882   2.5266  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  2.42388    0.49008   4.946 7.58e-07 ***
    ## Pclass      -1.19939    0.14049  -8.537  < 2e-16 ***
    ## Sex1         2.86321    0.23045  12.424  < 2e-16 ***
    ## Age         -0.04089    0.00931  -4.392 1.12e-05 ***
    ## SibSp       -0.41535    0.12053  -3.446 0.000569 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 882.30  on 667  degrees of freedom
    ## Residual deviance: 574.03  on 663  degrees of freedom
    ## AIC: 584.03
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
glm.probs=predict(glm.fit, newdata = test, type="response")
glm.pred=rep(0,223)
glm.pred[glm.probs>.5]=1
table(glm.pred, test$Survived)
```

    ##         
    ## glm.pred   0   1
    ##        0 110  30
    ##        1  20  63

``` r
mean(glm.pred==test$Survived)
```

    ## [1] 0.7757848

Unfortunately not better.

Let's try LDA now:

``` r
library(MASS)
lda.fit=lda(Survived~PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train)
lda.fit
```

    ## Call:
    ## lda(Survived ~ PassengerId + Pclass + Sex + Age + SibSp + Parch + 
    ##     Fare + Embarked, data = train)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.6272455 0.3727545 
    ## 
    ## Group means:
    ##   PassengerId   Pclass      Sex1      Age     SibSp     Parch     Fare
    ## 0    438.8687 2.525060 0.1479714 29.67661 0.6109785 0.3508353 23.08818
    ## 1    435.6546 1.923695 0.6907631 28.35944 0.4618474 0.4618474 47.30589
    ##   EmbarkedC  EmbarkedQ EmbarkedS
    ## 0 0.1384248 0.08353222 0.7780430
    ## 1 0.2690763 0.09638554 0.6305221
    ## 
    ## Coefficients of linear discriminants:
    ##                       LD1
    ## PassengerId  0.0002213353
    ## Pclass      -0.7400603426
    ## Sex1         2.1706172987
    ## Age         -0.0241330445
    ## SibSp       -0.1806313567
    ## Parch       -0.0646956428
    ## Fare         0.0006058321
    ## EmbarkedC   -0.2495345999
    ## EmbarkedQ   -0.1889312600
    ## EmbarkedS   -0.4674210936

``` r
plot(lda.fit)
```

![](titanic_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
lda.pred=predict(lda.fit, test)
lda.class=lda.pred$class
table(lda.class, test$Survived)
```

    ##          
    ## lda.class   0   1
    ##         0 114  31
    ##         1  16  62

``` r
mean(lda.class==test$Survived)
```

    ## [1] 0.7892377

Try QDA:

``` r
qda.fit=qda(Survived~+Pclass+Sex+Age+SibSp+Parch+Fare, data=train)
qda.class=predict(qda.fit,test)$class
table(qda.class, test$Survived)
```

    ##          
    ## qda.class   0   1
    ##         0 112  30
    ##         1  18  63

``` r
mean(qda.class==test$Survived)
```

    ## [1] 0.7847534

There's not enough information so we will skip it. KNN?

``` r
library(class)
train.X=train[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")]
test.X=test[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")]
set.seed(17)
knn.pred=knn(train.X, test.X, train$Survived, k=5)
table(knn.pred, test$Survived)
```

    ##         
    ## knn.pred   0   1
    ##        0 106  41
    ##        1  24  52

``` r
mean(knn.pred==test$Survived)
```

    ## [1] 0.7085202

It looks like the logistic regression performs the best.

Now let's compare to performance if we do not touch the NAs for age at all:

``` r
path1="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/titanic_train.csv"
path2="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/titanic_test.csv"
titanic=read.csv(path1)
titanic_test=read.csv(path2)
fix(titanic)
titanic$Sex=factor(titanic$Sex, levels=c("male", "female"), labels=c(0,1)) #male 0, female 1
numeric_titanic=titanic[, sapply(titanic, is.numeric)]
sample_size=floor(0.75 * nrow(titanic))
set.seed(17)
train_ind=sample(seq_len(nrow(titanic)), size = sample_size)
train=titanic[train_ind, ]
test=titanic[-train_ind, ]
```

``` r
glm.fit=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, family=binomial)
summary(glm.fit)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
    ##     Fare + Embarked, family = binomial, data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7185  -0.6439  -0.3483   0.5848   2.3707  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  1.369e+01  5.354e+02   0.026   0.9796    
    ## Pclass      -1.262e+00  1.880e-01  -6.711 1.94e-11 ***
    ## Sex1         2.850e+00  2.681e-01  10.628  < 2e-16 ***
    ## Age         -4.586e-02  9.857e-03  -4.653 3.28e-06 ***
    ## SibSp       -4.551e-01  1.480e-01  -3.076   0.0021 ** 
    ## Parch       -8.788e-02  1.503e-01  -0.585   0.5587    
    ## Fare         3.669e-04  2.615e-03   0.140   0.8884    
    ## EmbarkedC   -1.057e+01  5.354e+02  -0.020   0.9842    
    ## EmbarkedQ   -1.133e+01  5.354e+02  -0.021   0.9831    
    ## EmbarkedS   -1.094e+01  5.354e+02  -0.020   0.9837    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 721.14  on 535  degrees of freedom
    ## Residual deviance: 460.40  on 526  degrees of freedom
    ##   (132 observations deleted due to missingness)
    ## AIC: 480.4
    ## 
    ## Number of Fisher Scoring iterations: 12

``` r
glm.probs=predict(glm.fit, newdata = test, type="response")
glm.pred=rep(0,223)
glm.pred[glm.probs>.5]=1
table(glm.pred, test$Survived)
```

    ##         
    ## glm.pred   0   1
    ##        0 118  43
    ##        1  12  50

``` r
mean(glm.pred==test$Survived)
```

    ## [1] 0.7533632

It looks like the logistic regression is performing worse without the median values for age, so it looks like it is okay to keep the median replacements in the code. Try LDA too just in case:

``` r
lda.fit=lda(Survived~PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, na.action=na.omit)
lda.fit
```

    ## Call:
    ## lda(Survived ~ PassengerId + Pclass + Sex + Age + SibSp + Parch + 
    ##     Fare + Embarked, data = train, na.action = na.omit)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.6007463 0.3992537 
    ## 
    ## Group means:
    ##   PassengerId   Pclass      Sex1      Age     SibSp     Parch     Fare
    ## 0    435.6211 2.475155 0.1521739 30.18168 0.5714286 0.3819876 23.60525
    ## 1    449.0234 1.850467 0.6915888 28.41822 0.4859813 0.5186916 50.42459
    ##   EmbarkedC  EmbarkedQ EmbarkedS
    ## 0  0.121118 0.04658385 0.8322981
    ## 1  0.271028 0.03271028 0.6915888
    ## 
    ## Coefficients of linear discriminants:
    ##                       LD1
    ## PassengerId  0.0003564764
    ## Pclass      -0.7797665970
    ## Sex1         2.0986468635
    ## Age         -0.0268676463
    ## SibSp       -0.2501936595
    ## Parch       -0.0444539276
    ## Fare         0.0003916681
    ## EmbarkedC   -0.1861303957
    ## EmbarkedQ   -0.4807115948
    ## EmbarkedS   -0.4328311863

``` r
plot(lda.fit)
```

![](titanic_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
lda.pred=predict(lda.fit, test)
```

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

``` r
lda.class=lda.pred$class
table(lda.class, test$Survived)
```

    ##          
    ## lda.class  0  1
    ##         0 88 26
    ##         1 14 50

``` r
mean(lda.class==test$Survived, na.rm=TRUE)
```

    ## [1] 0.7752809

Still not better than the original logistic regression. It looks like that is the best we can do for now. Cleaned up code and summary in file titanic\_final.md.

Things to improve: get information out of the name category using dummy variables
