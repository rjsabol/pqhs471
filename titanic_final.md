titanic\_final
================
Rachel Sabol
February 6, 2018

``` r
path1="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/titanic_train.csv"
path2="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/titanic_test.csv"
path3="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/titanic_predictions.csv"
titanic=read.csv(path1)
titanic_test=read.csv(path2)

titanic$Sex=factor(titanic$Sex, levels=c("male", "female"), labels=c(0,1)) #male 0, female 1
med_age=median(titanic$Age, na.rm=TRUE)
titanic$Age[is.na(titanic$Age)]=med_age

titanic_test$Sex=factor(titanic_test$Sex, levels=c("male", "female"), labels=c(0,1))
titanic_test$Age[is.na(titanic_test$Age)]=med_age


glm.fit=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=titanic, family=binomial, na.action=na.exclude)
summary(glm.fit)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
    ##     Fare + Embarked, family = binomial, data = titanic, na.action = na.exclude)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6389  -0.5884  -0.4198   0.6200   2.4440  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  14.819400 610.559102   0.024   0.9806    
    ## Pclass       -1.098660   0.143543  -7.654 1.95e-14 ***
    ## Sex1          2.719922   0.200663  13.555  < 2e-16 ***
    ## Age          -0.039330   0.007845  -5.013 5.35e-07 ***
    ## SibSp        -0.324239   0.108889  -2.978   0.0029 ** 
    ## Parch        -0.088638   0.118505  -0.748   0.4545    
    ## Fare          0.001940   0.002374   0.817   0.4138    
    ## EmbarkedC   -12.292402 610.558906  -0.020   0.9839    
    ## EmbarkedQ   -12.355233 610.558958  -0.020   0.9839    
    ## EmbarkedS   -12.703877 610.558890  -0.021   0.9834    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  784.93  on 881  degrees of freedom
    ## AIC: 804.93
    ## 
    ## Number of Fisher Scoring iterations: 13

``` r
glm.probs=predict(glm.fit, newdata = titanic_test, type="response")
glm.pred=rep(0,nrow(titanic_test))
glm.pred[glm.probs>.5]=1

prediction_matrix=cbind(glm.pred)
row.names(prediction_matrix)=titanic_test$PassengerId

write.csv(prediction_matrix, path3)
```

There is a lot to be done with this data set in terms of prediction modeling. In general, I was very surprised at how well the basic logistic regression model performed on my test subset of the trianing titanic data. The predictive capability was almost 0.80, which I found to be high for such a simple method. I suppose this speaks to the power of even simple modeling in some cases. In terms of imrpoving the basic model, I was a little surprised that restricting the model to less variables didn't help the outcome. I suppose that all of the given variables would have fairly important contributing power, simply given the problem at hand. My biggest concern about my model was the use of the median age for the NA values. I did not want to exclude the age variable in general, since it had a significant p-value in the logistic regression. However, removing the passengers with NAs would have elimiataed 20% of the data, which seemed like too much of a loss. I wanted to attempt a multiple imputation, but my computer unfortunately could not handle the computational resources (I included some of the code in my full file, but the program kept crashing when I tried to run it. I'd like to try running this again on the HPC in the future). After a brief check, it seems as though adding the median value did not harm the model so I decided to keep it as part of the code. I think one of the greatest strengths of this model lies in the simplicity, something I saw mirrored in other published codes online.

After looking through some other documentations online, I found it very interesting that some people were able to recode the categorical variables to try to stratify the data into classes. One example was taking the name coding and breaking it down into a hierarchy of "Ms., Mrs., etc.", "Mr." and "Dr." to see if this would help break up this variable. I had tried some recoding with dummy variables in my model, in particular with the Embarking location, but it brought the quality of my model down. I thought that the name recoding was a clever work around that and a useful way to include some more potentially beneficial information. This could be applied to the ticket classes and cabins as well.

What I would really like to try in the future is utilizing a deep learning algorithm with this data set. I had originally thought of using the keras package to interact with the googlenet, but I felt that a model of that sort was beyond the scope of completion in this limited time frame. However, I think that it would really improve the quality of the model and I suspect that using that kind of algorithm is the solution to getting accurate predictions up to 1.000.
