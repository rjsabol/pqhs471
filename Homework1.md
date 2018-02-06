Homework 1
================
Rachel Sabol
February 5, 2018

Week 1 Assignment 1

1.  Matrix warm-up: unsure if there are any deliverables on this assignment.
2.  RStudio installed.
3.  R warm-up (ISLR Chapter 2 Lab):

``` r
x <- c(1,3,2,5)
x
```

    ## [1] 1 3 2 5

``` r
x = c(1,6,2)
x
```

    ## [1] 1 6 2

``` r
y=c(1,4,3)
y
```

    ## [1] 1 4 3

``` r
length(x)
```

    ## [1] 3

``` r
length(y)
```

    ## [1] 3

``` r
x+y
```

    ## [1]  2 10  5

``` r
ls()
```

    ## [1] "x" "y"

``` r
rm(x,y)
ls()
```

    ## character(0)

``` r
rm(list=ls())
```

``` r
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x
```

    ##      [,1] [,2]
    ## [1,]    1    3
    ## [2,]    2    4

``` r
x=matrix(c(1,2,3,4), 2, 2)
x
```

    ##      [,1] [,2]
    ## [1,]    1    3
    ## [2,]    2    4

``` r
matrix(c(1,2,3,4), 2, 2, byrow=TRUE)
```

    ##      [,1] [,2]
    ## [1,]    1    2
    ## [2,]    3    4

``` r
sqrt(x)
```

    ##          [,1]     [,2]
    ## [1,] 1.000000 1.732051
    ## [2,] 1.414214 2.000000

``` r
x^2
```

    ##      [,1] [,2]
    ## [1,]    1    9
    ## [2,]    4   16

``` r
x=rnorm(50)
y=x+rnorm(50, mean=50, sd=.1)
cor(x,y)
```

    ## [1] 0.9906853

``` r
set.seed(3)
y=rnorm(100)
mean(y)
```

    ## [1] 0.01103557

``` r
var(y)
```

    ## [1] 0.7328675

``` r
sqrt(var(y))
```

    ## [1] 0.8560768

``` r
sd(y)
```

    ## [1] 0.8560768

``` r
x=rnorm(100)
y=rnorm(100)
plot(x,y)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
plot(x,y,xlab="this is the x-axis", ylab="this is the y-axis", main="Plot of X vs. Y")
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()
```

    ## png 
    ##   2

``` r
x=seq(1,10)
x
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10

``` r
x=1:10
x
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10

``` r
x=seq(-pi, pi, length=50)
x
```

    ##  [1] -3.14159265 -3.01336438 -2.88513611 -2.75690784 -2.62867957
    ##  [6] -2.50045130 -2.37222302 -2.24399475 -2.11576648 -1.98753821
    ## [11] -1.85930994 -1.73108167 -1.60285339 -1.47462512 -1.34639685
    ## [16] -1.21816858 -1.08994031 -0.96171204 -0.83348377 -0.70525549
    ## [21] -0.57702722 -0.44879895 -0.32057068 -0.19234241 -0.06411414
    ## [26]  0.06411414  0.19234241  0.32057068  0.44879895  0.57702722
    ## [31]  0.70525549  0.83348377  0.96171204  1.08994031  1.21816858
    ## [36]  1.34639685  1.47462512  1.60285339  1.73108167  1.85930994
    ## [41]  1.98753821  2.11576648  2.24399475  2.37222302  2.50045130
    ## [46]  2.62867957  2.75690784  2.88513611  3.01336438  3.14159265

``` r
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45, add=T)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-11-2.png)

``` r
image(x,y,fa)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
persp(x,y,fa)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
persp(x,y,fa,theta=30)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-12-3.png)

``` r
persp(x,y,fa,theta=30, phi=20)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-12-4.png)

``` r
persp(x,y,fa,theta=30, phi=70)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-12-5.png)

``` r
persp(x,y,fa,theta=30, phi=40)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-12-6.png)

``` r
A=matrix(1:16,4,4)
A
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    5    9   13
    ## [2,]    2    6   10   14
    ## [3,]    3    7   11   15
    ## [4,]    4    8   12   16

``` r
A[2,3]
```

    ## [1] 10

``` r
A[c(1,3),c(2,4)]
```

    ##      [,1] [,2]
    ## [1,]    5   13
    ## [2,]    7   15

``` r
A[1:3,2:4]
```

    ##      [,1] [,2] [,3]
    ## [1,]    5    9   13
    ## [2,]    6   10   14
    ## [3,]    7   11   15

``` r
A[1:2,]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    5    9   13
    ## [2,]    2    6   10   14

``` r
A[,1:2]
```

    ##      [,1] [,2]
    ## [1,]    1    5
    ## [2,]    2    6
    ## [3,]    3    7
    ## [4,]    4    8

``` r
A[1,]
```

    ## [1]  1  5  9 13

``` r
A[-c(1,3),]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    2    6   10   14
    ## [2,]    4    8   12   16

``` r
dim(A)
```

    ## [1] 4 4

``` r
path="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/Auto.data.txt"
Auto=read.table(path)
fix(Auto)
```

``` r
Auto=read.table(path, header=T, na.strings="?")
fix(Auto)
dim(Auto)
```

    ## [1] 397   9

``` r
Auto[1:4,]
```

    ##   mpg cylinders displacement horsepower weight acceleration year origin
    ## 1  18         8          307        130   3504         12.0   70      1
    ## 2  15         8          350        165   3693         11.5   70      1
    ## 3  18         8          318        150   3436         11.0   70      1
    ## 4  16         8          304        150   3433         12.0   70      1
    ##                        name
    ## 1 chevrolet chevelle malibu
    ## 2         buick skylark 320
    ## 3        plymouth satellite
    ## 4             amc rebel sst

``` r
Auto=na.omit(Auto)
dim(Auto)
```

    ## [1] 392   9

``` r
names(Auto)
```

    ## [1] "mpg"          "cylinders"    "displacement" "horsepower"  
    ## [5] "weight"       "acceleration" "year"         "origin"      
    ## [9] "name"

``` r
plot(Auto$cylinders, Auto$mpg)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
attach(Auto)
plot(cylinders, mpg)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-18-2.png)

``` r
cylinders=as.factor(cylinders)
plot(cylinders, mpg)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
plot(cylinders, mpg, col="red")
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-19-2.png)

``` r
plot(cylinders, mpg, col="red", varwidth=T)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-19-3.png)

``` r
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-19-4.png)

``` r
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-19-5.png)

``` r
hist(mpg)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
hist(mpg, col=2)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-20-2.png)

``` r
hist(mpg, col=2, breaks=15)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-20-3.png)

``` r
pairs(Auto)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
pairs(~ mpg + displacement +horsepower + weight + acceleration, Auto)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-21-2.png)

``` r
plot(horsepower, mpg)
identify(horsepower, mpg, name)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-22-1.png)

    ## integer(0)

``` r
summary(Auto)
```

    ##       mpg          cylinders      displacement     horsepower   
    ##  Min.   : 9.00   Min.   :3.000   Min.   : 68.0   Min.   : 46.0  
    ##  1st Qu.:17.00   1st Qu.:4.000   1st Qu.:105.0   1st Qu.: 75.0  
    ##  Median :22.75   Median :4.000   Median :151.0   Median : 93.5  
    ##  Mean   :23.45   Mean   :5.472   Mean   :194.4   Mean   :104.5  
    ##  3rd Qu.:29.00   3rd Qu.:8.000   3rd Qu.:275.8   3rd Qu.:126.0  
    ##  Max.   :46.60   Max.   :8.000   Max.   :455.0   Max.   :230.0  
    ##                                                                 
    ##      weight      acceleration        year           origin     
    ##  Min.   :1613   Min.   : 8.00   Min.   :70.00   Min.   :1.000  
    ##  1st Qu.:2225   1st Qu.:13.78   1st Qu.:73.00   1st Qu.:1.000  
    ##  Median :2804   Median :15.50   Median :76.00   Median :1.000  
    ##  Mean   :2978   Mean   :15.54   Mean   :75.98   Mean   :1.577  
    ##  3rd Qu.:3615   3rd Qu.:17.02   3rd Qu.:79.00   3rd Qu.:2.000  
    ##  Max.   :5140   Max.   :24.80   Max.   :82.00   Max.   :3.000  
    ##                                                                
    ##                  name    
    ##  amc matador       :  5  
    ##  ford pinto        :  5  
    ##  toyota corolla    :  5  
    ##  amc gremlin       :  4  
    ##  amc hornet        :  4  
    ##  chevrolet chevette:  4  
    ##  (Other)           :365

``` r
summary(mpg)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    9.00   17.00   22.75   23.45   29.00   46.60

1.  Warm-up reading reading complete
2.  ISLR Chapter 2, HOML Chapter 1 reading complete
3.  Python and git installed

Week 1 Assignment 2 1. ISLR Chapter 2 Exercises 1,3,8

Exercise 1- a) A flexible model will likely perform better. b) An inflexible model will likely perform better. c) An inflexible model will likely perform better. d) An inflexible model will likely perform better.

Exercise 3- Graph is attached. Bias decreases as flexibility increases, as the model fits the "real life" data more closely. Training error also decreases as flexibility increases, as the model can exactly predit the points within that data set. However, testing error increases as the model is not as robust to new observations. The bayes error will also decrease, as the exact boundaries can be better estimated in flexible models. Finally, the variance increases as the flexibility increases, because the model is more sensitive to fluctuations in data.

Exercise 8

``` r
path="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/college.csv"
college=read.csv(path)
fix(college)
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)
```

``` r
summary(college)
```

    ##  Private        Apps           Accept          Enroll       Top10perc    
    ##  No :212   Min.   :   81   Min.   :   72   Min.   :  35   Min.   : 1.00  
    ##  Yes:565   1st Qu.:  776   1st Qu.:  604   1st Qu.: 242   1st Qu.:15.00  
    ##            Median : 1558   Median : 1110   Median : 434   Median :23.00  
    ##            Mean   : 3002   Mean   : 2019   Mean   : 780   Mean   :27.56  
    ##            3rd Qu.: 3624   3rd Qu.: 2424   3rd Qu.: 902   3rd Qu.:35.00  
    ##            Max.   :48094   Max.   :26330   Max.   :6392   Max.   :96.00  
    ##    Top25perc      F.Undergrad     P.Undergrad         Outstate    
    ##  Min.   :  9.0   Min.   :  139   Min.   :    1.0   Min.   : 2340  
    ##  1st Qu.: 41.0   1st Qu.:  992   1st Qu.:   95.0   1st Qu.: 7320  
    ##  Median : 54.0   Median : 1707   Median :  353.0   Median : 9990  
    ##  Mean   : 55.8   Mean   : 3700   Mean   :  855.3   Mean   :10441  
    ##  3rd Qu.: 69.0   3rd Qu.: 4005   3rd Qu.:  967.0   3rd Qu.:12925  
    ##  Max.   :100.0   Max.   :31643   Max.   :21836.0   Max.   :21700  
    ##    Room.Board       Books           Personal         PhD        
    ##  Min.   :1780   Min.   :  96.0   Min.   : 250   Min.   :  8.00  
    ##  1st Qu.:3597   1st Qu.: 470.0   1st Qu.: 850   1st Qu.: 62.00  
    ##  Median :4200   Median : 500.0   Median :1200   Median : 75.00  
    ##  Mean   :4358   Mean   : 549.4   Mean   :1341   Mean   : 72.66  
    ##  3rd Qu.:5050   3rd Qu.: 600.0   3rd Qu.:1700   3rd Qu.: 85.00  
    ##  Max.   :8124   Max.   :2340.0   Max.   :6800   Max.   :103.00  
    ##     Terminal       S.F.Ratio      perc.alumni        Expend     
    ##  Min.   : 24.0   Min.   : 2.50   Min.   : 0.00   Min.   : 3186  
    ##  1st Qu.: 71.0   1st Qu.:11.50   1st Qu.:13.00   1st Qu.: 6751  
    ##  Median : 82.0   Median :13.60   Median :21.00   Median : 8377  
    ##  Mean   : 79.7   Mean   :14.09   Mean   :22.74   Mean   : 9660  
    ##  3rd Qu.: 92.0   3rd Qu.:16.50   3rd Qu.:31.00   3rd Qu.:10830  
    ##  Max.   :100.0   Max.   :39.80   Max.   :64.00   Max.   :56233  
    ##    Grad.Rate     
    ##  Min.   : 10.00  
    ##  1st Qu.: 53.00  
    ##  Median : 65.00  
    ##  Mean   : 65.46  
    ##  3rd Qu.: 78.00  
    ##  Max.   :118.00

``` r
pairs(college[,1:10])
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
plot(college$Outstate, college$Private)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
Elite=rep("No", nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college, Elite)
summary(college)
```

    ##  Private        Apps           Accept          Enroll       Top10perc    
    ##  No :212   Min.   :   81   Min.   :   72   Min.   :  35   Min.   : 1.00  
    ##  Yes:565   1st Qu.:  776   1st Qu.:  604   1st Qu.: 242   1st Qu.:15.00  
    ##            Median : 1558   Median : 1110   Median : 434   Median :23.00  
    ##            Mean   : 3002   Mean   : 2019   Mean   : 780   Mean   :27.56  
    ##            3rd Qu.: 3624   3rd Qu.: 2424   3rd Qu.: 902   3rd Qu.:35.00  
    ##            Max.   :48094   Max.   :26330   Max.   :6392   Max.   :96.00  
    ##    Top25perc      F.Undergrad     P.Undergrad         Outstate    
    ##  Min.   :  9.0   Min.   :  139   Min.   :    1.0   Min.   : 2340  
    ##  1st Qu.: 41.0   1st Qu.:  992   1st Qu.:   95.0   1st Qu.: 7320  
    ##  Median : 54.0   Median : 1707   Median :  353.0   Median : 9990  
    ##  Mean   : 55.8   Mean   : 3700   Mean   :  855.3   Mean   :10441  
    ##  3rd Qu.: 69.0   3rd Qu.: 4005   3rd Qu.:  967.0   3rd Qu.:12925  
    ##  Max.   :100.0   Max.   :31643   Max.   :21836.0   Max.   :21700  
    ##    Room.Board       Books           Personal         PhD        
    ##  Min.   :1780   Min.   :  96.0   Min.   : 250   Min.   :  8.00  
    ##  1st Qu.:3597   1st Qu.: 470.0   1st Qu.: 850   1st Qu.: 62.00  
    ##  Median :4200   Median : 500.0   Median :1200   Median : 75.00  
    ##  Mean   :4358   Mean   : 549.4   Mean   :1341   Mean   : 72.66  
    ##  3rd Qu.:5050   3rd Qu.: 600.0   3rd Qu.:1700   3rd Qu.: 85.00  
    ##  Max.   :8124   Max.   :2340.0   Max.   :6800   Max.   :103.00  
    ##     Terminal       S.F.Ratio      perc.alumni        Expend     
    ##  Min.   : 24.0   Min.   : 2.50   Min.   : 0.00   Min.   : 3186  
    ##  1st Qu.: 71.0   1st Qu.:11.50   1st Qu.:13.00   1st Qu.: 6751  
    ##  Median : 82.0   Median :13.60   Median :21.00   Median : 8377  
    ##  Mean   : 79.7   Mean   :14.09   Mean   :22.74   Mean   : 9660  
    ##  3rd Qu.: 92.0   3rd Qu.:16.50   3rd Qu.:31.00   3rd Qu.:10830  
    ##  Max.   :100.0   Max.   :39.80   Max.   :64.00   Max.   :56233  
    ##    Grad.Rate      Elite    
    ##  Min.   : 10.00   No :699  
    ##  1st Qu.: 53.00   Yes: 78  
    ##  Median : 65.00            
    ##  Mean   : 65.46            
    ##  3rd Qu.: 78.00            
    ##  Max.   :118.00

``` r
plot(college$Outstate, college$Elite)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
par(mfrow=c(2,2))
hist(college$Accept)
hist(college$Accept, breaks=12)
hist(college$Books, breaks=3)
hist(college$Books, breaks=6)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-28-1.png) A brief summary- college acceptances are skewed to the left.

1.  Reading complete.
2.  ISLR Chapter 3 R labs

``` r
library(MASS)
library(ISLR)
```

    ## Warning: package 'ISLR' was built under R version 3.4.3

    ## 
    ## Attaching package: 'ISLR'

    ## The following object is masked _by_ '.GlobalEnv':
    ## 
    ##     Auto

``` r
fix(Boston)
names(Boston)
```

    ##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
    ##  [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"

``` r
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat)
    ## 
    ## Coefficients:
    ## (Intercept)        lstat  
    ##       34.55        -0.95

``` r
summary(lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.168  -3.990  -1.318   2.034  24.500 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 34.55384    0.56263   61.41   <2e-16 ***
    ## lstat       -0.95005    0.03873  -24.53   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.216 on 504 degrees of freedom
    ## Multiple R-squared:  0.5441, Adjusted R-squared:  0.5432 
    ## F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
names(lm.fit)
```

    ##  [1] "coefficients"  "residuals"     "effects"       "rank"         
    ##  [5] "fitted.values" "assign"        "qr"            "df.residual"  
    ##  [9] "xlevels"       "call"          "terms"         "model"

``` r
coef(lm.fit)
```

    ## (Intercept)       lstat 
    ##  34.5538409  -0.9500494

``` r
confint(lm.fit)
```

    ##                 2.5 %     97.5 %
    ## (Intercept) 33.448457 35.6592247
    ## lstat       -1.026148 -0.8739505

``` r
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval="confidence")
```

    ##        fit      lwr      upr
    ## 1 29.80359 29.00741 30.59978
    ## 2 25.05335 24.47413 25.63256
    ## 3 20.30310 19.73159 20.87461

``` r
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval="prediction")
```

    ##        fit       lwr      upr
    ## 1 29.80359 17.565675 42.04151
    ## 2 25.05335 12.827626 37.27907
    ## 3 20.30310  8.077742 32.52846

``` r
plot(lstat, medv)
abline(lm.fit)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-34-1.png)

``` r
plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-35-1.png)

``` r
plot(lstat, medv, col="red")
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-35-2.png)

``` r
plot(lstat, medv, pch=20)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-35-3.png)

``` r
plot(lstat, medv, pch="+")
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-35-4.png)

``` r
plot(1:20, 1:20, pch=1:20)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-35-5.png)

``` r
par(mfrow=c(2,2))
plot(lm.fit)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-36-1.png)

``` r
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-36-2.png)

``` r
which.max(hatvalues(lm.fit))
```

    ## 375 
    ## 375

``` r
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat + age, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.981  -3.978  -1.283   1.968  23.158 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 33.22276    0.73085  45.458  < 2e-16 ***
    ## lstat       -1.03207    0.04819 -21.416  < 2e-16 ***
    ## age          0.03454    0.01223   2.826  0.00491 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.173 on 503 degrees of freedom
    ## Multiple R-squared:  0.5513, Adjusted R-squared:  0.5495 
    ## F-statistic:   309 on 2 and 503 DF,  p-value: < 2.2e-16

``` r
lm.fit=lm(medv~., data=Boston)
summary(lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ ., data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.595  -2.730  -0.518   1.777  26.199 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
    ## crim        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
    ## zn           4.642e-02  1.373e-02   3.382 0.000778 ***
    ## indus        2.056e-02  6.150e-02   0.334 0.738288    
    ## chas         2.687e+00  8.616e-01   3.118 0.001925 ** 
    ## nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
    ## rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
    ## age          6.922e-04  1.321e-02   0.052 0.958229    
    ## dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
    ## rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
    ## tax         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
    ## ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
    ## black        9.312e-03  2.686e-03   3.467 0.000573 ***
    ## lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.745 on 492 degrees of freedom
    ## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7338 
    ## F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16

``` r
library(car)
```

    ## Warning: package 'car' was built under R version 3.4.3

``` r
vif(lm.fit)
```

    ##     crim       zn    indus     chas      nox       rm      age      dis 
    ## 1.792192 2.298758 3.991596 1.073995 4.393720 1.933744 3.100826 3.955945 
    ##      rad      tax  ptratio    black    lstat 
    ## 7.484496 9.008554 1.799084 1.348521 2.941491

``` r
lm.fit1=lm(medv~.-age, data=Boston)
summary(lm.fit1)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ . - age, data = Boston)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.6054  -2.7313  -0.5188   1.7601  26.2243 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  36.436927   5.080119   7.172 2.72e-12 ***
    ## crim         -0.108006   0.032832  -3.290 0.001075 ** 
    ## zn            0.046334   0.013613   3.404 0.000719 ***
    ## indus         0.020562   0.061433   0.335 0.737989    
    ## chas          2.689026   0.859598   3.128 0.001863 ** 
    ## nox         -17.713540   3.679308  -4.814 1.97e-06 ***
    ## rm            3.814394   0.408480   9.338  < 2e-16 ***
    ## dis          -1.478612   0.190611  -7.757 5.03e-14 ***
    ## rad           0.305786   0.066089   4.627 4.75e-06 ***
    ## tax          -0.012329   0.003755  -3.283 0.001099 ** 
    ## ptratio      -0.952211   0.130294  -7.308 1.10e-12 ***
    ## black         0.009321   0.002678   3.481 0.000544 ***
    ## lstat        -0.523852   0.047625 -10.999  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.74 on 493 degrees of freedom
    ## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7343 
    ## F-statistic: 117.3 on 12 and 493 DF,  p-value: < 2.2e-16

``` r
lm.fit1=update(lm.fit,~.-age)
summary(lm.fit1)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ crim + zn + indus + chas + nox + rm + dis + 
    ##     rad + tax + ptratio + black + lstat, data = Boston)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.6054  -2.7313  -0.5188   1.7601  26.2243 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  36.436927   5.080119   7.172 2.72e-12 ***
    ## crim         -0.108006   0.032832  -3.290 0.001075 ** 
    ## zn            0.046334   0.013613   3.404 0.000719 ***
    ## indus         0.020562   0.061433   0.335 0.737989    
    ## chas          2.689026   0.859598   3.128 0.001863 ** 
    ## nox         -17.713540   3.679308  -4.814 1.97e-06 ***
    ## rm            3.814394   0.408480   9.338  < 2e-16 ***
    ## dis          -1.478612   0.190611  -7.757 5.03e-14 ***
    ## rad           0.305786   0.066089   4.627 4.75e-06 ***
    ## tax          -0.012329   0.003755  -3.283 0.001099 ** 
    ## ptratio      -0.952211   0.130294  -7.308 1.10e-12 ***
    ## black         0.009321   0.002678   3.481 0.000544 ***
    ## lstat        -0.523852   0.047625 -10.999  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.74 on 493 degrees of freedom
    ## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7343 
    ## F-statistic: 117.3 on 12 and 493 DF,  p-value: < 2.2e-16

``` r
summary(lm(medv~lstat*age,data=Boston))
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat * age, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.806  -4.045  -1.333   2.085  27.552 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
    ## lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
    ## age         -0.0007209  0.0198792  -0.036   0.9711    
    ## lstat:age    0.0041560  0.0018518   2.244   0.0252 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.149 on 502 degrees of freedom
    ## Multiple R-squared:  0.5557, Adjusted R-squared:  0.5531 
    ## F-statistic: 209.3 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat + I(lstat^2))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.2834  -3.8313  -0.5295   2.3095  25.4148 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 42.862007   0.872084   49.15   <2e-16 ***
    ## lstat       -2.332821   0.123803  -18.84   <2e-16 ***
    ## I(lstat^2)   0.043547   0.003745   11.63   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.524 on 503 degrees of freedom
    ## Multiple R-squared:  0.6407, Adjusted R-squared:  0.6393 
    ## F-statistic: 448.5 on 2 and 503 DF,  p-value: < 2.2e-16

``` r
lm.fit=lm(medv~lstat)
anova(lm.fit, lm.fit2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: medv ~ lstat
    ## Model 2: medv ~ lstat + I(lstat^2)
    ##   Res.Df   RSS Df Sum of Sq     F    Pr(>F)    
    ## 1    504 19472                                 
    ## 2    503 15347  1    4125.1 135.2 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
par(mfrow=c(2,2))
plot(lm.fit2)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-46-1.png)

``` r
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ poly(lstat, 5))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.5433  -3.1039  -0.7052   2.0844  27.1153 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       22.5328     0.2318  97.197  < 2e-16 ***
    ## poly(lstat, 5)1 -152.4595     5.2148 -29.236  < 2e-16 ***
    ## poly(lstat, 5)2   64.2272     5.2148  12.316  < 2e-16 ***
    ## poly(lstat, 5)3  -27.0511     5.2148  -5.187 3.10e-07 ***
    ## poly(lstat, 5)4   25.4517     5.2148   4.881 1.42e-06 ***
    ## poly(lstat, 5)5  -19.2524     5.2148  -3.692 0.000247 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.215 on 500 degrees of freedom
    ## Multiple R-squared:  0.6817, Adjusted R-squared:  0.6785 
    ## F-statistic: 214.2 on 5 and 500 DF,  p-value: < 2.2e-16

``` r
summary(lm(medv~log(rm), data=Boston))
```

    ## 
    ## Call:
    ## lm(formula = medv ~ log(rm), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.487  -2.875  -0.104   2.837  39.816 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -76.488      5.028  -15.21   <2e-16 ***
    ## log(rm)       54.055      2.739   19.73   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.915 on 504 degrees of freedom
    ## Multiple R-squared:  0.4358, Adjusted R-squared:  0.4347 
    ## F-statistic: 389.3 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
fix(Carseats)
names(Carseats)
```

    ##  [1] "Sales"       "CompPrice"   "Income"      "Advertising" "Population" 
    ##  [6] "Price"       "ShelveLoc"   "Age"         "Education"   "Urban"      
    ## [11] "US"

``` r
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9208 -0.7503  0.0177  0.6754  3.3413 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.5755654  1.0087470   6.519 2.22e-10 ***
    ## CompPrice           0.0929371  0.0041183  22.567  < 2e-16 ***
    ## Income              0.0108940  0.0026044   4.183 3.57e-05 ***
    ## Advertising         0.0702462  0.0226091   3.107 0.002030 ** 
    ## Population          0.0001592  0.0003679   0.433 0.665330    
    ## Price              -0.1008064  0.0074399 -13.549  < 2e-16 ***
    ## ShelveLocGood       4.8486762  0.1528378  31.724  < 2e-16 ***
    ## ShelveLocMedium     1.9532620  0.1257682  15.531  < 2e-16 ***
    ## Age                -0.0579466  0.0159506  -3.633 0.000318 ***
    ## Education          -0.0208525  0.0196131  -1.063 0.288361    
    ## UrbanYes            0.1401597  0.1124019   1.247 0.213171    
    ## USYes              -0.1575571  0.1489234  -1.058 0.290729    
    ## Income:Advertising  0.0007510  0.0002784   2.698 0.007290 ** 
    ## Price:Age           0.0001068  0.0001333   0.801 0.423812    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.011 on 386 degrees of freedom
    ## Multiple R-squared:  0.8761, Adjusted R-squared:  0.8719 
    ## F-statistic:   210 on 13 and 386 DF,  p-value: < 2.2e-16

``` r
detach(Boston)
attach(Carseats)
contrasts(ShelveLoc)
```

    ##        Good Medium
    ## Bad       0      0
    ## Good      1      0
    ## Medium    0      1

``` r
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
```

``` r
LoadLibraries
```

    ## function(){
    ##   library(ISLR)
    ##   library(MASS)
    ##   print("The libraries have been loaded.")
    ## }

``` r
LoadLibraries()
```

    ## [1] "The libraries have been loaded."

1.  HOML Python code- in seperate file. (come back to this using jupyter and the notebook provided by HOML)

Week 2 Assignment 1 1. Reading- complete 2. ISLR Chapter 3 Lab- complete

Week 2 Assignment 2 1. ISLR Chapter 3 Exercises 9, 15

Exercise 9

``` r
path="C:/Users/Rachel Sabol/Documents/Graduate Coursework/PQHS 471/Auto.data.txt"
Auto=read.table(path, header=T, na.strings="?")
fix(Auto)
```

``` r
pairs(Auto)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-55-1.png)

``` r
names(Auto)
```

    ## [1] "mpg"          "cylinders"    "displacement" "horsepower"  
    ## [5] "weight"       "acceleration" "year"         "origin"      
    ## [9] "name"

``` r
dim(Auto)
```

    ## [1] 397   9

``` r
cor(Auto[,1:8])
```

    ##                     mpg  cylinders displacement horsepower     weight
    ## mpg           1.0000000 -0.7762599   -0.8044430         NA -0.8317389
    ## cylinders    -0.7762599  1.0000000    0.9509199         NA  0.8970169
    ## displacement -0.8044430  0.9509199    1.0000000         NA  0.9331044
    ## horsepower           NA         NA           NA          1         NA
    ## weight       -0.8317389  0.8970169    0.9331044         NA  1.0000000
    ## acceleration  0.4222974 -0.5040606   -0.5441618         NA -0.4195023
    ## year          0.5814695 -0.3467172   -0.3698041         NA -0.3079004
    ## origin        0.5636979 -0.5649716   -0.6106643         NA -0.5812652
    ##              acceleration       year     origin
    ## mpg             0.4222974  0.5814695  0.5636979
    ## cylinders      -0.5040606 -0.3467172 -0.5649716
    ## displacement   -0.5441618 -0.3698041 -0.6106643
    ## horsepower             NA         NA         NA
    ## weight         -0.4195023 -0.3079004 -0.5812652
    ## acceleration    1.0000000  0.2829009  0.2100836
    ## year            0.2829009  1.0000000  0.1843141
    ## origin          0.2100836  0.1843141  1.0000000

``` r
lm.Auto=lm(mpg~.-name, data=Auto)
summary(lm.Auto)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ . - name, data = Auto)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5903 -2.1565 -0.1169  1.8690 13.0604 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -17.218435   4.644294  -3.707  0.00024 ***
    ## cylinders     -0.493376   0.323282  -1.526  0.12780    
    ## displacement   0.019896   0.007515   2.647  0.00844 ** 
    ## horsepower    -0.016951   0.013787  -1.230  0.21963    
    ## weight        -0.006474   0.000652  -9.929  < 2e-16 ***
    ## acceleration   0.080576   0.098845   0.815  0.41548    
    ## year           0.750773   0.050973  14.729  < 2e-16 ***
    ## origin         1.426141   0.278136   5.127 4.67e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.328 on 384 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.8215, Adjusted R-squared:  0.8182 
    ## F-statistic: 252.4 on 7 and 384 DF,  p-value: < 2.2e-16

There is a relationship between the predictors and the response. The predictos displacement, weigh, year, and origin make statistically significant contributions to the response. The coefficient for the variable year suggests each year the mpg improves by 0.750773 units.

``` r
par(mfrow=c(2,2))
plot(lm.Auto)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-59-1.png) There seems to be evidence of a slightly nonlinear relationship in the plot of residuals vs. fitted. In terms of outliers, the residueals vs. leverage plot indicates the presence of some outliers and one very high leverage point which may be influencing the model.

``` r
lm.Auto2=lm(mpg~cylinders*displacement+displacement*weight+weight*cylinders, data=Auto[, 1:8])
summary(lm.Auto2)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ cylinders * displacement + displacement * 
    ##     weight + weight * cylinders, data = Auto[, 1:8])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.2147  -2.5202  -0.3991   1.7955  17.9360 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             4.903e+01  6.746e+00   7.269    2e-12 ***
    ## cylinders               1.916e+00  2.077e+00   0.923  0.35676    
    ## displacement           -9.647e-02  3.928e-02  -2.456  0.01449 *  
    ## weight                 -8.299e-03  3.026e-03  -2.742  0.00638 ** 
    ## cylinders:displacement -1.871e-03  3.827e-03  -0.489  0.62513    
    ## displacement:weight     2.542e-05  8.262e-06   3.076  0.00224 ** 
    ## cylinders:weight       -4.038e-04  6.732e-04  -0.600  0.54895    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.119 on 390 degrees of freedom
    ## Multiple R-squared:  0.7272, Adjusted R-squared:  0.723 
    ## F-statistic: 173.3 on 6 and 390 DF,  p-value: < 2.2e-16

Top interactions were chosen from the correlation matrix. The interaction between displacement and weight is statistically signficant and the others are not.

``` r
par(mfrow=c(2,2))
plot(Auto$weight, Auto$mpg)
plot(log(Auto$weight), Auto$mpg)
plot(sqrt(Auto$weight), Auto$mpg)
plot((Auto$weight)^2, Auto$mpg)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-61-1.png) The log transformation seems to make the relationship between weight and mpg the most linear.

Exercise 15

``` r
library(MASS)
library(ISLR)
fix(Boston)
```

``` r
names(Boston)
```

    ##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
    ##  [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"

``` r
fit.zn=lm(crim~zn, data=Boston)
summary(fit.zn)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ zn, data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.429 -4.222 -2.620  1.250 84.523 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.45369    0.41722  10.675  < 2e-16 ***
    ## zn          -0.07393    0.01609  -4.594 5.51e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.435 on 504 degrees of freedom
    ## Multiple R-squared:  0.04019,    Adjusted R-squared:  0.03828 
    ## F-statistic:  21.1 on 1 and 504 DF,  p-value: 5.506e-06

``` r
plot(fit.zn)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-64-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-64-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-64-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-64-4.png)

``` r
fit.indus=lm(crim~indus, data=Boston)
summary(fit.indus)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ indus, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.972  -2.698  -0.736   0.712  81.813 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.06374    0.66723  -3.093  0.00209 ** 
    ## indus        0.50978    0.05102   9.991  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.866 on 504 degrees of freedom
    ## Multiple R-squared:  0.1653, Adjusted R-squared:  0.1637 
    ## F-statistic: 99.82 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
plot(fit.indus)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-65-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-65-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-65-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-65-4.png)

``` r
fit.chas=lm(crim~chas, data=Boston)
summary(fit.chas)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ chas, data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.738 -3.661 -3.435  0.018 85.232 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   3.7444     0.3961   9.453   <2e-16 ***
    ## chas         -1.8928     1.5061  -1.257    0.209    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.597 on 504 degrees of freedom
    ## Multiple R-squared:  0.003124,   Adjusted R-squared:  0.001146 
    ## F-statistic: 1.579 on 1 and 504 DF,  p-value: 0.2094

``` r
plot(fit.chas)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-66-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-66-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-66-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-66-4.png)

``` r
fit.nox=lm(crim~nox, data=Boston)
summary(fit.nox)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ nox, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.371  -2.738  -0.974   0.559  81.728 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -13.720      1.699  -8.073 5.08e-15 ***
    ## nox           31.249      2.999  10.419  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.81 on 504 degrees of freedom
    ## Multiple R-squared:  0.1772, Adjusted R-squared:  0.1756 
    ## F-statistic: 108.6 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
plot(fit.nox)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-67-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-67-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-67-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-67-4.png)

``` r
fit.rm=lm(crim~rm, data=Boston)
summary(fit.rm)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ rm, data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -6.604 -3.952 -2.654  0.989 87.197 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   20.482      3.365   6.088 2.27e-09 ***
    ## rm            -2.684      0.532  -5.045 6.35e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.401 on 504 degrees of freedom
    ## Multiple R-squared:  0.04807,    Adjusted R-squared:  0.04618 
    ## F-statistic: 25.45 on 1 and 504 DF,  p-value: 6.347e-07

``` r
plot(fit.rm)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-68-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-68-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-68-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-68-4.png)

``` r
fit.age=lm(crim~age, data=Boston)
summary(fit.age)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ age, data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -6.789 -4.257 -1.230  1.527 82.849 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.77791    0.94398  -4.002 7.22e-05 ***
    ## age          0.10779    0.01274   8.463 2.85e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.057 on 504 degrees of freedom
    ## Multiple R-squared:  0.1244, Adjusted R-squared:  0.1227 
    ## F-statistic: 71.62 on 1 and 504 DF,  p-value: 2.855e-16

``` r
plot(fit.age)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-69-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-69-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-69-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-69-4.png)

``` r
fit.dis=lm(crim~dis, data=Boston)
summary(fit.dis)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ dis, data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -6.708 -4.134 -1.527  1.516 81.674 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   9.4993     0.7304  13.006   <2e-16 ***
    ## dis          -1.5509     0.1683  -9.213   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.965 on 504 degrees of freedom
    ## Multiple R-squared:  0.1441, Adjusted R-squared:  0.1425 
    ## F-statistic: 84.89 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
plot(fit.dis)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-70-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-70-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-70-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-70-4.png)

``` r
fit.rad=lm(crim~rad, data=Boston)
summary(fit.rad)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ rad, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -10.164  -1.381  -0.141   0.660  76.433 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.28716    0.44348  -5.157 3.61e-07 ***
    ## rad          0.61791    0.03433  17.998  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.718 on 504 degrees of freedom
    ## Multiple R-squared:  0.3913, Adjusted R-squared:   0.39 
    ## F-statistic: 323.9 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
plot(fit.rad)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-71-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-71-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-71-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-71-4.png)

``` r
fit.tax=lm(crim~tax, data=Boston)
summary(fit.tax)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ tax, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.513  -2.738  -0.194   1.065  77.696 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -8.528369   0.815809  -10.45   <2e-16 ***
    ## tax          0.029742   0.001847   16.10   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.997 on 504 degrees of freedom
    ## Multiple R-squared:  0.3396, Adjusted R-squared:  0.3383 
    ## F-statistic: 259.2 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
plot(fit.tax)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-72-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-72-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-72-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-72-4.png)

``` r
fit.ptratio=lm(crim~ptratio, data=Boston)
summary(fit.ptratio)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ ptratio, data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -7.654 -3.985 -1.912  1.825 83.353 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -17.6469     3.1473  -5.607 3.40e-08 ***
    ## ptratio       1.1520     0.1694   6.801 2.94e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.24 on 504 degrees of freedom
    ## Multiple R-squared:  0.08407,    Adjusted R-squared:  0.08225 
    ## F-statistic: 46.26 on 1 and 504 DF,  p-value: 2.943e-11

``` r
plot(fit.ptratio)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-73-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-73-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-73-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-73-4.png)

``` r
fit.black=lm(crim~black, data=Boston)
summary(fit.black)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ black, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.756  -2.299  -2.095  -1.296  86.822 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 16.553529   1.425903  11.609   <2e-16 ***
    ## black       -0.036280   0.003873  -9.367   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.946 on 504 degrees of freedom
    ## Multiple R-squared:  0.1483, Adjusted R-squared:  0.1466 
    ## F-statistic: 87.74 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
plot(fit.black)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-74-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-74-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-74-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-74-4.png)

``` r
fit.lstat=lm(crim~lstat, data=Boston)
summary(fit.lstat)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ lstat, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.925  -2.822  -0.664   1.079  82.862 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.33054    0.69376  -4.801 2.09e-06 ***
    ## lstat        0.54880    0.04776  11.491  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.664 on 504 degrees of freedom
    ## Multiple R-squared:  0.2076, Adjusted R-squared:  0.206 
    ## F-statistic:   132 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
plot(fit.lstat)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-75-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-75-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-75-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-75-4.png)

``` r
fit.medv=lm(crim~medv, data=Boston)
summary(fit.medv)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ medv, data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.071 -4.022 -2.343  1.298 80.957 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.79654    0.93419   12.63   <2e-16 ***
    ## medv        -0.36316    0.03839   -9.46   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.934 on 504 degrees of freedom
    ## Multiple R-squared:  0.1508, Adjusted R-squared:  0.1491 
    ## F-statistic: 89.49 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
plot(fit.medv)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-76-1.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-76-2.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-76-3.png)![](Homework1_files/figure-markdown_github/unnamed-chunk-76-4.png) Chas is the only predictor that does not have a signifcant p value for relationship (p&lt;0.05).

``` r
fit.all=lm(crim~., data=Boston)
summary(fit.all)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ ., data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.924 -2.120 -0.353  1.019 75.051 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  17.033228   7.234903   2.354 0.018949 *  
    ## zn            0.044855   0.018734   2.394 0.017025 *  
    ## indus        -0.063855   0.083407  -0.766 0.444294    
    ## chas         -0.749134   1.180147  -0.635 0.525867    
    ## nox         -10.313535   5.275536  -1.955 0.051152 .  
    ## rm            0.430131   0.612830   0.702 0.483089    
    ## age           0.001452   0.017925   0.081 0.935488    
    ## dis          -0.987176   0.281817  -3.503 0.000502 ***
    ## rad           0.588209   0.088049   6.680 6.46e-11 ***
    ## tax          -0.003780   0.005156  -0.733 0.463793    
    ## ptratio      -0.271081   0.186450  -1.454 0.146611    
    ## black        -0.007538   0.003673  -2.052 0.040702 *  
    ## lstat         0.126211   0.075725   1.667 0.096208 .  
    ## medv         -0.198887   0.060516  -3.287 0.001087 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.439 on 492 degrees of freedom
    ## Multiple R-squared:  0.454,  Adjusted R-squared:  0.4396 
    ## F-statistic: 31.47 on 13 and 492 DF,  p-value: < 2.2e-16

The following predictors have a p-value of under 0.05, for which we can reject the null hypothesis: zn, dis, rad, black, medv.

These results paint a different picture than the results of the individual regressions.

``` r
single=numeric(13)
single[1]=fit.zn$coefficients[2]
single[2]=fit.indus$coefficients[2]
single[3]=fit.chas$coefficients[2]
single[4]=fit.nox$coefficients[2]
single[5]=fit.rm$coefficients[2]
single[6]=fit.age$coefficients[2]
single[7]=fit.dis$coefficients[2]
single[8]=fit.rad$coefficients[2]
single[9]=fit.tax$coefficients[2]
single[10]=fit.ptratio$coefficients[2]
single[11]=fit.black$coefficients[2]
single[12]=fit.lstat$coefficients[2]
single[13]=fit.medv$coefficients[2]
                               
single                              
```

    ##  [1] -0.07393498  0.50977633 -1.89277655 31.24853120 -2.68405122
    ##  [6]  0.10778623 -1.55090168  0.61791093  0.02974225  1.15198279
    ## [11] -0.03627964  0.54880478 -0.36315992

``` r
mult=fit.all$coefficients[2:14]
mult
```

    ##            zn         indus          chas           nox            rm 
    ##   0.044855215  -0.063854824  -0.749133611 -10.313534912   0.430130506 
    ##           age           dis           rad           tax       ptratio 
    ##   0.001451643  -0.987175726   0.588208591  -0.003780016  -0.271080558 
    ##         black         lstat          medv 
    ##  -0.007537505   0.126211376  -0.198886821

``` r
plot(single, mult)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-80-1.png)

``` r
fit3.zn=lm(crim~poly(zn,3),data=Boston)
summary(fit3.zn)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(zn, 3), data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.821 -4.614 -1.294  0.473 84.130 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.6135     0.3722   9.709  < 2e-16 ***
    ## poly(zn, 3)1 -38.7498     8.3722  -4.628  4.7e-06 ***
    ## poly(zn, 3)2  23.9398     8.3722   2.859  0.00442 ** 
    ## poly(zn, 3)3 -10.0719     8.3722  -1.203  0.22954    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.372 on 502 degrees of freedom
    ## Multiple R-squared:  0.05824,    Adjusted R-squared:  0.05261 
    ## F-statistic: 10.35 on 3 and 502 DF,  p-value: 1.281e-06

``` r
fit3.indus=lm(crim~poly(indus,3),data=Boston)
summary(fit3.indus)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(indus, 3), data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -8.278 -2.514  0.054  0.764 79.713 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        3.614      0.330  10.950  < 2e-16 ***
    ## poly(indus, 3)1   78.591      7.423  10.587  < 2e-16 ***
    ## poly(indus, 3)2  -24.395      7.423  -3.286  0.00109 ** 
    ## poly(indus, 3)3  -54.130      7.423  -7.292  1.2e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.423 on 502 degrees of freedom
    ## Multiple R-squared:  0.2597, Adjusted R-squared:  0.2552 
    ## F-statistic: 58.69 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
fit2.chas=lm(crim~poly(chas,1),data=Boston)
summary(fit2.chas)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(chas, 1), data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.738 -3.661 -3.435  0.018 85.232 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.6135     0.3822   9.455   <2e-16 ***
    ## poly(chas, 1) -10.8036     8.5966  -1.257    0.209    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.597 on 504 degrees of freedom
    ## Multiple R-squared:  0.003124,   Adjusted R-squared:  0.001146 
    ## F-statistic: 1.579 on 1 and 504 DF,  p-value: 0.2094

``` r
fit3.nox=lm(crim~poly(nox,3),data=Boston)
summary(fit3.nox)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(nox, 3), data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.110 -2.068 -0.255  0.739 78.302 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.6135     0.3216  11.237  < 2e-16 ***
    ## poly(nox, 3)1  81.3720     7.2336  11.249  < 2e-16 ***
    ## poly(nox, 3)2 -28.8286     7.2336  -3.985 7.74e-05 ***
    ## poly(nox, 3)3 -60.3619     7.2336  -8.345 6.96e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.234 on 502 degrees of freedom
    ## Multiple R-squared:  0.297,  Adjusted R-squared:  0.2928 
    ## F-statistic: 70.69 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
fit3.rm=lm(crim~poly(rm,3),data=Boston)
summary(fit3.rm)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(rm, 3), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -18.485  -3.468  -2.221  -0.015  87.219 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.6135     0.3703   9.758  < 2e-16 ***
    ## poly(rm, 3)1 -42.3794     8.3297  -5.088 5.13e-07 ***
    ## poly(rm, 3)2  26.5768     8.3297   3.191  0.00151 ** 
    ## poly(rm, 3)3  -5.5103     8.3297  -0.662  0.50858    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.33 on 502 degrees of freedom
    ## Multiple R-squared:  0.06779,    Adjusted R-squared:  0.06222 
    ## F-statistic: 12.17 on 3 and 502 DF,  p-value: 1.067e-07

``` r
fit3.age=lm(crim~poly(age,3),data=Boston)
summary(fit3.age)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(age, 3), data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.762 -2.673 -0.516  0.019 82.842 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.6135     0.3485  10.368  < 2e-16 ***
    ## poly(age, 3)1  68.1820     7.8397   8.697  < 2e-16 ***
    ## poly(age, 3)2  37.4845     7.8397   4.781 2.29e-06 ***
    ## poly(age, 3)3  21.3532     7.8397   2.724  0.00668 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.84 on 502 degrees of freedom
    ## Multiple R-squared:  0.1742, Adjusted R-squared:  0.1693 
    ## F-statistic: 35.31 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
fit3.dis=lm(crim~poly(dis,3),data=Boston)
summary(fit3.dis)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(dis, 3), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -10.757  -2.588   0.031   1.267  76.378 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.6135     0.3259  11.087  < 2e-16 ***
    ## poly(dis, 3)1 -73.3886     7.3315 -10.010  < 2e-16 ***
    ## poly(dis, 3)2  56.3730     7.3315   7.689 7.87e-14 ***
    ## poly(dis, 3)3 -42.6219     7.3315  -5.814 1.09e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.331 on 502 degrees of freedom
    ## Multiple R-squared:  0.2778, Adjusted R-squared:  0.2735 
    ## F-statistic: 64.37 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
fit3.rad=lm(crim~poly(rad,3),data=Boston)
summary(fit3.rad)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(rad, 3), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -10.381  -0.412  -0.269   0.179  76.217 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.6135     0.2971  12.164  < 2e-16 ***
    ## poly(rad, 3)1 120.9074     6.6824  18.093  < 2e-16 ***
    ## poly(rad, 3)2  17.4923     6.6824   2.618  0.00912 ** 
    ## poly(rad, 3)3   4.6985     6.6824   0.703  0.48231    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.682 on 502 degrees of freedom
    ## Multiple R-squared:    0.4,  Adjusted R-squared:  0.3965 
    ## F-statistic: 111.6 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
fit3.tax=lm(crim~poly(tax,3),data=Boston)
summary(fit3.tax)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(tax, 3), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.273  -1.389   0.046   0.536  76.950 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.6135     0.3047  11.860  < 2e-16 ***
    ## poly(tax, 3)1 112.6458     6.8537  16.436  < 2e-16 ***
    ## poly(tax, 3)2  32.0873     6.8537   4.682 3.67e-06 ***
    ## poly(tax, 3)3  -7.9968     6.8537  -1.167    0.244    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.854 on 502 degrees of freedom
    ## Multiple R-squared:  0.3689, Adjusted R-squared:  0.3651 
    ## F-statistic:  97.8 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
fit3.ptratio=lm(crim~poly(ptratio,3),data=Boston)
summary(fit3.ptratio)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(ptratio, 3), data = Boston)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -6.833 -4.146 -1.655  1.408 82.697 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          3.614      0.361  10.008  < 2e-16 ***
    ## poly(ptratio, 3)1   56.045      8.122   6.901 1.57e-11 ***
    ## poly(ptratio, 3)2   24.775      8.122   3.050  0.00241 ** 
    ## poly(ptratio, 3)3  -22.280      8.122  -2.743  0.00630 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.122 on 502 degrees of freedom
    ## Multiple R-squared:  0.1138, Adjusted R-squared:  0.1085 
    ## F-statistic: 21.48 on 3 and 502 DF,  p-value: 4.171e-13

``` r
fit3.black=lm(crim~poly(black,3),data=Boston)
summary(fit3.black)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(black, 3), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.096  -2.343  -2.128  -1.439  86.790 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.6135     0.3536  10.218   <2e-16 ***
    ## poly(black, 3)1 -74.4312     7.9546  -9.357   <2e-16 ***
    ## poly(black, 3)2   5.9264     7.9546   0.745    0.457    
    ## poly(black, 3)3  -4.8346     7.9546  -0.608    0.544    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.955 on 502 degrees of freedom
    ## Multiple R-squared:  0.1498, Adjusted R-squared:  0.1448 
    ## F-statistic: 29.49 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
fit3.lstat=lm(crim~poly(lstat,3),data=Boston)
summary(fit3.lstat)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(lstat, 3), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.234  -2.151  -0.486   0.066  83.353 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.6135     0.3392  10.654   <2e-16 ***
    ## poly(lstat, 3)1  88.0697     7.6294  11.543   <2e-16 ***
    ## poly(lstat, 3)2  15.8882     7.6294   2.082   0.0378 *  
    ## poly(lstat, 3)3 -11.5740     7.6294  -1.517   0.1299    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.629 on 502 degrees of freedom
    ## Multiple R-squared:  0.2179, Adjusted R-squared:  0.2133 
    ## F-statistic: 46.63 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
fit3.medv=lm(crim~poly(medv,3),data=Boston)
summary(fit3.medv)
```

    ## 
    ## Call:
    ## lm(formula = crim ~ poly(medv, 3), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -24.427  -1.976  -0.437   0.439  73.655 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.614      0.292  12.374  < 2e-16 ***
    ## poly(medv, 3)1  -75.058      6.569 -11.426  < 2e-16 ***
    ## poly(medv, 3)2   88.086      6.569  13.409  < 2e-16 ***
    ## poly(medv, 3)3  -48.033      6.569  -7.312 1.05e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.569 on 502 degrees of freedom
    ## Multiple R-squared:  0.4202, Adjusted R-squared:  0.4167 
    ## F-statistic: 121.3 on 3 and 502 DF,  p-value: < 2.2e-16

Chapter 4 R Lab

``` r
names(Smarket)
```

    ## [1] "Year"      "Lag1"      "Lag2"      "Lag3"      "Lag4"      "Lag5"     
    ## [7] "Volume"    "Today"     "Direction"

``` r
summary(Smarket)
```

    ##       Year           Lag1                Lag2          
    ##  Min.   :2001   Min.   :-4.922000   Min.   :-4.922000  
    ##  1st Qu.:2002   1st Qu.:-0.639500   1st Qu.:-0.639500  
    ##  Median :2003   Median : 0.039000   Median : 0.039000  
    ##  Mean   :2003   Mean   : 0.003834   Mean   : 0.003919  
    ##  3rd Qu.:2004   3rd Qu.: 0.596750   3rd Qu.: 0.596750  
    ##  Max.   :2005   Max.   : 5.733000   Max.   : 5.733000  
    ##       Lag3                Lag4                Lag5         
    ##  Min.   :-4.922000   Min.   :-4.922000   Min.   :-4.92200  
    ##  1st Qu.:-0.640000   1st Qu.:-0.640000   1st Qu.:-0.64000  
    ##  Median : 0.038500   Median : 0.038500   Median : 0.03850  
    ##  Mean   : 0.001716   Mean   : 0.001636   Mean   : 0.00561  
    ##  3rd Qu.: 0.596750   3rd Qu.: 0.596750   3rd Qu.: 0.59700  
    ##  Max.   : 5.733000   Max.   : 5.733000   Max.   : 5.73300  
    ##      Volume           Today           Direction 
    ##  Min.   :0.3561   Min.   :-4.922000   Down:602  
    ##  1st Qu.:1.2574   1st Qu.:-0.639500   Up  :648  
    ##  Median :1.4229   Median : 0.038500             
    ##  Mean   :1.4783   Mean   : 0.003138             
    ##  3rd Qu.:1.6417   3rd Qu.: 0.596750             
    ##  Max.   :3.1525   Max.   : 5.733000

``` r
dim(Smarket)
```

    ## [1] 1250    9

``` r
pairs(Smarket)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-82-1.png)

``` r
cor(Smarket[,-9])
```

    ##              Year         Lag1         Lag2         Lag3         Lag4
    ## Year   1.00000000  0.029699649  0.030596422  0.033194581  0.035688718
    ## Lag1   0.02969965  1.000000000 -0.026294328 -0.010803402 -0.002985911
    ## Lag2   0.03059642 -0.026294328  1.000000000 -0.025896670 -0.010853533
    ## Lag3   0.03319458 -0.010803402 -0.025896670  1.000000000 -0.024051036
    ## Lag4   0.03568872 -0.002985911 -0.010853533 -0.024051036  1.000000000
    ## Lag5   0.02978799 -0.005674606 -0.003557949 -0.018808338 -0.027083641
    ## Volume 0.53900647  0.040909908 -0.043383215 -0.041823686 -0.048414246
    ## Today  0.03009523 -0.026155045 -0.010250033 -0.002447647 -0.006899527
    ##                Lag5      Volume        Today
    ## Year    0.029787995  0.53900647  0.030095229
    ## Lag1   -0.005674606  0.04090991 -0.026155045
    ## Lag2   -0.003557949 -0.04338321 -0.010250033
    ## Lag3   -0.018808338 -0.04182369 -0.002447647
    ## Lag4   -0.027083641 -0.04841425 -0.006899527
    ## Lag5    1.000000000 -0.02200231 -0.034860083
    ## Volume -0.022002315  1.00000000  0.014591823
    ## Today  -0.034860083  0.01459182  1.000000000

``` r
par(mfrow=c(1,2))
attach(Smarket)
plot(Volume)
plot(Year, Volume)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-84-1.png)

``` r
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
```

    ## 
    ## Call:
    ## glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
    ##     Volume, family = binomial, data = Smarket)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.446  -1.203   1.065   1.145   1.326  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) -0.126000   0.240736  -0.523    0.601
    ## Lag1        -0.073074   0.050167  -1.457    0.145
    ## Lag2        -0.042301   0.050086  -0.845    0.398
    ## Lag3         0.011085   0.049939   0.222    0.824
    ## Lag4         0.009359   0.049974   0.187    0.851
    ## Lag5         0.010313   0.049511   0.208    0.835
    ## Volume       0.135441   0.158360   0.855    0.392
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1731.2  on 1249  degrees of freedom
    ## Residual deviance: 1727.6  on 1243  degrees of freedom
    ## AIC: 1741.6
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
coef(glm.fit)
```

    ##  (Intercept)         Lag1         Lag2         Lag3         Lag4 
    ## -0.126000257 -0.073073746 -0.042301344  0.011085108  0.009358938 
    ##         Lag5       Volume 
    ##  0.010313068  0.135440659

``` r
summary(glm.fit)$coef
```

    ##                 Estimate Std. Error    z value  Pr(>|z|)
    ## (Intercept) -0.126000257 0.24073574 -0.5233966 0.6006983
    ## Lag1        -0.073073746 0.05016739 -1.4565986 0.1452272
    ## Lag2        -0.042301344 0.05008605 -0.8445733 0.3983491
    ## Lag3         0.011085108 0.04993854  0.2219750 0.8243333
    ## Lag4         0.009358938 0.04997413  0.1872757 0.8514445
    ## Lag5         0.010313068 0.04951146  0.2082966 0.8349974
    ## Volume       0.135440659 0.15835970  0.8552723 0.3924004

``` r
summary(glm.fit)$coef[,4]
```

    ## (Intercept)        Lag1        Lag2        Lag3        Lag4        Lag5 
    ##   0.6006983   0.1452272   0.3983491   0.8243333   0.8514445   0.8349974 
    ##      Volume 
    ##   0.3924004

``` r
glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]
```

    ##         1         2         3         4         5         6         7 
    ## 0.5070841 0.4814679 0.4811388 0.5152224 0.5107812 0.5069565 0.4926509 
    ##         8         9        10 
    ## 0.5092292 0.5176135 0.4888378

``` r
contrasts(Direction)
```

    ##      Up
    ## Down  0
    ## Up    1

``` r
glm.pred=rep("Down", 1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction)
```

    ##         Direction
    ## glm.pred Down  Up
    ##     Down  145 141
    ##     Up    457 507

``` r
(507+145)/1250
```

    ## [1] 0.5216

``` r
mean(glm.pred==Direction)
```

    ## [1] 0.5216

``` r
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
```

    ## [1] 252   9

``` r
Direction.2005=Direction[!train]
```

``` r
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit, Smarket.2005, type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction.2005)
```

    ##         Direction.2005
    ## glm.pred Down Up
    ##     Down   77 97
    ##     Up     34 44

``` r
mean(glm.pred==Direction.2005)
```

    ## [1] 0.4801587

``` r
mean(glm.pred!=Direction.2005)
```

    ## [1] 0.5198413

``` r
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit, Smarket.2005, type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
```

    ##         Direction.2005
    ## glm.pred Down  Up
    ##     Down   35  35
    ##     Up     76 106

``` r
mean(glm.pred==Direction.2005)
```

    ## [1] 0.5595238

``` r
predict(glm.fit, newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1, -0.8)), type="response")
```

    ##         1         2 
    ## 0.4791462 0.4960939

``` r
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
```

    ## Call:
    ## lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
    ## 
    ## Prior probabilities of groups:
    ##     Down       Up 
    ## 0.491984 0.508016 
    ## 
    ## Group means:
    ##             Lag1        Lag2
    ## Down  0.04279022  0.03389409
    ## Up   -0.03954635 -0.03132544
    ## 
    ## Coefficients of linear discriminants:
    ##             LD1
    ## Lag1 -0.6420190
    ## Lag2 -0.5135293

``` r
plot(lda.fit)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-93-1.png)

``` r
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
```

    ## [1] "class"     "posterior" "x"

``` r
lda.class=lda.pred$class
table(lda.class, Direction.2005)
```

    ##          Direction.2005
    ## lda.class Down  Up
    ##      Down   35  35
    ##      Up     76 106

``` r
mean(lda.class==Direction.2005)
```

    ## [1] 0.5595238

``` r
sum(lda.pred$posterior[,1]>=.5)
```

    ## [1] 70

``` r
sum(lda.pred$posterior[,1]<.5)
```

    ## [1] 182

``` r
lda.pred$posterior[1:20,1]
```

    ##       999      1000      1001      1002      1003      1004      1005 
    ## 0.4901792 0.4792185 0.4668185 0.4740011 0.4927877 0.4938562 0.4951016 
    ##      1006      1007      1008      1009      1010      1011      1012 
    ## 0.4872861 0.4907013 0.4844026 0.4906963 0.5119988 0.4895152 0.4706761 
    ##      1013      1014      1015      1016      1017      1018 
    ## 0.4744593 0.4799583 0.4935775 0.5030894 0.4978806 0.4886331

``` r
lda.class[1:20]
```

    ##  [1] Up   Up   Up   Up   Up   Up   Up   Up   Up   Up   Up   Down Up   Up  
    ## [15] Up   Up   Up   Down Up   Up  
    ## Levels: Down Up

``` r
sum(lda.pred$posterior[,1]>.9)
```

    ## [1] 0

``` r
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
```

    ## Call:
    ## qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
    ## 
    ## Prior probabilities of groups:
    ##     Down       Up 
    ## 0.491984 0.508016 
    ## 
    ## Group means:
    ##             Lag1        Lag2
    ## Down  0.04279022  0.03389409
    ## Up   -0.03954635 -0.03132544

``` r
qda.class=predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
```

    ##          Direction.2005
    ## qda.class Down  Up
    ##      Down   30  20
    ##      Up     81 121

``` r
mean(qda.class==Direction.2005)
```

    ## [1] 0.5992063

``` r
library(class)
train.X=cbind(Lag1, Lag2)[train,]
test.X=cbind(Lag1, Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
```

    ##         Direction.2005
    ## knn.pred Down Up
    ##     Down   43 58
    ##     Up     68 83

``` r
(83+43)/252
```

    ## [1] 0.5

``` r
knn.pred=knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
```

    ##         Direction.2005
    ## knn.pred Down Up
    ##     Down   48 54
    ##     Up     63 87

``` r
mean(knn.pred==Direction.2005)
```

    ## [1] 0.5357143

``` r
dim(Caravan)
```

    ## [1] 5822   86

``` r
attach(Caravan)
summary(Purchase)
```

    ##   No  Yes 
    ## 5474  348

``` r
348/5822
```

    ## [1] 0.05977327

``` r
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
```

    ## [1] 165.0378

``` r
var(Caravan[,2])
```

    ## [1] 0.1647078

``` r
var(standardized.X[,1])
```

    ## [1] 1

``` r
var(standardized.X[,2])
```

    ## [1] 1

``` r
test=1:1000
train.X=standardized.X[-test, ]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
```

    ## [1] 0.118

``` r
mean(test.Y!="No")
```

    ## [1] 0.059

``` r
table(knn.pred, test.Y)
```

    ##         test.Y
    ## knn.pred  No Yes
    ##      No  873  50
    ##      Yes  68   9

``` r
9/(68+9)
```

    ## [1] 0.1168831

``` r
knn.pred=knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
```

    ##         test.Y
    ## knn.pred  No Yes
    ##      No  920  54
    ##      Yes  21   5

``` r
5/26
```

    ## [1] 0.1923077

``` r
knn.pred=knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
```

    ##         test.Y
    ## knn.pred  No Yes
    ##      No  930  55
    ##      Yes  11   4

``` r
4/15
```

    ## [1] 0.2666667

``` r
glm.fit=glm(Purchase~.,data=Caravan, family=binomial, subset=-test)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
glm.probs=predict(glm.fit, Caravan[test,], type="response")
glm.pred=rep("No", 1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred, test.Y)
```

    ##         test.Y
    ## glm.pred  No Yes
    ##      No  934  59
    ##      Yes   7   0

``` r
glm.pred=rep("No", 1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred, test.Y)
```

    ##         test.Y
    ## glm.pred  No Yes
    ##      No  919  48
    ##      Yes  22  11

``` r
11/(22+11)
```

    ## [1] 0.3333333

Chapter 4 Exercise 13

``` r
names(Boston)
```

    ##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
    ##  [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"

``` r
summary(Boston)
```

    ##       crim                zn             indus            chas        
    ##  Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000  
    ##  1st Qu.: 0.08204   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000  
    ##  Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000  
    ##  Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917  
    ##  3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000  
    ##  Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000  
    ##       nox               rm             age              dis        
    ##  Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
    ##  1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
    ##  Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
    ##  Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
    ##  3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
    ##  Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
    ##       rad              tax           ptratio          black       
    ##  Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
    ##  1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
    ##  Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
    ##  Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
    ##  3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23  
    ##  Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
    ##      lstat            medv      
    ##  Min.   : 1.73   Min.   : 5.00  
    ##  1st Qu.: 6.95   1st Qu.:17.02  
    ##  Median :11.36   Median :21.20  
    ##  Mean   :12.65   Mean   :22.53  
    ##  3rd Qu.:16.95   3rd Qu.:25.00  
    ##  Max.   :37.97   Max.   :50.00

``` r
fix(Boston)
attach(Boston)
```

``` r
crime=Boston$crim
crime_med=median(crime)
truth=as.numeric(crime>crime_med) #1 if greater, 0 if less
Boston=data.frame(Boston, truth)
train=Boston[1:(length(crime)/2),]
test=Boston[((length(crime)/2)+1):length(crime),] #split into training and testing sets
dim(train)
```

    ## [1] 253  15

``` r
dim(test)
```

    ## [1] 253  15

Logistic Regression

``` r
glm.fit=glm(truth~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=train, family=binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(glm.fit)
```

    ## 
    ## Call:
    ## glm(formula = truth ~ zn + indus + chas + nox + rm + age + dis + 
    ##     rad + tax + ptratio + black + lstat + medv, family = binomial, 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.83229  -0.06593   0.00000   0.06181   2.61513  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -91.319906  19.490273  -4.685 2.79e-06 ***
    ## zn           -0.815573   0.193373  -4.218 2.47e-05 ***
    ## indus         0.354172   0.173862   2.037  0.04164 *  
    ## chas          0.167396   0.991922   0.169  0.86599    
    ## nox          93.706326  21.202008   4.420 9.88e-06 ***
    ## rm           -4.719108   1.788765  -2.638  0.00833 ** 
    ## age           0.048634   0.024199   2.010  0.04446 *  
    ## dis           4.301493   0.979996   4.389 1.14e-05 ***
    ## rad           3.039983   0.719592   4.225 2.39e-05 ***
    ## tax          -0.006546   0.007855  -0.833  0.40461    
    ## ptratio       1.430877   0.359572   3.979 6.91e-05 ***
    ## black        -0.017552   0.006734  -2.606  0.00915 ** 
    ## lstat         0.190439   0.086722   2.196  0.02809 *  
    ## medv          0.598533   0.185514   3.226  0.00125 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 329.367  on 252  degrees of freedom
    ## Residual deviance:  69.568  on 239  degrees of freedom
    ## AIC: 97.568
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
glm.probs=predict(glm.fit, type="response")
glm.pred=rep(0,253)
glm.pred[glm.probs>.5]=1
table(glm.pred, test$truth)
```

    ##         
    ## glm.pred  0  1
    ##        0 68 95
    ##        1 22 68

``` r
mean(glm.pred==test$truth)
```

    ## [1] 0.5375494

LDA

``` r
lda.fit=lda(truth~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=train)
lda.fit
```

    ## Call:
    ## lda(truth ~ zn + indus + chas + nox + rm + age + dis + rad + 
    ##     tax + ptratio + black + lstat + medv, data = train)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.6442688 0.3557312 
    ## 
    ## Group means:
    ##           zn     indus       chas       nox       rm      age      dis
    ## 0 17.4815951  7.042454 0.03680982 0.4702902 6.381067 54.68957 4.843857
    ## 1  0.2444444 13.959778 0.15555556 0.6119889 6.241967 85.67889 2.895154
    ##        rad      tax  ptratio    black    lstat     medv
    ## 0 4.239264 300.4417 17.82086 388.3934  9.88589 25.17362
    ## 1 5.155556 358.0111 18.08778 355.2777 14.06389 22.73889
    ## 
    ## Coefficients of linear discriminants:
    ##                   LD1
    ## zn       0.0007926925
    ## indus    0.0229879456
    ## chas     0.6266122072
    ## nox      9.6422913915
    ## rm       0.0999446464
    ## age      0.0102289386
    ## dis      0.0957096744
    ## rad      0.1996166655
    ## tax      0.0023945892
    ## ptratio  0.2717816533
    ## black   -0.0051634249
    ## lstat    0.0262962652
    ## medv     0.0662952546

``` r
plot(lda.fit)
```

![](Homework1_files/figure-markdown_github/unnamed-chunk-111-1.png)

``` r
lda.pred=predict(lda.fit, test)
lda.class=lda.pred$class
table(lda.class, test$truth)
```

    ##          
    ## lda.class   0   1
    ##         0  80  24
    ##         1  10 139

``` r
mean(lda.class==test$truth)
```

    ## [1] 0.8656126

QDA

``` r
qda.fit=qda(truth~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=train)
qda.fit
```

    ## Call:
    ## qda(truth ~ zn + indus + chas + nox + rm + age + dis + rad + 
    ##     tax + ptratio + black + lstat + medv, data = train)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.6442688 0.3557312 
    ## 
    ## Group means:
    ##           zn     indus       chas       nox       rm      age      dis
    ## 0 17.4815951  7.042454 0.03680982 0.4702902 6.381067 54.68957 4.843857
    ## 1  0.2444444 13.959778 0.15555556 0.6119889 6.241967 85.67889 2.895154
    ##        rad      tax  ptratio    black    lstat     medv
    ## 0 4.239264 300.4417 17.82086 388.3934  9.88589 25.17362
    ## 1 5.155556 358.0111 18.08778 355.2777 14.06389 22.73889

``` r
qda.class=predict(qda.fit,test)$class
table(qda.class, test$truth)
```

    ##          
    ## qda.class   0   1
    ##         0  84 159
    ##         1   6   4

``` r
mean(qda.class==test$truth)
```

    ## [1] 0.3478261

KNN

``` r
library(class)
train.X=train[, c("zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat","medv")]
test.X=test[, c("zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat","medv")]
set.seed(1)
knn.pred=knn(train.X, test.X, train$truth, k=1)
table(knn.pred, test$truth)
```

    ##         
    ## knn.pred   0   1
    ##        0  85 111
    ##        1   5  52

``` r
mean(knn.pred==test$truth)
```

    ## [1] 0.541502

``` r
knn.pred=knn(train.X, test.X, train$truth, k=3)
table(knn.pred, test$truth)
```

    ##         
    ## knn.pred   0   1
    ##        0  84  61
    ##        1   6 102

``` r
mean(knn.pred==test$truth)
```

    ## [1] 0.7351779

Using the most significant predictors (zn, nox, dis, rad, ptratio)

``` r
glm.fit=glm(truth~zn+nox+dis+rad+ptratio, data=train, family=binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(glm.fit)
```

    ## 
    ## Call:
    ## glm(formula = truth ~ zn + nox + dis + rad + ptratio, family = binomial, 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.42839  -0.23748  -0.00109   0.27036   2.93844  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -59.4967     9.4837  -6.274 3.53e-10 ***
    ## zn           -0.4673     0.1064  -4.392 1.12e-05 ***
    ## nox          71.9731    11.3542   6.339 2.31e-10 ***
    ## dis           2.1860     0.4626   4.726 2.29e-06 ***
    ## rad           1.2900     0.2350   5.489 4.05e-08 ***
    ## ptratio       0.4091     0.1367   2.992  0.00277 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 329.37  on 252  degrees of freedom
    ## Residual deviance: 105.86  on 247  degrees of freedom
    ## AIC: 117.86
    ## 
    ## Number of Fisher Scoring iterations: 9

``` r
glm.probs=predict(glm.fit, type="response")
glm.pred=rep(0,253)
glm.pred[glm.probs>.5]=1
table(glm.pred, test$truth)
```

    ##         
    ## glm.pred  0  1
    ##        0 69 88
    ##        1 21 75

``` r
mean(glm.pred==test$truth)
```

    ## [1] 0.56917

``` r
lda.fit=lda(truth~zn+nox+dis+rad+ptratio, data=train)
lda.pred=predict(lda.fit, test)
lda.class=lda.pred$class
table(lda.class, test$truth)
```

    ##          
    ## lda.class   0   1
    ##         0  80  18
    ##         1  10 145

``` r
mean(lda.class==test$truth)
```

    ## [1] 0.8893281

``` r
qda.fit=qda(truth~zn+nox+dis+rad+ptratio, data=train)
qda.class=predict(qda.fit,test)$class
table(qda.class, test$truth)
```

    ##          
    ## qda.class   0   1
    ##         0  68 150
    ##         1  22  13

``` r
mean(qda.class==test$truth)
```

    ## [1] 0.3201581

``` r
train.X=train[, c("zn", "nox","dis","rad","ptratio")]
test.X=test[,c("zn", "nox","dis","rad","ptratio")]
set.seed(1)
knn.pred=knn(train.X, test.X, train$truth, k=1)
table(knn.pred, test$truth)
```

    ##         
    ## knn.pred   0   1
    ##        0  81  23
    ##        1   9 140

``` r
mean(knn.pred==test$truth)
```

    ## [1] 0.8735178

``` r
library(class)
knn.pred=knn(train.X, test.X, train$truth, k=3)
table(knn.pred, test$truth)
```

    ##         
    ## knn.pred   0   1
    ##        0  82  23
    ##        1   8 140

``` r
mean(knn.pred==test$truth)
```

    ## [1] 0.8774704

Summary of model prediction capabilities using all variables: Logistic regression- 0.5375494 LDA- 0.8656126 QDA- 0.3478261 KNN1- 0.541502 KNN3- 0.7351779

Summary of model prediction capabilities using variables with most significant contributions to logistic regression model (p~0): Logistic regression- 0.56917 LDA- 0.8893281 QDA- 0.3201581 KNN1- 0.8735178 KNN3- 0.8774704

Narrowing down the predictors included in the model to the most significant contributors improves model prediction capabilities for all models (logistic regression, LDA, QDA, and KNN). Overall, the best performing model is the restricted LDA with an prediction accuracy in the test set of 0.889.

Chapter 5 Labs

``` r
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
```

    ## The following object is masked _by_ .GlobalEnv:
    ## 
    ##     cylinders

    ## The following objects are masked from Auto (pos = 11):
    ## 
    ##     acceleration, cylinders, displacement, horsepower, mpg, name,
    ##     origin, weight, year

``` r
Auto=Auto[complete.cases(Auto),]
mean((mpg-predict(lm.fit, Auto))[-train]^2)
```

    ## Warning in mpg - predict(lm.fit, Auto): longer object length is not a
    ## multiple of shorter object length

    ## [1] 52.11575

``` r
lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
```

    ## Warning in mpg - predict(lm.fit2, Auto): longer object length is not a
    ## multiple of shorter object length

    ## [1] 55.18484

``` r
lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)
```

    ## Warning in mpg - predict(lm.fit3, Auto): longer object length is not a
    ## multiple of shorter object length

    ## [1] 55.4663

``` r
set.seed(2)
train=sample(392, 196)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
```

    ## Warning in mpg - predict(lm.fit, Auto): longer object length is not a
    ## multiple of shorter object length

    ## [1] 46.17165

``` r
lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
```

    ## Warning in mpg - predict(lm.fit2, Auto): longer object length is not a
    ## multiple of shorter object length

    ## [1] 51.53789

``` r
lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)
```

    ## Warning in mpg - predict(lm.fit3, Auto): longer object length is not a
    ## multiple of shorter object length

    ## [1] 51.73349

``` r
glm.fit=glm(mpg~horsepower, data=Auto)
coef(glm.fit)
```

    ## (Intercept)  horsepower 
    ##  39.9358610  -0.1578447

``` r
lm.fit=lm(mpg~horsepower, data=Auto)
coef(lm.fit)
```

    ## (Intercept)  horsepower 
    ##  39.9358610  -0.1578447

``` r
library(boot)
```

    ## 
    ## Attaching package: 'boot'

    ## The following object is masked from 'package:car':
    ## 
    ##     logit

``` r
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta
```

    ## [1] 24.23151 24.23114

``` r
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
```

    ## [1] 24.23151 19.24821 19.33498 19.42443 19.03321

``` r
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10
```

    ##  [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609
    ##  [8] 19.71201 18.95140 19.50196

``` r
alpha.fn=function(data, index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
```

``` r
alpha.fn(Portfolio, 1:100)
```

    ## [1] 0.5758321

``` r
set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=T))
```

    ## [1] 0.5963833

``` r
boot(Portfolio, alpha.fn, R=1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Portfolio, statistic = alpha.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##      original        bias    std. error
    ## t1* 0.5758321 -7.315422e-05  0.08861826

``` r
boot.fn=function(data, index)
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
```

``` r
boot.fn(Auto, 1:392)
```

    ## (Intercept)  horsepower 
    ##  39.9358610  -0.1578447

``` r
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
```

    ## (Intercept)  horsepower 
    ##  38.7387134  -0.1481952

``` r
boot.fn(Auto, sample(392, 392, replace=T))
```

    ## (Intercept)  horsepower 
    ##  40.0383086  -0.1596104

``` r
boot(Auto, boot.fn, 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Auto, statistic = boot.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##       original      bias    std. error
    ## t1* 39.9358610  0.02972191 0.860007896
    ## t2* -0.1578447 -0.00030823 0.007404467

``` r
summary(lm(mpg~horsepower, data=Auto))$coef
```

    ##               Estimate  Std. Error   t value      Pr(>|t|)
    ## (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
    ## horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

``` r
boot.fn=function(data, index)
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset = index))
```

``` r
set.seed(1)
boot(Auto, boot.fn, 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Auto, statistic = boot.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##         original        bias     std. error
    ## t1* 56.900099702  6.098115e-03 2.0944855842
    ## t2* -0.466189630 -1.777108e-04 0.0334123802
    ## t3*  0.001230536  1.324315e-06 0.0001208339

``` r
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
```

    ##                     Estimate   Std. Error   t value      Pr(>|t|)
    ## (Intercept)     56.900099702 1.8004268063  31.60367 1.740911e-109
    ## horsepower      -0.466189630 0.0311246171 -14.97816  2.289429e-40
    ## I(horsepower^2)  0.001230536 0.0001220759  10.08009  2.196340e-21
