### Lesson 10 - Thursday 11/4/21

* For tonight's class, we turn to the issue of unbounded event count data.
* A good deal of criminological data takes the form of event counts so we will spend some time on this topic.
* Let's begin by reading in tonight's data set which consists of a continuous independent variable, *x*, a binary independent variable, *z*, and an unbounded event count outcome, *y*.
* Here is an exploratory data analysis:

```R
df <- read.csv(file="df.csv",sep=",",header=T)
table(df$y)

par(mfrow=c(2,2))
boxplot(df$y~df$z)
boxplot(df$x~df$z)

df.z0 <- subset(df,z==0)
df.z1 <- subset(df,z==1)

plot(x=df.z0$x,y=df.z0$y)
plot(x=df.z1$x,y=df.z1$y)
```

and here is the resulting plot:

<p align="left">
<img src="/gfiles/eda-plot.png" width="600px">
</p>

along with the marginal distribution of *y*:

```Rout
> table(df$y)

  0   1   2   3   4   5   6   7   8 
410 305 162  75  28  15   2   2   1 
> 
```

* Next, we estimate a linear regression model with outcome *y*, regressed on *x*, *z*, and the product of *x* and *z*:

```R
ls <- lm(y~1+x+z+x*z,data=df)
summary(ls)
logLik(ls)
```

and here is the output:


```Rout
> ls <- lm(y~1+x+z+x*z,data=df)
> summary(ls)

Call:
lm(formula = y ~ 1 + x + z + x * z, data = df)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.3967 -0.7028 -0.1697  0.5315  5.2266 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.87117    0.04942  17.629  < 2e-16 ***
x            0.42037    0.04991   8.422  < 2e-16 ***
z            0.37298    0.07081   5.267  1.7e-07 ***
x:z          0.22875    0.07113   3.216  0.00134 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.072 on 996 degrees of freedom
Multiple R-squared:  0.2512,	Adjusted R-squared:  0.2489 
F-statistic: 111.4 on 3 and 996 DF,  p-value: < 2.2e-16

> logLik(ls)
'log Lik.' -1486.748 (df=5)
> 
```

* Now, we calculate our own version of this estimator using the method of maximum likelihood:

```R
library(maxLik)

ls.ml <- function(parms)
  {
   a <- parms[1]
   b1 <- parms[2]
   b2 <- parms[3]
   b3 <- parms[4]
   sigma <- parms[5]
   yhat <- a+b1*df$x+b2*df$z+b3*df$x*df$z
   pt1 <- 1/(sigma*sqrt(2*pi))
   pt2 <- ((df$y-yhat)/sigma)^2
   pt3 <- exp(-1/2*pt2)
   pdf <- pt1*pt3
   lpdf <- log(pdf)
   return(lpdf)
  }

ls.nlm <- maxLik(ls.ml,start=c(0.83243243,0.42943023,0.3203243,0.2342402,1.03240320),
             method="BHHH",finalHessian="BHHH")
summary(ls.nlm)
```

and here is our output:


```Rout
> library(maxLik)
> 
> ls.ml <- function(parms)
+   {
+    a <- parms[1]
+    b1 <- parms[2]
+    b2 <- parms[3]
+    b3 <- parms[4]
+    sigma <- parms[5]
+    yhat <- a+b1*df$x+b2*df$z+b3*df$x*df$z
+    pt1 <- 1/(sigma*sqrt(2*pi))
+    pt2 <- ((df$y-yhat)/sigma)^2
+    pt3 <- exp(-1/2*pt2)
+    pdf <- pt1*pt3
+    lpdf <- log(pdf)
+    return(lpdf)
+   }
> 
> ls.nlm <- maxLik(ls.ml,start=c(0.83243243,0.42943023,0.3203243,0.2342402,1.03240320),
+              method="BHHH",finalHessian="BHHH")
> summary(ls.nlm)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 18 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -1486.748 
5  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  0.87099    0.06170  14.117  < 2e-16 ***
[2,]  0.42048    0.05922   7.101 1.24e-12 ***
[3,]  0.37305    0.07866   4.742 2.11e-06 ***
[4,]  0.22851    0.07453   3.066  0.00217 ** 
[5,]  1.07011    0.02128  50.291  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Now that we have both of these results in hand, let's do some post-processing to assess fit.
* We begin by calculating some key quantities:

```R
ls.int <- coef(ls)[1]
ls.b1 <- coef(ls)[2]
ls.b2 <- coef(ls)[3]
ls.b3 <- coef(ls)[4]

ls.yhat <- ls.int+ls.b1*df$x+ls.b2*df$z+ls.b3*df$x*df$z
ls.u <- df$y-ls.yhat
rss <- sum(ls.u^2)
ncases <- nrow(df)
rmse <- sqrt((1/(ncases-2))*rss)
rmse
```

and here is the output:

```Rout
> ls.int <- coef(ls)[1]
> ls.b1 <- coef(ls)[2]
> ls.b2 <- coef(ls)[3]
> ls.b3 <- coef(ls)[4]
> 
> ls.yhat <- ls.int+ls.b1*df$x+ls.b2*df$z+ls.b3*df$x*df$z
> ls.u <- df$y-ls.yhat
> rss <- sum(ls.u^2)
> ncases <- nrow(df)
> rmse <- sqrt((1/(ncases-2))*rss)
> rmse
[1] 1.071233
> 
```

* Now, let's see what happens when we calculate expected cell frequencies based on this model:

```R
lsn3 <- vector()
lsn2 <- vector()
lsn1 <- vector()
ls0 <- vector()
ls1 <- vector()
ls2 <- vector()
ls3 <- vector()
ls4 <- vector()
ls5 <- vector()
ls6 <- vector()
ls7 <- vector()
ls8 <- vector()
ls9 <- vector()
ls10 <- vector()
ls11 <- vector()
ls12 <- vector()

for(i in 1:ncases){
  lsn3[i] <- pnorm(-2.5,mean=ls.yhat[i],sd=rmse)-pnorm(-3.5,mean=ls.yhat[i],sd=rmse)
  lsn2[i] <- pnorm(-1.5,mean=ls.yhat[i],sd=rmse)-pnorm(-2.5,mean=ls.yhat[i],sd=rmse)
  lsn1[i] <- pnorm(-0.5,mean=ls.yhat[i],sd=rmse)-pnorm(-1.5,mean=ls.yhat[i],sd=rmse)
  ls0[i] <- pnorm(0.5,mean=ls.yhat[i],sd=rmse)-pnorm(-0.5,mean=ls.yhat[i],sd=rmse)
  ls1[i] <- pnorm(1.5,mean=ls.yhat[i],sd=rmse)-pnorm(0.5,mean=ls.yhat[i],sd=rmse)
  ls2[i] <- pnorm(2.5,mean=ls.yhat[i],sd=rmse)-pnorm(1.5,mean=ls.yhat[i],sd=rmse)
  ls3[i] <- pnorm(3.5,mean=ls.yhat[i],sd=rmse)-pnorm(2.5,mean=ls.yhat[i],sd=rmse)
  ls4[i] <- pnorm(4.5,mean=ls.yhat[i],sd=rmse)-pnorm(3.5,mean=ls.yhat[i],sd=rmse)
  ls5[i] <- pnorm(5.5,mean=ls.yhat[i],sd=rmse)-pnorm(4.5,mean=ls.yhat[i],sd=rmse)
  ls6[i] <- pnorm(6.5,mean=ls.yhat[i],sd=rmse)-pnorm(5.5,mean=ls.yhat[i],sd=rmse)
  ls7[i] <- pnorm(7.5,mean=ls.yhat[i],sd=rmse)-pnorm(6.5,mean=ls.yhat[i],sd=rmse)
  ls8[i] <- pnorm(8.5,mean=ls.yhat[i],sd=rmse)-pnorm(7.5,mean=ls.yhat[i],sd=rmse)
  ls9[i] <- pnorm(9.5,mean=ls.yhat[i],sd=rmse)-pnorm(8.5,mean=ls.yhat[i],sd=rmse)
  ls10[i] <- pnorm(10.5,mean=ls.yhat[i],sd=rmse)-pnorm(9.5,mean=ls.yhat[i],sd=rmse)
  ls11[i] <- pnorm(11.5,mean=ls.yhat[i],sd=rmse)-pnorm(10.5,mean=ls.yhat[i],sd=rmse)
  ls12[i] <- pnorm(12.5,mean=ls.yhat[i],sd=rmse)-pnorm(11.5,mean=ls.yhat[i],sd=rmse)
  }

sum(lsn3)
sum(lsn2)
sum(lsn1)
sum(ls0)
sum(ls1)
sum(ls2)
sum(ls3)
sum(ls4)
sum(ls5)
sum(ls6)
sum(ls7)
sum(ls8)
sum(ls9)
sum(ls10)
sum(ls11)
sum(ls12)

lspvec <- c(sum(lsn3),sum(lsn2),sum(lsn1),sum(ls0),sum(ls1),
            sum(ls2),sum(ls3),sum(ls4),sum(ls5),sum(ls6),
            sum(ls7),sum(ls8),sum(ls9),sum(ls10),sum(ls11),sum(ls12))
sum(lspvec)

# calculate a chi-square goodness of fit test

yvals <- seq(from=-3,to=12,by=1)
yvec <- c(0,0,0,410,305,162,75,28,15,2,2,1,0,0,0,0)
data.frame(yvals,yvec,lspvec)
chisq.ls <- sum((yvec-lspvec)^2/lspvec)
chisq.ls
df.ls <- 16-5 
pvalue.ls <- 1-pchisq(chisq.ls,df.ls)
pvalue.ls
```

* Here is the output:

```Rout
> lsn3 <- vector()
> lsn2 <- vector()
> lsn1 <- vector()
> ls0 <- vector()
> ls1 <- vector()
> ls2 <- vector()
> ls3 <- vector()
> ls4 <- vector()
> ls5 <- vector()
> ls6 <- vector()
> ls7 <- vector()
> ls8 <- vector()
> ls9 <- vector()
> ls10 <- vector()
> ls11 <- vector()
> ls12 <- vector()
> 
> for(i in 1:ncases){
+   lsn3[i] <- pnorm(-2.5,mean=ls.yhat[i],sd=rmse)-pnorm(-3.5,mean=ls.yhat[i],sd=rmse)
+   lsn2[i] <- pnorm(-1.5,mean=ls.yhat[i],sd=rmse)-pnorm(-2.5,mean=ls.yhat[i],sd=rmse)
+   lsn1[i] <- pnorm(-0.5,mean=ls.yhat[i],sd=rmse)-pnorm(-1.5,mean=ls.yhat[i],sd=rmse)
+   ls0[i] <- pnorm(0.5,mean=ls.yhat[i],sd=rmse)-pnorm(-0.5,mean=ls.yhat[i],sd=rmse)
+   ls1[i] <- pnorm(1.5,mean=ls.yhat[i],sd=rmse)-pnorm(0.5,mean=ls.yhat[i],sd=rmse)
+   ls2[i] <- pnorm(2.5,mean=ls.yhat[i],sd=rmse)-pnorm(1.5,mean=ls.yhat[i],sd=rmse)
+   ls3[i] <- pnorm(3.5,mean=ls.yhat[i],sd=rmse)-pnorm(2.5,mean=ls.yhat[i],sd=rmse)
+   ls4[i] <- pnorm(4.5,mean=ls.yhat[i],sd=rmse)-pnorm(3.5,mean=ls.yhat[i],sd=rmse)
+   ls5[i] <- pnorm(5.5,mean=ls.yhat[i],sd=rmse)-pnorm(4.5,mean=ls.yhat[i],sd=rmse)
+   ls6[i] <- pnorm(6.5,mean=ls.yhat[i],sd=rmse)-pnorm(5.5,mean=ls.yhat[i],sd=rmse)
+   ls7[i] <- pnorm(7.5,mean=ls.yhat[i],sd=rmse)-pnorm(6.5,mean=ls.yhat[i],sd=rmse)
+   ls8[i] <- pnorm(8.5,mean=ls.yhat[i],sd=rmse)-pnorm(7.5,mean=ls.yhat[i],sd=rmse)
+   ls9[i] <- pnorm(9.5,mean=ls.yhat[i],sd=rmse)-pnorm(8.5,mean=ls.yhat[i],sd=rmse)
+   ls10[i] <- pnorm(10.5,mean=ls.yhat[i],sd=rmse)-pnorm(9.5,mean=ls.yhat[i],sd=rmse)
+   ls11[i] <- pnorm(11.5,mean=ls.yhat[i],sd=rmse)-pnorm(10.5,mean=ls.yhat[i],sd=rmse)
+   ls12[i] <- pnorm(12.5,mean=ls.yhat[i],sd=rmse)-pnorm(11.5,mean=ls.yhat[i],sd=rmse)
+   }
> 
> sum(lsn3)
[1] 1.516445
> sum(lsn2)
[1] 15.7885
> sum(lsn1)
[1] 83.11883
> sum(ls0)
[1] 223.3485
> sum(ls1)
[1] 313.3888
> sum(ls2)
[1] 236.9566
> sum(ls3)
[1] 99.36013
> sum(ls4)
[1] 23.27357
> sum(ls5)
[1] 2.96932
> sum(ls6)
[1] 0.1968668
> sum(ls7)
[1] 0.006459338
> sum(ls8)
[1] 0.0001008159
> sum(ls9)
[1] 7.27185e-07
> sum(ls10)
[1] 2.374561e-09
> sum(ls11)
[1] 3.455125e-12
> sum(ls12)
[1] 1.887379e-15
> 
> lspvec <- c(sum(lsn3),sum(lsn2),sum(lsn1),sum(ls0),sum(ls1),
+             sum(ls2),sum(ls3),sum(ls4),sum(ls5),sum(ls6),
+             sum(ls7),sum(ls8),sum(ls9),sum(ls10),sum(ls11),sum(ls12))
> sum(lspvec)
[1] 999.9241
> 
> # calculate a chi-square goodness of fit test
> 
> yvals <- seq(from=-3,to=12,by=1)
> yvec <- c(0,0,0,410,305,162,75,28,15,2,2,1,0,0,0,0)
> data.frame(yvals,yvec,lspvec)
   yvals yvec       lspvec
1     -3    0 1.516445e+00
2     -2    0 1.578850e+01
3     -1    0 8.311883e+01
4      0  410 2.233485e+02
5      1  305 3.133888e+02
6      2  162 2.369566e+02
7      3   75 9.936013e+01
8      4   28 2.327357e+01
9      5   15 2.969320e+00
10     6    2 1.968668e-01
11     7    2 6.459338e-03
12     8    1 1.008159e-04
13     9    0 7.271850e-07
14    10    0 2.374561e-09
15    11    0 3.455125e-12
16    12    0 1.887379e-15
> chisq.ls <- sum((yvec-lspvec)^2/lspvec)
> chisq.ls
[1] 10884.87
> df.ls <- 16-5 
> pvalue.ls <- 1-pchisq(chisq.ls,df.ls)
> pvalue.ls
[1] 0
> 
```

* Now, we are going to turn our attention to a regression model based on the Poisson probability distribution:

```R
m1 <- glm(y~1+x+z+x*z,data=df,family=poisson(link="log"))
summary(m1)
logLik(m1)
```

* here is our output:

```Rout
> m1 <- glm(y~1+x+z+x*z,data=df,family=poisson(link="log"))
> summary(m1)

Call:
glm(formula = y ~ 1 + x + z + x * z, family = poisson(link = "log"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3029  -1.0628  -0.2553   0.5330   2.9728  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.26778    0.05220  -5.130 2.90e-07 ***
x            0.57092    0.05523  10.338  < 2e-16 ***
z            0.38266    0.07104   5.387 7.17e-08 ***
x:z         -0.08638    0.06954  -1.242    0.214    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 1470.7  on 999  degrees of freedom
Residual deviance: 1116.1  on 996  degrees of freedom
AIC: 2544.4

Number of Fisher Scoring iterations: 5

> logLik(m1)
'log Lik.' -1268.199 (df=4)
> 
```

* Notice that this model has one fewer parameter estimates than the normal linear regression model.
* Now, we consider the maximum likelihood estimator in detail:

```R
library(maxLik)

ll2 <- function(parms)
  {
   a <- parms[1]
   b1 <- parms[2]
   b2 <- parms[3]
   b3 <- parms[4]
   lmb <- exp(a+b1*df$x+b2*df$z+b3*df$x*df$z)
   pt1 <- exp(-lmb)
   pt2 <- lmb^df$y
   pt3 <- factorial(df$y)
   pmf <- pt1*pt2/pt3
   lpmf <- log(pmf)
   return(lpmf)
  }

m2 <- maxLik(ll2,start=c(0.87117,0.42037,0.37298,0.22875),
             method="BHHH",finalHessian="BHHH")
summary(m2)
```

* Here is the output:


```Rout
> library(maxLik)
> 
> ll2 <- function(parms)
+   {
+    a <- parms[1]
+    b1 <- parms[2]
+    b2 <- parms[3]
+    b3 <- parms[4]
+    lmb <- exp(a+b1*df$x+b2*df$z+b3*df$x*df$z)
+    pt1 <- exp(-lmb)
+    pt2 <- lmb^df$y
+    pt3 <- factorial(df$y)
+    pmf <- pt1*pt2/pt3
+    lpmf <- log(pmf)
+    return(lpmf)
+   }
> 
> m2 <- maxLik(ll2,start=c(0.87117,0.42037,0.37298,0.22875),
+              method="BHHH",finalHessian="BHHH")
> summary(m2)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 11 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -1268.199 
4  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,] -0.26778    0.05210  -5.140 2.75e-07 ***
[2,]  0.57092    0.05415  10.544  < 2e-16 ***
[3,]  0.38266    0.07026   5.446 5.15e-08 ***
[4,] -0.08638    0.06790  -1.272    0.203    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

* Here is the fit analysis:

```R
# check on the model's fit

a <- coef(m2)[1]
a 
b1 <- coef(m2)[2]
b1 
b2 <- coef(m2)[3]
b2 
b3 <- coef(m2)[4]
b3 

lmb <- exp(a+b1*df$x+b2*df$z+b3*df$x*df$z)

e0  <- sum(exp(-lmb)*lmb^0/factorial(0))
e1  <- sum(exp(-lmb)*lmb^1/factorial(1))
e2  <- sum(exp(-lmb)*lmb^2/factorial(2))
e3  <- sum(exp(-lmb)*lmb^3/factorial(3))
e4  <- sum(exp(-lmb)*lmb^4/factorial(4))
e5  <- sum(exp(-lmb)*lmb^5/factorial(5))
e6  <- sum(exp(-lmb)*lmb^6/factorial(6))
e7  <- sum(exp(-lmb)*lmb^7/factorial(7))
e8  <- sum(exp(-lmb)*lmb^8/factorial(8))
e9  <- sum(exp(-lmb)*lmb^9/factorial(9))
e10  <- sum(exp(-lmb)*lmb^10/factorial(10))
e11  <- sum(exp(-lmb)*lmb^11/factorial(11))
e12  <- sum(exp(-lmb)*lmb^12/factorial(12))

table(df$y)
yvals <- seq(from=0,to=12,by=1)
yvec <- c(410,305,162,75,28,15,2,2,1,0,0,0,0)
e <- c(e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)
sum(e)

data.frame(yvals,yvec,e,yvec-e)
chi.sq <- (yvec-e)^2/e
sum(chi.sq)
dof <- 13-4
dof
pval <- 1-pchisq(sum(chi.sq),dof)
pval
```

* Here are the results:

```rout
> # check on the model's fit
> 
> a <- coef(m2)[1]
> a 
[1] -0.2677798
> b1 <- coef(m2)[2]
> b1 
[1] 0.5709187
> b2 <- coef(m2)[3]
> b2 
[1] 0.3826645
> b3 <- coef(m2)[4]
> b3 
[1] -0.08638074
> 
> lmb <- exp(a+b1*df$x+b2*df$z+b3*df$x*df$z)
> 
> e0  <- sum(exp(-lmb)*lmb^0/factorial(0))
> e1  <- sum(exp(-lmb)*lmb^1/factorial(1))
> e2  <- sum(exp(-lmb)*lmb^2/factorial(2))
> e3  <- sum(exp(-lmb)*lmb^3/factorial(3))
> e4  <- sum(exp(-lmb)*lmb^4/factorial(4))
> e5  <- sum(exp(-lmb)*lmb^5/factorial(5))
> e6  <- sum(exp(-lmb)*lmb^6/factorial(6))
> e7  <- sum(exp(-lmb)*lmb^7/factorial(7))
> e8  <- sum(exp(-lmb)*lmb^8/factorial(8))
> e9  <- sum(exp(-lmb)*lmb^9/factorial(9))
> e10  <- sum(exp(-lmb)*lmb^10/factorial(10))
> e11  <- sum(exp(-lmb)*lmb^11/factorial(11))
> e12  <- sum(exp(-lmb)*lmb^12/factorial(12))
> 
> table(df$y)

  0   1   2   3   4   5   6   7   8 
410 305 162  75  28  15   2   2   1 
> yvals <- seq(from=0,to=12,by=1)
> yvec <- c(410,305,162,75,28,15,2,2,1,0,0,0,0)
> e <- c(e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)
> sum(e)
[1] 999.9991
> 
> data.frame(yvals,yvec,e,yvec-e)
   yvals yvec            e     yvec...e
1      0  410 4.019905e+02  8.009510936
2      1  305 3.142258e+02 -9.225784756
3      2  162 1.639520e+02 -1.952041649
4      3   75 7.285088e+01  2.149118800
5      4   28 2.960919e+01 -1.609193223
6      5   15 1.128183e+01  3.718174039
7      6    2 4.063157e+00 -2.063156662
8      7    2 1.386217e+00  0.613783429
9      8    1 4.481976e-01  0.551802387
10     9    0 1.373807e-01 -0.137380732
11    10    0 3.994996e-02 -0.039949962
12    11    0 1.103420e-02 -0.011034197
13    12    0 2.898916e-03 -0.002898916
> chi.sq <- (yvec-e)^2/e
> sum(chi.sq)
[1] 4.019964
> dof <- 13-4
> dof
[1] 9
> pval <- 1-pchisq(sum(chi.sq),dof)
> pval
[1] 0.9100937
> 
```

* Here is some code to help us interpret the results:

```R
xstar <- seq(from=-3,to=3,by=0.1)
zstar <- c(0,1)
xz <- expand.grid(xstar,zstar)
xz

xz$xst <- xz$Var1
xz$zst <- xz$Var2
xz$lambda <- exp(a+b1*xz$x+b2*xz$z+b3*xz$xst*xz$zst)
plot(x=xstar,y=xz$lambda[1:61],ylab="E(lambda|x=xstar)",type="l",lty=1,lwd=2,col="red")
lines(x=xstar,y=xz$lambda[62:122],lty=1,lwd=2,col="blue")
```

* Here are the results:

```Rout
> xstar <- seq(from=-3,to=3,by=0.1)
> zstar <- c(0,1)
> xz <- expand.grid(xstar,zstar)
> xz
    Var1 Var2
1   -3.0    0
2   -2.9    0
3   -2.8    0
4   -2.7    0
5   -2.6    0
6   -2.5    0
7   -2.4    0
8   -2.3    0
9   -2.2    0
10  -2.1    0
11  -2.0    0
12  -1.9    0
13  -1.8    0
14  -1.7    0
15  -1.6    0
16  -1.5    0
17  -1.4    0
18  -1.3    0
19  -1.2    0
20  -1.1    0
21  -1.0    0
22  -0.9    0
23  -0.8    0
24  -0.7    0
25  -0.6    0
26  -0.5    0
27  -0.4    0
28  -0.3    0
29  -0.2    0
30  -0.1    0
31   0.0    0
32   0.1    0
33   0.2    0
34   0.3    0
35   0.4    0
36   0.5    0
37   0.6    0
38   0.7    0
39   0.8    0
40   0.9    0
41   1.0    0
42   1.1    0
43   1.2    0
44   1.3    0
45   1.4    0
46   1.5    0
47   1.6    0
48   1.7    0
49   1.8    0
50   1.9    0
51   2.0    0
52   2.1    0
53   2.2    0
54   2.3    0
55   2.4    0
56   2.5    0
57   2.6    0
58   2.7    0
59   2.8    0
60   2.9    0
61   3.0    0
62  -3.0    1
63  -2.9    1
64  -2.8    1
65  -2.7    1
66  -2.6    1
67  -2.5    1
68  -2.4    1
69  -2.3    1
70  -2.2    1
71  -2.1    1
72  -2.0    1
73  -1.9    1
74  -1.8    1
75  -1.7    1
76  -1.6    1
77  -1.5    1
78  -1.4    1
79  -1.3    1
80  -1.2    1
81  -1.1    1
82  -1.0    1
83  -0.9    1
84  -0.8    1
85  -0.7    1
86  -0.6    1
87  -0.5    1
88  -0.4    1
89  -0.3    1
90  -0.2    1
91  -0.1    1
92   0.0    1
93   0.1    1
94   0.2    1
95   0.3    1
96   0.4    1
97   0.5    1
98   0.6    1
99   0.7    1
100  0.8    1
101  0.9    1
102  1.0    1
103  1.1    1
104  1.2    1
105  1.3    1
106  1.4    1
107  1.5    1
108  1.6    1
109  1.7    1
110  1.8    1
111  1.9    1
112  2.0    1
113  2.1    1
114  2.2    1
115  2.3    1
116  2.4    1
117  2.5    1
118  2.6    1
119  2.7    1
120  2.8    1
121  2.9    1
122  3.0    1
> 
> xz$xst <- xz$Var1
> xz$zst <- xz$Var2
> xz$lambda <- exp(a+b1*xz$x+b2*xz$z+b3*xz$xst*xz$zst)
> plot(x=xstar,y=xz$lambda[1:61],ylab="E(lambda|x=xstar)",type="l",lty=1,lwd=2,col="red")
> lines(x=xstar,y=xz$lambda[62:122],lty=1,lwd=2,col="blue")
> 
```

and a plot:

<p align="left">
<img src="/gfiles/poisson-plot.png" width="600px">
</p>
