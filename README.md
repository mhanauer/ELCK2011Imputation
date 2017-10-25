---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Here we are setting the WD to google drive.  See earlier versions for getting the original data source.
```{r}
#setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
#data = read.csv("ELCS-K-2011.csv", header = TRUE)
# These are all of the variables that can remain unchanged
data1 = cbind(X1TCHAPP = data$X1TCHAPP, X1TCHCON = data$X1TCHCON, X1TCHPER = data$X1TCHPER, X1TCHEXT = data$X1TCHEXT, X1TCHINT = data$X1TCHINT, X1RTHET = data$X1RTHET, X1MTHET = data$X1MTHET, X1BMI = data$X1BMI, X1PAR1AGE = data$X1PAR1AGE, X1PAR1EMP = data$X1PAR1EMP, X1NUMSIB = data$X1NUMSIB, X12SESL = data$X12SESL, X12PAR1ED_I = data$X12PAR1ED_I, W1P0 = data$W1P0, data[,10894:10973])
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
data1 = as.data.frame(data1)
head(data1)
# These are variables that will need to be changed to binary at a later date.
langSexMarital = cbind(X12LANGST = data$X12LANGST,  X_CHSEX_R = data$X_CHSEX_R, X1HPARNT = data$X1HPARNT)
langSexMarital = as.data.frame(apply(langSexMarital, 2, function(x){ifelse(x == -9, NA, x)}))

childEth = childEth = as.data.frame(data$X_RACETHP_R)
colnames(childEth) = c("childEth")
childEth = as.data.frame(apply(childEth, 2, function(x){ifelse(x == -9, NA, x)}))
```
Now we need to alter the variables to be binary in necessary.  First we create get all the variables where 1 is the interest and get those as 1 and rest as zero.  Then for parent ethnicty we change the ones to zero and everything else to one to have a non-white be one.  Then we need to grab a seperate subset of the all the remaining variables, so we don't double up on those variables when we cbind them togehter at the end.  I then needed to get the variables in the same order and rename with meaningful names.  Need to grab the correct data from data sets two and three, because those have the binary transformations.
```{r}
# Here is the ethnicity variable that needs to be transformed into the original variables that you used.  Remember that original variable were incorrect and just keeping the names the same here for consistency.
X_HISP_R = ifelse(is.na(childEth), NA, ifelse(childEth == 3, 1, 0))
X_HISP_R = as.data.frame(X_HISP_R)
names(X_HISP_R) = c("X_HISP_R")

X_BLACK_R = ifelse(is.na(childEth), NA, ifelse(childEth == 2, 1, 0))
X_BLACK_R = as.data.frame(X_BLACK_R)
names(X_BLACK_R) = c("X_BLACK_R")

X_ASIAN_R = ifelse(is.na(childEth), NA, ifelse(childEth == 5, 1, 0))
X_ASIAN_R = as.data.frame(X_ASIAN_R)
names(X_ASIAN_R) = c("X_ASIAN_R")

X_AMINAN_R = ifelse(is.na(childEth), NA, ifelse(childEth == 7, 1, 0))
X_AMINAN_R = as.data.frame(X_AMINAN_R)
names(X_AMINAN_R) = c("X_AMINAN_R")

X_HAWPI_R = ifelse(is.na(childEth), NA, ifelse(childEth == 6, 1, 0))
X_HAWPI_R = as.data.frame(X_HAWPI_R)
names(X_HAWPI_R) = c("X_HAWPI_R")

X_MULTR_R = ifelse(is.na(childEth), NA, ifelse(childEth == 8, 1, 0))
X_MULTR_R = as.data.frame(X_MULTR_R)
names(X_MULTR_R) = c("X_MULTR_R")

childEth = cbind(X_HISP_R, X_BLACK_R, X_ASIAN_R, X_AMINAN_R, X_HAWPI_R, X_MULTR_R)
childEth = as.data.frame(childEth)


parEth = as.data.frame(cbind(X1PAR1RAC = data$X1PAR1RAC))
parEth = apply(parEth, 2, function(x){ifelse(x == -9, NA, x)})
parEth = ifelse(is.na(parEth), NA, ifelse(parEth == 1, 0,1))
parEth = as.data.frame(parEth)
```
Reording the variables to be in the correct order.  Grab each variable from the correct data set from above.  Get the right weights.
```{r}
data1 = cbind(data1, langSexMarital, childEth, parEth)
data1 = as.data.frame(data1)
head(data1)
```

Here we will use Amelia.  Need to set m as five for five imputed data sets.  Then we place each of the variables into their appropriate categories.

```{r}
library(Amelia)
library(mitools)
library(survey)
m = 5
head(data1)
a.out = amelia(x = data1, m=m, ords = c("X1PAR1EMP", "X12PAR1ED_I"), logs = c("X1NUMSIB"), noms = c("X_HISP_R", "X_BLACK_R", "X_ASIAN_R", "X_AMINAN_R", "X_HAWPI_R", "X_MULTR_R", "X_CHSEX_R", "X1HPARNT", "X1PAR1RAC"))


# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
write.amelia(obj = a.out, file.stem = "ECLSK")
#head(a.out.imp[c(1:6)])
#test = a.out.imp$imputations$imp1


```

Here we are analyzing data from the first imputted data set ECLSK1 
```{r}
# setwd("~/Google Drive/MyProjects/Projects/ECLSK2011/ECLSK2011First/Data")
# ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
# ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
# ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
# ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
# ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
# ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
ECLSK1 = ECLSK1[c(-1)]
head(ECLSK1)
scdrep1 = svrepdesign(data = ECLSK1, type="JKn", repweights = ECLSK1[,14:94], weights = ECLSK1[,14], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean1 = svymean(ECLSK1, scdrep1)
head(ECLSK1)
modelSC1 = svyglm(log(X1TCHCON) ~ X1TCHAPP+ X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET+ X1BMI + X1PAR1AGE + X1PAR1EMP + X1NUMSIB + X1NUMSIB + X1NUMSIB + X12SESL + X12PAR1ED_I + X12LANGST+ X_CHSEX_R+ X1HPARNT+X_HISP_R+X_BLACK_R+X_ASIAN_R+X_AMINAN_R+X_HAWPI_R+X_MULTR_R +X1PAR1RAC+X_HISP_R*X_CHSEX_R + X_BLACK_R*X_CHSEX_R + X_ASIAN_R*X_CHSEX_R + X_AMINAN_R*X_CHSEX_R + X_HAWPI_R*X_CHSEX_R + X_MULTR_R*X_CHSEX_R, scdrep1)

summary(modelSC1)
# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC1Coef= summary(modelSC1)$coefficients[,1:2]
modelSC1Coef = as.data.frame(t(modelSC1Coef))


```
Here we are analyzing data from the second imputted data set ECLSK2
```{r}
#ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
ECLSK2 = ECLSK2[c(-1)]
ECLSK2 = as.data.frame(ECLSK2)
head(ECLSK2)
scdrep2 = svrepdesign(data = ECLSK2, type="JKn", repweights = ECLSK2[,14:94], weights = ECLSK2[,14], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean1 = svymean(ECLSK2, scdrep1)
head(ECLSK2)
modelSC2 = svyglm(log(X1TCHCON) ~ X1TCHAPP+ X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET+ X1BMI + X1PAR1AGE + X1PAR1EMP + X1NUMSIB + X1NUMSIB + X1NUMSIB + X12SESL + X12PAR1ED_I + X12LANGST+ X_CHSEX_R+ X1HPARNT+X_HISP_R+X_BLACK_R+X_ASIAN_R+X_AMINAN_R+X_HAWPI_R+X_MULTR_R +X1PAR1RAC+X_HISP_R*X_CHSEX_R + X_BLACK_R*X_CHSEX_R + X_ASIAN_R*X_CHSEX_R + X_AMINAN_R*X_CHSEX_R + X_HAWPI_R*X_CHSEX_R + X_MULTR_R*X_CHSEX_R, scdrep2)

summary(modelSC2)
# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC2Coef= summary(modelSC2)$coefficients[,1:2]
modelSC2Coef = as.data.frame(t(modelSC2Coef))


```
Now with imputed data set three
```{r}
ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
ECLSK3 = ECLSK3[c(-1)]
ECLSK3 = as.data.frame(ECLSK3)
head(ECLSK3)
scdrep3 = svrepdesign(data = ECLSK3, type="JKn", repweights = ECLSK3[,14:94], weights = ECLSK3[,14], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean1 = svymean(ECLSK3, scdrep1)
head(ECLSK3)
modelSC3 = svyglm(log(X1TCHCON) ~ X1TCHAPP+ X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET+ X1BMI + X1PAR1AGE + X1PAR1EMP + X1NUMSIB + X1NUMSIB + X1NUMSIB + X12SESL + X12PAR1ED_I + X12LANGST+ X_CHSEX_R+ X1HPARNT+X_HISP_R+X_BLACK_R+X_ASIAN_R+X_AMINAN_R+X_HAWPI_R+X_MULTR_R +X1PAR1RAC+X_HISP_R*X_CHSEX_R + X_BLACK_R*X_CHSEX_R + X_ASIAN_R*X_CHSEX_R + X_AMINAN_R*X_CHSEX_R + X_HAWPI_R*X_CHSEX_R + X_MULTR_R*X_CHSEX_R, scdrep3)

summary(modelSC3)
# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC3Coef= summary(modelSC3)$coefficients[,1:2]
modelSC3Coef = as.data.frame(t(modelSC3Coef))
```
How with imputed data four
```{r}
ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
ECLSK4 = ECLSK4[c(-1)]
ECLSK4 = as.data.frame(ECLSK4)
head(ECLSK4)
scdrep4 = svrepdesign(data = ECLSK4, type="JKn", repweights = ECLSK4[,14:94], weights = ECLSK4[,14], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean1 = svymean(ECLSK4, scdrep1)
head(ECLSK4)
modelSC4 = svyglm(log(X1TCHCON) ~ X1TCHAPP+ X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET+ X1BMI + X1PAR1AGE + X1PAR1EMP + X1NUMSIB + X1NUMSIB + X1NUMSIB + X12SESL + X12PAR1ED_I + X12LANGST+ X_CHSEX_R+ X1HPARNT+X_HISP_R+X_BLACK_R+X_ASIAN_R+X_AMINAN_R+X_HAWPI_R+X_MULTR_R +X1PAR1RAC+X_HISP_R*X_CHSEX_R + X_BLACK_R*X_CHSEX_R + X_ASIAN_R*X_CHSEX_R + X_AMINAN_R*X_CHSEX_R + X_HAWPI_R*X_CHSEX_R + X_MULTR_R*X_CHSEX_R, scdrep4)

summary(modelSC4)
# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC4Coef= summary(modelSC4)$coefficients[,1:2]
modelSC4Coef = as.data.frame(t(modelSC4Coef))
```
Now with the fifth imputed data set
```{r}
ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
ECLSK5 = ECLSK5[c(-1)]
ECLSK5 = as.data.frame(ECLSK5)
head(ECLSK5)
scdrep5 = svrepdesign(data = ECLSK5, type="JKn", repweights = ECLSK5[,14:94], weights = ECLSK5[,14], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean1 = svymean(ECLSK5, scdrep1)
head(ECLSK5)
modelSC5 = svyglm(log(X1TCHCON) ~ X1TCHAPP+ X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET+ X1BMI + X1PAR1AGE + X1PAR1EMP + X1NUMSIB + X1NUMSIB + X1NUMSIB + X12SESL + X12PAR1ED_I + X12LANGST+ X_CHSEX_R+ X1HPARNT+X_HISP_R+X_BLACK_R+X_ASIAN_R+X_AMINAN_R+X_HAWPI_R+X_MULTR_R +X1PAR1RAC+X_HISP_R*X_CHSEX_R + X_BLACK_R*X_CHSEX_R + X_ASIAN_R*X_CHSEX_R + X_AMINAN_R*X_CHSEX_R + X_HAWPI_R*X_CHSEX_R + X_MULTR_R*X_CHSEX_R, scdrep5)

summary(modelSC5)
# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC5Coef= summary(modelSC5)$coefficients[,1:2]
modelSC5Coef = as.data.frame(t(modelSC5Coef))

# SEtting the degrees of freedom here.
df =  modelSC5$df.null 
```
Now we need to rbind all of the parameter and sd estimates into two columns.  However, we need to grab the first row of these data sets, because the second row is the standard error.  We need seperate data set for the se's, because we need to stack the different sets on top of each other.

Here we have the SC model
```{r}
ParsSC = rbind(modelSC1Coef[1,], modelSC2Coef[1,], modelSC3Coef[1,], modelSC4Coef[1,], modelSC5Coef[1,])
ParsSC = as.matrix(ParsSC)
ParsSC

SeSC = rbind(modelSC1Coef[2,], modelSC2Coef[2,], modelSC3Coef[2,], modelSC4Coef[2,], modelSC5Coef[2,])
SeSC = as.matrix(SeSC)
SeSC

SCModelsCombine = mi.meld(q = ParsSC, se = SeSC)

SCModelsCombine = as.data.frame(cbind(Estimate = t(SCModelsCombine$q.mi),SE =  t(SCModelsCombine$se.mi)))

names(SCModelsCombine) = c("Estimate", "SE")
SCModelsCombine
SCModelsCombine$T_Stat = SCModelsCombine$Estimate / SCModelsCombine$SE
SCModelsCombine$P_Value = 2*pt(-abs(SCModelsCombine$T_Stat), df = df)
SCModelsCombine = round(SCModelsCombine,3)

SCModelsCombine$Sig = ifelse(SCModelsCombine$P_Value <= .000, "***", ifelse( SCModelsCombine$P_Value <= .01, "**", ifelse(SCModelsCombine$P_Value <= .05, "*","NS")))

SCModelsCombine

```
