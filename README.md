---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Here we are setting the WD to google drive.  See earlier versions for getting the original data source.
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")

# These are all of the variables that can remain unchanged
data1 = cbind(X1PRNCON = data$X1PRNCON, X1PRNSOC = data$X1PRNSOC, X1PRNSAD = data$X1PRNSAD, X1PRNIMP = data$X1PRNIMP, X1PRNAPP = data$X1PRNAPP, X1BMI = data$X1BMI, X1PAR1AGE = data$X1PAR1AGE, X1PAR1EMP = data$X1PAR1EMP, X1HTOTAL = data$X1HTOTAL, X1NUMSIB = data$X1NUMSIB, X2POVTY = data$X2POVTY, X12SESL = data$X12SESL, X12PAR1ED_I = data$X12PAR1ED_I, X12LANGST = data$X12LANGST,  X_CHSEX_R = data$X_CHSEX_R, X1RESREL = data$X1RESREL, X1HPARNT = data$X1HPARNT, X1PRIMNW = data$X1PRIMNW, W1P0 = data$W1P0, data[,10894:10973])
head(data1)


# These are variables that will need to be changed to binary at a later date.
data2 = cbind(X12LANGST = data$X12LANGST,  X_CHSEX_R = data$X_CHSEX_R, X1RESREL = data$X1RESREL, X1HPARNT = data$X1HPARNT, X1PRIMNW = data$X1PRIMNW)
head(data2)
# We will need to create new variables for each race with this variable.
data3 = cbind(X_RACETHP_R = data$X_RACETHP_R)

data4  = cbind( X1PAR1RAC = data$X1PAR1RAC)
data4 = as.data.frame(data4)

#Combining all four of the datas.  Keeping them seperate for data transformations below
data1 = cbind(data1, data2, data3, data4)
head(data1)
# Change the -9 to NAs
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
data1 = as.data.frame(data1)
head(data1)


```
Now we need to alter the variables to be binary in necessary.  First we create get all the variables where 1 is the interest and get those as 1 and rest as zero.  Then for parent ethnicty we change the ones to zero and everything else to one to have a non-white be one.  Then we need to grab a seperate subset of the all the remaining variables, so we don't double up on those variables when we cbind them togehter at the end.  I then needed to get the variables in the same order and rename with meaningful names.  Need to grab the correct data from data sets two and three, because those have the binary transformations.
```{r}

data2 = cbind(X12LANGST = data1$X12LANGST,  X_CHSEX_R = data1$X_CHSEX_R, X1RESREL = data1$X1RESREL, X1HPARNT = data1$X1HPARNT, X1PRIMNW = data1$X1PRIMNW)

data2 = ifelse(is.na(data2), NA, ifelse(data2 == 1, 1,0))
data2 = as.data.frame(data2)
head(data2)

# Here is the ethnicity variable that needs to be transformed into the original variables that you used.  Remember that original variable were incorrect and just keeping the names the same here for consistency.
X_HISP_R = ifelse(is.na(data3), NA, ifelse(data3 == 3, 1, 0))
X_HISP_R = as.data.frame(X_HISP_R)
names(X_HISP_R) = c("X_HISP_R")

X_WHITE_R = ifelse(is.na(data3), NA, ifelse(data3 == 1, 1, 0))
X_WHITE_R = as.data.frame(X_WHITE_R)
names(X_WHITE_R) = c("X_WHITE_R")
sum(X_WHITE_R, na.rm =TRUE)

X_BLACK_R = ifelse(is.na(data3), NA, ifelse(data3 == 2, 1, 0))
X_BLACK_R = as.data.frame(X_BLACK_R)
names(X_BLACK_R) = c("X_BLACK_R")

X_ASIAN_R = ifelse(is.na(data3), NA, ifelse(data3 == 5, 1, 0))
X_ASIAN_R = as.data.frame(X_ASIAN_R)
names(X_ASIAN_R) = c("X_ASIAN_R")

X_AMINAN_R = ifelse(is.na(data3), NA, ifelse(data3 == 7, 1, 0))
X_AMINAN_R = as.data.frame(X_AMINAN_R)
names(X_AMINAN_R) = c("X_AMINAN_R")

X_HAWPI_R = ifelse(is.na(data3), NA, ifelse(data3 == 6, 1, 0))
X_HAWPI_R = as.data.frame(X_HAWPI_R)
names(X_HAWPI_R) = c("X_HAWPI_R")

X_MULTR_R = ifelse(is.na(data3), NA, ifelse(data3 == 8, 1, 0))
X_MULTR_R = as.data.frame(X_MULTR_R)
names(X_MULTR_R) = c("X_MULTR_R")


data4 = cbind(X_HISP_R, X_WHITE_R, X_BLACK_R, X_ASIAN_R, X_AMINAN_R, X_AMINAN_R, X_HAWPI_R, X_MULTR_R)
data4 = as.data.frame(data4)

# Need to change 2 through 8 to be 1 and 1 to be zero
data6 = cbind(X1PAR1RAC = data1$X1PAR1RAC)
data6 = ifelse(is.na(data6), NA, ifelse(data6 == 1, 0,1))
data6 = as.data.frame(data6)
```
Reording the variables to be in the correct order.  Grab each variable from the correct data set from above.
```{r}
# Rearrange and then rename variables to get them in the correct order.  This includes getting data2 and data3 into the correct order as well, because you need to rename all of these variables. 

data5 = cbind(X1PRNCON = data1$X1PRNCON, X1PRNSOC = data1$X1PRNSOC, X1PRNSAD = data1$X1PRNSAD, X1PRNIMP = data1$X1PRNIMP, X1PRNAPP = data1$X1PRNAPP,X_HISP_R = data4$X_HISP_R, X_WHITE_R = data4$X_WHITE_R, X_BLACK_R = data4$X_BLACK_R, X_ASIAN_R = data4$X_ASIAN_R, X_AMINAN_R = data4$X_AMINAN_R, X_HAWPI_R = data4$X_HAWPI_R, X_MULTR_R = data4$X_MULTR_R, X_CHSEX_R = data2$X_CHSEX_R, X1BMI = data1$X1BMI,X1RESREL = data2$X1RESREL,X1HPARNT = data2$X1HPARNT, X1PAR1AGE = data1$X1PAR1AGE, X1PAR1RAC = data6$X1PAR1RAC,X12PAR1ED_I = data1$X12PAR1ED_I, X1PAR1EMP = data1$X1PAR1EMP, X1HTOTAL = data1$X1HTOTAL, X1NUMSIB = data1$X1NUMSIB, X1PRIMNW = data2$X1PRIMNW, X2POVTY = data1$X2POVTY, X12SESL = data1$X12SESL)

head(data5)


data1 = cbind(data5, W1P0 = data1[,19], data1[,20:99])
data1 = as.data.frame(data1)

head(data1)
```

Here we will use Amelia.  Need to set m as five for five imputed data sets.  Then we place each of the variables into their appropriate categories.

```{r}
library(Amelia)
library(mitools)
library(survey)
m = 5
a.out = amelia(x = data1, m=m, ords = c("X1PAR1EMP", "X2POVTY", "X12PAR1ED_I"), logs = c("X1HTOTAL", "X1NUMSIB"), noms = c("X_HISP_R", "X_WHITE_R", "X_BLACK_R", "X_ASIAN_R", "X_AMINAN_R", "X_HAWPI_R", "X_MULTR_R", "X_CHSEX_R", "X1RESREL", "X1HPARNT", "X1PRIMNW", "X1PAR1RAC"))
# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
write.amelia(obj = a.out, file.stem = "ECLSK")
#head(a.out.imp[c(1:6)])
#test = a.out.imp$imputations$imp1
```

Here we are analyzing data from the first imputted data set ECLSK1 
```{r}

ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
ECLSK1 = ECLSK1[c(-1)]
ECLSK1 = as.data.frame(ECLSK1)

scdrep1 = svrepdesign(data = ECLSK1, type="JKn", repweights = ECLSK1[,26:106], weights = ECLSK1[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean1 = svymean(ECLSK1, scdrep)

modelSC1 = svyglm(X1PRNCON ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC1Coef= summary(modelSC1)$coefficients[,1:2]
modelSC1Coef = as.data.frame(t(modelSC1Coef))


modelSI1 = svyglm(X1PRNSOC ~X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep1)


modelSI1Coef= summary(modelSI1)$coefficients[,1:2]
modelSI1Coef = as.data.frame(t(modelSI1Coef))



modelSL1 = svyglm(X1PRNSAD ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep1)

modelSL1Coef= summary(modelSL1)$coefficients[,1:2]
modelSL1Coef = as.data.frame(t(modelSL1Coef))

modelIO1 = svyglm(X1PRNIMP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep1)

modelIO1Coef= summary(modelIO1)$coefficients[,1:2]
modelIO1Coef = as.data.frame(t(modelIO1Coef))

modelAP1 = svyglm(X1PRNAPP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep1)

modelAP1Coef= summary(modelAP1)$coefficients[,1:2]
modelAP1Coef = as.data.frame(t(modelAP1Coef))

```
Here we are analyzing data from the second imputted data set ECLSK2
```{r}
ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
ECLSK2 = ECLSK2[c(-1)]
ECLSK2 = as.data.frame(ECLSK2)

scdrep2 = svrepdesign(data = ECLSK2, type="JKn", repweights = ECLSK2[,26:106], weights = ECLSK2[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean2= svymean(ECLSK2, scdrep2)

modelSC2 = svyglm(X1PRNCON ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC2Coef= summary(modelSC2)$coefficients[,1:2]
modelSC2Coef =  as.data.frame(t(modelSC2Coef))


modelSI2 = svyglm(X1PRNSOC ~X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep2)


modelSI2Coef= summary(modelSI2)$coefficients[,1:2]
modelSI2Coef = as.data.frame(t(modelSI2Coef))



modelSL2 = svyglm(X1PRNSAD ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep2)

modelSL2Coef= summary(modelSL2)$coefficients[,1:2]
modelSL2Coef = as.data.frame(t(modelSL2Coef))

modelIO2 = svyglm(X1PRNIMP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep2)

modelIO2Coef= summary(modelIO2)$coefficients[,1:2]
modelIO2Coef = as.data.frame(t(modelIO2Coef))

modelAP2 = svyglm(X1PRNAPP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep2)

modelAP2Coef= summary(modelAP2)$coefficients[,1:2]
modelAP2Coef =  as.data.frame(t(modelAP2Coef))
```
Now with imputed data set three
```{r}
ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
ECLSK3 = ECLSK3[c(-1)]
ECLSK3 = as.data.frame(ECLSK3)

scdrep3 = svrepdesign(data = ECLSK3, type="JKn", repweights = ECLSK3[,26:106], weights = ECLSK3[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean3= svymean(ECLSK3, scdrep3)

modelSC3 = svyglm(X1PRNCON ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC3Coef= summary(modelSC3)$coefficients[,1:2]
modelSC3Coef = as.data.frame(t(modelSC3Coef))


modelSI3 = svyglm(X1PRNSOC ~X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep3)


modelSI3Coef= summary(modelSI3)$coefficients[,1:2]
modelSI3Coef = as.data.frame(t(modelSI3Coef))



modelSL3 = svyglm(X1PRNSAD ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep3)

modelSL3Coef= summary(modelSL3)$coefficients[,1:2]
modelSL3Coef = as.data.frame(t(modelSL3Coef))

modelIO3 = svyglm(X1PRNIMP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep3)

modelIO3Coef= summary(modelIO3)$coefficients[,1:2]
modelIO3Coef = modelSL3Coef = as.data.frame(t(modelIO3Coef))

modelAP3 = svyglm(X1PRNAPP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep3)

modelAP3Coef= summary(modelAP3)$coefficients[,1:2]
modelAP3Coef= as.data.frame(t(modelAP3Coef))
```
How with imputed data four
```{r}
ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
ECLSK4 = ECLSK4[c(-1)]
ECLSK4 = as.data.frame(ECLSK4)

scdrep4 = svrepdesign(data = ECLSK4, type="JKn", repweights = ECLSK4[,26:106], weights = ECLSK4[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean4= svymean(ECLSK4, scdrep4)

modelSC4 = svyglm(X1PRNCON ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC4Coef= summary(modelSC4)$coefficients[,1:2]
modelSC4Coef = as.data.frame(t(modelSC4Coef))


modelSI4 = svyglm(X1PRNSOC ~X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep4)


modelSI4Coef= summary(modelSI4)$coefficients[,1:2]
modelSI4Coef = as.data.frame(t(modelSI4Coef))



modelSL4 = svyglm(X1PRNSAD ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep4)

modelSL4Coef= summary(modelSL4)$coefficients[,1:2]
modelSL4Coef = as.data.frame(t(modelSL4Coef))

modelIO4 = svyglm(X1PRNIMP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep4)

modelIO4Coef= summary(modelIO4)$coefficients[,1:2]
modelIO4Coef = as.data.frame(t(modelIO4Coef))

modelAP4 = svyglm(X1PRNAPP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep4)

modelAP4Coef= summary(modelAP4)$coefficients[,1:2]
modelAP4Coef = as.data.frame(t(modelAP4Coef))
```
Now with the fifth imputed data set
```{r}
ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
ECLSK5 = ECLSK5[c(-1)]
ECLSK5 = as.data.frame(ECLSK5)

scdrep5 = svrepdesign(data = ECLSK5, type="JKn", repweights = ECLSK5[,26:106], weights = ECLSK5[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean5= svymean(ECLSK5, scdrep5)

modelSC5 = svyglm(X1PRNCON ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

# Grab the parameter estimates and standard errors for each of the model to compute later 
modelSC5Coef= summary(modelSC5)$coefficients[,1:2]
modelSC5Coef = as.data.frame(t(modelSC5Coef))


modelSI5 = svyglm(X1PRNSOC ~X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep5)


modelSI5Coef= summary(modelSI5)$coefficients[,1:2]
modelSI5Coef = as.data.frame(t(modelSI5Coef))



modelSL5 = svyglm(X1PRNSAD ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep5)


modelSL5Coef= summary(modelSL5)$coefficients[,1:2]
modelSL5Coef = as.data.frame(t(modelSL5Coef))

modelIO5 = svyglm(X1PRNIMP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep5)

modelIO5Coef= summary(modelIO5)$coefficients[,1:2]
modelIO5Coef = as.data.frame(t(modelIO5Coef))

modelAP5 = svyglm(X1PRNAPP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep5)

modelAP5Coef= summary(modelAP5)$coefficients[,1:2]
modelAP5Coef = as.data.frame(t(modelAP5Coef))


# SEtting the degrees of freedom here.
df =  modelAP5$df.null 
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

Here we have the SI model
```{r}
ParsSI = rbind(modelSI1Coef[1,], modelSI2Coef[1,], modelSI3Coef[1,], modelSI4Coef[1,], modelSI5Coef[1,])
ParsSI = as.matrix(ParsSI)
ParsSI

SeSI = rbind(modelSI1Coef[2,], modelSI2Coef[2,], modelSI3Coef[2,], modelSI4Coef[2,], modelSI5Coef[2,])
SeSI = as.matrix(SeSI)
SeSI

SIModelsCombine = mi.meld(q = ParsSI, se = SeSI)

SIModelsCombine = as.data.frame(cbind(Estimate = t(SIModelsCombine$q.mi),SE =  t(SIModelsCombine$se.mi)))

names(SIModelsCombine) = c("Estimate", "SE")
SIModelsCombine
SIModelsCombine$T_Stat = SIModelsCombine$Estimate / SIModelsCombine$SE

SIModelsCombine$Sig = ifelse(SIModelsCombine$T_Stat > 2, "*", ifelse( SIModelsCombine$T_Stat < -2, "*", "NS"))

SIModelsCombine

```
NOw we have the third (SL) model for combining results
```{r}
ParsSL = rbind(modelSL1Coef[1,], modelSL2Coef[1,], modelSL3Coef[1,], modelSL4Coef[1,], modelSL5Coef[1,])
ParsSL = as.matrix(ParsSL)
ParsSL

SeSL = rbind(modelSL1Coef[2,], modelSL2Coef[2,], modelSL3Coef[2,], modelSL4Coef[2,], modelSL5Coef[2,])
SeSL = as.matrix(SeSL)
SeSL

SLModelsCombine = mi.meld(q = ParsSL, se = SeSL)

SLModelsCombine = as.data.frame(cbind(Estimate = t(SLModelsCombine$q.mi),SE =  t(SLModelsCombine$se.mi)))

names(SLModelsCombine) = c("Estimate", "SE")
SLModelsCombine
SLModelsCombine$T_Stat = SLModelsCombine$Estimate / SLModelsCombine$SE

SLModelSLombine$SLg = ifelse(SLModelSLombine$T_Stat > 2, "*", ifelse( SLModelSLombine$T_Stat < -2, "*", "NS"))

SLModelsCombine
```
Now we have the fourth (IO) model combined results
```{r}
ParsIO = rbind(modelIO1Coef[1,], modelIO2Coef[1,], modelIO3Coef[1,], modelIO4Coef[1,], modelIO5Coef[1,])
ParsIO = as.matrix(ParsIO)
ParsIO

SeIO = rbind(modelIO1Coef[2,], modelIO2Coef[2,], modelIO3Coef[2,], modelIO4Coef[2,], modelIO5Coef[2,])
SeIO = as.matrix(SeIO)
SeIO

IOModelsCombine = mi.meld(q = ParsIO, se = SeIO)

IOModelsCombine = as.data.frame(cbind(Estimate = t(IOModelsCombine$q.mi),SE =  t(IOModelsCombine$se.mi)))

names(IOModelsCombine) = c("Estimate", "SE")
IOModelsCombine
IOModelsCombine$T_Stat = IOModelsCombine$Estimate / IOModelsCombine$SE

IOModelsCombine$Sig = ifelse(IOModelsCombine$T_Stat > 2, "*", "NS")

IOModelsCombine
```
Now we have the fifth AP model
```{r}
ParsAP = rbind(modelAP1Coef[1,], modelAP2Coef[1,], modelAP3Coef[1,], modelAP4Coef[1,], modelAP5Coef[1,])
ParsAP = as.matrix(ParsAP)
ParsAP

SeAP = rbind(modelAP1Coef[2,], modelAP2Coef[2,], modelAP3Coef[2,], modelAP4Coef[2,], modelAP5Coef[2,])
SeAP = as.matrix(SeAP)
SeAP

APModelsCombine = mi.meld(q = ParsAP, se = SeAP)

APModelsCombine = as.data.frame(cbind(Estimate = t(APModelsCombine$q.mi),SE =  t(APModelsCombine$se.mi)))

names(APModelsCombine) = c("Estimate", "SE")
APModelsCombine
APModelsCombine$T_Stat = APModelsCombine$Estimate / APModelsCombine$SE

APModelsCombine$Sig = ifelse(APModelsCombine$T_Stat > 2, "*", "NS")

APModelsCombine

```


