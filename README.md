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
#a.out$imputations
a.out.imp= imputationList(na.omit(a.out$imputations))
#head(test)
dim(a.out.imp)
typeof(a.out.imp)
#head(a.out.imp[c(1:6)])
write.amelia(obj = a.out, file.stem = "ECLSK")
#head(a.out.imp[c(1:6)])
#test = a.out.imp$imputations$imp1
```

This is what the weights should look like:
scdrep = svrepdesign(data = data1, type="JKn", repweights = data1[,27:106], weights = data1[,27], combined.weights = TRUE, rscales = 1, scale = 1)
```{r}


scdrep = svrepdesign(data = (a.out.imp), type="JKn", repweights = a.out.imp[,], weights = a.out.imp[,25:105], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svymean(data1, scdrep)

modelSC = svyglm(X1PRNCON ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelSC)

modelSI = svyglm(X1PRNSOC ~X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelSI)


modelSL = svyglm(X1PRNSAD ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelSL)

modelIO = svyglm(X1PRNIMP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelIO)

modelAP = svyglm(X1PRNAPP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelAP)
```

