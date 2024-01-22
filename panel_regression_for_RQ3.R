library(plm)
library(car)
library(lmtest)
library("tseries")
library(readxl)

setwd("D:/op/Desktop/replication-package/RQ1-3/variables/")
data <- read_excel("data/data_for_RQ3.xlsx")

data[data == -1] <- NA
pdata <- pdata.frame(data, index = c("Project", "Time"))
data$Time <- as.Date(paste0(data$Time, "-01"), "%Y-%m-%d")
pdata <- pdata.frame(data, index = c("Project", "Time"))

#normalize
variables_to_normalize <- c("issueEntropy", "resultAge", "resultCore", "resultExternal", "resultFile", "resultForks", "resultIssue", "resultPr", "resultSubscribe")
numeric_columns <- sapply(pdata[, variables_to_normalize], is.numeric)
pdata[, variables_to_normalize] <- scale(pdata[, variables_to_normalize, drop = FALSE])

#company vs. noncompany
Company_data <- pdata[pdata$Project %in% c('go', 'bootstrap', 'react', 'tensorflow'), ]
Noncompany_data <- pdata[pdata$Project %in% c('rails','vue','electron','rust'), ]

#BugFixTime
result1 <- plm(resultBugFixTime~ issueEntropy + resultAge + resultCore  + resultFile  + resultForks +  resultPr + resultSubscribe + 
              issueEntropy:resultAge + issueEntropy:resultCore   + issueEntropy:resultFile + issueEntropy:resultForks  + issueEntropy:resultPr + issueEntropy:resultSubscribe ,
              data = pdata, 
              model = "within")
result2 <- plm(resultBugFixTime~ issueEntropy + resultAge + resultCore   + resultFile  + resultForks  + resultPr + resultSubscribe +
              issueEntropy:resultAge + issueEntropy:resultCore   + issueEntropy:resultFile + issueEntropy:resultForks  + issueEntropy:resultPr + issueEntropy:resultSubscribe ,
              data = Noncompany_data, 
              model = "within")
result3 <- plm(resultBugFixTime~ issueEntropy + resultAge + resultCore   + resultFile  + resultForks  + resultPr + resultSubscribe +
              issueEntropy:resultAge + issueEntropy:resultCore   + issueEntropy:resultFile + issueEntropy:resultForks  + issueEntropy:resultPr + issueEntropy:resultSubscribe ,
              data = Company_data, 
              model = "within")
summary(result1)
summary(result2)
summary(result3)

