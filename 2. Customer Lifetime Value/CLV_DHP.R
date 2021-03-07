# *****************************************

# Pareto NBD for CLV

# *****************************************


getwd()
setwd('C:/Users/antothomsong/Desktop/Desktop - Copy/Personal Dovuments/Babi/Capstone Project/Latest_my/dunnhumby_Carbo-Loading/dunnhumby - Carbo-Loading CSV/Final/Latest')
install.packages("dplyr ")


install.packages("tidyverse")

install.packages("lubridate")

install.packages("BTYD")

install.packages("purrr")





library(dplyr)

library(readxl)

library(tidyverse) #For data cleaning

library(lubridate) #For data cleaning

library(BTYD) #Buy till you die

library(BTYDplus)

library(purrr) #It takes a vector as input and applies a function to each element of the vector




#******** Load the data*****


dta<- read.csv("DHP.csv", header = T)

names(dta)





dta
# We select only the three columns that we need:

#  the Purchase Date, household, and the dollar_sales


dta <-
  
  dta %>%
  
  mutate(sales = dollar_sales, # Add 'sales' column
         
         date = mdy(Purchase.Date)) %>% 
  
  select(date, # Rename and select 'date', 'cust', and 'sales' columns
         
         cust = household,
         
         sales)

head(dta)

# Merge transactions to get date-wise customer-wise sales

dta <- dc.MergeTransactionsOnSameDate(dta)

dta

# Determine the period

min_date <- min(dta$date) #It is 2017-01-01
min_date
max_date <- max(dta$date) #It is 2018-12-29
max_date
end.period <- as_date(as.duration((max_date - min_date)))

n.periods <- round(as.numeric(max_date - min_date)/7) #Weekly
dta




# ElogToCbsCbt uses an event log to return calibration period CBT and CBS, holdout period CBT and CBS, and

# summary data for each customer (including times of first and last transactions).



# CBS: customer-by-sufficient-statistic

# CBS which consists of each customer’s frequency, recency (the time of their last transactions)

# and total time observed - but the timing of each and every transaction (other than the last)

# is not needed by the model



# CBS must contain columns for frequency ("x"), recency ("t.x"), and total time observed ("T.cal").

# Note that recency must be the time between the start of the calibration period

# and the customer’s last transaction, not the time between the

# customer’s last transaction and the end of the calibration period.



# CBT:  customer-by-time

# dc.ElogToCbsCbt produces a customer-by-time (CBT) matrix.



# This matrix consists of a row for every customer and a column for every date, and is

# populated by a statistic of your choice (reach, frequency, or spend).

# It is not necessary for any of the models presented in this package, but is used as a

# building block to produce the CBS.

# T.star: Length of the holdout period





calib <- dc.ElogToCbsCbt(dta,
                         
                         per = "week", T.cal = max(dta$date), statistic = "freq")


#T.cal: indicating when the calibration period ends





# Extract cbs matrix of the caliberation data

cal.cbs <- calib[["cal"]][["cbs"]]



# cal.cbs

#cal.cbs give "x" (Freq) and "t.x" (Recency)



# ********* Estimate parameters for Pareto NBD *********

params <- bgnbd.EstimateParameters(cal.cbs)


#This "params" est is using "calib" which has calculated at weekly frequency





# Find the log likelihood

LL <- pnbd.cbs.LL(params, cal.cbs)





# ************** Model Fit Testing ***************************

pnbd.PlotFrequencyInCalibration(params, cal.cbs, 5)







# ************** Predictions  ***************************

# Prediction for one year (52 weeks)

# Estimate the number transactions a new customer will make in 52 weeks



exp_transactions <- pnbd.Expectation(params, t = 52)

exp_transactions





# Estimate transactions for a specific customer after 52 weeks

Cust_12967 <- pnbd.ConditionalExpectedTransactions(params,
                                                   
                                                   n.periods,T.star = 52,cal.cbs["12967", "x"],
                                                   
                                                   cal.cbs["12967", "T.cal"])



Cust_12967



# Survival probability for a specific customer



Surv_12967 <- pnbd.PAlive(params, cal.cbs["12967", "x"],
                          
                          cal.cbs["12967", "t.x"], cal.cbs["12967", "T.cal"])



Surv_12967

# Survival probability of all customers

Surv_All <- pnbd.PAlive(params, cal.cbs[, "x"],
                          
                          cal.cbs[, "t.x"], cal.cbs[, "T.cal"])

pnbd.PlotDropoutRateHeterogeneity(params)

write.csv(Surv_All, file = 'C:/Users/antothomsong/Desktop/Desktop - Copy/Personal Dovuments/Babi/Capstone Project/Latest_my/dunnhumby_Carbo-Loading/dunnhumby - Carbo-Loading CSV/Final/Latest/Surv_all.csv')

# ************** Predictions  ***************************

cal.cbt <- dc.ElogToCbsCbt(dta, per = "day", T.cal = max(dta$date),
                           
                           statistic = "total.spend")

#T.cal: indicating when the calibration period ends







# Average spend function: Avg per purchase instance

calculateAvgSpend <- function(cbt.row)
  
{
  
  purchaseCols = which(cbt.row != 0)
  
  sum(cbt.row[purchaseCols]) / length(purchaseCols)
  
}



m.x <- apply(cal.cbs, 1, calculateAvgSpend) #Values in rows

m.x[which(is.na(m.x))] <- 0 #Handling NA

spendParams <- spend.EstimateParameters(m.x, cal.cbs[,1]) #Estimates of spending parameter

warnings()

end.period <- as.Date(end.period)

data_clv <- dc.ElogToCbsCbt(dta, per = "day", T.cal = max(dta$date))

cal.cbs <- data_clv[["cal"]][["cbs"]]

params_clv <- bgnbd.EstimateParameters(cal.cbs) #Parameters of CLV

#This "params_clv" est is using "data_clv" which has calculated at daily frequency

#These values would be slightly different from values in "params"



x <- cal.cbs[, "x"]

t.x <- cal.cbs[, "t.x"]

T.cal <- cal.cbs[, "T.cal"] #T.cal is the total time observed

d <- .10 # discount rate (per annum)



dis_exp_transactions <- pnbd.DERT(params_clv, x, t.x, T.cal, d / 52)

# DERT: Discounted Expected Residual Transactions



exp <- data.frame(cust = data_clv$cust.data$cust,
                  
                  dis_exp_transactions = dis_exp_transactions)



rownames(exp) <- NULL



expected_spend <- spend.expected.value(spendParams, m.x, cal.cbs[,1])

exp$expected_spend <- expected_spend

exp$projected_future_revenue <- exp$expected_spend * exp$dis_exp_transactions



exp # Prints the result
write.csv(exp, file = 'C:/Users/antothomsong/Desktop/Desktop - Copy/Personal Dovuments/Babi/Capstone Project/Latest_my/dunnhumby_Carbo-Loading/dunnhumby - Carbo-Loading CSV/Final/Latest/exp_DHP.csv')
