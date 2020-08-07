# Submited By RAvi Shankar
# Date : 27-08-2008

#----------------- Required Packages -------------------------# 

if (!require("fastDummies")) install.packages("fastDummies")
if (!require("mlr")) install.packages("mlr")
if (!require("pROC")) install.packages("pROC")
if (!require("plyr")) install.packages("plyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("skimr")) install.packages("skimr")
if (!require("GGally")) install.packages("GGally")
if (!require("plotly")) install.packages("plotly")
if (!require("caret")) install.packages("caret")
if (!require("data.table")) install.packages("data.table")
if (!require("xgboost")) install.packages("xgboost")
if (!require("magrittr")) install.packages("magrittr")

#----------------- Library loading ---------------------------#

 library("fastDummies")
 library("mlr")
 library("pROC")
 library("plyr")
 library("tidyverse")
 library("skimr")
 library("plotly")
 library("caret")
 library("data.table")
 library("xgboost")
 library("magrittr")
 library("randomForest")
 library("e1071")
 

#------------------ setting up the working directory -------------------# 

setwd("D:/Home Loan/10000")
getwd()


#----------- Reading all data set from working directory ---------------#

application<- read.csv('sample_application_train.csv',stringsAsFactors = FALSE)
dim(application)
application[1:6,1:6]
summarizeColumns(application)
table(application$TARGET)

pre_app <- read.csv('previous_application.csv',stringsAsFactors = FALSE)
dim(pre_app)
pre_app[1:6,1:6]
summarizeColumns(pre_app)


cash_bal <- read.csv('POS_CASH_balance.csv',stringsAsFactors = FALSE)
dim(cash_bal)
head(cash_bal)
summarizeColumns(cash_bal)


install_pay <- read.csv('installments_payments.csv',stringsAsFactors = F)
dim(install_pay)
head(install_pay)
summarizeColumns(install_pay)

credit_bal <- read.csv('credit_card_balance.csv',stringsAsFactors = F)
dim(credit_bal)
credit_bal[1:10,1:6]
summarizeColumns(credit_bal)

bureau <- read.csv('bureau.csv',stringsAsFactors = F)
dim(bureau)
bureau[1:10,1:7]
summarizeColumns(bureau)


bureau_bal <- read.csv('bureau_balance.csv',stringsAsFactors = F)
dim(bureau_bal)
head(bureau_bal)
summarizeColumns(bureau_bal)


# ----------------------DATA PREPARATION / Dummy variables --------------------------------#
#------------------------------------------------------------------------------------------#
bureau_bal_char_var <- as.vector( which(sapply(bureau_bal,is.character)))
length(bureau_bal_char_var)
head(bureau_bal[,bureau_bal_char_var])
table(bureau_bal$STATUS)
bureau_bal$STATUS <- as.character(revalue(bureau_bal$STATUS, c('C' = "B_closed",
                                                               'X' = "B_unknown", 
                                                               "0" = "B_No_DPD",
                                                               "1" = "B_DPD1",
                                                               "2" = "B_DPD2",
                                                               "3" = "B_DPD3",
                                                               "4" = "B_DPD4",
                                                               "5" = "B_DPD5")))


table(bureau_bal$STATUS)
dim(bureau_bal)
bureau_bal <-  fastDummies::dummy_cols(bureau_bal,select_columns = "STATUS")
dim(bureau_bal)
head(bureau_bal)
bureau_bal <- bureau_bal[,-which(names(bureau_bal) %in% c("STATUS","B_DPD5"))]
dim(bureau_bal)

breau_bal_char_var <- as.vector(which(sapply(bureau_bal,is.character)))
length(breau_bal_char_var)
bureau_bal[1:5,1:6]
summarizeColumns(bureau_bal)



bureau_char_var <- as.vector( which(sapply(bureau,is.character)))
length(bureau_char_var)
head(bureau[,bureau_char_var])

table(bureau$CREDIT_ACTIVE)

bureau$CREDIT_ACTIVE <- as.character(revalue(bureau$CREDIT_ACTIVE, c('Active' = "B_Acc_Active", 
                                                                     'Closed' = "B_Acc_Closed", 
                                                                     "Sold" = "B_Acc_Sold")))



table(bureau$CREDIT_TYPE)
bureau$CREDIT_TYPE <- as.character(revalue(bureau$CREDIT_TYPE, c("Credit card" = "B_Card_credit",
                                                                 "Another type of loan" = "B_Another_credit", 
                                                                 "Car loan" = "B_Car_Credit",
                                                                 "Consumer credit" = "B_Consumer_credit",
                                                                 "Loan for business development" = "B_Business_credit",
                                                                 "Loan for working capital replenishment" = "B_working_credit",
                                                                 "Unknown type of loan" = "B_unkown_credit",
                                                                 "Microloan" = "B_Microloan_credit",
                                                                 "Mortgage" = "B_Mortgage_credit",
                                                                 "Loan for purchase of shares (margin lending)" = "B_Purchase_Share", # 10000
                                                                 "Loan for the purchase of equipment" = "B_Purchase_equipments", #10000
                                                                 "Real estate loan" = "B_estate_credit")))
table(bureau$CREDIT_TYPE)


table(bureau$CREDIT_CURRENCY)
bureau$CREDIT_CURRENCY <- as.character(revalue(bureau$CREDIT_CURRENCY, c('currency 1' = "B_currency_1", 
                                                                         'currency 2' = "B_currency_2", 
                                                                         'currency 3' = "B_currency_3",
                                                                         'currency 4' = "B_currency_4")))


bureau <-  fastDummies::dummy_cols(bureau,select_columns = c( "CREDIT_ACTIVE","CREDIT_CURRENCY","CREDIT_TYPE"))
dim(bureau)

bureau  <- bureau[,-which(names(bureau) %in% c("CREDIT_ACTIVE","CREDIT_CURRENCY","CREDIT_TYPE","B_currency_4","B_estate_credit","B_Acc_Sold"))]
dim(bureau)
bureau[1:10,1:6]
bureau_char_var <- as.vector( which(sapply(bureau,is.character)))
options(warn=-1)
length(bureau_char_var)

###### Credit_bal DataSet

table(credit_bal$NAME_CONTRACT_STATUS)
credit_bal$NAME_CONTRACT_STATUS <- as.character(revalue(credit_bal$NAME_CONTRACT_STATUS, c('Active' = "Credit_Card_Active", 
                                                                                           "Completed" = "Credit_Card_Completed",
                                                                                           "Approved" = "Credit_Card_Approved", # 10000
                                                                                           "Sent proposal" = "Credit_Card_Prosal",
                                                                                           "Signed" = "Credit_Card_Signed")))
table(credit_bal$NAME_CONTRACT_STATUS)
dim(credit_bal)
credit_bal <-  fastDummies::dummy_cols(credit_bal,select_columns = "NAME_CONTRACT_STATUS")
dim(credit_bal)
credit_bal[1:6,1:6]

credit_bal  <- subset(credit_bal, select = -NAME_CONTRACT_STATUS)
dim(credit_bal)
credit_bal_char_var <- as.vector( which(sapply(credit_bal,is.character)))
length(bureau_char_var)


##### Cash_bal

# changing name of few variables which are  clashing with names of variable of other datasets
names(cash_bal)[names(cash_bal) == "SK_DPD"] <- "Cash_SK_DPD"
names(cash_bal)[names(cash_bal) == "SK_DPD_DEF"] <- "Cash_CCSK_DPD_DEF"

cash_bal$NAME_CONTRACT_STATUS <- as.character(revalue(cash_bal$NAME_CONTRACT_STATUS, c('Active' = "Card_Active", 
                                                                                       'Approved' = "Card_Approved", 
                                                                                       "Completed" = "Card_Completed",
                                                                                       "Demand" = "Card_Demand",
                                                                                       "Canceled" = "Card_Canceled", # 10000
                                                                                       "Returned to the store" = "Card_Returned",
                                                                                       "Signed"= "Card_Signed")))
table(cash_bal$NAME_CONTRACT_STATUS)

dim(cash_bal)
cash_bal <-  fastDummies::dummy_cols(cash_bal,select_columns = "NAME_CONTRACT_STATUS")
dim(cash_bal)

cash_bal  <- subset(cash_bal, select = -NAME_CONTRACT_STATUS)
dim(cash_bal)
cash_bal_char_var <- as.vector( which(sapply(cash_bal,is.character)))
length(cash_bal_char_var)


### Pre_app dataset


pre_app$NAME_CONTRACT_TYPE <- as.character(revalue(pre_app$NAME_CONTRACT_TYPE, c('Cash loans' = "Pre_Cash_loan", 
                                                                                 'Consumer loans' = "Pre_Consumer_loan", 
                                                                                 "Revolving loans" = "Pre_Revolving_loans",
                                                                                 "XNA" = "Pre_XNA_loan")))
table(pre_app$NAME_CONTRACT_TYPE)

pre_app$NAME_CASH_LOAN_PURPOSE <- as.character(revalue(pre_app$NAME_CASH_LOAN_PURPOSE, c('Building a house or an annex' = "Pur_House", 
                                                                                         'Business development' = "Pur_Business", 
                                                                                         "Buying a garage" = "Pur_Garage",
                                                                                         "Buying a holiday home / land" = "Pur_Land",
                                                                                         "Buying a home" = "Pur_Home",
                                                                                         "Buying a new car" = "Pur_New_Car",
                                                                                         "Buying a used car" = "Pur_Used_Car",
                                                                                         "Car repairs" = "Pur_Car_repairs",
                                                                                         "Education" = "Pur_Education",
                                                                                         "Everyday expenses" = "Pur_Expenses",
                                                                                         "Furniture" = "Pur_Furniture",
                                                                                         "Gasification / water supply" = "Pur_Supply",
                                                                                         "Journey" = "Pur_Journey",
                                                                                         "Medicine" = "Pur_Medicine",
                                                                                         "Other" = "Pur_Other",
                                                                                         "Payments on other loans" = "Pur_Payments_loans",
                                                                                         "Purchase of electronic equipment" = "Pur_Electronics",
                                                                                         "Repairs" = "Pur_Repairs",
                                                                                         "Urgent needs" = "Pur_Urgent",
                                                                                         "Wedding / gift / holiday" = "Pur_wedding",
                                                                                         "XAP" = "Pur_XAP",
                                                                                         "XNA" = "Pur_XNA")))
table(pre_app$NAME_CASH_LOAN_PURPOSE)

pre_app$NAME_CONTRACT_STATUS <- as.character(revalue(pre_app$NAME_CONTRACT_STATUS, c('Approved' = "Pre_Contract_Approved", 
                                                                                     'Canceled' = "Pre_Contract_Canceled", 
                                                                                     "Refused" = "Pre_Contract_Refused",
                                                                                     "Unused offer" = "Pre_Contract_Unused")))
table(pre_app$NAME_CONTRACT_STATUS)



pre_app$CODE_REJECT_REASON <- as.character(revalue(pre_app$CODE_REJECT_REASON, c('CLIENT' = "Reject_Client", 
                                                                                 'HC' = "Reject_HC", 
                                                                                 "LIMIT" = "Reject_LIMIT",
                                                                                 "SCO" = "Reject_SCO",
                                                                                 "SCOFR" = "Reject_SCOFR",
                                                                                 "SYSTEM" = "Reject_SYSTEM",
                                                                                 "VERIF" = "Reject_VERIF",
                                                                                 "XAP" = "Reject_XAP",
                                                                                 "XNA" = "Reject_XNA")))
table(pre_app$CODE_REJECT_REASON)


pre_app$NAME_CLIENT_TYPE <- as.character(revalue(pre_app$NAME_CLIENT_TYPE, c('New' = "Pre_Client_New", 
                                                                             'Refreshed' = "Pre_Client_Refreshed", 
                                                                             "Repeater" = "Pre_Client_Repeater",
                                                                             "XNA" = "Pre_Client_XNA")))
table(pre_app$NAME_CLIENT_TYPE)



pre_app$NAME_GOODS_CATEGORY <- as.character(revalue(pre_app$NAME_GOODS_CATEGORY, c('Additional Service' = "Pre_Goods_Service", 
                                                                                   'Audio/Video' = "Pre_Goods_AV", 
                                                                                   "Auto Accessories" = "Pre_Goods_Auto",
                                                                                   "Clothing and Accessories" = "Pre_Goods_Cloths",
                                                                                   "Computers" = "Pre_Goods_Computers",
                                                                                   "Construction Materials" = "Pre_Goods_Construction",
                                                                                   "Consumer Electronics" = "Pre_Goods_Electronics",
                                                                                   "Direct Sales" = "Pre_Goods_Sales",
                                                                                   "Education" = "Pre_Goods_Education",
                                                                                   "Furniture" = "Pre_Goods_Furniture",
                                                                                   "Gardening" = "Pre_Goods_Gardening",
                                                                                   "Homewares" = "Pre_Goods_Homewares",
                                                                                   "Jewelry" = "Pre_Goods_Jewelry",
                                                                                   "Medical Supplies" = "Pre_Goods_Medical",
                                                                                   "Medicine" = "Pre_Goods_Medicine",
                                                                                   "Mobile" = "Pre_Goods_Mobile",
                                                                                   "Office Appliances" = "Pre_Goods_Office",
                                                                                   "Other" = "Pre_Goods_Other",
                                                                                   "Photo / Cinema Equipment" = "Pre_Goods_Photo",
                                                                                   "Sport and Leisure" = "Pre_Goods_Sport",
                                                                                   "Tourism" = "Pre_Goods_Tourism",
                                                                                   "Vehicles" = "Pre_Goods_Vehicles",
                                                                                   "XNA" = "Pre_Goods_XNA",
                                                                                   "Fitness" = "Pre_Goods_Fitness", #10000
                                                                                   "Insurance" = "Pre_Goods_Insurance"))) #10000

table(pre_app$NAME_GOODS_CATEGORY)



pre_app$NAME_PORTFOLIO <- as.character(revalue(pre_app$NAME_PORTFOLIO, c('Cards' = "Pre_Portfolio_Cards", 
                                                                         'Cars' = "Pre_Portfolio_Cars", 
                                                                         "Cash" = "Pre_Portfolio_Cash",
                                                                         "XNA" = "Pre_Portfolio_XNA",
                                                                         "POS" = "Pre_Potfolio_POS")))
table(pre_app$NAME_PORTFOLIO)


pre_app$NAME_PRODUCT_TYPE <- as.character(revalue(pre_app$NAME_PRODUCT_TYPE, c('walk-in' = "Pre_Porduct_walk_in", 
                                                                               'x-sell' = "Pre_Porduct_x_sell", 
                                                                               "XNA" = "Pre_Porduct_XNA")))
table(pre_app$NAME_PRODUCT_TYPE)


pre_app$NAME_SELLER_INDUSTRY <- as.character(revalue(pre_app$NAME_SELLER_INDUSTRY, c('Auto technology' = "Seller_Auto", 
                                                                                     'Clothing' = "Seller_Clothing", 
                                                                                     "Connectivity" = "Seller_Connectivity",
                                                                                     "Construction" = "Seller_Construction",
                                                                                     "Consumer electronics" = "Seller_Electronics",
                                                                                     "Furniture" = "Seller_Furniture",
                                                                                     "Industry" = "Seller_Industry",
                                                                                     "Jewelry" = "Seller_Jewelry",
                                                                                     "MLM partners" = "Seller_MLM",
                                                                                     "Tourism" = "Seller_Tourism",
                                                                                     "XNA" = "Seller_XNA")))
table(pre_app$NAME_SELLER_INDUSTRY)



pre_app$NAME_YIELD_GROUP <- as.character(revalue(pre_app$NAME_YIELD_GROUP, c("high" = "Pre_Yield_high", 
                                                                             "low_action" = "Pre_Yield_low_action", 
                                                                             "low_normal" = "Pre_Yield_low_normal",
                                                                             "middle" = "Pre_Yield_middle",
                                                                             "XNA" = "Pre_Yield_XNA")))
table(pre_app$NAME_YIELD_GROUP)


pre_app$PRODUCT_COMBINATION[pre_app$PRODUCT_COMBINATION == ""] <- "None"
pre_app$PRODUCT_COMBINATION <- as.character(revalue(pre_app$PRODUCT_COMBINATION, c("None" = "Product_None",
                                                                                   "Card Street" = "Product_Card_Street", 
                                                                                   "Card X-Sell" = "Product_Card_X_Sell", 
                                                                                   "Cash" = "Product_Cash",
                                                                                   "Cash Street: high" = "Product_Cash_Street_high",
                                                                                   "Cash Street: low" = "Product_Cash_Street_low",
                                                                                   "Cash Street: middle" = "Product_Cash_Street_middle",
                                                                                   "Cash X-Sell: high" = "Product_Cash_X_Sell_high",
                                                                                   "Cash X-Sell: low" = "Product_Cash_X_Sell_low",
                                                                                   "Cash X-Sell: middle" = "Product_Cash_X_Sell_middle",
                                                                                   "POS household with interest" = "Product_house_interest",
                                                                                   "POS household without interest" = "Product_house_without_interest",
                                                                                   "POS industry with interest" = "Product_industry_interest",
                                                                                   "POS industry without interest" = "Product_industry_without_interest",
                                                                                   "POS mobile with interest" = "Product_mobile_interest",
                                                                                   "POS mobile without interest" = "Product_mobile_without_interest",
                                                                                   "POS other with interest" = "Product_other_interest",
                                                                                   "POS others without interest" = "Product_others_without_interest")))
table(pre_app$PRODUCT_COMBINATION)



table(pre_app$WEEKDAY_APPR_PROCESS_START)

pre_app$WEEKDAY_APPR_PROCESS_START <- as.character(revalue(pre_app$WEEKDAY_APPR_PROCESS_START, c("MONDAY" = "Pre_MONDAY", 
                                                                             "TUESDAY" = "Pre_TUESDAY", 
                                                                             "WEDNESDAY" = "Pre_WEDNESDAY",
                                                                             "THURSDAY" = "Pre_THURSDAY",
                                                                             "FRIDAY" = "Pre_FRIDAY",
                                                                             "SATURDAY" = "Pre_SATURDAY",
                                                                             "SUNDAY" = "Pre_SUNDAY")))

table(pre_app$WEEKDAY_APPR_PROCESS_START)


table(pre_app$FLAG_LAST_APPL_PER_CONTRACT)
pre_app$FLAG_LAST_APPL_PER_CONTRACT <- as.character(revalue(pre_app$FLAG_LAST_APPL_PER_CONTRACT, c("N" = "Pre_No", 
                                                                                                 "Y" = "Pre_Yes")))

table(pre_app$NAME_PAYMENT_TYPE)
pre_app$NAME_PAYMENT_TYPE <- as.character(revalue(pre_app$NAME_PAYMENT_TYPE, c("Cash through the bank" = "Pre_bank", 
                                                                               "Cashless from the account of the employer" = "Pre_cashless",
                                                                               "Non-cash from your account" = "Pre_Non_cash",
                                                                               "XNA" = "Pre_XNA_Payment")))

table(pre_app$NAME_TYPE_SUITE)

pre_app$NAME_TYPE_SUITE[pre_app$NAME_TYPE_SUITE == ""] <- "blank"
pre_app$NAME_TYPE_SUITE <- as.character(revalue(pre_app$NAME_TYPE_SUITE, c( "blank" = "Pre_blank_suite", 
                                                                            "Children" = "Pre_Children",
                                                                            "Family" = "Pre_Family",
                                                                            "Group of people" = "Pre_Group_people",
                                                                            "Other_A" = "Pre_Other_A",
                                                                            "Other_B" = "Pre_Other_B",
                                                                            "Spouse, partner" = "Pre_pertner",
                                                                            "Unaccompanied" = "Pre_Unaccompanied")))


table(pre_app$CHANNEL_TYPE)
pre_app$CHANNEL_TYPE <- as.character(revalue(pre_app$CHANNEL_TYPE, c( "AP+ (Cash loan)" = "Pre_Channel_Cash_loan", 
                                                                      "Car dealer" = "Pre_Car_dealer",
                                                                      "Channel of corporate sales" = "Pre_corporate_sales",
                                                                      "Contact center" = "Pre_Contact_center",
                                                                      "Country-wide" = "Pre_Country_wide",
                                                                      "Credit and cash offices" = "Pre_offices",
                                                                      "Regional / Local" = "Pre_Regional_Local",
                                                                      "Stone" = "Pre_Stone")))



pre_app <-  fastDummies::dummy_cols(pre_app,select_columns = c("NAME_CONTRACT_STATUS","NAME_CASH_LOAN_PURPOSE","NAME_CONTRACT_TYPE",
                                                               "CODE_REJECT_REASON","NAME_CLIENT_TYPE","NAME_GOODS_CATEGORY",
                                                               "NAME_PORTFOLIO","NAME_PRODUCT_TYPE","NAME_SELLER_INDUSTRY",
                                                               "NAME_YIELD_GROUP", "PRODUCT_COMBINATION","WEEKDAY_APPR_PROCESS_START",
                                                               "FLAG_LAST_APPL_PER_CONTRACT","NAME_PAYMENT_TYPE","NAME_TYPE_SUITE",
                                                               "NAME_CLIENT_TYPE","CHANNEL_TYPE"))


dim(pre_app)

pre_app  <- pre_app[,-which((names(pre_app) %in% c("NAME_CONTRACT_STATUS","NAME_CASH_LOAN_PURPOSE","NAME_CONTRACT_TYPE",
                                                   "CODE_REJECT_REASON","NAME_CLIENT_TYPE","NAME_GOODS_CATEGORY",
                                                   "NAME_PORTFOLIO","NAME_PRODUCT_TYPE","NAME_SELLER_INDUSTRY",
                                                   "NAME_YIELD_GROUP", "PRODUCT_COMBINATION","WEEKDAY_APPR_PROCESS_START",
                                                   "FLAG_LAST_APPL_PER_CONTRACT","NAME_PAYMENT_TYPE","NAME_TYPE_SUITE",
                                                   "NAME_CLIENT_TYPE","CHANNEL_TYPE")))]
dim(pre_app)
pre_app_char_var <- as.vector( which(sapply(pre_app,is.character)))
length(pre_app_char_var)

# Summarizing Numerical variables and creating new ones

functions <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

Bureau_balance_ops <- bureau_bal %>% group_by(SK_ID_BUREAU) %>% summarise_all(functions) 
dim(Bureau_balance_ops)


All_bureau <- merge(x = bureau, y = Bureau_balance_ops, by = "SK_ID_BUREAU", all.x = TRUE)
All_bureau[1:6,1:5]

All_bureau_ops <- All_bureau %>% dplyr::select(-SK_ID_BUREAU) %>% group_by(SK_ID_CURR) %>% summarise_all(functions)
dim(All_bureau_ops)


credit_bal_ops <- credit_bal %>% dplyr::select(-SK_ID_PREV) %>% group_by(SK_ID_CURR) %>% summarise_all(functions)
dim(credit_bal_ops)


install_pay_ops <- install_pay %>% dplyr::select(-SK_ID_PREV) %>% group_by(SK_ID_CURR) %>% summarise_all(functions)
                                                            
dim(install_pay_ops)


cash_bal_ops <- cash_bal %>% dplyr::select(-SK_ID_PREV) %>% group_by(SK_ID_CURR) %>% summarise_all(functions)
dim(cash_bal_ops)


pre_app_ops <- pre_app %>% dplyr::select(-SK_ID_PREV) %>% group_by(SK_ID_CURR) %>% summarise_all(functions)
                                           

dim(pre_app_ops)

# Merging all dataset into 1

All_Data_set <- list( application,
                      All_bureau_ops,
                      credit_bal_ops,
                      pre_app_ops,
                      cash_bal_ops,
                      install_pay_ops)

Merge_data <- Reduce(function(x, y) merge(x, y,by= "SK_ID_CURR",all.x = TRUE),All_Data_set)
dim(Merge_data)


Merge_data_ops <- Merge_data %>% dplyr::select(-SK_ID_CURR) %>% mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
                                                                mutate( CREDIT_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
                                                                        SOURCES_Combined = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3)

Merge_data_ops[1:10,1:8]
dim(Merge_data_ops)
dim(Merge_data_ops)

# Replacing NA with mean value of the columns

for(i in 1:ncol(Merge_data_ops)){
  Merge_data_ops[is.na(Merge_data_ops[,i]), i] <- mean(Merge_data_ops[,i], na.rm = TRUE)
}

#---------------------------------------------------------------------------#
#--------------------- Feature Selection: FisherScore ----------------------#
#---------------------------------------------------------------------------#

FisherScore <- function(basetable, depvar, IV_list) {
 
  DV <- unique(basetable[, depvar])
  
  IV_FisherScore <- c()
  
  for (v in IV_list) {
    fs <- abs((mean(basetable[which(basetable[, depvar]==DV[1]), v]) - mean(basetable[which(basetable[, depvar]==DV[2]), v]))) /
      sqrt((var(basetable[which(basetable[, depvar]==DV[1]), v]) + var(basetable[which(basetable[, depvar]==DV[2]), v])))
    IV_FisherScore <- c(IV_FisherScore, fs)
  }
  
  return(data.frame(IV=IV_list, fisher_score=IV_FisherScore))
}

col_names <- names(Merge_data_ops)
col_names <- col_names[!col_names == "TARGET"] 

variables_Score <- FisherScore(basetable = Merge_data_ops, depvar = "TARGET", IV_list = col_names)
variables_Score <- arrange(variables_Score, desc(fisher_score))
dim(variables_Score)

# Selecting top 40 variables 
 
Selected_variables <- as.data.frame( variables_Score[1:40,1])
Selected_variables


Basetable <- Merge_data_ops[, names(Merge_data_ops) %in% Selected_variables$`variables_Score[1:40, 1]`]
Basetable$TARGET <- Merge_data_ops$TARGET

int_var <- as.vector( which(sapply(Basetable,is.integer)))
Basetable[,int_var] <- sapply(Basetable[,int_var], as.numeric)
summarizeColumns(Basetable)
names(Basetable)
# Writting Basetable
write.csv(Basetable, file = "Basetable.csv",row.names=FALSE)
names(Basetable)


# -------------------------Gaadient Boosting ---------------------------------#
# ----------------------------------------------------------------------------#

set.seed(100)
#Create 10 equally size folds
folds <- cut(seq(1,nrow(Basetable)),breaks=5,labels=FALSE)
#Perform 10 fold cross validation

Model_AUC <- c()
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainSet <- Basetable[-testIndexes, ]
  Target_train <- trainSet$TARGET
  trainSet <- as.matrix( subset(trainSet, select = -TARGET))
  coln <- names(trainSet)
  train_matrix <- xgb.DMatrix(data = trainSet, label = Target_train)
  
  testSet <- Basetable[testIndexes, ]
  Target_test <- testSet$TARGET
  test_matrix <- as.matrix(subset(testSet,select = -TARGET))
  dval_matrix <- xgb.DMatrix(data = test_matrix, label = Target_test)
  
  Model_formula <- list(objective = "binary:logistic",booster = "gbtree",eval_metric = "auc",min_child_weight = 30,nrounds = 3000)
  
  Model <- xgb.train(Model_formula, train_matrix, Model_formula$nrounds, list(val = dval_matrix), early_stopping_rounds = 400)
  Model_AUC <- append(Model_AUC,Model$best_score)
  
  #Use the test and train data partitions however you desire...
}
Mean_AUC = mean(Model_AUC) 
cat("\n", "Mean AUC of Boosting = " , Mean_AUC)
xgb.importance(coln, model=Model) %>% xgb.plot.importance(top_n = 20)

# ----------------- Logistic regression -----------------------#
# -------------------------------------------------------------#


Model_AUC <- c()
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- Basetable[testIndexes, ]
  trainData <- Basetable[-testIndexes, ]
  
  Model <- glm(TARGET ~ . ,data = trainData,family = binomial)
  
  result= predict(Model,testData,type = "response")
  result = ifelse(result >= 0.4,1,0)
  AUC_R = auc(result, testData$TARGET)
  Model_AUC <- append(Model_AUC,AUC_R)
  
  #Use the test and train data partitions however you desire...
}
Mean_AUC = mean(Model_AUC) 
cat("\n", "Mean AUC Logistic Regression = " , Mean_AUC)

#---------------------------- SVM --------------------------------------#


basetable_x <- as.matrix(subset(Basetable, select= -TARGET ))
Basetable_y <- as.matrix(Basetable$TARGET)

svm_tune <- tune(svm, train.x=basetable_x, train.y=Basetable_y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tune
#Create 10 equally size folds
folds <- cut(seq(1,nrow(Basetable)),breaks=5,labels=FALSE)
#Perform 10 fold cross validation

Model_AUC <- c()
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- Basetable[testIndexes, ]
  trainData <- Basetable[-testIndexes, ]
  
  svm_radial <- svm(TARGET~., data=trainData,cost=1, gamma=0.5 )
 
  result = predict(svm_radial, newdata = testData)
  AUC_R = pROC::auc(result, testData$TARGET)
  Model_AUC <- append(Model_AUC,AUC_R)
  
  #Use the test and train data partitions however you desire...
}

Mean_AUC = mean(Model_AUC) 
cat("\n", "Mean AUC for SVM = " , Mean_AUC)



#------------------------RANDOM FOREST -----------------------------------#
# ------------------------------------------------------------------------#


trControl <- trainControl(method = "cv",number = 5,search = "grid")
rf_default <- train( TARGET~.,data = Basetable,method = "rf",metric = "AUC",trControl = trControl)


Model_AUC <- c()
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- Basetable[testIndexes, ]
  trainData <- Basetable[-testIndexes, ]
  
  Model <- randomForest(TARGET ~ .,data = Basetable, mtry = 6, importance = TRUE)
  
  result= predict(Model,testData)
  AUC_R = auc(result, testData$TARGET)
  Model_AUC <- append(Model_AUC,AUC_R)
  
  #Use the test and train data partitions however you desire...
}

Mean_AUC = mean(Model_AUC) 
cat("\n", "Mean AUC Random Forest= " , Mean_AUC)



# ----------------------- Business Part -  Variable importance ------------#

# ----------------------- EXT_SOURCE_2 -----------------------------------#
source2 <- Basetable[,c("EXT_SOURCE_2","TARGET")]
summarizeColumns(source2)
source2$Groups <- cut(x= source2$EXT_SOURCE_2, breaks=seq(from=0, to= 0.9, by = 0.1))
Bygroup = tapply(source2$TARGET, source2$Groups, NROW)
Bygroup1 = tapply(source2$TARGET, source2$Groups, sum) 

barplot(height = Bygroup, xlab = "Source 2 ( Bin = 0.5)", ylab = " NUmber of Observation in Bin",beside=TRUE, width=2,
        space=c(0.25,0.25))
barplot(height = Bygroup1, xlab = "Source 2 (Bin = 0.5)", ylab = "Number of Defaulter in Bin",beside=TRUE, width=2,
        space=c(0.25,0.25))
# ------------------------- EXT_SOURCE_3 ------------------------------#
source3 <- Basetable[,c("EXT_SOURCE_3","TARGET")]
summarizeColumns(source3)
source3$Groups <- cut(x= source3$EXT_SOURCE_3, breaks=seq(from=0, to= 0.9, by = 0.1))
Bygroup2 = tapply(source3$TARGET, source3$Groups, NROW)
Bygroup3 = tapply(source3$TARGET, source2$Groups, sum) 

barplot(height = Bygroup2, xlab = "Source 3 ( Bin = 0.5)", ylab = " NUmber of Observation in Bin",beside=TRUE, width=1)
barplot(height = Bygroup3, xlab = "Source 3 (Bin = 0.5)", ylab = "Number of Defaulter in Bin",beside=TRUE, width=1)

# ---------------------------- DAYS_BIRTH -------------------------------------#

birth <- Basetable[,c("DAYS_BIRTH","TARGET")]
summarizeColumns(birth)
birth<-  birth %>% mutate( old = floor(-(birth$DAYS_BIRTH/365)))
head(birth)
class(birth$old)
birth$Groups <- cut(x= birth$old, breaks=seq(from=20, to= 70, by = 5))
Bygroup4 = tapply(birth$TARGET, birth$Groups, NROW)
Bygroup5 = tapply(birth$TARGET, birth$Groups, sum) 

barplot(height = Bygroup4, xlab = " Age of applicants ( Bin = 5 Years)", ylab = " NUmber of Observation in Bin",beside=TRUE, width=1)
barplot(height = Bygroup5, xlab = "Age of applicants ( Bin = 5 Years)", ylab = "Number of Defaulter in Bin",beside=TRUE, width=1)


# -------------------- AMT_GOODS_PRICE ---------------------------------------#

goods <- Basetable[,c("AMT_GOODS_PRICE","TARGET")]
summarizeColumns(goods)
goods <- goods[goods$AMT_GOODS_PRICE <= 1200000,]
dim(goods)
table(goods$TARGET)
goods$Groups <- cut(x= goods$AMT_GOODS_PRICE, breaks=seq(from=0, to= 1200000, by = 100000))
Bygroup6 = tapply(goods$TARGET, goods$Groups, NROW)
Bygroup7 = tapply(goods$TARGET, goods$Groups, sum) 

barplot(height = Bygroup6, xlab = " Goods Price ( Bin = 500000)", ylab = " NUmber of Observation in Bin",beside=TRUE, width=1)
barplot(height = Bygroup7, xlab = " Goods Price ( Bin = 500000)", ylab = "Number of Defaulter in Bin",beside=TRUE, width=1)





