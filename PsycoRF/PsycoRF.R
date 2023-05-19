### PsycoRF for predicting psychosis

### pROC was used for drawing ROC cirve and computing AUC; mltools was used for computing MCC
## If you do not have true labels for your own data, or no need to compute AUC or MCC, these two packages were not necessary
## Be free to use your own blood metabolite data for psychosis prediction
library(openxlsx)
library(caret)
library(pROC)
library(mltools)

# Data import -------------------------------------------------------------

### data:     example data; 49 columns of blood metabolite indicators; last column for label (N=non-psychosis, while P=psychosis)
### sum_data: the first four columns are statistics of training data;
###           the last two columns are min and max of training data after pre-processing, used for feature clipping & min-max normalization
data <- read.xlsx("example_data.xlsx",colNames = T)
sum_data <- read.xlsx("summary_data.xlsx",colNames = T)
load("PsycoRF.Rda")

# data <- Valida_na_unprep

# Detect missing values ---------------------------------------------------
data[,1:49] <- apply(data[,1:49],2,as.numeric)
anyNA(data) #TRUE or FALSE
which(data==0) # output: index (like 1,2,3) or integer(0)
data[is.na(data)] <- 0
which(data==0) 

### FALSE and integer(0) and  indicate that the data provided contain no missing values, skip to "Data Pre-processing"
### else follow the steps to replace the missing values (at most 3 missing feature):

Detect_na <- apply(data[,1:49],1,function(x){which(x==0) %>% length}) %>% as.data.frame()
colnames(Detect_na) <- "number of NA"
ind_rm <- which(Detect_na$`number of NA`>3)
if(length(ind_rm)>0){
  data <- data[-ind_rm,] ### remove records with >3 missing values out of 49 features
  } 

for (i in 1:nrow(data)){
  for (j in 1:49){
    if(data[i,j] == 0){
      data[i,j] <- sum_data[j,"mean"]}
    
  }
}


# Data Pre-processing ------------------------------------------------------

### Feature clipping
for (i in 1:nrow(data)){
  for (j in 1:49){
    data[i,j] <- ifelse(data[i,j] < sum_data[j,"min_fin"], sum_data[j,"min_fin"],
                        ifelse(data[i,j] > sum_data[j,"max_fin"], sum_data[j,"max_fin"], data[i,j]))
      }
}



#####  Normalization by (x-min)/(max-min)
for (i in 1:nrow(data)){
  for (j in 1:49){
    data[i,j] <- (data[i,j]-sum_data[j,"min_fin"])/(sum_data[j,"max_fin"]-sum_data[j,"min_fin"])
  }
}


# PsycoRF prediction --------------------------------------------------------
pred <- predict(PsycoRF,newdata=data,type="prob")
pred_final <- ifelse(pred$P>0.4067,"P","N")
data$pred_final <- pred_final

# If you have the true labels of data, following steps for generating evaluation metrics --------

C_pred2tab <- function(model, data){
  pred_prob <- predict(model,newdata=data,type="prob")
  label <- as.factor(data$dig)
  pred_final <- ifelse(pred_prob$P>0.4067,"P","N") %>% factor(levels = c("N","P"))
  confM <- confusionMatrix(data = pred_final,reference=label,positive = 'P')
  mcc <- mcc(preds = pred_final, actuals = label)
  auc <- auc(predictor=pred_prob$N, response=label,smooth=T)
  pred_tab <- data.frame(confM$overall[["Accuracy"]],confM$byClass[["Precision"]],confM[["byClass"]][["Sensitivity"]],
                         confM[["byClass"]][["Specificity"]],mcc,auc)
  colnames(pred_tab) <- c("Acc","Prec","Sens","Spec","MCC","AUC")
  pred_tab <- as.data.frame(t(pred_tab))
  colnames(pred_tab) <- model$method
  return(pred_tab)
}

tab <- C_pred2tab(PsycoRF, data)
tab