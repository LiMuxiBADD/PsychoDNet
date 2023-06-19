### training the PsycoRF model
library(randomForest)
library(mlbench)
library(caret)
library(e1071)
library(pROC)
library(doMC)
library(skimr)

trainn_data <- read.csv("train_data.csv",row.names = 1)

# fixed parameter training -----------------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=10, 
                        summaryFunction=twoClassSummary,
                        classProbs=TRUE)
tunegrid <- expand.grid(.mtry = 8) 

### set seed to improve reproducibility
set.seed(3334)
### set number of cores used for model training
registerDoMC(cores = 30)
rf_gridsearch_ROC<- caret::train(dig ~ ., 
                                 data =trainn_data,
                                 method = 'rf',
                                 metric = 'ROC',
                                 tuneGrid = tunegrid,
                                 trControl=control,
                                 ntree=1500
)


# get training results -- 10 fold cross-validation ------------------------

### ROC model required
caret_cv2tab <- function(model){
  model_cv <- model[["resampledCM"]]
  colnames(model_cv)[1:4] <- c("TN","FP","FN","TP")
  model_cv$sum <- rowSums(model_cv[,1:4])
  model_cv$acc <- (model_cv$TN+model_cv$TP)/model_cv$sum
  model_cv$precision <- model_cv$TP/(model_cv$TP+model_cv$FP)
  model_cv$MCC <- mltools::mcc(TP=model_cv$TP,TN=model_cv$TN,FP=model_cv$FP,FN=model_cv$FN)
  
  cv_tab <- data.frame(Acc=mean(model_cv$acc),AccSD=sd(model_cv$acc),Prec=mean(model_cv$precision),PrecSD=sd(model_cv$precision),
                       model[["results"]],MCC=mean(model_cv$MCC),MCCSD=sd(model_cv$MCC))
  cv_tab <- cv_tab[,c("Acc","AccSD","Prec","PrecSD","Sens","SensSD","Spec","SpecSD","MCC","MCCSD","ROC","ROCSD")]
  colnames(cv_tab)[which(colnames(cv_tab)=="ROC")] <- "AUC"
  colnames(cv_tab)[which(colnames(cv_tab)=="ROCSD")] <- "AUCSD"
  cv_tab <- data.frame(t(cv_tab))
  colnames(cv_tab) <- model[["method"]]
  return(cv_tab)
}

result_tab <- caret_cv2tab(rf_gridsearch_ROC)

### The deciding threshold for PsycoRF is determined as about 0.4067 according to Youden Index
### randomly sample 1000 individuals
set.seed(333)
ind_sample <- sample(1:nrow(trainn_data),1000)

### get confusion matrix for threshold-adjusted model on these 1000 individuals
label <- trainn_data[ind_sample,"dig"]%>% as.factor()
rf_pre <- predict(rf_gridsearch_ROC,newdata=trainn_data[ind_sample,],type = "prob")
pred_final <- ifelse(rf_pre$P>0.4067,"P","N") %>% as.factor()
confM <- confusionMatrix(data = pred_final,reference=label,positive = 'P')

