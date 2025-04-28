rm(list = ls())
library(sqldf)
library(pROC)
library(MuMIn)
library(mgcv)

gam_crossvalidation <- function(dataInput, response, model, iterations) {
  DataInput <- dataInput 
  DataInput_Positive_Loc<- which(DataInput[,c(response)]==1) #resp var column, presences
  DataInput_Positive<-DataInput[DataInput_Positive_Loc,] #extracting positive vals
  DataInput_Zero_Loc<- which(DataInput[,c(response)]==0) #absence var column
  DataInput_Zero<-DataInput[DataInput_Zero_Loc,] #extracting negative vals
  DataInput_bound_Positive  <- floor((nrow(DataInput_Positive)/4)*3) #how many rows for presence and absence
  DataInput_bound_Zero <- floor((nrow(DataInput_Zero)/4)*3) #define % of training and test set (how many rows)
  x = seq(1, iterations, 1) #1-50, by integers of 1, creating seeds
  AUC <- list()
  for (i in (1:length(x))){
  set.seed(x[i])
  DataInput_train_Positive<- DataInput_Positive[sample(nrow(DataInput_Positive),DataInput_bound_Positive),] #draw of 3/4 presence stations
  set.seed(x[i])
  DataInput_train_Zero<- DataInput_Zero[sample(nrow(DataInput_Zero),DataInput_bound_Zero),]
  #print(head(DataInput_train_Positive))
  #print(head(DataInput_train_Zero))
  DataInput_train<-rbind(DataInput_train_Positive, DataInput_train_Zero) #combines to be new training df
  #print("Made Datainput_Train")
  #print(head(DataInput_train))
  #print(class(DataInput_train))
  DataInput_test<- sqldf('SELECT * FROM DataInput EXCEPT SELECT * FROM DataInput_train') #select all rows not in df, sampling without replacement
  DataInput_train <- as.data.frame(DataInput_train)

  #input model 
  avgmod.95p <- model.avg(model, cumsum(weight) <= .95, model.fit = TRUE) #train over training dataset
  #print("averaging done")
  prediction <- predict(avgmod.95p,  newdata = DataInput_test, type= "response")    #data input test instead of taape mhi
  
  #refitting model over trained dataset
  #doign predict over test dataset
  #averages indivdual models , not permanently trained to df 
  
  #install pROC
  ROC <- roc(DataInput_test[,c(response)], prediction)
  
  AUC[[i]] <- auc(ROC)
  print(i)
}
  return(list(AUC)) 
}

# Loading the required models and response
response <- "presence"
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_full.RData")
dataInput <- spc_reduced  # Ensure this dataset is available and loaded

#load in the model we want to use 

# dredge_taape_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_full.rds")
# dredge_taape_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_mhi.rds")
# dredge_toau_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_full.rds")
# dredge_toau_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_mhi.rds")
# dredge_roi_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_full.rds")
 dredge_roi_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_mhi.rds")
 
 ####### taape full #####
 taape <- subset(spc_reduced, species == "LUKA") #if doing mhi, make sure to subset
 taape_full_auc <- gam_crossvalidation(dataInput = taape, response = "presence", model = dredge_taape_full, iterations = 50)
 # extract the numbers
 taape_full_auc_num <- sapply(taape_full_auc[[1]], function(x) {
   as.numeric(sub("Area under the curve: ", "", x))
 })
 
 mean(taape_full_auc_num)
 sd(taape_full_auc_num)
 saveRDS(taape_full_auc_num, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/crossvalid/taape_full_crossvalid.rds")
 
 #### taape mains ####
 taape_MHI <- subset(spc_reduced, species == "LUKA" & region == "MHI") #if doing mhi, make sure to subset
 taape_mhi_auc <- gam_crossvalidation(dataInput = taape_MHI, response = "presence", model = dredge_taape_mhi, iterations = 50)
 
# extract the numbers
 taape_mhi_auc_num <- sapply(taape_mhi_auc[[1]], function(x) {
   as.numeric(sub("Area under the curve: ", "", x))
 })
 
 mean(taape_mhi_auc_num)
 sd(taape_mhi_auc_num)
saveRDS(taape_mhi_auc_num, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/crossvalid/taape_mhi_crossvalid.rds")
 
 ###### toau full ######
toau_full <- subset(spc_reduced, species == "LUFU") #if doing mhi, make sure to subset
toau_full_auc <- gam_crossvalidation(dataInput = toau_full, response = "presence", model = dredge_toau_full, iterations = 50)
 # extract the numbers
toau_full_auc_num <- sapply(toau_full_auc[[1]], function(x) {
   as.numeric(sub("Area under the curve: ", "", x))
 })
 
mean(toau_full_auc_num)
sd(toau_full_auc_num)
saveRDS(toau_full_auc_num, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/crossvalid/toau_full_crossvalid.rds")
 
####### toau mhi #####
toau_mhi <- subset(spc_reduced, species == "LUFU" & region == "MHI") #if doing mhi, make sure to subset
toau_mhi_auc <- gam_crossvalidation(dataInput = toau_mhi, response = "presence", model = dredge_toau_mhi, iterations = 50)
# extract the numbers
toau_mhi_auc_num <- sapply(toau_mhi_auc[[1]], function(x) {
  as.numeric(sub("Area under the curve: ", "", x))
})

mean(toau_mhi_auc_num)
sd(toau_mhi_auc_num)
saveRDS(toau_mhi_auc_num, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/crossvalid/toau_mhi_crossvalid.rds")


#### ROi full ###3
roi <- subset(spc_reduced, species == "CEAR") #if doing mhi, make sure to subset
roi_full_auc <- gam_crossvalidation(dataInput = roi, response = "presence", model = dredge_roi_full, iterations = 50)
# extract the numbers
roi_full_auc_num <- sapply(roi_full_auc[[1]], function(x) {
  as.numeric(sub("Area under the curve: ", "", x))
})

mean(roi_full_auc_num)
sd(roi_full_auc_num)
saveRDS(roi_full_auc_num, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/crossvalid/roi_full_crossvalid.rds")


#### ROI MHI ###
roi_MHI <- subset(spc_reduced, species == "CEAR" & region == "MHI") #if doing mhi, make sure to subset
roi_mhi_auc <- gam_crossvalidation(dataInput = roi_MHI, response = "presence", model = dredge_roi_mhi, iterations = 50)
# extract the numbers
roi_mhi_auc_num <- sapply(roi_mhi_auc[[1]], function(x) {
  as.numeric(sub("Area under the curve: ", "", x))
})

mean(roi_mhi_auc_num)
sd(roi_mhi_auc_num)
saveRDS(roi_mhi_auc_num, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/crossvalid/roi_mhi_crossvalid.rds")
############
