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
  print(class(DataInput_train))
  DataInput_test<- sqldf('SELECT * FROM DataInput EXCEPT SELECT * FROM DataInput_train') #select all rows not in df, sampling without replacement
  DataInput_train <- as.data.frame(DataInput_train)

  #input model instead of toau mhi
  avgmod.95p <- model.avg(model, cumsum(weight) <= .95, model.fit = TRUE) #train over training dataset
  print("averaging done")
  prediction <- predict(avgmod.95p,  newdata = DataInput_test, type= "response")    #data input test instead of taape mhi
  
  #refitting model over trained dataset
  #doign predict over test dataset
  #averages indivdual models , not permanently trained to df 
  
  #install pROC
  ROC <- roc(DataInput_test[,c(response)], prediction)
  
  AUC[[i]] <- auc(ROC)
}
  return(list(AUC)) 
}


# Loading the required models and response
response <- "presence"
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_full.RData")
#dataInput <- spc_reduced  # Ensure this dataset is available and loaded

# dredge_roi_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_full.rds")
# dredge_roi_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_mhi.rds")
# dredge_taape_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_full.rds")
# dredge_taape_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_mhi.rds")
# dredge_toau_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_full.rds")
 dredge_toau_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_mhi.rds")

toau_mhi <- subset(spc_reduced, species == "LUFU" & region == "MHI") #if doing mhi, make sure to subset
toau_mains_auc <- gam_crossvalidation(dataInput = toau_mhi, response = "presence", model = dredge_toau_mhi, iterations = 3)

############
