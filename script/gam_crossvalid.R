gam_crossvalidation <- function(dataInput, response, model) {
  DataInput <- dataInput 
  DataInput_Positive_Loc<- which(DataInput[,c(response)]==1) #resp var column, presences
  DataInput_Positive<-DataInput[DataInput_Positive_Loc,] #extractijg
  DataInput_Zero_Loc<- which(DataInput[,c(response)]==0) #absence var column
  DataInput_Zero<-DataInput[DataInput_Zero_Loc,] 
  DataInput_bound_Positive  <- floor((nrow(DataInput_Positive)/4)*3) #define % of training and test set, 75/25 here from presence and absence
  DataInput_bound_Zero <- floor((nrow(DataInput_Zero)/4)*3) #define % of training and test set
  DataInput_train_Positive<- DataInput_Positive[sample(nrow(DataInput_Positive),DataInput_bound_Positive),] #draw of 3/4 presence stations
  DataInput_train_Zero<- DataInput_Zero[sample(nrow(DataInput_Zero),DataInput_bound_Zero),]
  #print(head(DataInput_train_Positive))
  #print(head(DataInput_train_Zero))
  DataInput_train<-rbind(DataInput_train_Positive, DataInput_train_Zero) #combines to be new training df
  #print("Made Datainput_Train")
  #print(head(DataInput_train))
  print(class(DataInput_train))
  DataInput_test<- sqldf('SELECT * FROM DataInput EXCEPT SELECT * FROM DataInput_train') #select all rows not in df, sampling without replacement
  DataInput_train <- as.data.frame(DataInput_train)
  #call in the best output from the gam
  
  #input model instead of toau mhi
  model <- dredge() # do we rerun dredge with the training dataset; getting replacement has 409 rows, data has 1024
  avgmod.95p <- model.avg(model, cumsum(weight) <= .95, model.fit = TRUE, data = DataInput_train) #train over training dataset
  model$pred <- predict(avgmod.95p,  DataInput_test, type= "response")    #data input test instead of taape mhi
  
  #refitting model over trained dataset
  #doign predict over test dataset
  #averages indivdual models , not permanently trained to df 
  
  #install pROC
  ROC <- roc(DataInput_test[,c(response)], DataInput_test$pred)
  
  AUC <- auc(ROC)
  return(list(AUC))
}

# Loading the required models and response
response <- "presence"
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_full.RData")
#dataInput <- spc_reduced  # Ensure this dataset is available and loaded

dredge_roi_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_full.rds")
dredge_roi_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_mhi.rds")
dredge_taape_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_full.rds")
dredge_taape_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_mhi.rds")
dredge_toau_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_full.rds")
dredge_toau_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_mhi.rds")

spc_reduced_toau_mhi <- subset(spc_reduced, species == "LUFU" & region == "MHI")
gam_crossvalidation(dataInput = spc_reduced_toau, response = "presence", model = dredge_toau_mhi)

models <- list(
  taape_full = dredge_taape_full,
  taape_mhi  = dredge_taape_mhi,
  toau_full  = dredge_toau_full,
  toau_mhi   = dredge_toau_mhi,
  roi_full   = dredge_roi_full,
  roi_mhi    = dredge_roi_mhi
)

# Run cross-validation
AUC_summary <- gam_crossvalidation(dataInput = dataInput, response= response, models, num_iterations = 50)
print(AUC_summary)


############


gam_crossvalidation(dataInput = , response = presence , model = )
gam_crossvalidation <- function(dataInput, response, model, iterations = 50) {
  library(MuMIn)
  library(pROC)
  library(sqldf)
  auc_results <- list()
  for (model_name in names(models)) {
    auc_values <- numeric(iterations)
  
  for (i in 1:iterations) {
  DataInput <- dataInput 
    DataInput_Positive_Loc<- which(DataInput[,c(response)]==1) #resp var column, presences
    DataInput_Positive<-DataInput[DataInput_Positive_Loc,] #extracti
    DataInput_Zero_Loc<- which(DataInput[,c(response)]==0) #absence var column
    DataInput_Zero<-DataInput[DataInput_Zero_Loc,] 
   
    DataInput_bound_Positive  <- floor((nrow(DataInput_Positive)/4)*3) #define % of training and test set, 75/25 here from presence and absence
    DataInput_bound_Zero <- floor((nrow(DataInput_Zero)/4)*3) #define % of training and test set
    DataInput_train_Positive<- DataInput_Positive[sample(nrow(DataInput_Positive),DataInput_bound_Positive),] #draw of 3/4 presence stations
    DataInput_train_Zero<- DataInput_Zero[sample(nrow(DataInput_Zero),DataInput_bound_Zero),]
    DataInput_train<-rbind(DataInput_train_Positive, DataInput_train_Zero) #combines to be new training df
    
    DataInput_test<- sqldf('SELECT * FROM DataInput EXCEPT SELECT * FROM DataInput_train') #select all rows not in df, sampling without replacement
    
#call in the best output from the gam
    
#input model instead of toau mhi
avgmod.95p <- model.avg(models[[model_name]], cumsum(models[[model_name]]$weight) <= .95, model.fit = TRUE, data = DataInput_train)
#avgmod.95p <- model.avg(model, cumsum(model$weight) <= .95, model.fit = TRUE, data = DataInput_train) #train over training dataset
DataInput_test$pred <- predict(avgmod.95p,  DataInput_test, type= "response")    #data input test instead of taape mhi

#install pROC
ROC <- roc(DataInput_test[, c(response)], DataInput_test$pred)
auc_values[i] <- auc(ROC)  # Store AUC value for this iteration
  }
    avg_auc <- mean(auc_values)
    std_auc <- sd(auc_values)
    
    auc_results[[model_name]] <- list(mean_auc = avg_auc, std_auc = std_auc, all_auc = auc_values)
  }
    return(auc_results)
}

#refitting model over trained dataset
#doign predict over test dataset
#averages indivdual models , not permanently trained to df 

load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_full.RData")
dredge_roi_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_full.rds")
dredge_roi_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_mhi.rds")
dredge_taape_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_full.rds")
dredge_taape_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_mhi.rds")
dredge_toau_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_full.rds")
dredge_toau_mhi <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_mhi.rds")

models <- list(
  dredge_roi_full = dredge_roi_full,
  dredge_roi_mhi = dredge_roi_mhi,
  dredge_taape_full = dredge_taape_full,
  dredge_taape_mhi = dredge_taape_mhi,
  dredge_toau_full = dredge_toau_full,
  dredge_toau_mhi = dredge_toau_mhi
)

results <- gam_crossvalidation(dataInput = spc_reduced, response = "presence", models = models, iterations = 50)
results$dredge_roi_full$mean_auc  # The mean AUC for dredge_roi_full
results$dredge_roi_full$std_auc   # The standard deviation of AUC for dredge_roi_full
results$dredge_roi_mhi$mean_auc   # The mean AUC for dredge_roi_mhi
results$dredge_roi_mhi$std_auc   

results$dredge_taape_full$mean_auc  # The mean AUC for dredge_roi_full
results$dredge_taape_full$std_auc   # The standard deviation of AUC for dredge_roi_full
results$dredge_taape_mhi$mean_auc   # The mean AUC for dredge_roi_mhi
results$dredge_taape_mhi$std_auc

results$dredge_toau_full$mean_auc  # The mean AUC for dredge_roi_full
results$dredge_toau_full$std_auc   # The standard deviation of AUC for dredge_roi_full
results$dredge_toau_mhi$mean_auc   # The mean AUC for dredge_roi_mhi
results$dredge_toau_mhi$std_auc



# Run for each model and store the results
results <- list()
results$dredge_roi_full <- gam_crossvalidation(dataInput = spc_reduced, response = "presence", model = dredge_roi_full, iterations = 50)
results$dredge_roi_mhi <- gam_crossvalidation(dataInput = spc_reduced, response = "presence", model = dredge_roi_mhi, iterations = 50)
results$dredge_taape_full <- gam_crossvalidation(dataInput = spc_reduced, response = "presence", model = dredge_taape_full, iterations = 50)
results$dredge_taape_mhi <- gam_crossvalidation(dataInput = spc_reduced, response = "presence", model = dredge_taape_mhi, iterations = 50)
results$dredge_toau_full <- gam_crossvalidation(dataInput = spc_reduced, response = "presence", model = dredge_toau_full, iterations = 50)
results$dredge_toau_mhi <- gam_crossvalidation(dataInput = spc_reduced, response = "presence", model = dredge_toau_mhi, iterations = 50)

# View results (average AUC and standard deviation for each model)
results_df <- data.frame(
  Model = c("dredge_roi_full", "dredge_roi_mhi", "dredge_taape_full", "dredge_taape_mhi", "dredge_toau_full", "dredge_toau_mhi"),
  Avg_AUC = sapply(results, function(res) res$avg_auc),
  SD_AUC = sapply(results, function(res) res$sd_auc)
)


gam_crossvalidation <- function(dataInput, response, models, iterations = 50) {
  library(MuMIn)
  library(pROC)
  library(sqldf)
  
  # Create a list to store AUC values for each model
  auc_results <- list()
  
  # Loop over each model
  for (model_name in names(models)) {
    auc_values <- numeric(iterations)  # Store AUC values for the 50 iterations
    
    for (i in 1:iterations) {
      DataInput <- dataInput
      
      # Split data into presence and absence
      DataInput_Positive_Loc <- which(DataInput[, c(response)] == 1)  # Presence locations
      DataInput_Positive <- DataInput[DataInput_Positive_Loc, ]  # Subset for presence
      DataInput_Zero_Loc <- which(DataInput[, c(response)] == 0)  # Absence locations
      DataInput_Zero <- DataInput[DataInput_Zero_Loc, ]  # Subset for absence
      
      # Define training and testing data (75%/25% split)
      DataInput_bound_Positive <- floor((nrow(DataInput_Positive) / 4) * 3)
      DataInput_bound_Zero <- floor((nrow(DataInput_Zero) / 4) * 3)
      
      # Randomly sample 75% for training
      DataInput_train_Positive <- DataInput_Positive[sample(nrow(DataInput_Positive), DataInput_bound_Positive), ]
      DataInput_train_Zero <- DataInput_Zero[sample(nrow(DataInput_Zero), DataInput_bound_Zero), ]
      
      # Combine to form the training set
      DataInput_train <- rbind(DataInput_train_Positive, DataInput_train_Zero)
      
      # Create the test set (all remaining data not in training)
      DataInput_test <- sqldf('SELECT * FROM DataInput EXCEPT SELECT * FROM DataInput_train')
      
      # Combine models using model.avg() based on weights
      avgmod.95p <- model.avg(models[[model_name]], cumsum(models[[model_name]]$weight) <= .95, model.fit = TRUE, data = DataInput_train)
      
      # Get predictions for the test set
      DataInput_test$pred <- predict(avgmod.95p, DataInput_test, type = "response")
      
      # Calculate the ROC curve and AUC
      ROC <- roc(DataInput_test[, c(response)], DataInput_test$pred)
      auc_values[i] <- auc(ROC)  # Store AUC value for this iteration
    }
    
    # Calculate the mean and standard deviation for the AUC values for this model
    avg_auc <- mean(auc_values)
    std_auc <- sd(auc_values)
    
    # Store the results in the auc_results list
    auc_results[[model_name]] <- list(mean_auc = avg_auc, std_auc = std_auc, all_auc = auc_values)
  }
  
  # Return the results
  return(auc_results)
}

print(results_df)