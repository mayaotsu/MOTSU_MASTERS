#running models
#example running of BRTs using Kole distribution from SPC data for MHI
rm(list = ls())
library(matrixStats)
library(fmsb)
#getwd()
source("/Users/mayaotsu/Documents/MOTSU_MASTERS/BRT_Workshop-main/BRT_Eval_Function_JJS.R")
df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.07") 

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
df[is.nan(df)] <- NA
df$Random <- rnorm(nrow(df))
set.seed(101) 
Random <- rnorm(nrow(df))
df$Random = Random
colnames(df)
Predictors<-c( 1,2,11, 14,16:22) 
#re-add island (1), year (factor variable) 10
#depth2, lat5, lon6, year10, rugosity13, mean 1 mo chla ESA 15, q05&951yrSSTjpl16&17,
#nearshore sediment18, coral cover19, effluent20, MHI spear 21, random 22
#boxplot(taape$density ~ taape$year)

Response<-which(colnames(df) %in% c("presence") )
# Test predictors for colinearity using correlation matrix chart -- SAL and SLA are very correlated (cor = 0.74)
library(PerformanceAnalytics)
#preds<-which(!colnames(df) %in% c("biom","PA", "species", "sci", "Island", "subregion", "Year", "Lat","Lon", "PA","random"))
preds = df[, Predictors]
chart.Correlation(preds)

df <- as.data.frame(df)
Response<-which(colnames(df) %in% c("presence") )

#specify running for full or MHI
taape <- df[df$species=="LUKA",]
taape <- df[df$species == "LUKA" & df$region == "MHI", ]

start = Sys.time()
PA_Model_Step<-fit.brt.n_eval_Balanced(taape, gbm.x=Predictors, gbm.y= c(Response), lr=0.001, tc=3, family = "bernoulli",bag.fraction=0.75, n.folds= 10, 50)
end = Sys.time()
end - start 

save(PA_Model_Step, file = paste0("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/taape_mhi_step_0.001_0.75_07.7.Rdata"))

#lr 0.001
#try bag fractions 0.6, 0.75
#function creates ensemble of your choice size, learning rate and tree complexity, low learning rate better
#for learning rate, at least 1000 trees, bag fraction 0.5-0.8 or 0.9 range, 0.9 is pretty high
#number of folds: how to cross validate predictive skill (usually 4-5 for gbm step, gbm step determining best # of trees)
#number of iterations (ensemble size) probably 50-100 range

PA_Model<-PA_Model_Step[[1]]
Model_Evals_PA<-unlist(unlist(PA_Model_Step[[2]]))

Model_PA_Eval<-matrix(,length(PA_Model),2)
for (i in 1:length(PA_Model)){
  Model_PA_Eval[i,1]<-Model_Evals_PA[[i]]@auc
  Model_PA_Eval[i,2]<-max(Model_Evals_PA[[i]]@TPR+Model_Evals_PA[[i]]@TNR-1)
}

print(summary(Model_PA_Eval[,1]))
print(summary(Model_PA_Eval[,2]))

#now reduce to 'non-random' predictors
var_tested<-names(taape[,Predictors])

iters=length(PA_Model)
percent_contrib<-NULL#list()
for(q in 1:iters){                               
  sum1<-summary(PA_Model[q][[1]]  , plot=F )
  sum2<-sum1[order(sum1[,1], levels = var_tested),]
  percent_contrib<-cbind(percent_contrib, sum2[,2])
  rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
}

#mean importance percentage for each predictor
Mean_PA_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))

#removing predictors less important than random
Predictors_to_Keep_Index<-which(Mean_PA_Contributions>Mean_PA_Contributions$Random)

Predictors_to_Keep<-Mean_PA_Contributions[,Predictors_to_Keep_Index]
Reduced_Predictors<-which(colnames(taape) %in% colnames(Predictors_to_Keep))

#refit model
start = Sys.time()
PA_Model_Reduced<-fit.brt.n_eval_Balanced(taape, gbm.x=Reduced_Predictors, gbm.y= c(Response), lr=0.001, tc=3, family = "bernoulli",bag.fraction=0.60, n.folds=10, 50)
end = Sys.time()
end - start 

save(PA_Model_Reduced, file = paste0("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/taape_mhi_reduced_0.001_0.75_07.7.Rdata"))

#re-evaluate model fit
PA_Model<-PA_Model_Reduced[[1]]

Model_Evals_PA<-unlist(unlist(PA_Model_Reduced[[2]]))

Model_PA_Eval<-matrix(,length(PA_Model),2)

for (i in 1:length(PA_Model)){
  Model_PA_Eval[i,1]<-Model_Evals_PA[[i]]@auc
  Model_PA_Eval[i,2]<-max(Model_Evals_PA[[i]]@TPR+Model_Evals_PA[[i]]@TNR-1)
}

print(summary(Model_PA_Eval[,1]))
print(summary(Model_PA_Eval[,2]))

#recalculate variable importance for the reduced model
var_tested<-names(taape[,Reduced_Predictors])

percent_contrib<-NULL
iters=length(PA_Model)
part_plot<-list()
part_plot<-list()
percent_contrib<-NULL#list()
Cont_Preds<-names(Filter(is.numeric,taape[,Reduced_Predictors]))
Num_Preds<-which(var_tested %in% Cont_Preds)

for(q in 1:iters){                                #this was 50 
  mod<-PA_Model[q][[1]] 
  ###
  part_plot1<-data.frame(row.names=1:100) #return grid of points for predictor variables, looping through each variable
  for(x in Num_Preds){ ###
    pp<-plot(mod ,var_tested[x],return.grid=T) ###
    part_plot1<-cbind(part_plot1, pp) ###
  }###
  
  ###
  part_plot[[q]]<-part_plot1 ###
  
  sum1<-summary(PA_Model[q][[1]]  , plot=F )
  sum2<-sum1[order(sum1[,1], levels = var_tested),]
  percent_contrib<-cbind(percent_contrib, sum2[,2])
  rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
}
All_percent_contribution<-cbind(rownames(percent_contrib), paste(round(rowMeans(percent_contrib),2), round(rowSds(percent_contrib),2), sep=" ± "))
Combined_All_percent_contribution<-All_percent_contribution
saveRDS(All_percent_contribution, file = paste0("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/taape_mhi_reduced_0.001_0.75_precentcont07.7.rds"))

Mean_PA_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))
PA_Predictors_Plot<- rbind(rep(max(Mean_PA_Contributions),length(var_tested)) , rep(0,length(var_tested)) , Mean_PA_Contributions)
PA_Predictors_Plot[]<-sapply(PA_Predictors_Plot, as.numeric)
par(mfrow=c(1,1))

#shows visual interpretation of imoortant variables
radarchart(PA_Predictors_Plot,  pfcol=rgb(0.0,0.3,0.5,0.5), pcol=rgb(0.0,0.3,0.5,0.5), title="taape P/A" )

Variable_List<-as.data.frame(t(Mean_PA_Contributions))
Variable_List$Variables<-rownames(Variable_List)
Variable_List<-Variable_List[order(-Variable_List$V1),]


Num_Preds<-which(rownames(Variable_List) %in% Cont_Preds)

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/taape_mhi_reduced_0.001_0.75_pdp_07.7.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,3))
mn_part_plot<-list()  
for(y in Num_Preds){
  id<-which(colnames(part_plot[[1]])==Variable_List$Variables[y])
  all1<-NULL
  all2<-NULL
  for(z in 1:iters){											 #
    all1<-rbind(all1, cbind(c(part_plot[[z]][,id]))) #binding model estimtes
    all2<-rbind(all2, cbind(c(part_plot[[z]][,id+1])))
  }
  all3<-cbind(all1, all2)
  all1<-all3[order(all3[,1]),]
  
  plot(all1, xlab=Variable_List$Variables[y], col="white", ylab=paste("f(",Variable_List$Variables[y], ")", sep=""),cex.axis=1.2, cex.lab=1.2) #, ylim=c(-8,2))
  plx<-predict(loess(all1[,2] ~ all1[,1], span = 0.3), se=T) #loess spline
  mn_part_plot[[y]]<- cbind(all1[,1], plx$fit)      
  lines(all1[,1],plx$fit)
  lines(all1[,1],plx$fit - qt(0.975,plx$df)*plx$se, lty=2)#0.975 #conf intervals
  lines(all1[,1],plx$fit + qt(0.975,plx$df)*plx$se, lty=2)
  rug(na.omit(unlist(taape[Variable_List$Variables[y]])))
  legend("bottomright", paste(All_percent_contribution[which(All_percent_contribution[,1]==Variable_List$Variables[y]),2],"%", sep=" "), bty="n", cex=1.4)
}
dev.off()

# Make Forest plots (easier interpretation for partial responses)
#png(paste0("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/forest/taape_full_reduced_0.001_0.75_forestplot07.7.png"), units = "in", height = 5, width = 5, res = 500)
#taapePA_0.001_0.75_AllPercentCont
PA_sp = data.frame(predictor = taape_full_reduced_0.001_0.75_precentcont07.7[,1],
                   percent_imp = as.numeric(sub("\\ .*", "", taape_full_reduced_0.001_0.75_precentcont07.7[,2])),
                   sd = as.numeric(substr(taape_full_reduced_0.001_0.75_precentcont07.7[,2], nchar(taape_full_reduced_0.001_0.75_precentcont07.7[,2])-4+1, 
                   nchar(taape_full_reduced_0.001_0.75_precentcont07.7[,2]))),
                   color = c("blue","blue", "gray", "red", 
                             "blue", "red", "blue", "gray"))

ggplot(data=PA_sp, aes(y=reorder(predictor, percent_imp), x=percent_imp, xmin=(percent_imp-sd), xmax=(percent_imp+sd))) +
  geom_point(colour = PA_sp$color, size = 2.5) + 
  geom_errorbarh(height=.1, colour = PA_sp$color) +
  scale_fill_discrete() +
  labs(title = 'L. kasmira (Full Archipelago)', x='Percent Contribution', y = '') +
  scale_x_continuous(limits = c(0, 40))+
  #geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_classic() + theme(axis.text = element_text(size=14), axis.title = element_text(size=14))
ggsave("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/forest/taape_full_reduced_0.001_0.75_forestplot07.7.png", width = 5, height = 5, units = "in")
dev.off()

## taape full
color = c("blue","blue", "gray", "red", 
          "blue", "red", "blue", "gray"))

#taape MHI
color = c("blue","blue", "gray", "red", 
          "red", "red", "blue", "gray"))


######now make abund. only model#################

taape_pres<-taape[taape$presence==1,]
taape_pres$Log_Abund<-log(taape_pres$density)
Response<-which(colnames(taape_pres) %in% c("Log_Abund") )

#fit model to all predictors
Abund_Model_Step<-fit.brt.n_eval_Balanced(taape_pres, gbm.x=Predictors, gbm.y= c(Response), lr=0.001, tc=3, family = "gaussian",bag.fraction=0.75, n.folds=5, 3)
save(Abund_Model_Step, file = paste0("/Users/mayaotsu/Documents/MOTSU_MASTERS/models/0.001_0.75/taape_abun_model_step_0.001_bf0.75.Rdata"))

Abund_Model<-Abund_Model_Step[[1]]

#check model fit for R2 and RMSE 
Model_Evals_Abund<- data.frame(matrix(unlist(Abund_Model_Step[[2]]), nrow=length(Abund_Model_Step[[2]]), byrow=TRUE))
colnames(Model_Evals_Abund)<-c("R2","RMSE")

print(summary(Model_Evals_Abund[,1]))
print(summary(Model_Evals_Abund[,2]))


#now reduce to 'non-random' predictors
var_tested<-names(taape_pres[,Predictors])

iters=length(Abund_Model)
percent_contrib<-NULL#list()
for(q in 1:iters){                               
  sum1<-summary(Abund_Model[q][[1]]  , plot=F )
  sum2<-sum1[order(sum1[,1], levels = var_tested),]
  percent_contrib<-cbind(percent_contrib, sum2[,2])
  rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
}


Mean_PA_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))

Predictors_to_Keep_Index<-which(Mean_PA_Contributions>Mean_PA_Contributions$Random)

Predictors_to_Keep<-Mean_PA_Contributions[,Predictors_to_Keep_Index]
Reduced_Predictors<-which(colnames(taape_pres) %in% colnames(Predictors_to_Keep))

#refit model
Abund_Model_Reduced<-fit.brt.n_eval_Balanced(taape_pres, gbm.x=Reduced_Predictors, gbm.y= c(Response), lr=0.001, tc=3, family = "gaussian",bag.fraction=0.75, n.folds=5, 3)
save(Abund_Model_Reduced, file = paste0("/Users/mayaotsu/Documents/MOTSU_MASTERS/models/0.001_0.75/taape_abun_model_reduced_0.001_bf0.75.Rdata"))


#re-evaluate model fit


Abund_Model<-Abund_Model_Reduced[[1]]


Model_Evals_Abund<- data.frame(matrix(unlist(Abund_Model_Reduced[[2]]), nrow=length(Abund_Model_Reduced[[2]]), byrow=TRUE))
colnames(Model_Evals_Abund)<-c("R2","RMSE")

print(summary(Model_Evals_Abund[,1]))
print(summary(Model_Evals_Abund[,2]))


#plot variable importance and partial dependence plots.

var_tested<-names(taape_pres[,Reduced_Predictors])

percent_contrib<-NULL
iters=length(Abund_Model)
part_plot<-list()
part_plot<-list()
percent_contrib<-NULL
Cont_Preds<-names(Filter(is.numeric,taape_pres[,Reduced_Predictors]))
Num_Preds<-which(var_tested %in% Cont_Preds)

for(q in 1:iters){                               
  mod<-Abund_Model[q][[1]] 
  ###
  part_plot1<-data.frame(row.names=1:100)
  for(x in Num_Preds){ ###
    pp<-plot(mod ,var_tested[x],return.grid=T) ###
    part_plot1<-cbind(part_plot1, pp) ###
  }###
  
  ###
  part_plot[[q]]<-part_plot1 ###
  
  sum1<-summary(Abund_Model[q][[1]]  , plot=F )
  sum2<-sum1[order(sum1[,1], levels = var_tested),]
  percent_contrib<-cbind(percent_contrib, sum2[,2])
  rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
}
All_percent_contribution<-cbind(rownames(percent_contrib), paste(round(rowMeans(percent_contrib),2), round(rowSds(percent_contrib),2), sep=" ± "))
Combined_All_percent_contribution<-All_percent_contribution
saveRDS(All_percent_contribution, file = paste0("/Users/mayaotsu/Documents/MOTSU_MASTERS/models/0.001_0.75/taapeAbun_AllPercentCont.rds"))

Mean_Abund_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))
Abund_Predictors_Plot<- rbind(rep(max(Mean_Abund_Contributions),length(var_tested)) , rep(0,length(var_tested)) , Mean_Abund_Contributions)
Abund_Predictors_Plot[]<-sapply(Abund_Predictors_Plot, as.numeric)
par(mfrow=c(1,1))

radarchart(Abund_Predictors_Plot,  pfcol=rgb(0.0,0.3,0.5,0.5), pcol=rgb(0.0,0.3,0.5,0.5), title="taape Abund." )

Variable_List<-as.data.frame(t(Mean_Abund_Contributions))
Variable_List$Variables<-rownames(Variable_List)
Variable_List<-Variable_List[order(-Variable_List$V1),]


Num_Preds<-which(rownames(Variable_List) %in% Cont_Preds)

png("taape_abundance_trial0.001_0.75bf.png", res = 300, height = 10, width = 8, units = "in")
par(mfrow=c(5,3))
mn_part_plot<-list()  
for(y in Num_Preds){
  id<-which(colnames(part_plot[[1]])==Variable_List$Variables[y])
  all1<-NULL
  all2<-NULL
  for(z in 1:iters){											 
    all1<-rbind(all1, cbind(c(part_plot[[z]][,id])))
    all2<-rbind(all2, cbind(c(part_plot[[z]][,id+1])))
  }
  all3<-cbind(all1, all2)
  all1<-all3[order(all3[,1]),]
  
  plot(all1, xlab=Variable_List$Variables[y], col="white", ylab=paste("f(",Variable_List$Variables[y], ")", sep=""),cex.axis=1.2, cex.lab=1.2) #, ylim=c(-8,2))
  plx<-predict(loess(all1[,2] ~ all1[,1], span = 0.3), se=T)
  mn_part_plot[[y]]<- cbind(all1[,1], plx$fit)      
  lines(all1[,1],plx$fit)
  lines(all1[,1],plx$fit - qt(0.975,plx$df)*plx$se, lty=2)#0.975
  lines(all1[,1],plx$fit + qt(0.975,plx$df)*plx$se, lty=2)
  rug(na.omit(unlist(taape_pres[Variable_List$Variables[y]])))
  legend("bottomright", paste(All_percent_contribution[which(All_percent_contribution[,1]==Variable_List$Variables[y]),2],"%", sep=" "), bty="n", cex=1.4)
}
dev.off()

#########Now compare hurdle model fit############
#predict.gmb predicting P/A models
#number of rows and length of ensemble, creating a matrix of site level estimates times models and abundance seprately
#avg of each row, combining them in same taape, taking multiplicative result of 2 for hurdle estimate
PA_Predictions<-matrix(, nrow=nrow(taape), ncol=length(PA_Model))
Abund_Predictions<-matrix(, nrow=nrow(taape), ncol=length(Abund_Model))

for (k in 1:length(PA_Model)){
  PA_Predictions[,k]<-predict.gbm(PA_Model_Reduced[[1]][[k]], taape, n.trees=PA_Model_Reduced[[1]][[k]]$n.trees, type="response")
  Abund_Predictions[,k]<-predict.gbm(Abund_Model_Reduced[[1]][[k]], taape, n.trees=Abund_Model_Reduced[[1]][[k]]$n.trees, type="response")
  
}                   
PA_Estimates<-rowMeans(PA_Predictions,na.rm=T)
Abund_Estimates<-rowMeans(Abund_Predictions,na.rm=T)
taape<-cbind(taape, PA_Estimates, Abund_Estimates)
taape$Hurdle_Estimate<-taape$PA_Estimates*exp(taape$Abund_Estimates)

cor.test(taape$CTST,taape$Hurdle_Estimate)
cor(taape$CTST,taape$Hurdle_Estimate)^2
plot(taape$Hurdle_Estimate, taape$CTST)
plot(taape$Hurdle_Estimate, taape$CTST)
plot(taape$PA_Estimates, taape$CTST)
plot(taape$Abund_Estimates, taape$CTST)

