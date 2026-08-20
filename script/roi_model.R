#running models
#example running of BRTs using Kole distribution from SPC data for MHI
rm(list = ls())
library(matrixStats)
library(fmsb)
source("/Users/mayaotsu/Documents/MOTSU_MASTERS/BRT_Workshop-main/BRT_Eval_Function_JJS.R")
df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RDS") 

#drop 2022
df <- df[df$year != "2022", ]
#df$year <- droplevels(df$year)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
df[is.nan(df)] <- NA
df$Random <- rnorm(nrow(df))
set.seed(101) 
Random <- rnorm(nrow(df))
df$Random = Random
colnames(df)
Predictors<-c(2, 11, 14:21, 23) 

Response<-which(colnames(df) %in% c("presence") )
# Look at predictor covariance and plot predictors across space to make sure they look right
# Test predictors for colinearity using correlation matrix chart -- SAL and SLA are very correlated (cor = 0.74)
library(PerformanceAnalytics)
#preds<-which(!colnames(df) %in% c("biom","PA", "species", "sci", "Island", "subregion", "Year", "Lat","Lon", "PA","random"))
preds = df[, Predictors]
chart.Correlation(preds)

df <- as.data.frame(df)
Response<-which(colnames(df) %in% c("presence") )

#specify domain for full or mhi 
roi <- df[df$species=="CEAR",]
roi <- df[df$species == "CEAR" & df$region == "MHI", ]

#boxplot(roi$density ~ roi$year)
#dev.off()
start = Sys.time()
PA_Model_Step<-fit.brt.n_eval_Balanced(roi, gbm.x=Predictors, gbm.y= c(Response), lr=0.001, tc=3, family = "bernoulli",bag.fraction=0.75, n.folds=10, 50)
end = Sys.time()
end - start 

save(PA_Model_Step, file = paste0("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/roi_mhi_step_no_island.Rdata"))
#lr 0.001
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
var_tested<-names(roi[,Predictors])

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
Reduced_Predictors<-which(colnames(roi) %in% colnames(Predictors_to_Keep))

#refit model
start = Sys.time()
PA_Model_Reduced<-fit.brt.n_eval_Balanced(roi,gbm.x=Reduced_Predictors, gbm.y= c(Response), lr=0.001, tc=3, family = "bernoulli",bag.fraction=0.75, n.folds=10, 50)
end = Sys.time()
end - start 

save(PA_Model_Reduced, file = paste0("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/roi_mhi_reduced_no_island.Rdata"))

#re-evaluate model fit
PA_Model<-PA_Model_Reduced[[1]]
Model_Evals_PA<-unlist(unlist(PA_Model_Reduced[[2]]))

Model_PA_Eval<-matrix(,length(PA_Model),2)

for (i in 1:length(PA_Model)){
  Model_PA_Eval[i,1]<-Model_Evals_PA[[i]]@auc
  Model_PA_Eval[i,2]<-max(Model_Evals_PA[[i]]@TPR+Model_Evals_PA[[i]]@TNR-1)
}

print(summary(Model_PA_Eval[,1])) #AUC
print(summary(Model_PA_Eval[,2])) #test statistc

#recalculate variable importance for the reduced model
var_tested<-names(roi[,Reduced_Predictors])

percent_contrib<-NULL
iters=length(PA_Model)
part_plot<-list()
part_plot<-list()
percent_contrib<-NULL#list()
Cont_Preds<-names(Filter(is.numeric,roi[,Reduced_Predictors]))
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
save(All_percent_contribution, file = paste0("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/roi_mhi_reduced_percentcont_no_island.Rdata"))

Mean_PA_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))
PA_Predictors_Plot<- rbind(rep(max(Mean_PA_Contributions),length(var_tested)) , rep(0,length(var_tested)) , Mean_PA_Contributions)
PA_Predictors_Plot[]<-sapply(PA_Predictors_Plot, as.numeric)
par(mfrow=c(1,1))

#shows visual interpretation of imoortant variables
radarchart(PA_Predictors_Plot,  pfcol=rgb(0.0,0.3,0.5,0.5), pcol=rgb(0.0,0.3,0.5,0.5), title="roi P/A" )

Variable_List<-as.data.frame(t(Mean_PA_Contributions))
Variable_List$Variables<-rownames(Variable_List)
Variable_List<-Variable_List[order(-Variable_List$V1),]
boxplot(roi$density ~ roi$year)
dev.off()
Num_Preds<-which(rownames(Variable_List) %in% Cont_Preds)

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/roi_mhi_reduced_no_island.png", res = 300, height = 10, width = 8, units = "in")
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
  rug(na.omit(unlist(roi[Variable_List$Variables[y]])))
  legend("bottomright", paste(All_percent_contribution[which(All_percent_contribution[,1]==Variable_List$Variables[y]),2],"%", sep=" "), bty="n", cex=1.4)
}
dev.off()

###
# Make Forest plots (easier interpretation for partial responses)
png(paste0("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/forest_plots/08.12.26/roi_full_reduced_no_island.png"), units = "in", height = 5, width = 7, res = 500)
PA_sp = data.frame(predictor = All_percent_contribution[,1],
                   percent_imp = as.numeric(sub("\\ .*", "", All_percent_contribution[,2])),
                   sd = as.numeric(substr(All_percent_contribution[,2], nchar(All_percent_contribution[,2])-4+1,
                  nchar(All_percent_contribution[,2]))),
                   color = c("gray", "gray", "red", "red", 
                             "blue", "blue"))

ggplot(data=PA_sp, aes(y=reorder(predictor, percent_imp), x=percent_imp, xmin=(percent_imp-sd), xmax=(percent_imp+sd))) +
  geom_point(colour = PA_sp$color, size = 2.5) + 
  geom_errorbarh(height=.1, colour = PA_sp$color) +
  scale_fill_discrete() +
  scale_x_continuous(limits = c(0, 50))+
  labs(title = 'Roi (MHI)', x='Percent Contribution', y = '') +
  #geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_classic() + theme(axis.text = element_text(size=14), axis.title = element_text(size=14))
ggsave("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/forest_plots/08.12.26/roi_mhi_reduced_no_island.png", width = 7, height = 5, units = "in")
dev.off()


#full color                    
color = c("gray", "red", "gray", "red", 
          "blue", "red", "blue", "gray"))

#mhi
color = c("gray", "gray", "red", "red", 
          "blue", "blue"))


###### check residuals #################
n_iters <- length(PA_Model)
pred_matrix <- matrix(NA, nrow(roi), n_iters)

for (q in 1:n_iters) {
  mod <- PA_Model[[q]]
  pred_matrix[, q] <- predict.gbm(mod, newdata = roi,
                                  n.trees = mod$gbm.call$best.trees,
                                  type = "response")
}

roi$pred_prob <- rowMeans(pred_matrix)

# response (raw) residuals
roi$resid_response <- roi$presence - roi$pred_prob

# deviance residuals (better for bernoulli — more symmetric, easier to test)
roi$resid_deviance <- sign(roi$presence - roi$pred_prob) *
  sqrt(-2 * (roi$presence * log(roi$pred_prob) +
               (1 - roi$presence) * log(1 - roi$pred_prob)))

boxplot(resid_deviance ~ island, data = roi,
        ylab = "Deviance residual", xlab = "Island",
        main = "Residuals by island (no island predictor)")
abline(h = 0, lty = 2, col = "red")

kruskal.test(resid_deviance ~ island, data = taape)
summary(aov(resid_deviance ~ island, data = taape))

#morans i
library(ape)

# inverse-distance weights
coords <- cbind(taape$lon, taape$lat)
dists <- as.matrix(dist(coords))
dists.inv <- 1 / dists
diag(dists.inv) <- 0
dists.inv[is.infinite(dists.inv)] <- 0

Moran.I(taape$resid_deviance, dists.inv)


library(ggplot2)
ggplot(roi, aes(x = lon, y = lat, color = resid_deviance)) +
  geom_point(size = 2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  facet_wrap(~island, scales = "free") +
  theme_classic()

