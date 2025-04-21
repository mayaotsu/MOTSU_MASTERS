rm(list=ls())
library(mgcv)
library(MuMIn)
taape <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full") 
taape <- taape[taape$species=="LUKA",]
colSums(is.na(taape))

########  dredge taape full #########
taape_full_Gam <-gam(presence~s(depth)+year+s(rugosity, k =6)
                   +island+s(mean_1mo_chla_ESA, k =6)
                   +s(q05_1yr_sst_CRW, k=6)
                   +s(otp_nearshore_sediment, k=6)
                   +s(otp_all_effluent, k=6)
                   +s(MHI_spear, k=6)
                   +s(coral_cover, k=6)
                   +s(com_net, k=6),
                     data = taape, family = binomial)

#dredge function
options(na.action = "na.fail")
dredge_taape_full <- dredge(taape_full_Gam, trace = 5)
saveRDS(dredge_taape_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_full.rds")

model.sel(dredge_taape_full)[1:10] #best 10 models
sw(dredge_taape_full) #sum of weights across all models for each predictor
best_model_1 <- get.models(dredge_taape_full, 1)[[1]]
summary(best_model_1)


library(gratia)
#png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/taape_full_draw.png", res = 300, height = 10, width = 10, units = "in")
#draw(best_model_1)

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/plot/taape_full_plot.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
plot(best_model_1, pages = 1)
dev.off()

########  dredge taape mhi #########
taape_MHI<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi") 
taape_MHI <- taape_MHI[taape_MHI$species=="LUKA",]
taape_MHI <- taape_MHI[taape_MHI$region=="MHI",]
taape_MHI_Gam <-gam(presence~s(depth)+year+s(rugosity, k =6)+island
                    +s(mean_1mo_chla_ESA, k =6)
                    +s(q05_1yr_sst_CRW, k=6)
                    +s(otp_nearshore_sediment, k=6)
                    +s(otp_all_effluent, k=6)
                    +s(MHI_spear, k=6)+
                    +s(coral_cover, k=6)
                    +s(com_net, k=6),
                    data = taape_MHI, family = binomial)

options(na.action = "na.fail")
dredge_taape_mhi <- dredge(taape_MHI_Gam, trace = 5)
head(dredge_taape_mhi)
saveRDS(dredge_taape_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_taape_mhi.rds")

#taape_mhi_dredge_2 = readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/taape_mhi_dredge_2.rds")
model.sel(dredge_taape_mhi)[1:10]
sw(dredge_taape_mhi)
best_model_2 <- get.models(dredge_taape_mhi, 1)[[1]]
summary(best_model_2)

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/taape_mhi_draw.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
draw(best_model_2, parametric = TRUE)
dev.off()

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/plot/taape_mhi_plot.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
plot(best_model_2, pages = 1)
dev.off()

##### toau full dredge model #####
toau_full <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full") 
toau_full <- toau_full[toau_full$species=="LUFU",]

toau_full_Gam <-gam(presence~s(depth)+year+s(rugosity, k =6)
                        +island+s(mean_1mo_chla_ESA, k =6)
                        +s(q05_1yr_sst_CRW, k=6)
                        +s(otp_nearshore_sediment, k=6)
                        +s(otp_all_effluent, k=6)
                        +s(MHI_spear, k=6)
                        +s(coral_cover, k=6)
                        +s(com_net, k=6),
                        data = toau_full, family = binomial)
dredge_toau_full <- dredge(toau_full_Gam, trace = 5)
dredge_toau_full
saveRDS(dredge_toau_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_full.rds")

model.sel(dredge_toau_full)[1:10]
sw(dredge_toau_full)
best_model_3 <- get.models(dredge_toau_full, 1)[[1]]

draw(best_model_3, parametric = TRUE)
#png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/toau_full_draw.png", res = 300, height = 10, width = 10, units = "in")

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/plot/toau_full_plot.png", res = 300, height = 10, width = 10, units = "in")
plot(best_model_3, pages =1)
dev.off()

##### toau mhi dredge model #####
toau_mhi <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi") 
toau_mhi <- toau_mhi[toau_mhi$species=="LUFU",]
toau_mhi <- toau_mhi[toau_mhi$region=="MHI",]
toau_mhi$island <- as.factor(toau_mhi$island)

toau_mhi_Gam <- gam(presence~s(depth)+year+s(rugosity, k =6)
                    +island+s(mean_1mo_chla_ESA, k =6)
                    +s(q05_1yr_sst_CRW, k=6)
                    +s(otp_nearshore_sediment, k=6)
                    +s(otp_all_effluent, k=6)
                    +s(MHI_spear, k=6)
                    +s(coral_cover, k=6)
                    +s(com_net, k=6),
                    data = toau_mhi, family = binomial)

options(na.action = "na.fail")
dredge_toau_mhi <- dredge(toau_mhi_Gam, trace = 5)
dredge_toau_mhi
saveRDS(dredge_toau_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_mhi.rds")

model.sel(dredge_toau_mhi)[1:10]
sw(dredge_toau_mhi)
best_model_4 <- get.models(dredge_toau_mhi, 1)[[1]]
draw(best_model_4, parametric = TRUE)

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/toau_mhi_draw.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,3))
draw(best_model_4, parametric = TRUE)
dev.off()

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/plot/toau_mhi_plot.png", res = 300, height = 10, width = 10, units = "in")
plot(best_model_4)
dev.off()

### ROI FULL #########
roi <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full") 
roi <- roi[roi$species=="CEAR",]
colSums(is.na(roi))

roi_full_Gam <-gam(presence~s(depth)+year+s(rugosity, k =6)
                     +island+s(mean_1mo_chla_ESA, k =6)
                     +s(q05_1yr_sst_CRW, k=6)
                     +s(otp_nearshore_sediment, k=6)
                     +s(otp_all_effluent, k=6)
                     +s(MHI_spear, k=6)
                     +s(coral_cover, k=6)
                     +s(com_net, k=6),
                     data = roi, family = binomial)

options(na.action = "na.fail")
dredge_roi_full <- dredge(roi_full_Gam, trace = 5)
saveRDS(dredge_roi_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_full.rds")

model.sel(dredge_roi_full)[1:10] #best 10 models
sw(dredge_roi_full) #sum of weights across all models for each predictor
best_model_5 <- get.models(dredge_roi_full, 1)[[1]]
summary(best_model_5)

#png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/roi_full_draw.png", res = 300, height = 10, width = 10, units = "in")
draw(best_model_5)
dev.off()

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/plot/roi_full_plot.png", res = 300, height = 10, width = 10, units = "in")
plot(best_model_5, pages = 1)
dev.off()

### dredge roi mhi ######
roi_MHI<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi") 
roi_MHI <- roi_MHI[roi_MHI$species=="CEAR",]
roi_MHI <- roi_MHI[roi_MHI$region=="MHI",]
roi_MHI_Gam <-gam(presence~s(depth)+year+s(rugosity, k =6)+island
                    +s(mean_1mo_chla_ESA, k =6)
                    +s(q05_1yr_sst_CRW, k=6)
                    +s(otp_nearshore_sediment, k=6)
                    +s(otp_all_effluent, k=6)
                    +s(MHI_spear, k=6)+
                      +s(coral_cover, k=6)
                    +s(com_net, k=6),
                    data = roi_MHI, family = binomial)

options(na.action = "na.fail")
dredge_roi_mhi <- dredge(roi_MHI_Gam, trace = 5)
head(dredge_roi_mhi)
saveRDS(dredge_roi_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_roi_mhi.rds")

model.sel(dredge_roi_mhi)[1:10]
sw(dredge_roi_mhi)
best_model_6 <- get.models(dredge_roi_mhi, 1)[[1]]
summary(best_model_6)

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/roi_mhi_draw.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
draw(best_model_6, parametric = TRUE)
dev.off()

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/plot/roi_mhi_plot.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
plot(best_model_6, pages = 1)
dev.off()
