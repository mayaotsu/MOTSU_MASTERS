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
draw(best_model_1)
dev.off()
plot(best_model_1)

########  dredge taape mhi #########
taape_MHI<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi") 
taape_MHI <- taape_MHI[taape_MHI$species=="LUKA",]
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

draw(best_model_2, parametric = TRUE)
#png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/taape_mhi_draw.png", res = 300, height = 10, width = 10, units = "in")
dev.off()
plot(best_model_2)

##### toau full dredge model #####
toau_full <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full") 
toau_full <- toau[toau$species=="LUFU",]

toau_full_Gam <-gam(presence~s(depth)+year+s(rugosity)+island+s(mean_1mo_chla_ESA)
                    +s(q05_1yr_sst_CRW)+s(otp_nearshore_sediment)+
                      +s(otp_all_effluent)+s(MHI_spear)+
                      +s(coral_cover),
                    data = toau, family = binomial)
dredge_toau_full <- dredge(toau_full_Gam, trace = 5)
dredge_toau_full
saveRDS(dredge_toau_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_full.rds")

model.sel(dredge_toau_full)[1:10]
sw(dredge_toau_full)
best_model_3 <- get.models(dredge_toau_full, 1)[[1]]

draw(best_model_3, parametric = TRUE)
#png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/toau_full_draw.png", res = 300, height = 10, width = 10, units = "in")
dev.off()
plot(best_model_3)

##### toau mhi dredge model #####
toau_mhi <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi") 
toau_mhi <- toau_mhi[toahu_mhi$species=="LUFU",]
toau_mhi$island <- as.factor(toau_mhi$island)

toau_mhi_Gam <- gam(presence~s(depth)+year+s(rugosity)+island+s(mean_1mo_chla_ESA)
                    +s(q05_1yr_sst_CRW)+
                      +s(otp_nearshore_sediment)+s(otp_all_effluent)+s(MHI_spear)+
                      +s(coral_cover),
                    data = toau_mhi, family = binomial)

options(na.action = "na.fail")
dredge_toau_mhi <- dredge(toau_mhi_Gam, trace = 5)
dredge_toau_mhi
saveRDS(dredge_toau_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/dredge_toau_mhi.rds")

model.sel(dredge_toau_mhi)[1:10]
sw(dredge_toau_mhi)
best_model_4 <- get.models(dredge_toau_mhi, 1)[[1]]
draw(best_model_4, parametric = TRUE)
#png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/toau_mhi_draw.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,3))
draw(best_model_4)
dev.off()


