rm(list=ls())
library(mgcv)
library(MuMIn)
taape <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_cumulative") 
taape <- taape[taape$species=="LUKA",]

#taape$island <- as.factor(taape$island)
#is.nan.data.frame <- function(x)
#  do.call(cbind, lapply(x, is.nan))
#taape[is.nan(taape)] <- NA

#na.exclude(taape)
#na.omit(taape)
# not including com_net because all NA for NWHI
taape_full_Gam <-gam(presence~s(depth)+year+s(rugosity)+island+s(mean_1mo_chla_ESA)
                   +s(q05_1yr_sst_CRW)
                   +s(otp_nearshore_sediment)+s(otp_all_effluent)+s(MHI_spear)+
                   +s(coral_cover),
                     data = taape, family = binomial)

#dredge function
options(na.action = "na.fail")
dredge_model <- dredge(taape_full_Gam, trace = 5)
dredge_model
saveRDS(dredge_model, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/taape_full_dredge_2.rds")

model.sel(dredge_model)[1:10]
sw(dredge_model)
best_model_1 <- get.models(dredge_model, 1)[[1]]
summary(best_model_1)
library(gratia)
draw(best_model_1)
dev.off()
plot(best_model_1)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/taape_full_draw.png", res = 300, height = 10, width = 10, units = "in")
draw(best_model_1)
dev.off()

df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_cumulative_JUSTMHI") 
taape_MHI <- df[df$species=="LUKA",]
taape_MHI$island <- as.factor(taape_MHI$island)
taape_MHI_Gam <-gam(presence~s(depth)+year+s(rugosity)+island+s(mean_1mo_chla_ESA)
                    +s(q05_1yr_sst_CRW)
                    +s(otp_nearshore_sediment)+s(otp_all_effluent)+s(MHI_spear)+
                      +s(coral_cover),
                    data = taape_MHI, family = binomial)
dredge_mhi <- dredge(taape_full_Gam, trace = 5)
head(dredge_mhi)
saveRDS(dredge_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/taape_mhi_dredge_2.rds")

model.sel(dredge_mhi)[1:10]
sw(dredge_mhi)
best_model_2 <- get.models(dredge_mhi, 1)[[1]]
summary(best_model_2)
library(effects)
plot(allEffects(best_model_2))
draw(best_model_2, parametric = TRUE)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/taape_mhi_draw.png", res = 300, height = 10, width = 10, units = "in")
draw(best_model_2)
dev.off()

toau <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_cumulative") 
toau <- toau[toau$species=="LUFU",]

toau_full_Gam <-gam(presence~s(depth)+year+s(rugosity)+island+s(mean_1mo_chla_ESA)
                    +s(q05_1yr_sst_CRW)+s(otp_nearshore_sediment)+
                      +s(otp_all_effluent)+s(MHI_spear)+
                      +s(coral_cover),
                    data = toau, family = binomial)
dredge_toau_full <- dredge(toau_full_Gam, trace = 5)
dredge_toau_full
saveRDS(dredge_toau_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/toau_full_dredge_2.rds")

model.sel(dredge_toau_full)[1:10]
sw(dredge_toau_full)
best_model_3 <- get.models(dredge_toau_full, 1)[[1]]
draw(best_model_3, parametric = TRUE)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/toau_full_draw.png", res = 300, height = 10, width = 10, units = "in")
draw(best_model_3)
dev.off()

df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_cumulative_JUSTMHI") 
toau_mhi <- df[df$species=="LUFU",]
toau_mhi$island <- as.factor(toau_mhi$island)

toau_mhi_Gam <- gam(presence~s(depth)+year+s(rugosity)+island+s(mean_1mo_chla_ESA)
                    +s(q05_1yr_sst_CRW)+
                      +s(otp_nearshore_sediment)+s(otp_all_effluent)+s(MHI_spear)+
                      +s(coral_cover),
                    data = toau_mhi, family = binomial)

options(na.action = "na.fail")
dredge_toau_mhi <- dredge(toau_mhi_Gam, trace = 5)
dredge_toau_mhi
saveRDS(dredge_toau_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/toau_mhi_dredge_2.rds")

model.sel(dredge_toau_mhi)[1:10]
sw(dredge_toau_mhi)
best_model_4 <- get.models(dredge_toau_mhi, 1)[[1]]
draw(best_model_4, parametric = TRUE)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/draw/toau_mhi_draw.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,3))
draw(best_model_4)
dev.off()
model.sel(dredge_toau_mhi)[1:10]
sw(dredge_toau_mhi)
