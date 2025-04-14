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
taape_full_Gam <-gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
                     +s(q05_1yr_sst_CRW, k=4)
                       +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_spear, k=4)+
                       +s(coral_cover, k=4),
                     data = taape, family = binomial)

#dredge function
options(na.action = "na.fail")
dredge_model <- dredge(taape_full_Gam, trace = 5)
dredge_model
saveRDS(dredge_model, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/taape_full_dredge.rds")

model.sel(dredge_model)[1:10]
sw(dredge_model)
best_model_1 <- get.models(dredge_model, 1)[[1]]
summary(best_model_1)
draw(best_model_1)
plot(best_model_1)
draw(taape_full_Gam)

df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_cumulative_JUSTMHI") 
taape_MHI <- df[df$species=="LUKA",]
taape_MHI$island <- as.factor(taape_MHI$island)
taape_MHI_Gam <-gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
                    +s(q05_1yr_sst_CRW, k=4)
                    +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_spear, k=4)+
                      +s(coral_cover, k=4),
                    data = taape_MHI, family = binomial)
dredge_mhi <- dredge(taape_full_Gam, trace = 5)
head(dredge_mhi)
saveRDS(dredge_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/taape_mhi_dredge.rds")

model.sel(dredge_mhi)[1:10]
sw(dredge_mhi)
best_model_2 <- get.models(dredge_mhi, 1)[[1]]
summary(best_model_2)
library(effects)
plot(allEffects(best_model_2))

toau <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_cumulative") 
toau <- toau[toau$species=="LUKA",]

toau_full_Gam <-gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
                    +s(q05_1yr_sst_CRW, k=4)+s(otp_nearshore_sediment, k=4)+
                      +s(otp_all_effluent, k=4)+s(MHI_spear, k=4)+
                      +s(coral_cover, k=4),
                    data = toau, family = binomial)
dredge_toau_full <- dredge(toau_full_Gam, trace = 5)
dredge_toau_full
saveRDS(dredge_toau_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/toau_full_dredge.rds")

model.sel(dredge_toau_full)[1:10]
sw(dredge_toau_full)

df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_cumulative_JUSTMHI") 
toau_mhi <- df[df$species=="LUFU",]
toau_mhi$island <- as.factor(toau_mhi$island)

toau_mhi_Gam <- gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
                    +s(q05_1yr_sst_CRW, k=4)+
                      +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_spear, k=4)+
                      +s(coral_cover, k=4),
                    data = toau_mhi, family = binomial)

dredge_toau_mhi <- dredge(toau_mhi_Gam, trace = 5)
dredge_toau_mhi
saveRDS(dredge_toau_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/toau_mhi_dredge.rds")

model.sel(dredge_toau_mhi)[1:10]
sw(dredge_toau_mhi)
