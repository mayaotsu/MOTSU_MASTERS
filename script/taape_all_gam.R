#start with specified knots
rm(list=ls())
library(mgcv)
taape <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited") 
taape <- taape[taape$species=="LUKA",]
taape$island <- as.factor(taape$island)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
taape[is.nan(taape)] <- NA


# not including com_net because all NA for NWHI
taape_full_Gam <-gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
               +s(mean_1mo_sst_CRW, k=4)+s(q05_1yr_sst_CRW, k=4)+s(q95_1yr_sst_CRW, k=4)+
              +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_Boat_Spear_hr.tif, k=4)+
              +s(MHI_Shore_Spear_hr.tif, k=4)+s(coral_cover, k=4),
               data = taape, family = binomial)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/taape_full_gam.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
plot(taape_full_Gam)
dev.off()

rm(df)
df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_CEAR_JUSTMHI") 
taape_MHI <- df[df$species=="LUKA",]
taape_MHI$island <- as.factor(taape_MHI$island)
#taape_MHI <- na.omit(taape_MHI)
taape_MHI_Gam<-gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
               +s(mean_1mo_sst_CRW, k=4)+s(q05_1yr_sst_CRW, k=4)+s(q95_1yr_sst_CRW, k=4)+
                 +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_Boat_Spear_hr.tif, k=4)+
                 +s(MHI_Shore_Spear_hr.tif, k=4)+s(coral_cover, k=4)+s(com_net, k=4),
               data = taape_MHI, family = binomial)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/taape_MHI_gam.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
plot(taape_MHI_Gam)
gam.check(taape_MHI_Gam)

#TRY TOAU
df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited") 
toau_full <- df[df$species=="LUFU",]
toau_full$island <- as.factor(toau_full$island)

toau_full <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited") 
toau_full <- toau[toau$species=="LUFU",]
toau$island <- as.factor(toau$island)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
toau[is.nan(toau)] <- NA

toau_full_Gam <-gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
                   +s(mean_1mo_sst_CRW, k=4)+s(q05_1yr_sst_CRW, k=4)+s(q95_1yr_sst_CRW, k=4)+
                     +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_Boat_Spear_hr.tif, k=4)+
                     +s(MHI_Shore_Spear_hr.tif, k=4)+s(coral_cover, k=4)+s(com_net, k=4),
                   data =toau_full, family = binomial)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/toau_full_gam.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
plot(toau_full_Gam)
dev.off()


#toau mhi
df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_CEAR_JUSTMHI") 
toau_mhi <- df[df$species=="LUFU",]
toau_mhi$island <- as.factor(toau_mhi$island)

toau_mhi_Gam <- gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
                   +s(mean_1mo_sst_CRW, k=4)+s(q05_1yr_sst_CRW, k=4)+s(q95_1yr_sst_CRW, k=4)+
                     +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_Boat_Spear_hr.tif, k=4)+
                     +s(MHI_Shore_Spear_hr.tif, k=4)+s(coral_cover, k=4)+s(com_net, k=4),
                   data =toau_mhi, family = binomial)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/toau_MHI_gam.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
plot(toau_mhi_Gam)
dev.off()

#TRY ROI
df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited") 
roi_full <- df[df$species=="CEAR",]
roi_full$island <- as.factor(roi_full$island)
roi_full_Gam<-gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
                   +s(mean_1mo_sst_CRW, k=4)+s(q05_1yr_sst_CRW, k=4)+s(q95_1yr_sst_CRW, k=4)+
                     +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_Boat_Spear_hr.tif, k=4)+
                     +s(MHI_Shore_Spear_hr.tif, k=4)+s(coral_cover, k=4)+s(com_net, k=4),
                   data = roi_full, family = binomial)
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/roi_full_gam.png", res = 300, height = 10, width = 10, units = "in")
par(mfrow=c(3,5))
plot(roi_full_Gam)
dev.off()

Sword_Gam<-gam(Sword_PA~s(SOI)+Years+s(DOY)+bait+s(Moon_Phase)+s(Lite_Hk)+s(Area_Anomaly), 
               data = SSLL_Catch_Jan_Feb_Hab_Comp, family = binomial)


#FIXED MODEL SELECTION
#TRY TOAU
df<-readRDS("/Users/Kisei.Tanaka/Desktop/spc_edited") 
toau_full <- df[df$species=="LUFU",]
toau_full$island <- as.factor(toau_full$island)
toau_full_Gam <-gam(presence~#s(depth, k=4)+
                      # year+s(rugosity, k=4)+
                      island+
                      s(mean_1mo_chla_ESA, k=4)+s(mean_1mo_sst_CRW, k=4)+
                      s(q05_1yr_sst_CRW, k=4)+
                      s(q95_1yr_sst_CRW, k=4)+
                      +s(otp_nearshore_sediment, k=4)+
                      # s(otp_all_effluent, k=4)+
                      # s(MHI_Boat_Spear_hr.tif, k=4)+
                      +s(MHI_Shore_Spear_hr.tif, k=4)+
                      s(coral_cover, k=4),
                    # s(com_net, k=4),
                    data =toau_full, family = binomial)
summary(toau_full_Gam)
plot(toau_full_Gam, pages = 1)

df<-readRDS("/Users/Kisei.Tanaka/Desktop/spc_edited_CEAR_JUSTMHI") 
toau_mhi <- df[df$species=="LUFU",]
toau_mhi$island <- as.factor(toau_mhi$island)

toau_mhi_Gam <- gam(presence~s(depth, k=4)+
                      # year+s(rugosity, k=4)+
                      island+s(mean_1mo_chla_ESA, k=4)+
                      s(mean_1mo_sst_CRW, k=4)+
                      s(q05_1yr_sst_CRW, k=4)+
                      s(q95_1yr_sst_CRW, k=4)+
                      +s(otp_nearshore_sediment, k=4)+
                      # s(otp_all_effluent, k=4)+
                      # s(MHI_Boat_Spear_hr.tif, k=4)+
                      +s(MHI_Shore_Spear_hr.tif, k=4)+
                      s(coral_cover, k=4),
                    # s(com_net, k=4),
                    data =toau_mhi, family = binomial)

summary(toau_mhi_Gam)
plot(toau_mhi_Gam, pages = 1)
