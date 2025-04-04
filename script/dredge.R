rm(list=ls())
library(mgcv)
taape <-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited") 
taape <- taape[taape$species=="LUKA",]
taape$island <- as.factor(taape$island)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
taape[is.nan(taape)] <- NA

na.exclude(taape)
#na.omit(taape)
# not including com_net because all NA for NWHI
taape_full_Gam <-gam(presence~s(depth, k=4)+year+s(rugosity, k=4)+island+s(mean_1mo_chla_ESA, k=4)
                     +s(mean_1mo_sst_CRW, k=4)+s(q05_1yr_sst_CRW, k=4)+s(q95_1yr_sst_CRW, k=4)+
                       +s(otp_nearshore_sediment, k=4)+s(otp_all_effluent, k=4)+s(MHI_Boat_Spear_hr.tif, k=4)+
                       +s(MHI_Shore_Spear_hr.tif, k=4)+s(coral_cover, k=4),
                     data = taape, family = binomial)

#dredge function
options(na.action = "na.fail")
dredge_model <- dredge(taape_full_Gam, fixed = c("year", "island"), trace = 5)
