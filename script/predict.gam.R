rm(list = ls())
library(MuMIn)
library(mgcv)   

#####################
#### TAAPE FULL #####
#####################
taape_full_Gam <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/gams/taape_full_Gam.rds")

taape_full <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
taape_full <- taape_full[taape_full$species=="LUKA",]

predict_taape_full <- predict(taape_full_Gam, newdata = taape_full, type = "link", se.fit = TRUE)

#adding predictions to df
taape_full$fit_link <- predict_taape_full$fit
taape_full$se_link <- predict_taape_full$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
taape_full$lower_link <- taape_full$fit_link - 1.96 * taape_full$se_link #lower bound of 95 CI (log odds scale)
taape_full$upper_link <- taape_full$fit_link + 1.96 * taape_full$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
taape_full$fit_prob <- plogis(taape_full$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
taape_full$lower_prob <- plogis(taape_full$lower_link) #95 CI lower bound back transformed from lower link
taape_full$upper_prob <- plogis(taape_full$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(taape_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predicted gams/taape_full_predicted.rds")
rm(list = ls())

#####################
#### TAAPE MHI ######
#####################
taape_MHI_Gam <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/gams/taape_MHI_Gam.rds")

taape_MHI <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
taape_MHI <- taape_MHI[taape_MHI$species == "LUKA" & taape_MHI$region == "MHI", ]

predict_taape_MHI <- predict(taape_MHI_Gam, newdata = taape_MHI, type = "link", se.fit = TRUE)

#adding predictions to df
taape_MHI$fit_link <- predict_taape_MHI$fit
taape_MHI$se_link <- predict_taape_MHI$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
taape_MHI$lower_link <- taape_MHI$fit_link - 1.96 * taape_MHI$se_link #lower bound of 95 CI (log odds scale)
taape_MHI$upper_link <- taape_MHI$fit_link + 1.96 * taape_MHI$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
taape_MHI$fit_prob <- plogis(taape_MHI$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
taape_MHI$lower_prob <- plogis(taape_MHI$lower_link) #95 CI lower bound back transformed from lower link
taape_MHI$upper_prob <- plogis(taape_MHI$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(taape_MHI, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predicted gams/taape_MHI_predicted.rds")
rm(list = ls())

#####################
#### TOAU FULL #####
#####################
toau_full_Gam <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/gams/toau_full_Gam.rds")

toau_full <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
toau_full <- toau_full[toau_full$species=="LUFU",]

predict_toau_full <- predict(toau_full_Gam, newdata = toau_full, type = "link", se.fit = TRUE)

#adding predictions to df
toau_full$fit_link <- predict_toau_full$fit
toau_full$se_link <- predict_toau_full$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
toau_full$lower_link <- toau_full$fit_link - 1.96 * toau_full$se_link #lower bound of 95 CI (log odds scale)
toau_full$upper_link <- toau_full$fit_link + 1.96 * toau_full$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
toau_full$fit_prob <- plogis(toau_full$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
toau_full$lower_prob <- plogis(toau_full$lower_link) #95 CI lower bound back transformed from lower link
toau_full$upper_prob <- plogis(toau_full$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(toau_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predicted gams/toau_full_predicted.rds")
rm(list = ls())
#####################
#### TOAU MHI ######
#####################
toau_MHI_Gam <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/gams/toau_MHI_Gam.rds")

toau_MHI <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
toau_MHI <- toau_MHI[toau_MHI$species == "LUFU" & toau_MHI$region == "MHI", ]

predict_toau_MHI <- predict(toau_MHI_Gam, newdata = toau_MHI, type = "link", se.fit = TRUE)

#adding predictions to df
toau_MHI$fit_link <- predict_toau_MHI$fit
toau_MHI$se_link <- predict_toau_MHI$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
toau_MHI$lower_link <- toau_MHI$fit_link - 1.96 * toau_MHI$se_link #lower bound of 95 CI (log odds scale)
toau_MHI$upper_link <- toau_MHI$fit_link + 1.96 * toau_MHI$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
toau_MHI$fit_prob <- plogis(toau_MHI$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
toau_MHI$lower_prob <- plogis(toau_MHI$lower_link) #95 CI lower bound back transformed from lower link
toau_MHI$upper_prob <- plogis(toau_MHI$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(toau_MHI, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predicted gams/toau_MHI_predicted.rds")

#####################
#### ROI FULL #####
#####################
roi_full_Gam <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/gams/roi_full_Gam.rds")

roi_full <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
roi_full <- roi_full[roi_full$species=="CEAR",]

predict_roi_full <- predict(roi_full_Gam, newdata = roi_full, type = "link", se.fit = TRUE)

#adding predictions to df
roi_full$fit_link <- predict_roi_full$fit
roi_full$se_link <- predict_roi_full$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
roi_full$lower_link <- roi_full$fit_link - 1.96 * roi_full$se_link #lower bound of 95 CI (log odds scale)
roi_full$upper_link <- roi_full$fit_link + 1.96 * roi_full$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
roi_full$fit_prob <- plogis(roi_full$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
roi_full$lower_prob <- plogis(roi_full$lower_link) #95 CI lower bound back transformed from lower link
roi_full$upper_prob <- plogis(roi_full$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(roi_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predicted gams/roi_full_predicted.rds")
rm(list = ls())

#####################
#### ROI MHI ######
#####################
roi_MHI_Gam <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/gams/roi_MHI_Gam.rds")

roi_MHI <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
roi_MHI <- roi_MHI[roi_MHI$species == "CEAR" & roi_MHI$region == "MHI", ]

predict_roi_MHI <- predict(roi_MHI_Gam, newdata = roi_MHI, type = "link", se.fit = TRUE)

#adding predictions to df
roi_MHI$fit_link <- predict_roi_MHI$fit
roi_MHI$se_link <- predict_roi_MHI$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
roi_MHI$lower_link <- roi_MHI$fit_link - 1.96 * roi_MHI$se_link #lower bound of 95 CI (log odds scale)
roi_MHI$upper_link <- roi_MHI$fit_link + 1.96 * roi_MHI$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
roi_MHI$fit_prob <- plogis(roi_MHI$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
roi_MHI$lower_prob <- plogis(roi_MHI$lower_link) #95 CI lower bound back transformed from lower link
roi_MHI$upper_prob <- plogis(roi_MHI$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(roi_MHI, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predicted gams/roi_MHI_predicted.rds")
