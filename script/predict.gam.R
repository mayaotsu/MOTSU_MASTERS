rm(list = ls())
library(MuMIn)
library(mgcv)   

#####################
#### TAAPE FULL #####
#####################
#plug in model avg
taape <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21")
taape <- taape[taape$species=="LUKA",]

dredge_taape_full <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/dredge/dredge_taape_full.rds")
avgmod.95p_taape_full <- model.avg(dredge_taape_full, cumsum(weight) <= 0.95, fit = TRUE)
confint(avgmod.95p_taape_full)

predict_taape_full <- predict(avgmod.95p_taape_full, newdata = taape, type = "link", se.fit = TRUE)

#adding predictions to df
taape$fit_link <- predict_taape_full$fit
taape$se_link <- predict_taape_full$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
taape$lower_link <- taape$fit_link - 1.96 * taape$se_link #lower bound of 95 CI (log odds scale)
taape$upper_link <- taape$fit_link + 1.96 * taape$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
taape$fit_prob <- plogis(taape$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
taape$lower_prob <- plogis(taape$lower_link) #95 CI lower bound back transformed from lower link
taape$upper_prob <- plogis(taape$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(taape, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predict/taape_full_predicted.rds")
rm(list = ls())

#####################
#### TAAPE MHI ######
#####################
taape_MHI <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21")
taape_MHI <- taape_MHI[taape_MHI$species == "LUKA" & taape_MHI$region == "MHI", ]

dredge_taape_mhi <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/dredge/dredge_taape_mhi.rds")
avgmod.95p_taape_mhi <- model.avg(dredge_taape_mhi, cumsum(weight) <= 0.95, fit = TRUE)
confint(avgmod.95p_taape_mhi)

predict_taape_MHI <- predict(avgmod.95p_taape_mhi, newdata = taape_MHI, type = "link", se.fit = TRUE)

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

saveRDS(taape_MHI, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predict/taape_MHI_predicted.rds")
rm(list = ls())

#####################
#### TOAU FULL #####
#####################
toau_full <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21")
toau_full <- toau_full[toau_full$species=="LUFU",]

dredge_toau_full <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/dredge/dredge_toau_full.rds")
avgmod.95p_toau_full <- model.avg(dredge_toau_full, cumsum(weight) <= 0.95, fit = TRUE)
confint(avgmod.95p_toau_full)

predict_toau_full <- predict(avgmod.95p_toau_full, newdata = toau_full, type = "link", se.fit = TRUE)

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

saveRDS(toau_full, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predict/toau_full_predicted.rds")
rm(list = ls())
#####################
#### TOAU MHI ######
#####################
toau_mhi <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
toau_mhi <- toau_mhi[toau_mhi$species == "LUFU" & toau_mhi$region == "MHI", ]

dredge_toau_mhi <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/dredge/dredge_toau_mhi.rds")
avgmod.95p_toau_mhi <- model.avg(dredge_toau_mhi, cumsum(weight) <= 0.95, fit = TRUE)
confint(avgmod.95p_toau_mhi)

predict_toau_mhi <- predict(avgmod.95p_toau_mhi, newdata = toau_mhi, type = "link", se.fit = TRUE)

#adding predictions to df
toau_mhi$fit_link <- predict_toau_mhi$fit
toau_mhi$se_link <- predict_toau_mhi$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
toau_mhi$lower_link <- toau_mhi$fit_link - 1.96 * toau_mhi$se_link #lower bound of 95 CI (log odds scale)
toau_mhi$upper_link <- toau_mhi$fit_link + 1.96 * toau_mhi$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
toau_mhi$fit_prob <- plogis(toau_mhi$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
toau_mhi$lower_prob <- plogis(toau_mhi$lower_link) #95 CI lower bound back transformed from lower link
toau_mhi$upper_prob <- plogis(toau_mhi$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(toau_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predict/toau_MHI_predicted.rds")
rm(list = ls())

#####################
#### ROI FULL #####
#####################
roi <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21")
roi <- roi[roi$species=="CEAR",]

dredge_roi_full <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/dredge/dredge_roi_full.rds")
avgmod.95p_roi_full <- model.avg(dredge_roi_full, cumsum(weight) <= 0.95, fit = TRUE)
confint(avgmod.95p_roi_full)

predict_roi_full <- predict(avgmod.95p_roi_full, newdata = roi, type = "link", se.fit = TRUE)

#adding predictions to df
roi$fit_link <- predict_roi_full$fit
roi$se_link <- predict_roi_full$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
roi$lower_link <- roi$fit_link - 1.96 * roi$se_link #lower bound of 95 CI (log odds scale)
roi$upper_link <- roi$fit_link + 1.96 * roi$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
roi$fit_prob <- plogis(roi$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
roi$lower_prob <- plogis(roi$lower_link) #95 CI lower bound back transformed from lower link
roi$upper_prob <- plogis(roi$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(roi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predict/roi_full_predicted.rds")
rm(list = ls())

#####################
#### ROI MHI ######
#####################
roi_mhi <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
roi_mhi <- roi_mhi[roi_mhi$species == "CEAR" & roi_mhi$region == "MHI", ]

dredge_roi_mhi <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/dredge/dredge_roi_mhi.rds")
avgmod.95p_roi_mhi <- model.avg(dredge_roi_mhi, cumsum(weight) <= 0.95, fit = TRUE)
confint(avgmod.95p_roi_mhi)

predict_roi_mhi <- predict(avgmod.95p_roi_mhi, newdata = roi_mhi, type = "link", se.fit = TRUE)

#adding predictions to df
roi_mhi$fit_link <- predict_roi_mhi$fit
roi_mhi$se_link <- predict_roi_mhi$se.fit #se of predicted log-odds (fit_link), measuring uncertainty around the prediction on the link scale

#conf intervals
roi_mhi$lower_link <- roi_mhi$fit_link - 1.96 * roi_mhi$se_link #lower bound of 95 CI (log odds scale)
roi_mhi$upper_link <- roi_mhi$fit_link + 1.96 * roi_mhi$se_link #upper bound of 95 CI (log odds scale)

# Back-transform to probability scale
roi_mhi$fit_prob <- plogis(roi_mhi$fit_link) #The predicted value back-transformed to the probability scale, using the logistic function: the predicted probability of presence (between 0 and 1).
roi_mhi$lower_prob <- plogis(roi_mhi$lower_link) #95 CI lower bound back transformed from lower link
roi_mhi$upper_prob <- plogis(roi_mhi$upper_link) #95 CI upper bound back transformed from upper link

saveRDS(roi_mhi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/gams/predict/roi_MHI_predicted.rds")
