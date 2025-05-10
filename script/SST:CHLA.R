rm(list = ls())
load("/Users/mayaotsu/Downloads/eds_time (1).Rdata")

#SST full
ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = q95_Sea_Surface_Temperature_CRW_daily_01yr)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "q95 SST (Annual)", color = "SST (°C)")

# SST MHI
ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = q95_Sea_Surface_Temperature_CRW_daily_01yr)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Q 95 SST (Annual)", color = "SST (°C)") +
  coord_cartesian(xlim = c(199.5, 205), ylim = c(18.9, 22.3))

#q05
ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = q05_Sea_Surface_Temperature_CRW_daily_01yr)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Q 05 SST (Annual)", color = "SST (°C)")

ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = q05_Sea_Surface_Temperature_CRW_daily_01yr)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Q 05 SST (Annual)", color = "SST (°C)") +
  coord_cartesian(xlim = c(199.5, 205), ylim = c(18.9, 22.3))

#CHLA FUll
ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Mean Chla Concentration (Monthly)", color = "Concentration")

#CHla MHI
ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Mean Chla Concentration (Monthly)", color = "Concentration") +
  coord_cartesian(xlim = c(199.5, 205), ylim = c(18.9, 22.3))
