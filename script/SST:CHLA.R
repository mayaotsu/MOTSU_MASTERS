load("/Users/mayaotsu/Downloads/eds_time (1).Rdata")

ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = q95_Sea_Surface_Temperature_CRW_daily_01yr)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "q95 SST (CRW)", color = "SST (°C)")

#MHI
ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = q95_Sea_Surface_Temperature_CRW_daily_01yr)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "q95 SST (CRW)", color = "SST (°C)") +
  coord_cartesian(xlim = c(199.5, 205), ylim = c(18.9, 22.3))

ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = q05_Sea_Surface_Temperature_CRW_daily_01yr)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "q05 SST (CRW)", color = "SST (°C)")

ggplot(df, aes(x = lon, y = lat)) +
  geom_point(aes(color = mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "mean 1mo Chla", color = "SST (°C)")
