#density
spc_reduced$species <- factor(spc_reduced$species, levels = c('LUFU', 'LUKA')) 
spc_reduced %>% 
  filter(density>0) %>% 
  mutate(lon=round(lon, 1),
     (lat=round(lat, 1))) %>% 
  group_by(lon, lat, island, region, species) %>% 
  summarise(density=mean(density)) %>% 
  ggplot(aes(lon, lat, size = density, fill = species)) + 
  geom_point(shape = 21, alpha = 1) +
  labs(title = "Density Distribution by Species",
       x = "Longitude",
       y = "Latitude") #+
  #facet_wrap(~island, scale = "free")
ggsave(last_plot(), file = "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/density_dist_species.png")


#presence
spc_reduced %>% 
  filter(density>0) %>% 
  mutate(lon=round(lon, 2),
         (lat=round(lat, 2))) %>% 
  group_by(lon, lat, region, species) %>% 
  summarise(density=mean(density)) %>% 
  ggplot(aes(lon, lat, fill = species)) + 
  geom_point(shape = 21) +
  facet_wrap(~region, scale = "free")

#depth profile
library(patchwork)
p1 = spc_reduced %>% 
  filter(species=="LUFU") %>% 
  ggplot(aes(depth, density, colour = species)) + 
  geom_smooth(method = "loess", span=1, show.legend = FALSE) +
  ggtitle("Toʻau") +
  labs(y = "density per 100 m^2", x = "depth (m)") +
  theme_classic(base_size = 15)

p2 = spc_reduced %>% 
  filter(species=="LUKA") %>% 
  ggplot(aes(depth, density)) + 
  geom_smooth(method = "loess", span=1, show.legend = FALSE) +
  ggtitle("Taʻape") +
  labs(y = "density per 100 m^2", x = "depth (m)") +
  theme_classic(base_size = 15)

p1+p2
ggsave(last_plot(), file = "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/depth_profile.png")


