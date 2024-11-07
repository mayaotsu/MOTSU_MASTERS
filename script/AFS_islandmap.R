library(tidyr)
library(dplyr)
library(colorRamps)
library(patchwork)

select = dplyr::select
# load survey data, adjust columns, create id
df = readRDS("/Users/mayaotsu/Downloads/NCRMP_fish_site_20102019.rds")

df$ISLAND = gsub(" ", "_", df$ISLAND)
df$DATE_ = as.character(df$DATE_)
df$DATE_ = substr(df$DATE_, 1, 10)

colnames(df)[14] = "LAT"
colnames(df)[15] = "LON"

colnames(df)[7:9] = c("Year", "Month", "Day")
df$Year = substr(df$DATE_, 1, 4)
df$Month = substr(df$DATE_, 6, 7)
df$Day = substr(df$DATE_, 9, 10)

df$DATE = paste0(df$Month, "-", df$Day, "-", df$Year)
df$DATE_R = lubridate::mdy(df$DATE)
df$id = paste(df$LON, df$LAT, df$DATE_R, df$ISLAND, sep = "-")

islands = c("Kauai", #1
            "Lehua", #2
            "Niihau", #3
            "Kaula", #4
            "Oahu", #5
            "Molokai", #6
            "Maui", #7
            "Lanai", #8
            "Molokini", #9
            "Kahoolawe", #10
            "Hawaii")#[7:11]

# filter by islands, region
# choose abundance as response variable
# filter by Uku (APVI)


LUKA = df %>%
  # subset(ISLAND %in% islands) %>%
  subset(REGION %in% c("MHI", "NWHI")) %>%
  subset(Response == "Abund_m2") %>%
  select(id, names(df[,1:18]), names(select(df, contains("LUKA"))))

LUFU = df %>%
  # subset(ISLAND %in% islands) %>%
  subset(REGION %in% c("MHI", "NWHI")) %>%
  subset(Response == "Abund_m2") %>%
  select(id, names(df[,1:18]), names(select(df, contains("LUFU"))))

# look at size distributions
islands = unique(df$ISLAND)

big_size_bins = NULL

for (i in 1:length(islands)) {
  
  df_i = df %>% subset(ISLAND == islands[i])
  
  size_bins = df_i[,c(21:dim(df_i)[2])] %>%
    gather() %>%
    mutate(key = gsub("LUFU_", "", key)) %>%
    group_by(key) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(freq = value/sum(value))
  
  size_bins$island = islands[i]
  
  big_size_bins = rbind(big_size_bins, size_bins)
  
}

big_size_bins$key <- factor(big_size_bins$key, levels = c("[0,10]",
                                                          "(10,20]",
                                                          "(20,30]",
                                                          "(30,40]",
                                                          "(40,50]",
                                                          "(50,60]",
                                                          "(60,70]",
                                                          "(70,80]",
                                                          "(80,90]",
                                                          "(90,100]",
                                                          "(100,Inf]"))
(f1a = big_size_bins %>%
    ggplot(aes(x = key, y = freq, fill = island)) +
    geom_bar(stat = "identity", show.legend = T) +
    xlab("Size_bins (cm)") + ylab("Proportion (%)") +
    theme_classic() +
    scale_fill_manual("", values = matlab.like(18)) +
    theme(legend.position = c(0, 1),
          legend.justification = c(-0.1, 0.9),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10)))
  #+ labs(tag = "(a)"))

rm(size_bins)

# for Uku:
# Total numerical density estimates (individuals per 100 m2) were obtained by dividing 
#fish counts in each survey by the survey area (353 m2 from two 15-m diameter survey cylinders) 
#and multiplying by 100. - Nadon et al. 2020

df = df %>%
  select(names(df[,1:20])) %>%
  mutate(DATE = as.numeric(as.POSIXct(df$DATE_)),
         YEAR = substr(DATE_, 1, 4)) %>%
  group_by(LON, LAT, DATE, YEAR, ISLAND) %>%
  summarise(response = sum(LUFU, na.rm = T)*100) %>%
  # subset(response < quantile(response, prob = 0.999)) %>%
  na.omit()

(f1b = df %>%
    group_by(YEAR, ISLAND) %>%
    summarise(sd = sd(response, na.rm = T),
              response = mean(response, na.rm = T)) %>%
    ggplot(aes(YEAR, response)) +
    geom_pointrange(
      aes(ymin = ifelse(response - sd < 0, 0, response - sd),
          ymax = response+sd, color = ISLAND),
      position = position_dodge(0.5)) +
    # scale_color_viridis_d("", end = 0.8) +
    scale_color_manual(values = matlab.like(18), "") +
    # ylab("Individuals per 100 sq.m") + xlab("") +
    labs(y = expression("log(Individuals per 100)" ~ m^2~""), x = "Year") +
    theme_classic() +
    theme(legend.position = c(1, 1),
          legend.justification = c(1,1),
          legend.direction = "horizontal")) 
 # + labs(tag = "(b)")

f1a + f1b

df %>%
  group_by(LON, LAT, DATE, YEAR, ISLAND) %>%
  ggplot(aes(response)) +
  geom_histogram(bins = 10)

scale_x_longitude <- function(xmin = -180, xmax = 180, step = 0.2, ...) {
  ewbrks <- seq(xmin,xmax,step)
  ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(x, "W"), ifelse(x > 0, paste(x, "E"),x))))
  return(scale_x_continuous("", breaks = ewbrks, labels = ewlbls, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin = -90, ymax = 90, step = 0.2, ...) {
  nsbrks <- seq(ymin,ymax,step)
  nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "S"), ifelse(x > 0, paste(x, "N"),x))))
  return(scale_y_continuous("", breaks = nsbrks, labels = nslbls, expand = c(0, 0), ...))
}

(f1c = LUKA %>%
    filter(response > 0) %>% 
   mutate(lon = round(LON, 1), #uncomment 145, 146 and 147, 148 to change between plots
    lat = round(LAT, 1)) %>%
   # mutate(lon = ceiling(LON*2) / 2,
         #  lat = ceiling(LAT*2) / 2) %>%
    group_by(lon, lat, ISLAND) %>%
    summarise(#n = mean(response, na.rm = T),
              n = log(mean(response, na.rm = T) + 1)) %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(size = n, fill = n, color = n), shape = 21, alpha = 0.7) +
    # coord_fixed() +
    # scale_fill_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    # scale_color_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    scale_color_gradientn(colours = matlab.like(100), guide = "legend") +
    scale_fill_gradientn(colours = matlab.like(100), guide = "legend") +
    theme_classic() +
    # facet_wrap(.~ISLAND, scales = "free", ncol = 4) +
    # scale_y_latitude() +
    # scale_x_longitude() +
    # ylab("Latitude (dec deg)") + xlab("Longitude (dec deg)") +
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = ""))) +
    guides(color = guide_legend(expression(log("Individuals per 100" ~ m^2~""))),
           fill = guide_legend(expression(log("Individuals per 100" ~ m^2~""))),
           size = guide_legend(expression(log("Individuals per 100" ~ m^2~"")))) +
    theme(legend.position = "bottom")) 
 # + labs(tag =("c") ))

(f1a + f1b) / f1c
f1a + f1c
f1b + f1c
#png("outputs/fig1.png", units = "in", height = 12, width = 15, res = 500)
(f1a + f1b) / f1c
#dev.off()

(f1cc = df %>%
    filter(response > 0) %>% 
    mutate(lon = round(LON, 1), #uncomment 145, 146 and 147, 148 to change between plots
           lat = round(LAT, 1)) %>%
    # mutate(lon = ceiling(LON*2) / 2,
    #  lat = ceiling(LAT*2) / 2) %>%
    group_by(lon, lat, ISLAND) %>%
    summarise(#n = mean(response, na.rm = T),
      n = log(mean(response, na.rm = T) + 1)) %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(size = n, fill = n, color = n), shape = 21, alpha = 0.7) +
    # coord_fixed() +
    # scale_fill_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    # scale_color_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    scale_color_gradientn(colours = matlab.like(100), guide = "legend") +
    scale_fill_gradientn(colours = matlab.like(100), guide = "legend") +
    theme_classic() +
    # facet_wrap(.~ISLAND, scales = "free", ncol = 4) +
    # scale_y_latitude() +
    # scale_x_longitude() +
    # ylab("Latitude (dec deg)") + xlab("Longitude (dec deg)") +
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = ""))) +
    guides(color = guide_legend(expression(log("Individuals per 100" ~ m^2~""))),
           fill = guide_legend(expression(log("Individuals per 100" ~ m^2~""))),
           size = guide_legend(expression(log("Individuals per 100" ~ m^2~"")))) +
    theme(legend.position = "bottom")) 
# + labs(tag =("c") ))





######## TESTER MAPS
library(colorRamps)
library(ggplot2)
library(dplyr)

unit = expression("Individuals (n) per 2.4" ~ km^2~"")

load("/Users/mayaotsu/Downloads/calibr_LUKA_abund.RData"); df1 = df
load("/Users/mayaotsu/Downloads/calibr_LUFU_abund.RData"); df2 = df

df = rbind(df1, df2)

max <- df %>%
  filter(density > 0, method == "nSPC_BLT_TOW") %>%
  mutate(longitude = round(longitude, 1),
         latitude = round(latitude, 1)) %>%
  group_by(longitude, latitude) %>%
  summarise(mean_density = mean(density), .groups = 'drop') %>%
  summarise(max_density = max(mean_density)) %>%
  round(0) %>%
  as.numeric()

df %>%
  filter(density > 0) %>%
  filter(method == "nSPC_BLT_TOW") %>%
  mutate(longitude = round(longitude, 1),
         latitude = round(latitude, 1)) %>%
  group_by(longitude, latitude, region, species) %>%
  summarise(density = mean(density)) %>%
  ggplot(aes(longitude, latitude)) +
  geom_polygon(data = fortify(maps::map("world2", plot = F, fill = T)), aes(x = ifelse(long < 0, long + 360, long), y = lat, group = group)) +
  geom_point(aes(size = density, fill = density), shape = 21, alpha = 0.7) +
  scale_fill_gradientn(colours = matlab.like(100), guide = "legend") +
  scale_color_gradientn(colours = matlab.like(100), guide = "legend") +
  scale_size_continuous(limits = c(0, max), breaks = seq(0, max, by = 10)) +
  guides(fill = guide_legend(expression("log(Density)" ~ m^2~"")), size = guide_legend(expression("log(Density)" ~ m^2~""))) +
  facet_grid(species ~ .) +
  coord_equal(xlim = range(df$longitude), ylim = range(df$latitude)) +
  labs(x = expression(paste("Longitude ", degree, "", sep = "")),
       y = expression(paste("Latitude ", degree, "", sep = ""))) +
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA))
