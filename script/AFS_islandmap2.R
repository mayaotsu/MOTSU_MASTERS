# load survey data, adjust columns, create id
rm(list = ls()) 
df = readRDS("/Users/mayaotsu/Downloads/NCRMP_fish_site_20102019.rds")
library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(colorRamps)
library(patchwork)
library(gridExtra)
select = dplyr::select

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

df1 = df %>%
  # subset(ISLAND %in% islands) %>%
  subset(REGION %in% c("MHI", "NWHI")) %>%
  subset(Response == "Abund_m2") %>%
  select(id, names(df[,1:18]), names(select(df, contains("LUFU"))))

df1 = df1 %>%
  select(names(df1[,1:20])) %>%
  mutate(DATE = as.numeric(as.POSIXct(df1$DATE_)),
         YEAR = substr(DATE_, 1, 4)) %>%
  group_by(LON, LAT, DATE, YEAR, ISLAND) %>%
  summarise(response = sum(LUFU, na.rm = T)*100) %>%
  # subset(response < quantile(response, prob = 0.999)) %>%
  na.omit()


df2 = df %>%
  # subset(ISLAND %in% islands) %>%
  subset(REGION %in% c("MHI", "NWHI")) %>%
  subset(Response == "Abund_m2") %>%
  select(id, names(df[,1:18]), names(select(df, contains("LUKA"))))

df2 = df2 %>%
  select(names(df2[,1:20])) %>%
  mutate(DATE = as.numeric(as.POSIXct(df2$DATE_)),
         YEAR = substr(DATE_, 1, 4)) %>%
  group_by(LON, LAT, DATE, YEAR, ISLAND) %>%
  summarise(response = sum(LUKA, na.rm = T)*100) %>%
  # subset(response < quantile(response, prob = 0.999)) %>%
  na.omit()

df3 = df %>%
  # subset(ISLAND %in% islands) %>%
  subset(REGION %in% c("MHI", "NWHI")) %>%
  subset(Response == "Abund_m2") %>%
  select(id, names(df[,1:18]), names(select(df, contains("CEAR"))))

df3 = df3 %>%
  select(names(df3[,1:20])) %>%
  mutate(DATE = as.numeric(as.POSIXct(df3$DATE_)),
         YEAR = substr(DATE_, 1, 4)) %>%
  group_by(LON, LAT, DATE, YEAR, ISLAND) %>%
  summarise(response = sum(CEAR, na.rm = T)*100) %>%
  # subset(response < quantile(response, prob = 0.999)) %>%
  na.omit()

df1$sp = "LUFU"
df2$sp = "LUKA"
df3$sp = "CEAR"
df = rbind(df1, df2, df3)

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

(fa = df %>%
    filter(sp == "LUKA") %>%
    filter(response > 0) %>%
    mutate(lon = round(LON, 1),
           lat = round(LAT, 1)) %>%
    group_by(lon, lat, ISLAND, sp) %>%
    summarise(n = mean(response, na.rm = T)) %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(size = n, fill = n, color = n), shape = 21, alpha = 0.6) +  
    coord_cartesian(xlim = range(df$LON), ylim = range(df$LAT)) +  # Set axis limits

   # coord_fixed() +
   #scale_fill_viridis_c(guide = "legend", begin = 0, end = 0.8) +
   #scale_color_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    scale_color_gradientn(colours = matlab.like(100), guide = "legend") +
    scale_fill_gradientn(colours = matlab.like(100), guide = "legend")
   # + annotation_map(map_data("world")) 
  + 
    # theme_classic() +
     facet_wrap(.~sp, scales = "free", ncol = 4) +
    # scale_y_latitude() +
    # scale_x_longitude() +
    # ylab("Latitude (dec deg)") + xlab("Longitude (dec deg)") +
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = ""))) +
    guides(color = guide_legend(expression("Individuals per 100" ~ m^2~"")),
           fill = guide_legend(expression("Individuals per 100" ~ m^2~"")),
           size = guide_legend(expression("Individuals per 100" ~ m^2~""))) +
    scale_size_continuous(range = c(1, 6)) +  # Adjust bubble sizes here
    theme(legend.position = c(0.15, 0.3),
          legend.background = element_blank()) +
    labs(tag = "(a)"))

(fb = df %>%
    filter(sp == "LUFU") %>%
    filter(response > 0) %>%
    mutate(lon = round(LON, 1),
           lat = round(LAT, 1)) %>%
    group_by(lon, lat, ISLAND, sp) %>%
    summarise(n = (mean(response+1)), na.rm = T) %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(size = n, fill = n, color = n), shape = 21, alpha = 0.7) +
     #coord_fixed() +
   # scale_fill_viridis_c(guide = "legend", begin = 0, end = 0.8) +
   # scale_color_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    scale_color_gradientn(colours = matlab.like(100), guide = "legend") +
    scale_fill_gradientn(colours = matlab.like(100), guide = "legend") +
    #annotation_map(map_data("world")) + 
    # theme_light() +
    facet_wrap(.~sp, scales = "free", ncol = 4) +
    coord_cartesian(xlim = range(df$LON), ylim = range(df$LAT)) +  
    # scale_y_latitude() +
    # scale_x_longitude() +
    # ylab("Latitude (dec deg)") + xlab("Longitude (dec deg)") +
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = ""))) +
    guides(color = guide_legend(expression("Individuals per 100" ~ m^2~"")),
           fill = guide_legend(expression("Individuals per 100" ~ m^2~"")),
           size = guide_legend(expression("Individuals per 100" ~ m^2~""))) +
    theme(legend.position = c(0.15, 0.3),
          legend.background = element_blank()) +
    labs(tag = "(b)"))

(fc = df %>%
    filter(sp == "CEAR") %>%
    filter(response > 0) %>%
    mutate(lon = round(LON, 1),
           lat = round(LAT, 1)) %>%
    group_by(lon, lat, ISLAND, sp) %>%
    summarise(n = (mean(response+1)), na.rm = T) %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(size = n, fill = n, color = n), shape = 21, alpha = 0.7) +
    #coord_fixed() +
    # scale_fill_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    # scale_color_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    scale_color_gradientn(colours = matlab.like(100), guide = "legend") +
    scale_fill_gradientn(colours = matlab.like(100), guide = "legend") +
    #annotation_map(map_data("world")) + 
    # theme_light() +
    facet_wrap(.~sp, scales = "free", ncol = 4) +
    coord_cartesian(xlim = range(df$LON), ylim = range(df$LAT)) +  
    # scale_y_latitude() +
    # scale_x_longitude() +
    # ylab("Latitude (dec deg)") + xlab("Longitude (dec deg)") +
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = ""))) +
    guides(color = guide_legend(expression("Individuals per 100" ~ m^2~"")),
           fill = guide_legend(expression("Individuals per 100" ~ m^2~"")),
           size = guide_legend(expression("Individuals per 100" ~ m^2~""))) +
    theme(legend.position = c(0.15, 0.3),
          legend.background = element_blank()) +
    labs(tag = "(c)"))

fa / fb
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/proposal_plot.png", units = "in", height = 8, width = 10, res = 500)
grid.arrange(fa, fb, ncol = 1)
dev.off()

hawaii_map <- map_data("state") %>% filter(region == "hawaii")


