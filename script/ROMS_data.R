getwd()
print(unique(df$year))

load("/Users/mayaotsu/Downloads/calibr_LUKA_abund.RData")
library(ncdf4)

#example 2019
roms2019 <- nc_open("4121aa99aca9d92ecad0f283a34e1b93.nc")
print(roms2019)

#raster(netcdfname, var=eastward_sea_water_velocity)
time <- ncvar_get(roms2019, "time") #in seconds
roms_date <- (as.POSIXct(time, format="%Y-%M-%D")) #ymd format

#FUNCTION FOR ROMS YMD
ymd_s <- function(nc_file){
  roms <- nc_open(nc_file)
  time <- ncvar_get(roms, "time")
  roms_date <- (as.POSIXct(time, format="%Y-%M-%D"))
  return(roms_date)
}

roms2009 <- ymd_s("~/Downloads/2009/0.25m/87b7f025cc519ab35306e073a240d859.nc")
roms2010 <- ymd_s("~/Downloads/2010/0.25m/6ba3cdeee6efcb72fd94bed101944f16.nc")
roms2011<- ymd_s("~/Downloads/2011/0.25m/eee98d9f06d435869ff1201dcd599dae.nc")
roms2012<- ymd_s("~/Downloads/2012/0.25m/24ab8d06ea99a3ac5686bcd9edb602d5.nc")
roms2013<- ymd_s("~/Downloads/2013/0.25m/2570e902459385289a7e27199fcf13fa.nc")
roms2014<- ymd_s("~/Downloads/2014/0.25m/5e0f9928e2623db25453d96d8af13471.nc")
roms2015<- ymd_s("~/Downloads/2015/0.25m/b0f2d4774a250ecef99577f999022c2c.nc")
roms2016<- ymd_s("~/Downloads/2016/0.25m/8f588958beafafd6f3d8988c9f605b2f.nc")
roms2017<- ymd_s("~/Downloads/2017/0.25m/f8f2d90808084c59002545e7d04cf352.nc")
roms2018<- ymd_s("~/Downloads/2018/0.25m/3bea881268b84cf8f6da8f23f85dc7c2.nc")
roms2019 <- ymd_s("~/Downloads/2019/0.25m/4121aa99aca9d92ecad0f283a34e1b93.nc")
roms2020 <- ymd_s("~/Downloads/2020/0.25m/240d7e59e4bebf9ed1fc48e8085cb5a1.nc")

#teseting with 2019
unique(roms2019==roms_date)

#trim roms data to daily avgs, take every 3 hourly section, sum absolute values of u and vs, 8 TKE vals, avg every 8 time steps
#pull all u and v vals for each day, and calculate TKE 1/2(u^2+v^2)
#for (1 in nrow(df) {}

#2019 example for u, v and tke
u2019<-stack("~/Downloads/2019/0.25m/4121aa99aca9d92ecad0f283a34e1b93.nc", varname="u")
plot(u2019[[1:6]])

v2019<-stack("~/Downloads/2019/0.25m/4121aa99aca9d92ecad0f283a34e1b93.nc", varname="v")
plot(v2019[[1:6]])

tke2019<-0.5*(u2019^2+v2019^2)
plot(tke2019[[1:6]])

#try uv function
uv_stack <- function(nc_file){
  u <- (stack(nc_file, varname ="u"))
  v <- (stack(nc_file, varname = "v"))
  tke <- 0.5*(u^2+v^2)
  my_list = list("u" = u, "v" = v, "tke" = tke)
  return(my_list) #return(tke) return(my_list)
}

uv_stack2009 <- uv_stack("~/Downloads/2009/0.25m/87b7f025cc519ab35306e073a240d859.nc")
uv_stack2010 <- uv_stack("~/Downloads/2010/0.25m/6ba3cdeee6efcb72fd94bed101944f16.nc")
uv_stack2011 <- uv_stack("~/Downloads/2011/0.25m/eee98d9f06d435869ff1201dcd599dae.nc")
uv_stack2012 <- uv_stack("~/Downloads/2012/0.25m/24ab8d06ea99a3ac5686bcd9edb602d5.nc")
uv_stack2013 <- uv_stack("~/Downloads/2013/0.25m/2570e902459385289a7e27199fcf13fa.nc")
uv_stack2014 <- uv_stack("~/Downloads/2014/0.25m/5e0f9928e2623db25453d96d8af13471.nc")
uv_stack2015 <- uv_stack("~/Downloads/2015/0.25m/b0f2d4774a250ecef99577f999022c2c.nc")
uv_stack2016 <- uv_stack("~/Downloads/2016/0.25m/8f588958beafafd6f3d8988c9f605b2f.nc")
uv_stack2017 <- uv_stack("~/Downloads/2017/0.25m/f8f2d90808084c59002545e7d04cf352.nc")
uv_stack2018 <- uv_stack("~/Downloads/2018/0.25m/3bea881268b84cf8f6da8f23f85dc7c2.nc")
uv_stack2019 <- uv_stack("~/Downloads/2019/0.25m/4121aa99aca9d92ecad0f283a34e1b93.nc")
uv_stack2020 <- uv_stack("~/Downloads/2020/0.25m/240d7e59e4bebf9ed1fc48e8085cb5a1.nc")
plot(uv_stack2009$tke[[1:6]])

names(uv_stack2009$tke) # what is this
length(names(uv_stack2009$tke)) # sanity check
length(roms2009) # sanity check
names(uv_stack2009$tke) <- roms2009 # run this to replace tke time layer
names(uv_stack2010$tke) <- roms2010 # run this to replace tke time layer
names(uv_stack2011$tke) <- roms2011 # run this to replace tke time layer


plot(uv_stack2009$tke[[1:6]]) # time is in HST
plot(uv_stack2009$u[[1:6]]) # time is still in GMT/UTC
# names(uv_stack2009$u) <- roms2009 # replacing uʻs time layer with roms2009 to put it on HST 

#append outputs into brick
#uv_append <- list(uv_stack2009, uv_stack2010, uv_stack2011, uv_stack2012, uv_stack2013, uv_stack2014, uv_stack2015, 
                 # uv_stack2016, uv_stack2017, uv_stack2018, uv_stack2019, uv_stack2020)
#uv_brick <- brick(uv_append)

# make a dataframe with column for date/time, lat, lon, u, v, tke
u_vec = values(u2019[[1]]) # first layer 
v_vec = values(v2019[[1]])
tke_vec = values(tke2019[[1]])
date_time_vec = rep(roms_date[1], length(u_vec))
#lat 
#lon 

test = data.frame(U = u_vec, V= v_vec, TKE = tke_vec, Date = date_time_vec)
#trim roms data to daily avgs, take every 3 hourly section, sum absolute values of u and vs, 8 TKE vals, avg every 8 time steps
#pull all u and v vals for each day, and calculate TKE 1/2(u^2+v^2)
#for (1 in nrow(df) {}

#pull 8 u and v vals each day 

#make tke rasters and df ymd coordinated
#loop thru every year then save a new raster brick tke one large superstack of tke


#for df data use from 2009 forward, skip 2018
#do this for every year you have data in df , turn into one brick (years in order)
indices <- substr(names(uv_stack2009$tke), 2, 11)
daily2009tke <- stackApply(uv_stack2009$tke, indices, fun=mean)
plot(daily2009tke[[1:3]])

rm(uv_stack2009)

#make list of uv stacks, run steps then fill empty list with daily avg products

year_vec <- c("uv_stack2009", "uv_stack2010", "uv_stack2011", "uv_stack2012", "uv_stack2013",
              "uv_stack2014", "uv_stack2015", "uv_stack2016", "uv_stack2017", "uv_stack2019")

tke_list <- list() #empty list for tke products

# names(uv_stack2009$tke) <- roms2009 # run this to replace tke time layer
roms_dates <- c("roms2009","roms2010","roms2011","roms2012","roms2013","roms2014","roms2015",
                "roms2016","roms2017","roms2019")
print(length(year_vec) == length(roms_dates)) #years for uv data same as df

for (i in 1:length(year_vec)){
  dates = get(roms_dates[i]) #getting contents from roms dates
  uv = get(year_vec[i]) #uv basically the same as uv_stack2009 for first layer
  names(uv$tke) = dates #rename layer 1 with date instead
  
  indices <- substr(names(uv$tke), 2, 11) #grabbing the dates instead of x2009.blahblah
  tke_list[i] <- stackApply(uv$tke, indices, fun=mean) #stackapply averages everything with samed date
  #plot(tke_list[[i]])
  print(i)
}

tke_brick <- brick(tke_list) #combine raster bricks
saveRDS(tke_brick, "tkebrick.rds")

