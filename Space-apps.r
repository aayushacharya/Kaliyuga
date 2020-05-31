#Load required libraries
#library('tidyr')
#library('dplyr')
#library('ggplot2')
#library('ggmap')
#library('viridis')
#library('readr')
#library('weathermetrics')
library(ncdf4)
library(dplyr)
library(chron)
library(RColorBrewer)
library(lattice)
library(ggplot2)
theme_set(theme_bw())
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)

#open files in netcdf,h5,xls and csv format

#Aerosols
aerosol_2015<-nc_open('Aerosols/Monthly/April-yearly/2015.nc')
aerosol_2016<-nc_open('Aerosols/Monthly/April-yearly/2016.nc')
aerosol_2017<-nc_open('Aerosols/Monthly/April-yearly/2017.nc')
aerosol_2018<-nc_open('Aerosols/Monthly/April-yearly/2018.nc')
aerosol_2019<-nc_open('Aerosols/Monthly/April-yearly/2019.nc')
aerosol_2020<-nc_open('Aerosols/Monthly/April-yearly/2020.nc')

#Ice Area
january_ice_extent<-read.csv('Ice_area/January.csv')
february_ice_extent<-read.csv('Ice_area/February.csv')
march_ice_extent<-read.csv('Ice_area/March.csv')
april_ice_extent<-read.csv('Ice_area/April.csv')
may_ice_extent<-read.csv('Ice_area/May.csv')
june_ice_extent<-read.csv('Ice_area/June.csv')
july_ice_extent<-read.csv('Ice_area/July.csv')
august_ice_extent<-read.csv('Ice_area/August.csv')
september_ice_extent<-read.csv('Ice_area/September.csv')
october_ice_extent<-read.csv('Ice_area/October.csv')
november_ice_extent<-read.csv('Ice_area/November.csv')
december_ice_extent<-read.csv('Ice_area/December.csv')
all_region_ice_extent<-read.csv('Ice_area/Ice_Extent_By_Region_SQKM_4KM.csv')

#Ocean
ocean_temperature_2004<-nc_open('Ocean/Temperature/2004_NSST.nc')
ocean_temperature_2019<-nc_open('Ocean/Temperature/2019_NSST.nc')
ocean_temperature_2020<-nc_open('Ocean/Temperature/2020_NSST.nc')


#Preprocessing functions
plotMap <- function(lat,lon,tas,titleText,scaleName,gradient='viridis',limits=c(0,3),gradient_direction=-1) #model and perc should be a string
{
  
  expand.grid(lon, lat) %>%
    rename(lon = Var1, lat = Var2) %>%
    mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
           tas = as.vector(tas)) %>% 
    ggplot() + 
    geom_point(aes(x = lon, y = lat, color = tas),size = 1) + 
    borders("world", colour="black", fill=NA) +
    xlab("Longitude")+
    ylab("Latitude")+
    scale_color_viridis_c(alpha=0.8,na.value="white",name = scaleName,option =gradient,limits=limits,direction=gradient_direction) + 
    theme(legend.direction="vertical", legend.position="right", legend.key.width=unit(0.4,"cm"), legend.key.height=unit(1.5,"cm")) + 
    coord_quickmap() + 
    ggtitle(titleText) 
}
#Processing Aerosols
#2015
aer_2015_angstrom_exponent<-ncvar_get(aerosol_2015,"Angstrom_Exponent_Land_Ocean_Maximum")
aer_2015_latitude<-ncvar_get(aerosol_2015,"Latitude_1D")
aer_2015_longitude<-ncvar_get(aerosol_2015,"Longitude_1D")
aer_2015_title<-"Angstrom Exponent in April of 2015"
aer_2015_unit<-"Angstrom Exponent"
aer_2015_gradient<-'magma'
aer_2015_limits<-c(0,2)

#2019
aer_2019_angstrom_exponent<-ncvar_get(aerosol_2019,"Angstrom_Exponent_Land_Ocean_Maximum")
aer_2019_latitude<-ncvar_get(aerosol_2019,"Latitude_1D")
aer_2019_longitude<-ncvar_get(aerosol_2019,"Longitude_1D")
aer_2019_title<-"Angstrom Exponent in April of 2019"
aer_2019_unit<-"Angstrom Exponent"
aer_2019_gradient<-'magma'
aer_2019_limits<-c(0,2)

#2020
aer_2020_angstrom_exponent<-ncvar_get(aerosol_2020,"Angstrom_Exponent_Land_Ocean_Maximum")
aer_2020_latitude<-ncvar_get(aerosol_2020,"Latitude_1D")
aer_2020_longitude<-ncvar_get(aerosol_2020,"Longitude_1D")
aer_2020_title<-"Angstrom Exponent in April of 2020"
aer_2020_unit<-"Angstrom Exponent"
aer_2020_gradient<-'magma'
aer_2020_limits<-c(0,2)

#plotMap(aer_2015_latitude,aer_2015_longitude,aer_2015_angstrom_exponent,aer_2015_title,aer_2015_unit,gradient=aer_2015_gradient,limits=aer_2015_limits)

#plotMap(aer_2019_latitude,aer_2019_longitude,aer_2019_angstrom_exponent,aer_2019_title,aer_2019_unit,gradient=aer_2019_gradient,limits=aer_2019_limits)

#plotMap(aer_2020_latitude,aer_2020_longitude,aer_2020_angstrom_exponent,aer_2020_title,aer_2020_unit,gradient=aer_2020_gradient,limits=aer_2020_limits)

#Ice-area processing

# ggplot(data=april_ice_extent)+
#   geom_point(aes(x=year,y=extent,color=year))+
#   ggtitle('Ice extent (mil. sq. km) in April of every year')+
#   xlab('Year')+
#   ylab('Ice Extent in mil. sq. km')+
#   scale_color_gradient(low='#A9C8F3',high='#0C2389')
# 
# ice_extent_2015<-c(january_ice_extent$extent[january_ice_extent$year==2015],february_ice_extent$extent[february_ice_extent$year==2015],march_ice_extent$extent[march_ice_extent$year==2015],april_ice_extent$extent[april_ice_extent$year==2015])
# ice_extent_2016<-c(january_ice_extent$extent[january_ice_extent$year==2016],february_ice_extent$extent[february_ice_extent$year==2016],march_ice_extent$extent[march_ice_extent$year==2016],april_ice_extent$extent[april_ice_extent$year==2016])
# ice_extent_2017<-c(january_ice_extent$extent[january_ice_extent$year==2017],february_ice_extent$extent[february_ice_extent$year==2017],march_ice_extent$extent[march_ice_extent$year==2017],april_ice_extent$extent[april_ice_extent$year==2017])
# ice_extent_2018<-c(january_ice_extent$extent[january_ice_extent$year==2018],february_ice_extent$extent[february_ice_extent$year==2018],march_ice_extent$extent[march_ice_extent$year==2018],april_ice_extent$extent[april_ice_extent$year==2018])
# ice_extent_2019<-c(january_ice_extent$extent[january_ice_extent$year==2019],february_ice_extent$extent[february_ice_extent$year==2019],march_ice_extent$extent[march_ice_extent$year==2019],april_ice_extent$extent[april_ice_extent$year==2019])
# ice_extent_2020<-c(january_ice_extent$extent[january_ice_extent$year==2020],february_ice_extent$extent[february_ice_extent$year==2020],march_ice_extent$extent[march_ice_extent$year==2020],april_ice_extent$extent[april_ice_extent$year==2020])
# ice_extent_month<-c('January','February','March','April')
# ice_extent_monthly_data_2015<-data.frame(Month=ice_extent_month,Extent=ice_extent_2015)
# ice_extent_monthly_data_2016<-data.frame(Month=ice_extent_month,Extent=ice_extent_2016)
# ice_extent_monthly_data_2017<-data.frame(Month=ice_extent_month,Extent=ice_extent_2017)
# ice_extent_monthly_data_2018<-data.frame(Month=ice_extent_month,Extent=ice_extent_2018)
# ice_extent_monthly_data_2019<-data.frame(Month=ice_extent_month,Extent=ice_extent_2019)
# ice_extent_monthly_data_2020<-data.frame(Month=ice_extent_month,Extent=ice_extent_2020)
# 
#cols<-c('2020'='maroon','2019'='skyblue','2018'='lightgreen','2017'='peachpuff4','2016'='darkorchid3','2015'='midnightblue')
# ggplot(data=ice_extent_monthly_data_2020)+
#   geom_line(aes(x=factor(Month,level=ice_extent_month),y=Extent,group=1, colour="2020"),size=1.0)+
#   geom_point(aes(x=factor(Month,level=ice_extent_month),y=Extent,colour='2020'))+
#   geom_line(data=ice_extent_monthly_data_2019,aes(x=factor(Month,level=ice_extent_month),y=Extent,group=1, colour="2019"),size=1.0)+
#   geom_point(data=ice_extent_monthly_data_2019,aes(x=factor(Month,level=ice_extent_month),y=Extent,colour='2019'))+
#   geom_line(data=ice_extent_monthly_data_2018,aes(x=factor(Month,level=ice_extent_month),y=Extent,group=1, colour="2018"),size=1.0)+
#   geom_point(data=ice_extent_monthly_data_2018,aes(x=factor(Month,level=ice_extent_month),y=Extent,colour='2018'))+
#   geom_line(data=ice_extent_monthly_data_2017,aes(x=factor(Month,level=ice_extent_month),y=Extent,group=1, colour="2017"),size=1.0)+
#   geom_point(data=ice_extent_monthly_data_2017,aes(x=factor(Month,level=ice_extent_month),y=Extent,colour='2017'))+
#   geom_line(data=ice_extent_monthly_data_2016,aes(x=factor(Month,level=ice_extent_month),y=Extent,group=1, colour="2016"),size=1.0)+
#   geom_point(data=ice_extent_monthly_data_2016,aes(x=factor(Month,level=ice_extent_month),y=Extent,colour='2016'))+
#   geom_line(data=ice_extent_monthly_data_2015,aes(x=factor(Month,level=ice_extent_month),y=Extent,group=1, colour="2015"),size=1.0)+
#   geom_point(data=ice_extent_monthly_data_2015,aes(x=factor(Month,level=ice_extent_month),y=Extent,colour='2015'))+
#   ggtitle('Monthly Ice extent in million sq. km in 2015-2020')+
#   scale_color_manual(name='Year',values=cols)+
#   xlab('Month')+
#   ylab('Ice Extent in mill. sq. km')
#   

#Greenland Sea
# greenland_sea_no_days_2015<-1:sum(all_region_ice_extent$yyyyddd>=2015001 & all_region_ice_extent$yyyyddd<=2015149)
# greenland_sea_no_days_2016<-1:sum(all_region_ice_extent$yyyyddd>=2016001 & all_region_ice_extent$yyyyddd<=2016149)
# greenland_sea_no_days_2017<-1:sum(all_region_ice_extent$yyyyddd>=2017001 & all_region_ice_extent$yyyyddd<=2017149)
# greenland_sea_no_days_2018<-1:sum(all_region_ice_extent$yyyyddd>=2018001 & all_region_ice_extent$yyyyddd<=2018149)
# greenland_sea_no_days_2019<-1:sum(all_region_ice_extent$yyyyddd>=2019001 & all_region_ice_extent$yyyyddd<=2019149)
# greenland_sea_no_days_2020<-1:sum(all_region_ice_extent$yyyyddd>=2020001 & all_region_ice_extent$yyyyddd<=2020149)
# greenland_sea_extent_2015<-all_region_ice_extent$X.7..Greenland_Sea[all_region_ice_extent$yyyyddd>=2015001 & all_region_ice_extent$yyyyddd<=2015149]
# greenland_sea_extent_2016<-all_region_ice_extent$X.7..Greenland_Sea[all_region_ice_extent$yyyyddd>=2016001 & all_region_ice_extent$yyyyddd<=2016149]
# greenland_sea_extent_2017<-all_region_ice_extent$X.7..Greenland_Sea[all_region_ice_extent$yyyyddd>=2017001 & all_region_ice_extent$yyyyddd<=2017149]
# greenland_sea_extent_2018<-all_region_ice_extent$X.7..Greenland_Sea[all_region_ice_extent$yyyyddd>=2018001 & all_region_ice_extent$yyyyddd<=2018149]
# greenland_sea_extent_2019<-all_region_ice_extent$X.7..Greenland_Sea[all_region_ice_extent$yyyyddd>=2019001 & all_region_ice_extent$yyyyddd<=2019149]
# greenland_sea_extent_2020<-all_region_ice_extent$X.7..Greenland_Sea[all_region_ice_extent$yyyyddd>=2020001 & all_region_ice_extent$yyyyddd<=2020149]
# 
# greenland_sea_2015<-data.frame(Days=greenland_sea_no_days_2015,Extent=greenland_sea_extent_2015)
# greenland_sea_2016<-data.frame(Days=greenland_sea_no_days_2016,Extent=greenland_sea_extent_2016)
# greenland_sea_2017<-data.frame(Days=greenland_sea_no_days_2017,Extent=greenland_sea_extent_2017)
# greenland_sea_2018<-data.frame(Days=greenland_sea_no_days_2018,Extent=greenland_sea_extent_2018)
# greenland_sea_2019<-data.frame(Days=greenland_sea_no_days_2019,Extent=greenland_sea_extent_2019)
# greenland_sea_2020<-data.frame(Days=greenland_sea_no_days_2020,Extent=greenland_sea_extent_2020)
# 
# ggplot(data=greenland_sea_2015)+
#   geom_line(aes(x=Days,y=Extent,colour='2015',group=1),size=1.0)+
#   geom_point(aes(x=Days,y=Extent,colour='2015'))+
#   geom_line(data=greenland_sea_2016,aes(x=Days,y=Extent,colour='2016',group=1),size=1.0)+
#   geom_point(data=greenland_sea_2016,aes(x=Days,y=Extent,colour='2016'))+
#   geom_line(data=greenland_sea_2017,aes(x=Days,y=Extent,colour='2017',group=1),size=1.0)+
#   geom_point(data=greenland_sea_2017,aes(x=Days,y=Extent,colour='2017'))+
#   geom_line(data=greenland_sea_2018,aes(x=Days,y=Extent,colour='2018',group=1),size=1.0)+
#   geom_point(data=greenland_sea_2018,aes(x=Days,y=Extent,colour='2018'))+
#   geom_line(data=greenland_sea_2019,aes(x=Days,y=Extent,colour='2019',group=1),size=1.0)+
#   geom_point(data=greenland_sea_2019,aes(x=Days,y=Extent,colour='2019'))+
#   geom_line(data=greenland_sea_2020,aes(x=Days,y=Extent,colour='2020',group=1),size=1.0)+
#   geom_point(data=greenland_sea_2020,aes(x=Days,y=Extent,colour='2020'))+
#   ggtitle('Greenland Sea Ice extent in sq. km in 2015-2020')+
#   scale_color_manual(name='Year',values=cols)+
#   xlab('No. of days')+
#   ylab('Ice Extent in sq. km')
  
