library(ncdf4)
library(reshape2)
library(dplyr)

#set study area
lonmax <- -81 #top northern most coordinate
lonmin <- -98 #bottom southern coordinate
latmax <- 30.5 #left eastern coordinate
latmin <- 18.5 #right western coordinate

#list netCDF files in your directory
c=list.files(pattern=".nc",path="C:/Users/Em/Documents/Course Materials/Spring 2021/Data Management/Data/lab 10 data/Lab10_data/chlor_a",full.names=T, recursive = T)
head(c)

chl <- plyr::adply(c, 1, function(file) {

# open netCDF file
  data<-nc_open(file) #replace all "c" objects with "file" if using the plyr function

 fname=basename(file) #extracts the terminal file name and saves an object
 
# extract data
  lon<-ncvar_get(data,"lon")
  lat<-ncvar_get(data,"lat")
  tmp.array <- ncvar_get(data, data$var$chlor_a)
  dunits <- ncatt_get(nc = data, varid = "chlor_a", attname = "units")$value
  fillvalue <- ncatt_get(data, varid = "chlor_a", attname = "_FillValue")$value

#get dimensions
  dim(tmp.array)

# replace the pixes=ls with missing data by finding those with the "_FillValue" and replacing that value with a "NA"
  tmp.array[tmp.array == fillvalue] <- NA

#  matrix to data.frame
  dimnames(tmp.array)<-list(lon=lon,lat=lat)
  dat.var<-melt(tmp.array,id="lon")

# select data from the study area using the values you set in the first part of the script
  dat.varSAtmp<-subset(dat.var, lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin)

# extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  day<-substring(datemean,9,10)

# prepare final data set by adding meta data to your data frame
  dat.varSA<-data.frame(fname,rep(x=as.integer(year), nrow(dat.varSAtmp)),
                      rep(as.integer(month),nrow(dat.varSAtmp)),
                      rep(as.integer(day),nrow(dat.varSAtmp)),
                      dat.varSAtmp,
                      rep("chla", nrow(dat.varSAtmp)),
                      rep(dunits,nrow(dat.varSAtmp)))

  names(dat.varSA)<-c("file.name","year","month","day","lon","lat","value","unit","var")

  return(dat.varSA)
  nc_close(data)



}, .progress = "text", .inform = T)

# save csv file

write.csv(x=chl,file = "C:/Users/Em/Documents/Course Materials/Spring 2021/Data Management/Data/lab 10 data/Lab10_data/processed_GOM_chla.csv",row.names=FALSE)

closeAllConnections()

#performing the summary 
chl_sum= chl %>% group_by(lon,lat) %>%summarise(
  mean_chl= mean(value, na.rm=T),
  std_chl= sd(value, na.rm=T),
  var_chl= var(value, na.rm=T),
  n_chl=n())

summary(chl_sum)
head(chl_sum)

