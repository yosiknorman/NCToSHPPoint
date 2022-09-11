#!/usr/local/bin/Rscript
library(raster)
library(rgdal)
library(ncdf4)
library(openxlsx)
# rm(list = ls())
# NC = nc_open("Input/NC/java_t2mcorr_L0_m01.nc")
# DIMMM = brick("Input/NC/java_t2mcorr_L0_m01.nc")
# DataGrid4D = ncvar_get(NC,"T2mcorr" )

args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied.")
  
  NC = nc_open("Input/NC/java_t2mcorr_L0_m01.nc")
  DataGrid4D = ncvar_get(NC,"T2mcorr" )
  DIMMM = brick("Input/NC/java_t2mcorr_L0_m01.nc")
  
}else{
  for(i in 1:length(args)){
    print(args[i])
    NC = nc_open(paste0(args[i]))
    DataGrid4D = ncvar_get(NC,"T2mcorr")
    DIMMM = brick(paste0(args[i]))
  }
}


SHP = readOGR("Input/SHP_KAB_JAWA/kab_jawa_2021.shp")
crs(SHP) = crs(raster())


D23 = function(X){
  # X = 1
  KKK = DataGrid4D[,,X,]
  RasterSementara = list()
  for(i in 1:dim(KKK)[3]){
    RasterSementara[[i]] = raster(apply(t(KKK[, , i]), 2, FUN = rev))
  }
  RasterSementaraB = do.call(brick, RasterSementara)
  return(RasterSementaraB)
}

JanganPakeDulu = 1:2
NaMean = function(X){
  return(mean(X[!is.na(X)]))
}

DataGrid3D = apply(DataGrid4D, c(1, 2, 4), FUN = NaMean)

RGrid3DList = list()
for(i in 1:dim(DataGrid3D)[3]){
  RGrid3DList[[i]] = raster(apply(t(DataGrid3D[,,i]), 2, FUN = rev))
}
RGrid3D = do.call(brick, RGrid3DList)
extent(RGrid3D) = DIMMM
RGrid3D = resample(RGrid3D, DIMMM)

RGrid3DSHPList = list()
Buang = 1:2
for(i in 1:length(SHP)){
  # i = 28
  RGrid3DSHPList[[i]] = as.numeric(apply(rasterToPoints(mask(RGrid3D, SHP[i, ])), 2, FUN = NaMean)[-Buang])
  if(any(is.na(RGrid3DSHPList[[i]]))){
    RGrid3DS = disaggregate(RGrid3D, 10, method = "bilinear")
    RGrid3DSHPList[[i]] = as.numeric(apply(rasterToPoints(mask(RGrid3DS, SHP[i, ])), 2, FUN = NaMean)[-Buang])
  }
}
RGrid3DSHP = round(data.frame(do.call(rbind, RGrid3DSHPList)), 2)
colnames(RGrid3DSHP) = paste0("Time_", 1:ncol(RGrid3DSHP))
HasilMask = data.frame( Provinsi = SHP$WADMPR, Kabupaten = SHP$WADMKK, RGrid3DSHP)

write.xlsx(HasilMask, file = "Output/HasilMask.xlsx")

