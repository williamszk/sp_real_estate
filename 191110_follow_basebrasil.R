#191110_follow_basebrasil

#preliminaries -------------------------------------------------------------
library(spdep)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(dismo)
library(rgdal)
library(INLA)
library(xts)
library(spatstat)
library(ggplot2)
library(tidyverse)
library(ggmap)


path1 = "C:/Users/willi/Dropbox/working/RAW_DATA/BASESCEM"
#New Residences Metropolitan Region -------------------------------------------------
#lançamento de imoveis na região metropolitana
shape=readOGR( paste(path1,"/LANRES_85_13_RMSP_CEM.shp",sep=""), 
               layer="LANRES_85_13_RMSP_CEM") 
shape=shape[shape$MUNICIPIO=='SAO PAULO',]
#plot(shape)

#poligono
distritos=readOGR(paste(path1,"/DisRM07.shp",sep=""), 
                  layer="DisRM07")
#plot(distritos,col=2)
#distritos@data %>% View()
#Para colocar o 'w' que será usado na parte da distância à escola
distritossp=distritos[distritos$MUN_NOME=="SAO PAULO",]
#unionSpatialPolygons take only the outer union of borders
nc.bordersp <- unionSpatialPolygons(distritossp, rep(1, nrow(distritossp)))
bsp=nc.bordersp 
#plot(bsp)
#the following code takes all points of the polygon
cbsp = (bsp@polygons[[1]]@Polygons[[1]]@coords)
#plot(cbsp)

###
bsp@polygons[[1]]@Polygons[[1]]@coords
class(bsp@polygons)
bsp@polygons[[1]]
class(bsp@polygons[[1]])
bsp@polygons[[1]]
getClass("Polygons")
aa=bsp@polygons[[1]]@Polygons
length(aa)
class(bsp@polygons[[1]]@Polygons)
aa=bsp@polygons[[1]]@Polygons[[1]]
class(aa)
getClass("Polygon")
bsp@polygons[[1]]@Polygons[[1]]@coords
###


wl=length(cbsp[,1])
#creates a window witha set of points
w <- owin(poly=list(x=cbsp[wl:1,1],y=cbsp[wl:1,2]))
class(w)
plot(w)

data=cbind(coordinates(shape),
           shape$TIPO_EMP,
           shape$ANO_LAN,
           shape$DORM_UNID,
           shape$AR_TT_UNID)

table(shape$DORM_UNID)
shape$ANO_LAN %>% class
shape$ANO_LAN %>% table

lancNum = table(shape$ANO_LAN)
class(lancNum)
lancNum[1]
names(lancNum)
NumLanc = as.numeric(lancNum)
time = as.numeric(names(lancNum)) 
plot(time,NumLanc, type='l')
#1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 
#366  745  272  338  388  247  184  167  379  443  477  443  419  329  329  473  426  489  538  553  485  486 
#2007 2008 2009 2010 2011 2012 2013 
#628  560  526  652  667  477  762 

X <- ppp(data[,1],data[,2], window=w)
plot(X)
plot(density(X))
X <- ppp(data[,1],data[,2], 
         window=w,
         marks=as.factor(shape$DORM_UNID))
plot(X)
plot(density(X))

#Fit Point Process Model to Data
ppm(X~1)

#Luminosity Data -----------------------------------------------------------------------
#data about luminosity
#https://eogdata.mines.edu/dmsp/download_radcal.html

str_name2<-
  paste(path1,
      "/F16_20100111-20101209_rad_v4.geotiff/F16_20100111-20101209_rad_v4.avg_vis.tif",
      sep="")
l2010calib=raster(str_name2)
bb= c( -47.05275, -46.02732, -23.77716, -23.31552)
rl2010calib <- crop(l2010calib, bb)
plot(rl2010calib)
lines(distritossp)


shaper=readOGR("c:/users/laurini/Dropbox/cursoespacial/BASESCEM/LOG2016_CEM_RMSP.shp", 
               layer="LOG2016_CEM_RMSP")
shaperv=shaper[shaper$TIPOLOG=="RV",]

#Rus Streets Avenues Roads ---------------------------------------------------------------
#rua, streets and avenues
shaper = readOGR( paste(path1,"/LOG2016_CEM_RMSP.shp",sep=""), layer="LOG2016_CEM_RMSP") 
shaperv=shaper[shaper$TIPOLOG=="RV",] #RV must be rodovia, roads
class(shaper)
plot(shaper)
plot(shaperv)
lines(distritossp, col='blue')
shaperv@data %>% View()
shaper@data$TIPOLOG %>% table


shaper2 = shaper[shaper$TIPOLOG=="AV",] 
dimShaper1 <- dim(shaper2)[1]
shaper3 = shaper[sample(dimShaper1,100) ,] 
plot(shaper3)
shaper3@data %>% View()

plot(rl2010calib)
lines(shaperv)

plot(rl2010calib)
lines(shaper)

# Censo2000------------------------------------------------------------------
shapec2000=readOGR(paste(path1,"/SC_2000/2000/Arquivo Geográfico/Shape/SCens00CEM.shp",sep=""), 
                   layer="SCens00CEM")
crs(shapec2000)=crs('+proj=longlat +ellps=GRS80 +no_defs')
shapect=spTransform(shapec2000, crs("+proj=utm +zone=22s +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +unit=m +no_defs"))


#usando gamma
ext=extent(shape) #box in which to plot shape
dsp <- fortify(distritossp)

#maps from Google ---------------------------------------------------------------
map <- get_map(location =c(-46.95, -24.1 ,-46,-23.2),maptype = 'satellite')
maph <- get_map(location =c(-46.95, -24.1 ,-46,-23.2),maptype = 'roadmap')

d <- data.frame(lat=coordinates(shape)[,2],
                lon=coordinates(shape)[,1])

figlan=ggmap(maph,  ylab = "Latitude", xlab = "Longitude") + 
  geom_polygon(aes(x=long, y=lat, group=group), 
               fill='grey', size=.2, color='blue', data=dsp, alpha=0)+
  geom_point(data=d, aes(x=lon, y=lat),size=.05)
ggsave("lanmapa.jpg", width = 20, height = 20, units = "cm",dpi = 600)

setwd("C:/Users/willi/Dropbox/working/Projects/191109_explore_estatitica_espacial")
getwd()

figlan=ggmap(map,  ylab = "Latitude", xlab = "Longitude") + 
  geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='blue', 
               data=dsp, alpha=0)+
  geom_point(data=d, aes(x=lon, y=lat),size=.05)
ggsave("lansat.jpg", width = 20, height = 20, units = "cm",dpi = 600)



#luminosidade, Night Time Light -------------------------------------------------------------------------------------------------------
#data about night time luminosity:
#https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html
#see also: https://damien-c-jacques.rbind.io/post/nighttime-lights-calibration/
#path to Luminosity data
pathLum = "C:/Users/willi/Dropbox/working/RAW_DATA/NightTimeLights"

str_name1993<-paste(pathLum,"/F101993.v4/F101993.v4b_web.stable_lights.avg_vis.tif/F101993.v4b_web.stable_lights.avg_vis.tif",sep="")
str_name2013<-paste(pathLum,"/F182013.v4/F182013.v4c_web.stable_lights.avg_vis.tif/F182013.v4c_web.stable_lights.avg_vis.tif",sep="")
l1993=raster(str_name1993) #luminosity 1993
l2013=raster(str_name2013) #luminosity 2013
bb= c( -47, -46, -24.00786 , -23.35623 )
rl1993<- crop(l1993, bb)
rl2013<- crop(l2013, bb)


jpeg("luminosidade1993.jpg")
plot(rl1993)
lines(distritossp)
dev.off() 

jpeg("luminosidade2013.jpg")
plot(rl2013)
lines(distritossp)
dev.off() 

#IDEA: build a GIF showing how the luminosity around the metropolitan region changed
#in time.
#for this we need to download all files available.
#Them use them to make the GIF.


#CCE firms in metropolitan region--------------------------------------------------
shapeemp=readOGR(paste(path1,"/CCE00_SP.shp",sep=""), layer="CCE00_SP") 
shapeempf=shapeemp[is.na(shapeemp$CNAE_E)==F,]
table(is.na(shapeemp$CNAE_E))
#FALSE   TRUE 
#446443   6373 


#load data about districts
distritos=readOGR(paste(path1,"/DisRM07.shp",sep=""), layer="DisRM07")
plot(distritos[distritos$MUN_NOME=="SAO PAULO",])
distritossp=distritos[distritos$MUN_NOME=="SAO PAULO",]
nc.bordersp <- unionSpatialPolygons(distritossp, rep(1, nrow(distritossp))) #cut outer border
bsp=nc.bordersp #change name
plot(bsp) #see map of outer border
cbsp= (bsp@polygons[[1]]@Polygons[[1]]@coords) #take coordinates of outerborder
plot(cbsp)  #see coordinates of outerborder
wl=length(cbsp[,1]) #define window
w <- owin(poly=list(x=cbsp[wl:1,1],y=cbsp[wl:1,2]))#define window which is São Paulo's border
plot(w) #see window, sp's border


table(shapeempf$CNAE_E)
shapea=shapef2=shapeempf[substr(shapeempf$CNAE_E,1,2)==80,]
#firms selected are schools
#80 EDUCAÇAO for CNAE-fiscal 1.0
#https://concla.ibge.gov.br/busca-online-cnae.html?view=secao&tipo=cnae&versaosubclasse=2&versaoclasse=1&secao=M
#Notas Explicativas: ##
#Esta seçao reúne unidades que realizam atividades de ensino, público e privado, 
#em qualquer nível e para qualquer profissao, na forma presencial ou através de rádio 
#e televisao ou outro meio de comunicaçao. Inclui o sistema educacional nos seus vários 
#graus e o ensino de formaçao contínua (exemplos: cursos de idiomas, cursos de 
#aprendizagem e treinamento gerencial e profissional). Nao inclui o ensino de esportes 
#(atividades desportivas). Quando uma instituiçao atua em mais de um nível da educaçao 
#seriada, é classificada na classe CNAE correspondente ao nível mais elevado
##
shapea@data %>% View() #see the names of schools
anos=unique(year) #which years we have?
substrRight <- function(x, n){ #function will use later
  substr(x, nchar(x)-n+1, nchar(x))
}
anof=as.double(substrRight(shapea$DATAFUN,4))
table(anof) #historic since 1953 open of firms of education
shapea$anof=anof #include variable of year into SpatialDataFrame
cd=coordinates(shapea) #take coordinates of SpatialDataFrame
len1 = length(cd[,1]) #use later
educ_ppp <- ppp(cd[,1],cd[,2],  #ppp object for education in sp city
          window=w, 
          marks=rep(1,len1))

sigm=bw.diggle(educ_ppp) 
#bw.diggle: Uses cross-validation to select a smoothing bandwidth 
#           for the kernel estimation of point process intensity.
plot(sigm, xlim=c(0,0.0015))
denemp1 = density(educ_ppp, sigm)
jpeg("densidadeeducacao.jpg")
plot(denemp1,main="Densidade Educação")
dev.off()




#favela ----------------------------------------------------------------------------

shapefavela=readOGR(paste(path1,"/HABI_Favelas_2008.shp",sep=""), 
                    layer="HABI_Favelas_2008")
plot(shapefavela)
#utm projection
crs(shapefavela)=crs("+proj=utm +zone=23 +south +ellps=aust_SA +units=m +no_defs")
#use long lat projection
shapefavelat = spTransform(shapefavela, 
                           crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
shapefavelat %>% class
plot(shapefavelat)
xyfav=coordinates(shapefavelat[shapefavelat$AREA>.005,]) #take only big favelas
d <- data.frame(lat=xyfav[,2], #coordinate points with big favelas
                lon=xyfav[,1])


figfav=ggmap(maph,  ylab = "Latitude", xlab = "Longitude") + 
  geom_polygon(aes(x=long, y=lat, group=group), fill='grey', 
               size=.2,color='black', data=dsp, alpha=0)+
  geom_point(data=d, aes(x=lon, y=lat),size=.05)
ggsave("favelamapa.jpg", width = 20, height = 20, units = "cm",dpi = 600)

figfavs=ggmap(map,  ylab = "Latitude", xlab = "Longitude") + 
  geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='blue', 
               data=dsp, alpha=0)+
  geom_point(data=d, aes(x=lon, y=lat),size=.3)
ggsave("favelasat.jpg", width = 20, height = 20, units = "cm",dpi = 600)



#some other maps ---------------------------------------
#I couldn't find the following file:
#CEM_ESCPUB01.shp
#in the directory of curso espacial
#for the following code
shapeescolasb=readOGR("c:/users/laurini/Dropbox/cursoespacial/BASESCEM/Escolas Públicas/CEM_ESCPUB01.shp", layer="CEM_ESCPUB01") #will load the 

#I couldn't find the file:
#UBS_2007.shp
shapeubs=readOGR("c:/users/laurini/Dropbox/cursoespacial/BASESCEM/CEM_(dados DATASUS)_2001_e_2007/UBS_2007.shp", layer="UBS_2007") #will load the 


















