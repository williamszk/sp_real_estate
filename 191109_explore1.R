#this is study is based on basesbrasil.txt

library(rgdal)
library(tidyverse)

path1 = "C:/Users/willi/Dropbox/working/RAW_DATA/BASESCEM"

#lançamento de imoveis na região metropolitana de sp
shape=readOGR( paste(path1,"/LANRES_85_13_RMSP_CEM.shp",sep=""), layer="LANRES_85_13_RMSP_CEM") 

plot(shape)
shape@data %>% View()
class(shape)
#shape=shape[shape$MUNICIPIO=='SAO PAULO',]
#distritos da região metropolitana
distritos=readOGR(paste(path1,"/DisRM07.shp",sep=""), layer="DisRM07")
class(distritos)
plot(distritos)

#LB15_LI_MSP_CEM_V1.shp linha de onibus
onibus = readOGR( paste(path1,"/LB15_LI_MSP_CEM_V1.shp",sep=""), layer="LB15_LI_MSP_CEM_V1") 
class(exp1)
exp1@data %>% View()
plot(exp1)

#favela
favela = readOGR( paste(path1,"/HABI_Favelas_2008.shp",sep=""), layer="HABI_Favelas_2008") 
class(favela)
#area of favelas
plot(favela)


#empresa em sp, firms at São Paulo metropolitan region
empresa = readOGR( paste(path1,"/CCE00_SP.shp",sep=""), layer="CCE00_SP") 

#area_sc 
#acho que é area sencitaria
path2 = "C:/Users/willi/Dropbox/working/RAW_DATA/BASESCEM/sc_2000/2000/Arquivo Geográfico/Shape"
area_sc = readOGR( paste(path2,"/SCens00CEM.shp",sep=""), layer="SCens00CEM") 
plot(area_sc)

#rua, streets and avenues
shaper = readOGR( paste(path1,"/LOG2016_CEM_RMSP.shp",sep=""), layer="LOG2016_CEM_RMSP") 
shaperv=shaper[shaper$TIPOLOG=="RV",] #RV must be rodovia, roads
class(shaper)
plot(shaper)
plot(shaperv)
shaperv@data %>% View()











