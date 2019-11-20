#191110_follow_basesbrasil_file

library(rgdal)
library(tidyverse)

#trying to install packages in R ubuntu
#sudo apt-get install libssl-dev
#install.packages("tidyverse", dependencies = TRUE)
#sudo apt-get install apt-file
#sudo apt-file update
#sudo apt-file search gdal-config
#sudo apt-file search tidyverse-config

#install.packages("rgdal", dependencies = TRUE)
#gdal-config
#sudo apt install libgdal-dev

path1 = "~/Dropbox/working/RAW_DATA/BASESCEM"

#lancamento de imoveis na regiao metropolitana
shape=readOGR( paste(path1,"/LanRes_85_13_RMSP_CEM.shp",sep=""), 
               layer="LanRes_85_13_RMSP_CEM")
shape@data %>% View()
data <- shape@data
str(data)
names(data)

#transform all variables that are factor into character
data2 <- data
len1 <- length(names(data)) 
ii <- 45
for (ii in 1:len1) {
  vv <- data[,ii]
  if (is.factor(vv)) {
    vv1 <- as.character(vv)
    data2[,ii] <- vv1
  }
  
}
str(data2)
#shape@data <- data2

#those variables have the same information. 
#the information is date
#we'll keep only one of them
data2$MES_LAN #m?s lan?amento
data2$ANO_LAN #ano lan?amento
data2$DATA_ENT #data entrega
identical(data2$MES_LAN,data2$ANO_LAN)
identical(data2$ANO_LAN,
          data2$DATA_ENT)
#see what are the differences between lan?amento and entrega
pos1=data2$ANO_LAN!=data2$DATA_ENT
data2[pos1,] %>% View()
#exclude variabel ANO_LAN, have the same information as MES_LAN
data3 <- data2 %>% select(-ANO_LAN)
names(data3)

#dates
#transform character dates into Date format
ch1 = data3$MES_LAN
data3$MES_LAN = as.Date(ch1, format="%d-%B-%Y")

ch1 = data3$DATA_ENT
data3$DATA_ENT = as.Date(ch1, format="%d-%B-%Y")

str(data3)

#DORM_UNID: quantidade de dormit?rios por unidade.
#BANH_UNID: quantidade de banheiros por unidade.
#GAR_UNID: quantidade de vagas por unidade.
#ELEV: quantidade de elevadores por lan?amento.
#COB: quantidade de unidades na cobertura.
#BLOCOS: quantidade de blocos no empreendimento.
#UNIDAND: quantidade de unidades por andar.
#ANDARES: quantidade de andares do empreendimento.
#AR_UT_UNID: ?rea ?til da unidade em m?.
#PC_TT_UN: vari?vel dependente ?pre?o de venda da unidade.

#transform into numeric
AR_UT_UNID
PC_TT_UN_U

#to transform as factor
DORM_UNID
BANH_UNID
GAR_UNID
ELEV
COB
BLOCOS
UNIDAND

table(data3$BANH_UNID)
table(data3$UNIDAND)
table(data3$DORM_UNID)
























