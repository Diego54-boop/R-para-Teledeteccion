##Instalacion de los paquetes necesarias
install.packages("raster")
install.packages("landsat")
install.packages("tidyverse")
install.packages("sf")

##Llamado de las librerias correspondientes
library(raster)
library(tidyverse)
library(landsat)
library(sf)


################Procesamiento de los datos del archivo metadata necesario para el algoritmo DOS(Estos valores son independientes de las bandas espectrales 
por lo que son constantes para la imagen satelital) 

##Cargar metadata
metadata_r <- read.csv("C:/Users/LENOVO/Desktop/R-spatial/R_teledeteccion/LC08_L2SP_004070_20251119_20251202_02_T1_MTL.txt", sep = "=")


##Fecha de adquisicion
fecha_df <- metadata_r %>% filter(GROUP == '    DATE_ACQUIRED ')%>% select(Valor)
fecha <- fecha_df[,1]


#Earth sun distance 
Earth_Sun_El <- as.numeric(metadata_r %>% filter(GROUP == '    EARTH_SUN_DISTANCE ') %>% select(Valor))[1]

# Sun elevation
Sun_Elev <- as.numeric(metadata_r %>% filter(GROUP == '    SUN_ELEVATION ') %>% select(Valor))[1]


################Procesamiento de los datos del archivo metadata dependientes de las bandas espectrales (existe un unico valor por cada banda)

##Valores Grescale y Brescale para todas las bandas obtenidas de la metadata de la imagen
tabla_brescale_grescale <- read.csv("C:/Users/LENOVO/Downloads/radiancias_brescale_grescale.csv")##Las radiancias brescale y grescale son obtenidas externamente desde
                                                                                                 ## python-pandas
tabla_bresacle_grescale_1_7 <- tabla_brescale_grescale[c(1,2,3,4,5,7),] ##Se descarta la banda 6 pues segun el algoritmo DOS no se puede corregir radiometricamente

valores_grescale_brescale <-tabla_bresacle_grescale_1_7 %>% select(-BANDAS)  

#Transformacion de la tabla de valores a matriz numerica
matriz_valores <- cbind(valores_grescale_brescale[,1],valores_grescale_brescale[,2])

##Esun
tabla_esun<- read.csv("C:/Users/LENOVO/Documents/ESUN.csv") ##Tabla obtenida del servicio geologico de los estados unidos(USGS)
colnames(tabla_esun)<- c("Banda", "Valor")
row.names(tabla_esun)<- c(1,2,3,4,5,6)
tabla_esun_ <- tabla_esun %>% select(-Banda)

#################Composicion de bandas espectrales para la imagen satelital
carp <-  "C:/Users/LENOVO/Desktop/R-spatial/R_teledeteccion/bandas_landsat"
lista_bandas  <- list.files(carp , pattern ='\\TIF$' , full.names = TRUE)
img_1 <- stack(lista_bandas)

#################Aplicacion del algorito DOS para todas las bandas espectrales suceptibles a correcion radiometrica

##Dimensiones de la imagen satelital como matriz numerica
nrow <- dim(as.matrix(img_1[[1]]))[1]
ncol <- dim(as.matrix(img_1[[1]]))[2]

##Extension de la imagen satelital sin tratamiento radiometrico
extension_ext <- projectRaster(img_1[[1]], crs =CRS("+init=EPSG:32718"))

##Estrutura iterativa para aplicar el algoritmo a las bandas espectrales, con condicionales respecto de la
##clasificacion de los valores niebla (SHV)

##########CLASIFICACION DE VALORES SHV
###	-4.0: Very Clear	SHV <= 55
###	-2.0: Clear		SHV 56-75
### 	-1.0: Moderate		SHV 76-95
###	-0.7: Hazy		SHV 96-115
###	-0.5: Very Hazy		SHV >115

for (i in c(1,2,3,4,5,6)){ ##La banda 6 no se puede corregir radiometricamente con el metodo DOS
  
  SHV <- table((img_1[[i]][]*0.00002 -0.1)*10000)
  SHV <- min(as.numeric(names(SHV)[SHV > 1000]))
  
  banda <- img_1[[i]][] ##valores digitales de la imagen satelital expresada como un objeto vector.
  
  if(SHV<=55){
    banda.DOS <- DOS(sat=7, SHV=SHV, SHV.band=1, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]],
                       sunelev= 65.265 , edist=ESdist("2025-11-19"))$DNfinal.mean
    banda_dos <- banda.DOS[,1]
    banda.DOSrefl <- radiocorr(banda, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]], 
                                 sunelev= 65.265, edist=ESdist("2025-11-19"), Esun=as.numeric(tabla_esun_[c(i),])*10 , 
                                 Lhaze= banda_dos[i]  , method="DOS")
  }else if(SHV>56 & SHV <75){
    banda.DOS <- DOS(sat=7, SHV=SHV, SHV.band=1, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]],
                       sunelev= 65.265 , edist=ESdist("2025-11-19"))$DNfinal.mean
    banda_dos <- banda.DOS[,2]
    banda.DOSrefl <- radiocorr(banda, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]], 
                                 sunelev= 65.265, edist=ESdist("2025-11-19"), Esun=as.numeric(tabla_esun_[c(i),])*10 , 
                                 Lhaze= banda_dos[i]  , method="DOS")
    
  }else if(SHV>76 & SHV <95){
    banda.DOS <- DOS(sat=7, SHV=SHV, SHV.band=1, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]],
                       sunelev= 65.265 , edist=ESdist("2025-11-19"))$DNfinal.mean
    banda_dos <- banda.DOS[,3]
    banda.DOSrefl <- radiocorr(banda, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]], 
                                 sunelev= 65.265, edist=ESdist("2025-11-19"), Esun=as.numeric(tabla_esun_[c(i),])*10 , 
                                 Lhaze= banda_dos[i]  , method="DOS")
    
  }else if(SHV>96 & SHV <115){
    banda.DOS <- DOS(sat=7, SHV=SHV, SHV.band=1, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]],
                       sunelev= 65.265 , edist=ESdist("2025-11-19"))$DNfinal.mean
    banda_dos <- banda.DOS[,4]
    banda.DOSrefl <- radiocorr(banda, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]], 
                                 sunelev= 65.265, edist=ESdist("2025-11-19"), Esun=as.numeric(tabla_esun_[c(i),])*10 , 
                                 Lhaze= banda_dos[i]  , method="DOS")
  }else if (SHV > 115){
    banda.DOS <- DOS(sat=7, SHV=SHV, SHV.band=1, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]],
                       sunelev= 65.265 , edist=ESdist("2025-11-19"))$DNfinal.mean
    banda_dos <- banda.DOS[,5]
    banda.DOSrefl <- radiocorr(banda, Grescale= matriz_valores[[i,1]], Brescale=matriz_valores[[i,2]], 
                                 sunelev= 65.265, edist=ESdist("2025-11-19"), Esun=as.numeric(tabla_esun_[c(i),])*10 , 
                                 Lhaze= banda_dos[i]  , method="DOS")
  }else{
    print("SHV Fuera de rango")
  }
  ##El resultado final se encuentra en uan variable tipo vector , por lo que estos valores deben tomar una estrutura raster
  ## tal y como se detalla a continuacion
  
  new_raster <- raster(ncol = ncol ,nrow = nrow , ext = extent(extension_ext), crs =CRS("+init=EPSG:32718"))
  values <- banda.DOSrefl ##Valores digitales corregidos resultantes de la aplicacion del algoritmo
  new_raster <- setValues(new_raster, values)
  filename <- paste("C:/Users/LENOVO/Desktop/R-spatial/R_teledeteccion/BANDAS_CORREGIDAS_DOS","/Banda_corregida","_", i,".tif", sep ='')
  writeRaster(new_raster, filename , format="Gtiff")##Guardado de cada imagen banda 
  
}