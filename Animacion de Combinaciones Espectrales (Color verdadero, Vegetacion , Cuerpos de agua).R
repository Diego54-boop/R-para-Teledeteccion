##ANIMACION DE COMBINACIONES ESPECTRALES PARA COLOR VERDADERO , VEGETACION Y CUERPOS DE AGUA 

##Instalacion de paquetes
install.packages("sf")
install.packages("raster")
install.packages("magick")

##importacion de librerias instaladas
library(magick) 
library(sf)
library(raster)

##Ubicacion de archivo
ruta_bandas <- "D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA"

##Bandas espectrales del 1 al 7
ARCHIVOS_RASTER <-list.files(ruta_bandas, pattern = "\\TIF$", full.names = TRUE)
Bandas_necesarias <- ARCHIVOS_RASTER[c(1,4,5,6,7,8,9)]

##Composicion de bandas  
imagen_composite  <- stack(Bandas_necesarias)
plot(imagen_composite)

##Shapefile de la extension de los cuerpos de agua
extension  <- st_read("D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/RECTANGLE.shp")

## Reproyeccion de la imagen satelital 
raster_4326_ <-projectRaster(imagen_composite , crs = CRS("+init=EPSG:4326"))

##Cortado segun la extension del shapefile de la extension de cuerpos de agua
raster_cortado <-crop(raster_4326_  , extent(extension))
plot(raster_cortado)
## Guardar raster en carpeta

writeRaster(raster_cortado, 
            filename = "D:/CURSO R/RSTUDIO_HIDRO/Tema_10/Resultados/RASTER_T_FINAL.tif",
            format ="Gtiff",
             overwrite =TRUE)



##Ubicacion de archivo

ruta_tif <- "D:/CURSO R/RSTUDIO_HIDRO/Tema_10/Resultados/RASTER_T_FINAL.tif"

## creacion de objeto para el manejo de archivo multicapa , como el archivo utilizado
sat_im_brick <- brick(ruta_tif)

##combinacion de bandas espectrales

comb1<- c(4,3,2)##color verdadero 4
comb2<- c(5,4,3)##vegetacion 543
comb3<- c(6,3,2)##cuerpos de agua 632 

##Matriz de las combinaciones expectrales
matrix <- rbind(comb1, comb2,comb3)
matrix
nombres<- c("color verdadero","vegetacion","cuerpos de agua") 

##Dataframe cuadro respecto a las combinaciones espectrales
dataframe <- data.frame(index =c(1:nrow(matrix)),nombres, matrix)
dataframe

## carpeta de salida 
carpeta_salida <- "D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/archivos_png"
dir.create(carpeta_salida)


##Estructura iterativa para operar secuencialmente los archivos cada combinacion
for (i in c(1:nrow(matrix))){
  for (j in c(1)){
    bandas_combinadas <- stack(sat_im_brick[[matrix[[i,j, drop = FALSE]]]]
                               ,sat_im_brick[[matrix[[i,j+1, drop = FALSE]]]]
                               ,sat_im_brick[[matrix[[i,j+2, drop = FALSE]]]])
    #plotRGB(bandas_combinadas, axes= FALSE, stretch = 'lin') 
    bandas_combinadas<- bandas_combinadas  / maxValue(bandas_combinadas ) * 255
    png(file.path(carpeta_salida, paste("bandas_combinadas",i,".png")), width = 800, height = 800)
    plotRGB(bandas_combinadas, axes =FALSE, stretch ='lin')
    dev.off()
  }
}

## Lista de imagenes con formato png
archivos_png <- list.files(carpeta_salida, pattern = "\\.png$", full.names = TRUE)


print(archivos_png)


##Ciclo almacenar en una lista los nombres de cada combinacion  
lista_nombres <- list()
for (i in c(1:nrow(matrix))){
  imagenes_con_titulo <- 
  image_annotate(image_read(archivos_png[i]), text = dataframe$nombre[i], size = 25
                 , gravity = "north", color = "black")
  lista_nombres<-c(lista_nombres, imagenes_con_titulo)
}
##Archivo gif 
gif <- image_animate(image_join(lista_nombres), fps = 1)

# Guardar el GIF en la direccion especificada
image_write(gif, "D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/archivos_png/imagen.gif")  
