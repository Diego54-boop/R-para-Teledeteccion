###ANIMACION MULTITEMPORAL DE LA EVOLUION DE CUERPOS DE AGUA DESDE EL 2020 AL 2025

library(raster)
library(sf)
library(magick)

##Imagenes crudas sin composicion de bandas con extension completa
imagenes_crudas_2025 <- "D:/CURSO R/RSTUDIO_HIDRO/R PRACTICA/SCRIPTS_PRACTICA/2025_imagenes"
imagenes_crudas_2022 <- "D:/CURSO R/RSTUDIO_HIDRO/R PRACTICA/SCRIPTS_PRACTICA/2022_imagenes"
imagenes_crudas_2020 <- "D:/CURSO R/RSTUDIO_HIDRO/R PRACTICA/SCRIPTS_PRACTICA/2020_imagenes"

##Consulta al directorio  o carpeta local segun el patron formato 
lista_2025 <- list.files(imagenes_crudas_2025, pattern = "\\TIF$", full.names =TRUE )
lista_2022<-  list.files(imagenes_crudas_2022, pattern = "\\TIF$", full.names =TRUE )
lista_2020 <- list.files(imagenes_crudas_2020, pattern = "\\TIF$", full.names =TRUE )


##Dataframe de las imagenes satelitales para poder iterar con los nombres de las columndas posteriormente 
df <- data.frame(Año_2020 = lista_2020 , Año_2022 = lista_2022 , Año_2025 = lista_2025)

##Iteracion con el ciclo for para componer las bandas de la imagenes satelitales automaticamente 
for (i in colnames(df)){
  imagen_stack<- stack(unlist(as.list(df[i]))[1:length(df)]) ##NOTA df$ es un dataframe se convierte en lista y la lista a vector que es el tipo correcto de arreglo  para stack
  writeRaster(imagen_stack, filename = paste("D:/CURSO R/RSTUDIO_HIDRO/R PRACTICA/SCRIPTS_PRACTICA/TRABAJO FINAL","/",i,".tif",sep=''),overwrite =TRUE)
}

##Se extraen las direcciones  de las imagenes satelitales compuestas en la carpeta donde fueron almacenadas 
lista_años  <- list("D:/CURSO R/RSTUDIO_HIDRO/R PRACTICA/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8/cuerpos_agua_2020.tif",
                    "D:/CURSO R/RSTUDIO_HIDRO/R PRACTICA/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8/cuerpos_agua_2022.tif",
                    "D:/CURSO R/RSTUDIO_HIDRO/R PRACTICA/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8/cuerpos_agua_2025.tif")

##Region de estudio 
ruta_contorno_shp <- "D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/RECTANGLE.shp"
contorno <-st_read(ruta_contorno_shp)


##Iteracion con el ciclo for para cortar las imagenes satelitales superponiendo la extension de la region de estudio 
for (i in lista_años){
  raster_4326 <-projectRaster(brick(i)  ,crs = CRS("+init=EPSG:4326"))
  raster_cortado <-crop(raster_4326, extent(contorno))
  writeRaster(raster_cortado, filename = paste("D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8","/","cortado_",basename(i), sep='')
              , format = "Gtiff", overwrite =TRUE)
}

##Se extraen las direcciones de las imagenes cortadas   
c_agua_2020 <- "D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8/cortado_cuerpos_agua_2020.tif"
c_agua_2022 <- "D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8/cortado_cuerpos_agua_2022.tif"
c_agua_2025 <- "D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8/cortado_cuerpos_agua_2025.tif"

##Creacion de lista con las direcciones de cada imagen
lista_c_agua <- list(c_agua_2020,c_agua_2022,c_agua_2025)

##Creacion de directorio donde se almacenaran los outputs posteriores
carpeta_sal <- "D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8"
dir.create(carpeta_sal)

##Iteracion con el ciclo for para la combinacion espectral (6,3,2) para la teledeteccion de cuerpos 
##de agua de las imagenes satelitales , y su exportacion a formato png
for (i in lista_c_agua){
  comb_cuerpos_agua <- stack(brick(i)[[6]], brick(i)[[3]], brick(i)[[2]])
  comb_cuerpos_agua <- comb_cuerpos_agua/maxValue(comb_cuerpos_agua) * 255
  png(file.path(carpeta_sal, paste("comb_cuerpos_agua",tools::file_path_sans_ext(basename(i)),".png", sep ='')),width = 800, height = 800 )
  plotRGB(comb_cuerpos_agua, axes=FALSE , stretch ='lin')
  dev.off()
}
##Extraccion de las direcciones de las imagenes con formato png
archivos_png <- list.files(carpeta_sal, pattern = "\\.png$", full.names = TRUE)
archivos_png

##Creacion de lista vacia  con un ciclo for para llenarla de los atributos de las imagenes
lista_nombres_png <- list()
años <- c(2020,2022,2025)
for (i in c(1:rnow(años))){
  imagenes_titulo<- image_annotate(image_read(archivos_png[i]), text = años[i],  size = 25
                                   , gravity = "north", color = "black")
  lista_nombres_png <- c(lista_nombres_png, imagenes_titulo)
}

##Finalmente se crea la imagen en formato gif con las imagenes png antes creadas con sus atributos 
##añadidos de la lista creada anteriormente 
gif <- image_animate(image_join(lista_nombres_png),  fps = 1)
gif

##Se exporta la imagen en formato gif
image_write(gif,"D:/CURSO R/RSTUDIO_HIDRO/SCRIPTS_PRACTICA/TRABAJO FINAL/TALLER 8/cuerpos_agua.gif")






##Anexo 
##r tool para obtener el nombre de la direccion de una archivo
tools::file_path_sans_ext(basename(c_agua_2025))

paste("comb_cuerpos_agua","_",tools::file_path_sans_ext(basename(i)),".png", sep ='')


