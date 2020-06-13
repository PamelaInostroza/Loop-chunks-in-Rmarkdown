#instalar paquetes R necesarios para ejecución de la sintaxis
install.packages(c("knitr","png","grid","reshape2","ggplot2","plyr","gridExtra",
                 "cowplot","dplyr","RColorBrewer","gtable","stringi","rmarkdown"))

#Directorio donde se encuentran las carpetas Datos, Logos, Sintaxis y Fichas
directorio<<-'C:/Users/pamela.inostroza/Dropbox/DIVESUP/DIVESUP' 

#Listado de universidades
IDU<-c(70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,993,994)
#nu=70

for (nu in IDU){
  
  rmarkdown::render(paste0(directorio,'/Sintaxis/Script1_',nu,'.Rmd'),
                    output_format =c("word_document"),
                    output_file = paste0(directorio,'/Fichas/Ficha_U',nu,'.docx'))
}
