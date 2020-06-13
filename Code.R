###descargar paquetes desde r.
library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)
library(cowplot)
library(dplyr)
library(RColorBrewer)
library(gtable)
library(stringi)

# se define no impresion en notación cientifica y los decimales con coma (,) y miles con punto (.)
options(scipen=999)
options(OutDec= ",")  

#############################################################################################
###########################lee archivos con informacion para fichas##########################
#############################################################################################
#Listado de capitulos a incluir en la ficha
areaG<-read.csv(paste0(directorio,"/Datos/Areas.csv"),header = T,sep = ";",stringsAsFactors=FALSE)
areaG<-areaG[with(areaG, order(Narea)),]
#Listado de graficos para incluir en la ficha
graficosG<-read.csv(paste0(directorio,"/Datos/graficos.csv"),header = T,sep = ";",stringsAsFactors=FALSE)
#graficosG<-read.csv(paste0(directorio,"/Datos/graficos2.csv"),header = T,sep = ";",stringsAsFactors=FALSE)
graficosG<-graficosG[with(graficosG, order(Narea, Graph)),]
#Listado de variables a incluir en la ficha
codigosG<-read.csv(paste0(directorio,"/Datos/Variables.csv"),header = T,sep = ";",stringsAsFactors=FALSE)
#codigosG<-read.csv(paste0(directorio,"/Datos/Variables2.csv"),header = T,sep = ";",stringsAsFactors=FALSE)
codigosG<-codigosG[with(codigosG, order(Graph, orden)),]
#Listado de Universidades
UesG<-read.csv(paste0(directorio,"/Datos/Universidades.csv"),header = T,sep = ";",stringsAsFactors=FALSE)
#Base de datos con información
data1<-read.csv(paste0(directorio,"/Datos/Data.csv"),header = T,dec=",",sep = ";", quote="")

#Selecciona solo información de la universidad
data<-data1[data1$IDU==nu,]

#selecciona las variables que se deben graficar, que aparecen disponibles para la universidad en Data.csv
TCodes<-codigosG[codigosG$VARIABLE %in% as.vector(data$VARIABLE),]
TCodes<-TCodes[with(TCodes, order(Graph, orden)),]

#selecciona los graficos correspondientes a las variables disponibles
graf<-graficosG[graficosG$Graph %in% as.vector(TCodes$Graph),]
#selecciona las areas a las que corresponden los graficos disponibles
areas<-areaG[!is.na(graf$Narea),]
areas<-ddply(areas, "Narea", function(z) head(z,1))
areas<-areas[!is.na(areas$Narea),]
areas1<-paste0(areas$n,". ",areas$AREA)

#genera salto de linea para los simbolos ? con un espacio previo
graf$NOTAS<-gsub(" \\? ", "  \n###### ",graf$NOTAS) 

#############################################################################################
#################################### indice de contenidos####################################
#############################################################################################
graff<-graf[,c("Graph","TITULO")]
tim=63
par=c(2)
pag=c(6)
narr=c(1)
Graph=0

tabla<-as.data.frame(cbind(TITULO="",par,pag,narr,Graph),stringsAsFactors=FALSE)
i=1
tabla[i,"indice"]<-paste0("Introducción",paste0(rep(".",(tim-(nchar("Introduccion")+2))),collapse = ""),as.numeric(tabla[i,"pag"]))
tabla[i,"Graph"]<-0
tabla[i+1,"pag"]<-as.numeric(tabla[i,"pag"])+1
tabla[i+1,"indice"]<-paste0("Notas generales",paste0(rep(".",(tim-(nchar("Notas generales")+2))),collapse = ""),as.numeric(tabla[i+1,"pag"]))
tabla[i+1,"Graph"]<-0
tabla[i+2,"pag"]<-as.numeric(tabla[i+1,"pag"])+1
tabla[i+2,"indice"]<-paste0("Descripción de la Universidad",paste0(rep(".",(tim-(nchar("Descripcion de la Universidad")+2))),collapse = ""),as.numeric(tabla[i+2,"pag"]))
tabla[i+2,"Graph"]<-0
i=i+3
graff$TITULO<-gsub("/ ", "",graff$TITULO) 
graff$TITULO<-gsub(" \\(1\\)", "",graff$TITULO) 
for (h in 1:nrow(areas)){
  ningraf<-graf[graf$Narea==areas$Narea[h],"Graph"]
    tabla[i,"TITULO"]<-areas1[h]
    tabla[i,"Graph"]<-0
    tabla[i,"par"]<-2
    if (h==1) tabla[i,"pag"]<-as.numeric(tabla[i-1,"pag"])+1 else tabla[i,"pag"]<-as.numeric(tabla[i-1,"pag"])+1
    tabla[i,"narr"]<-areas$Narea[h]
  if (nchar(as.character(tabla[i,"TITULO"]))+nchar(as.character(tabla[i,"pag"]))<tim) tabla[i,"indice"]<-paste0(gsub("/", "",tabla[i,"TITULO"]),paste0(rep(".",(tim-(nchar(as.character(tabla[i,"TITULO"]))+nchar(as.character(tabla[i,"pag"])))-1)),collapse = ""),tabla[i,"pag"])
  else tabla[i,"indice"]<-paste0(gsub("/", "",tabla[i,"TITULO"]),paste0(rep(".",(tim*2-(nchar(as.character(tabla[i,"TITULO"]))+nchar(as.character(tabla[i,"pag"])))-1)),collapse = ""),tabla[i,"pag"])
  i=i+1 
  tabla[i,"par"]<-0
  for (g in ningraf){
    tabla[i,"TITULO"]<-graff[graff$Graph==g,"TITULO"]
    tabla[i,"Graph"]<-graff[graff$Graph==g,"Graph"]
    if (tabla[i,"par"]==2) {
      tabla[i,"pag"]<-as.numeric(tabla[i-1,"pag"])+1
      tabla[i,"narr"]<-graf[graf$Graph==g,"Narea"]
     i=i+1
     tabla[i,"par"]<-0
    } else  if (tabla[i,"par"]==0) {
      tabla[i,"pag"]<-as.numeric(tabla[i-1,"pag"])+1
      tabla[i,"narr"]<-graf[graf$Graph==g,"Narea"]
      i=i+1
      tabla[i,"par"]<-1
    } else  if (tabla[i,"par"]==1) {
      tabla[i,"pag"]<-as.numeric(tabla[i-1,"pag"])
      tabla[i,"narr"]<-graf[graf$Graph==g,"Narea"]
      i=i+1
      tabla[i,"par"]<-0
    }
    largo<-nchar(as.character(tabla[i-1,"TITULO"]))+nchar(as.character(tabla[i-1,"pag"]))
    resto<-tabla[i-1,"TITULO"]
    corteimprim1<-NULL
    corteimprim2<-NULL
    corteimprim3<-NULL
    
        if (largo>=tim) { #si el largo no alcanza en una fila
          cortemaximo<-substr(tabla[i-1,"TITULO"],1,tim-1)
          if (regexpr("\\-[^\\-]*$", cortemaximo)>0){
            corteimprim1<-paste0("\n  \n### ",substr(cortemaximo,1,regexpr("\\-[^\\-]*$", cortemaximo)))
            resto<-substr(tabla[i-1,"TITULO"],regexpr("\\-[^\\-]*$", cortemaximo)+1,largo)
          }
          if (resto>tim | (regexpr("\\-[^\\-]*$", cortemaximo)<0)){
            corteimprim1<-paste0("\n  \n### ",substr(cortemaximo,1,regexpr("\\ [^\\ ]*$", cortemaximo)))
            resto<-substr(tabla[i-1,"TITULO"],regexpr("\\ [^\\ ]*$",  cortemaximo)+1,largo)
          }
        }
        if (nchar(resto)>=tim){ #si el largo no alcanza en dos filas
          cortemaximo<-substr(resto,1,regexpr("\\ [^\\ ]*$", substr(resto,1,tim)))
          corteimprim2<-paste0("\n  \n### ",substr(cortemaximo,1,regexpr("\\ [^\\ ]*$", cortemaximo)))
          resto<-substr(resto,regexpr("\\ [^\\ ]*$", cortemaximo)+1,largo)
        }
        if (nchar(resto)<tim){
          corteimprim3<-paste0("\n  \n### ",substr(resto,1,largo))
        }
        if (largo<tim ){ 
          corteimprim3<-paste0("\n  \n### ",substr(resto,1,tim))
        }
    
    
    tabla[i-1,"indice"]<-paste0(corteimprim1,
                                corteimprim2,
                                corteimprim3,paste0(rep(".",tim-(nchar(resto)+nchar(as.character(tabla[i-1,"pag"]))+1)),collapse = ""),
                                tabla[i-1,"pag"])
  }
}

tabla[i,"indice"]<-paste0("Glosario",paste0(rep(".",(tim-(nchar("Glosario")+nchar(as.character(tabla[i-1,"pag"]))+1))),collapse = ""),as.numeric(tabla[i-1,"pag"])+1)
tabla[i,"Graph"]<-0

#############################################################################################
##############################Selección de Colores ##########################################
#############################################################################################
#opcion de colores, escala de azules, o escala desde amarillo, rojo, lilas
#coloresline<-c("#3C54A6","#6F7BB4","#848DBC","#979EC4","#A9AECB","#B9BDD2","#C8CAD8","#D4D5DD","#DDDEE0","#E2E2E2") 
coloresline<-c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2","#9e0142","#ffffbf")

#############################################################################################
##############################Función para gráficar##########################################
#############################################################################################

  plot<-function(gr){
    
    var<-TCodes[TCodes$Graph==gr,"VARIABLE"]
    Codes<-TCodes[TCodes$Graph==gr,]
    anos<-seq(as.numeric(graf[graf$Graph==gr,"Inicio"]),as.numeric(graf[graf$Graph==gr,"Fin"]),1)
    datgraf<-graf[graf$Graph==gr,]
    
    df <- data[data$VARIABLE %in% var,c("VARIABLE",paste0("X",anos))]
    df<-join(df,Codes[,c("VARIABLE","orden","ETIQUETA")],by="VARIABLE")

    #################################
    #Asignar etiquetas a las variables
    df<-df[order(as.numeric(df$orden)),]
    df$orden <- factor(df$orden, levels=as.vector(as.character(df$orden)), 
                       labels=as.vector(as.character(df$ETIQUETA)))
    df<-df[,colnames(df)!="ETIQUETA"]

    #Obtener titulo del grafico
    titulo<-as.character(graf[graf$Graph==gr,"TITULO"])
    titulo<-gsub("/", " \n ",titulo) 
    none <- element_blank()
    
    #############################################################################################
    ##############################Gráfico de barras##############################################
    #############################################################################################
    #Cuando se debe graficar barras
	if (!is.na(graf[graf$Graph==gr,"grafbarra"])){ 
      
		dft<-df[df$VARIABLE==Codes[!is.na(Codes$linea),"VARIABLE"],]
		df<-df[df$VARIABLE!=Codes[!is.na(Codes$linea),"VARIABLE"],]
		df<-df[df$VARIABLE==Codes[Codes$orden!="99","VARIABLE"],]
		dfm2<-melt(df,id.vars = c("VARIABLE","orden"), variable.name = "ANO", value.name = "VALOR")
		dfmt<-melt(dft,id.vars = c("VARIABLE","orden"), variable.name = "ANO", value.name = "total")
		dfm1<-join(dfm2,dfmt[,c("ANO","total")],by=c("ANO"))
		
		dfm1[is.na(dfm1$VALOR),"VALOR"]<-0 #asignar 0 a NA para calcular suma acumulada
		
		#Calcular total, porcentaje, valores para graficar etiquetas de porcentajes
		dfm <- dfm1  %>% group_by(ANO,VARIABLE,VALOR) %>%
		  group_by(ANO) %>%
		  mutate(pct = (VALOR / sum(VALOR,na.rm=TRUE)*100), 
				 pcttot=cumsum(pct),
				 fe=datgraf$ejeY1_max/datgraf$ejeY2_max,
				 totgr=total*fe,
				 npos=pcttot-3)
		
	  
		  #Ajustar ejes secundarios de totales
		dfm<-dfm[order(dfm$ANO,-as.numeric(dfm$orden)),]
		
		#primera version de grafico de barras con 2 ejes y etiqueta de valores 
		p1<-ggplot(dfm)  + 
		  geom_bar(aes(x=ANO, y=pct,fill=reorder(orden, -as.numeric(orden))),stat="identity")+
		  geom_line(aes(x=ANO,  y=(totgr),group=1,linetype=Codes[!is.na(Codes$linea),"ETIQUETA"]),size=1)+   
		  geom_point(aes(x=ANO,y=totgr,group=1,shape=Codes[!is.na(Codes$linea),"ETIQUETA"]),size=2) +
		  scale_y_continuous(datgraf$ejeY1_etiqueta,
							 breaks = seq(datgraf$ejeY1_min,datgraf$ejeY1_max,by=datgraf$ejeY1_interv),
							 labels = function(y) paste0(sprintf("%1.0f",y),"%"),
							 sec.axis = sec_axis(~./dfm$fe[1],
												breaks = seq(datgraf$ejeY2_min,datgraf$ejeY2_max,by=datgraf$ejeY2_interv),
												labels=function(x) format(x, big.mark = ".", scientific = FALSE),
												name=datgraf$ejeY2_etiqueta))+
		  scale_fill_manual(values=coloresline, guide=FALSE)
		
		
		#segunda version de grafico de barras eliminando etiqueta de valores inferior a 5%
		p2 <- p1 + 
		  scale_x_discrete(labels=anos) +
		  labs(panel.grid.major = none, panel.grid.minor = none) + 
		  labs(panel.background = none) + 
		  xlab(NULL) + ylab(NULL)+ 
		  background_grid(major = "y", minor = "none" )+
		  geom_text(aes(x=ANO,label=ifelse(pct<=5,"",paste0(sprintf("%1.0f",pct),"%")),y=npos), colour="black",size=3)
		
		#tercera version de grafico de barras ajuste de legenda, y ajuste de tamaños de grafico y tipo de letra  
		p3<-p2 + theme(legend.title=none,legend.position = "top",
					   legend.text=element_text(size=8),
					   legend.margin=margin(t=0.2,unit="line"),
					   legend.key.height=unit(0.2,"line"), 
					   axis.text = element_text(size = 8),
					   plot.margin=unit(c(0.1,0.2,-0.6,1.3), "cm"),
					   axis.title=element_text(size=8))
    }
	
    #############################################################################################
    ##############################Gráfico de lineas##############################################
    #############################################################################################
	#cuando el grafico es de lineas
    else if (!is.na(graf[graf$Graph==gr,"graflinea"])){
	
		dfm<-melt(df,id.vars = c("VARIABLE","orden"), variable.name = "ANO", value.name = "VALOR")
    
		#primera version del grafico de lineas valores y ejes 
		p1<-ggplot(dfm) + 
			geom_line(aes(x=ANO,  y=VALOR,group=orden, color=reorder(orden, -as.numeric(orden))),size=1)+  
			geom_point(aes(x=ANO,y=VALOR,color=orden,fill=orden),shape=15,size=2) +
			scale_y_continuous(datgraf$ejeY1_etiqueta,
							   breaks = seq(datgraf$ejeY1_min,datgraf$ejeY1_max,by=datgraf$ejeY1_interv), 
							   limits = c(datgraf$ejeY1_min,datgraf$ejeY1_max),
							   labels = function(y) format(y, big.mark = ".", scientific = FALSE))+
			scale_color_manual(values=coloresline)
		  
		#segunda version del grafico de lineas opciones de ejes
		p2<-p1 + 
			scale_x_discrete(labels=anos) +
			labs(panel.grid.major = none, panel.grid.minor = none) + 
			labs(panel.background = none) + 
			xlab(NULL) + ylab(NULL)+ 
			background_grid(major = "y", minor = "none" )
		
		#tercera version del grafico de lineas opciones de legenda
		p3<-p2 + theme(legend.title=none,legend.position = "none",
					 legend.text=element_text(size=8),
					 legend.margin=margin(t=0.2,unit="line"),
					 legend.key.height=unit(0.8,"line"), 
					 axis.text = element_text(size = 8),
					 plot.margin=unit(c(0.1,1.3,-0.2,1.6), "cm"),
					 axis.title=element_text(size=8))
    }
	
	#cambia formato de grafico
    pg <- ggplotGrob(p3)#transformar grafico a formato Grob
    
	#arregla el formato de grafico e incluye titulo
    grid.arrange(arrangeGrob(pg,
                             top=textGrob(titulo,gp=gpar(fontsize=10,font=2,col="black"),just = "center")))
  } #fin de la funcion plot
  
#############################################################################################
##############################Función para generar tablas####################################
#############################################################################################
#generar tabla que acompaña al grafico
  tablas<-function(gr){
    titulo<-as.character(graf[graf$Graph==gr,"TITULO"])
    titulo<-gsub("/", " \n ",titulo)
    
    var<-TCodes[TCodes$Graph==gr,"VARIABLE"]
    Codes<-TCodes[TCodes$Graph==gr,]
    anos<-seq(as.numeric(graf[graf$Graph==gr,"Inicio"]),as.numeric(graf[graf$Graph==gr,"Fin"]),1)
    
    df <- data[data$VARIABLE %in% var,c("VARIABLE",paste0("X",anos))]
    df<-join(df,Codes[,c("VARIABLE","orden","ETIQUETA")],by="VARIABLE")
    df<-df[order(df$orden),]
    
    #Asignar etiquetas a las variables
    df$orden <- factor(df$orden, levels=as.vector(as.character(df$orden)), 
                       labels=as.vector(as.character(df$ETIQUETA)))
    df<-df[,colnames(df)!="ETIQUETA"]
    
	#redondear valores decimales, si columna dec esta vacia la sintaxis se cae acá
    redond<-join(df,TCodes[TCodes$Graph==gr,c("VARIABLE","dec")],by="VARIABLE")
    d<-as.data.frame(round(redond[,paste0("X",anos)],redond$dec))
    m<-max(d,na.rm = TRUE)
    d<-apply(d,2,as.character)

	#asignar vacio a valores perdidos en la tabla
    if (nchar(m)>3 & (m-round(m))==0) {
      d<-prettyNum(d, big.mark=".",big.interval=3)
      d[d=="   NA"]<-""      
      d[d=="    NA"]<-""
    }
    d[is.na(d)]<-""
    if (length(nrow(d))==0) dformat<-as.data.frame(t(d)) else dformat<-as.data.frame(d)
    
    #excepcion para tabla de acreditación, cambia valor 1 a "Si" y 0 a "No" Si cambia orden ajustar acá el numero de grafico
    if (gr==30)  {
      dformat[,paste0("X",anos)] = apply(dformat[,paste0("X",anos)], 2, function(x) as.character(x))
      dformat[dformat=="1"]<-"Si"     
      dformat[dformat=="0"]<-"No"     
    }
    
	#igualar colores impresos en grafico de barra para que aparezcan en tabla segun orden establecido
    if (!is.na(graf[graf$Graph==gr,"grafbarra"])){
      rcolo<-c(rev(coloresline[1:nrow(dformat)-1]),"white")
     # if (gr==34)  rcolo<-c(rev(coloresline[1:(nrow(dformat)-3)]),rep("white",3)) #excepcion para tabla con mas variables que las graficadas
    } else if (!is.na(graf[graf$Graph==gr,"graflinea"])){
      rcolo<-c(rev(coloresline[1:nrow(dformat)]))
    } else rcolo<-c(rep("white",nrow(dformat)))
      
    #ajusta formato de tabla, colores y tamaño de fuente
    mythemegra <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = 0.6),
                    bg_params = list(fill = "white",#rcolo, 
                                     col=NA,alpha = rep(c(1,0.5), each=nrow(dformat)+1))),
        colhead = list(fg_params=list(cex = 0.6),
                       bg_params = list(fill ="white",col=NA)),
        rowhead = list(fg_params=list(cex = 0.6),
                       bg_params = list(fill =c("white",rcolo),
                                        col=NA,alpha = rep(c(1,0.5), each=nrow(dformat)+1))))
										
    #cambia formato de tabla para impresion, asigna formato
	table<-tableGrob(dformat,rows = c(levels(df$orden)),cols=anos,theme = mythemegra)

	#se dibujan las lineas en la tabla
	table <- gtable::gtable_add_grob(table,grobs = rectGrob(gp=gpar(fill=NA,lwd=2)), 
				   t = 1, b = nrow(table), l = 1, r =  ncol(table))
	table <- gtable::gtable_add_grob(table,grobs = rectGrob(gp=gpar(fill=NA,lwd=2)), 
				   t = 1, b = 1, l = 1, r =  ncol(table))
	table <- gtable::gtable_add_grob(table,grobs = rectGrob(gp=gpar(fill=NA,lwd=2)), 
				   t = 1, b = nrow(table), l =1, r = 1)
	for (c in 2:ncol(table)){
		table <- gtable::gtable_add_grob(table,grobs = rectGrob(gp=gpar(fill=NA,lwd=1)), 
											t = 1, b = nrow(table), l = c, r = c)
	}
	
	#se asigna tamaño de ancho de tabla, de acuerdo a la cantidad de años impresos
	table$widths <- unit(c(0.230769230769231,rep(0.769230769230769/(ncol(table)-1),ncol(table)-1)),"npc")

	#se fija alto de alta para que no supere un maximo a tener 7 variables
	if (nrow(table)>7) table$heights <- unit(c(0.09,rep((1-0.09)/(nrow(table)-1), nrow(table)-1)), "npc")
	else table$heights <- unit(rep(1/(nrow(table)), nrow(table)), "npc")

	#genera objeto de tabla
	grid.newpage()
	if (is.na(graf[graf$Graph==gr,"solotabla"])){
		grid.arrange(table)
	} else  grid.arrange(table,top=textGrob(titulo,gp=gpar(fontsize=10,font=2,col="black"),just = "center")) 

  } #fin de funcion tablas
  
 
