# Plotting TFG  
# Rafa, 23.02.2025
##############################

rm(list = ls()) #remove all the elements from your working enviroment
getwd() #gives you where you are
dir <- 'C:/Users/MNCN_RL/Archivos/C/Rafa/Courses/Historia/6/TFG/Analisis' #set the directory as an object 'dir'
setwd(file.path(dir)) #set working directory using the object and the subfolder

# Load libraries:
library(ggplot2)
library(dplyr)
library("RColorBrewer")
library(data.table)

library(reshape2)
library(tidyverse)
library(zoo)
library(vegan)
library(ggrepel)
#source("D:/Programs/scripts/mostAbundant.R")

##############################

### Import demographic_data ####
Barrios <- read.csv("./Datos_brutos/barrios.csv", header = TRUE, sep=";", encoding="UTF-8")
Distritos <- read.csv("./Datos_brutos/distritos.csv", header = TRUE, sep=";")

### Datos mascotas ####
Fuentes_masc <- read.csv("./Datos_brutos/fuentesmascotas202502.csv", header = TRUE, sep=";")
Papeleras_caninas <- read.csv("./Datos_brutos/papelerascaninas202502.csv", header = TRUE, sep=";")
Areas_caninas <- read.csv("./Datos_brutos/areas_caninas202502.csv", header = TRUE, sep=";")

Fuentes_masc$DISTRITO <- gsub(" - ","-", Fuentes_masc$DISTRITO)
Papeleras_caninas$DISTRITO <- gsub(" - ","-", Papeleras_caninas$DISTRITO)
Areas_caninas$DISTRITO <- gsub(" - ","-", Areas_caninas$DISTRITO)

#Format data

papeleras_summary <- Papeleras_caninas %>%
  group_by(DISTRITO,COD_DISTRITO,BARRIO,COD_BARRIO, ESTADO) %>%
  summarise(num_totales = n())

areas_caninas_summary <- Areas_caninas %>%
  group_by(DISTRITO,COD_DISTRITO,BARRIO,COD_BARRIO, ESTADO) %>%
  summarise(num_totales = n())

fuentes_summary <- Fuentes_masc %>%
  group_by(DISTRITO,COD_DISTRITO,BARRIO,COD_BARRIO, ESTADO) %>%
  summarise(num_totales = n())

papeleras_summary$tipo <- "papelera"
fuentes_summary$tipo <- "fuentes_mascotas"
areas_caninas_summary$tipo <- "áreas_caninas"

datos_mascotas <- rbind(papeleras_summary, areas_caninas_summary, fuentes_summary)
#datos_mascotas$DISTRITO <- gsub(" - ","-", datos_mascotas$DISTRITO)
datos_mascotas$BARRIO <- gsub("VILLAVERDE ALTO - CASCO HISTORICO DE VILLAVERDE","VILLAVERDE ALTO", datos_mascotas$BARRIO)

datos_mascotas$BARRIO <- factor(datos_mascotas$BARRIO, levels=c("PALACIO","EMBAJADORES","CORTES","JUSTICIA","UNIVERSIDAD","SOL","IMPERIAL","ACACIAS","CHOPERA","LEGAZPI","DELICIAS","PALOS DE LA FRONTERA","ATOCHA","PACIFICO","ADELFAS","ESTRELLA","IBIZA","LOS JERONIMOS","NIÑO JESUS","RECOLETOS","GOYA","FUENTE DEL BERRO","GUINDALERA","LISTA","CASTELLANA","EL VISO","PROSPERIDAD","CIUDAD JARDIN","HISPANOAMERICA","NUEVA ESPAÑA","CASTILLA","BELLAS VISTAS","CUATRO CAMINOS","CASTILLEJOS","ALMENARA","VALDEACEDERAS","BERRUGUETE","GAZTAMBIDE","ARAPILES","TRAFALGAR","ALMAGRO","RIOS ROSAS","VALLEHERMOSO","EL PARDO","FUENTELARREINA","PEÑAGRANDE","PILAR","LA PAZ","VALVERDE","MIRASIERRA","EL GOLOSO","CASA DE CAMPO","ARGÜELLES","CIUDAD UNIVERSITARIA","VALDEZARZA","VALDEMARIN","EL PLANTIO","ARAVACA","LOS CARMENES","PUERTA DEL ANGEL","LUCERO","ALUCHE","CAMPAMENTO","CUATRO VIENTOS","AGUILAS","COMILLAS","OPAÑEL","SAN ISIDRO","VISTA ALEGRE","PUERTA BONITA","BUENAVISTA","ABRANTES","ORCASITAS","ORCASUR","SAN FERMIN","ALMENDRALES","MOSCARDO","ZOFIO","PRADOLONGO","ENTREVIAS","SAN DIEGO","PALOMERAS BAJAS","PALOMERAS SURESTE","PORTAZGO","NUMANCIA","PAVONES","HORCAJO","MARROQUINA","MEDIA LEGUA","FONTARRON","VINATEROS","VENTAS","PUEBLO NUEVO","QUINTANA","LA CONCEPCIÓN","SAN PASCUAL","SAN JUAN BAUTISTA","COLINA","ATALAYA","COSTILLARES","PALOMAS","PIOVERA","CANILLAS","PINAR DEL REY","APOSTOL SANTIAGO","VALDEFUENTES","VILLAVERDE ALTO","SAN CRISTOBAL","BUTARQUE","LOS ROSALES","ANGELES","CASCO HISTORICO DE VALLECAS","SANTA EUGENIA","ENSANCHE DE VALLECAS","CASCO HISTORICO DE VICALVARO","VALDEBERNARDO","VALDERRIVAS","EL CAÑAVERAL","SIMANCAS","HELLIN","AMPOSTA","ARCOS","ROSAS","REJAS","CANILLEJAS","EL SALVADOR","ALAMEDA DE OSUNA","AEROPUERTO","CASCO HISTORICO DE BARAJAS","TIMON","CORRALEJOS"))
datos_mascotas$dato <- "bruto"

#Originales
#datos_mascotas$BARRIO <- factor(datos_mascotas$BARRIO, levels=c("PALACIO","EMBAJADORES","CORTES","JUSTICIA","UNIVERSIDAD","SOL","IMPERIAL","ACACIAS","CHOPERA","LEGAZPI","DELICIAS","PALOS DE LA FRONTERA","ATOCHA","PACIFICO","ADELFAS","ESTRELLA","IBIZA","LOS JERONIMOS","NIÑO JESUS","RECOLETOS","GOYA","FUENTE DEL BERRO","GUINDALERA","LISTA","CASTELLANA","EL VISO","PROSPERIDAD","CIUDAD JARDIN","HISPANOAMERICA","NUEVA ESPAÑA","CASTILLA","BELLAS VISTAS","CUATRO CAMINOS","CASTILLEJOS","ALMENARA","VALDEACEDERAS","BERRUGUETE","GAZTAMBIDE","ARAPILES","TRAFALGAR","ALMAGRO","RIOS ROSAS","VALLEHERMOSO","EL PARDO","FUENTELARREINA","PEÑAGRANDE","PILAR","LA PAZ","VALVERDE","MIRASIERRA","EL GOLOSO","CASA DE CAMPO","ARGÜELLES","CIUDAD UNIVERSITARIA","VALDEZARZA","VALDEMARIN","EL PLANTIO","ARAVACA","LOS CARMENES","PUERTA DEL ANGEL","LUCERO","ALUCHE","CAMPAMENTO","CUATRO VIENTOS","AGUILAS","COMILLAS","OPAÑEL","SAN ISIDRO","VISTA ALEGRE","PUERTA BONITA","BUENAVISTA","ABRANTES","ORCASITAS","ORCASUR","SAN FERMIN","ALMENDRALES","MOSCARDO","ZOFIO","PRADOLONGO","ENTREVIAS","SAN DIEGO","PALOMERAS BAJAS","PALOMERAS SURESTE","PORTAZGO","NUMANCIA","PAVONES","HORCAJO","MARROQUINA","MEDIA LEGUA","FONTARRON","VINATEROS","VENTAS","PUEBLO NUEVO","QUINTANA","LA CONCEPCIÓN","SAN PASCUAL","SAN JUAN BAUTISTA","COLINA","ATALAYA","COSTILLARES","PALOMAS","PIOVERA","CANILLAS","PINAR DEL REY","APOSTOL SANTIAGO","VALDEFUENTES","VILLAVERDE ALTO - CASCO HISTORICO DE VILLAVERDE","SAN CRISTOBAL","BUTARQUE","LOS ROSALES","ANGELES","CASCO HISTORICO DE VALLECAS","SANTA EUGENIA","ENSANCHE DE VALLECAS","CASCO HISTORICO DE VICALVARO","VALDEBERNARDO","VALDERRIVAS","EL CAÑAVERAL","SIMANCAS","HELLIN","AMPOSTA","ARCOS","ROSAS","REJAS","CANILLEJAS","EL SALVADOR","ALAMEDA DE OSUNA","AEROPUERTO","CASCO HISTORICO DE BARAJAS","TIMON","CORRALEJOS"))
#datos_mascotas$DISTRITO <- factor(datos_mascotas$BARRIO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS"))

datos_mascotas_km2 <- merge(datos_mascotas, Barrios, by.x="COD_BARRIO", by.y="cod_barrio", all.y=TRUE) 
datos_mascotas_km2$num_totales <- datos_mascotas_km2$num_totales/datos_mascotas_km2$superficie_km
datos_mascotas_km2 <- datos_mascotas_km2[,c(2:4,1,5:7)]

datos_mascotas_km2$dato <- "per_km2" 

datos_mascotas_summary <- rbind(datos_mascotas, datos_mascotas_km2)


#write.csv(datos_mascotas_summary, file="./Resultados/datos_mascostas_summary.csv", quote = FALSE, row.names = FALSE, col.names = TRUE, sep=";")

medias_faceta <- datos_mascotas_summary %>%
  group_by(tipo, dato) %>%
  summarise(media = median(num_totales, na.rm = TRUE), .groups = "drop")

datos_mascotas_graph <- ggplot(arrange(datos_mascotas_summary, COD_BARRIO),aes(x=BARRIO, y=num_totales)) +
  geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),
            stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas$DISTRITO)))) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=6, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  geom_hline(data = medias_faceta, aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_wrap( tipo ~ dato, scales="free_y", ncol=2) +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
datos_mascotas_graph

ggsave("./Resultados/datos_mascotas_barrios_juntos_w_legend.pdf", plot=datos_mascotas_graph, scale=3)

datos_mascotas_km2_graph <- ggplot(arrange(datos_mascotas_km2, COD_BARRIO),aes(x=BARRIO, y=num_totales)) +
  geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),
            stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas$DISTRITO)))) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=8, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_grid(tipo ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
datos_mascotas_km2_graph

ggsave("./Resultados/datos_mascotas_barrios_km2.pdf", plot=datos_mascotas_km2_graph, scale=3
       )

datos_mascotas_brutos <- ggplot(arrange(datos_mascotas, COD_BARRIO),aes(x=BARRIO, y=num_totales)) +
  geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),
            stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas$DISTRITO)))) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=8, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  theme(strip.text = element_text(color = "black", size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_grid(tipo ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
datos_mascotas_brutos

ggsave("./Resultados/datos_mascotas_barrios_brutos.pdf", plot=datos_mascotas_brutos, scale=3
)

# Data por distrito
  
datos_mascotas_distrito <- datos_mascotas %>%
  group_by(DISTRITO,COD_DISTRITO, ESTADO, tipo, dato) %>%
  summarise(num_totales = sum(num_totales))

datos_mascotas_distrito <- merge(datos_mascotas_distrito, Distritos, by.x="COD_DISTRITO",by.y="cod_distrito")
#datos_mascotas_distrito$per_num_perros_per_mil <- datos_mascotas_distrito$num_totales/datos_mascotas_distrito$Perros_2023*1000
datos_mascotas_distrito$per_km2 <- datos_mascotas_distrito$num_totales/datos_mascotas_distrito$superficie_km
datos_mascotas_distrito <- datos_mascotas_distrito[,c(1:4,6,20#,21
                                                      )]
datos_mascotas_distrito.m <- melt(datos_mascotas_distrito, id.vars=1:4)

medias_faceta_distrito <- datos_mascotas_distrito.m %>%
  group_by(tipo, variable) %>%
  summarise(media = median(value, na.rm = TRUE), .groups = "drop")

datos_mascotas_distrito.m$DISTRITO <- factor(datos_mascotas_distrito.m$DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS"))
  
datos_mascotas_distrito_graph <- ggplot(arrange(datos_mascotas_distrito.m, COD_DISTRITO),aes(x=DISTRITO, y=value)) +
  geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),
            stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas_distrito.m$DISTRITO)))) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  geom_hline(data = medias_faceta_distrito, aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_wrap (  tipo~ variable, scales="free_y", ncol=2) +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
datos_mascotas_distrito_graph

ggsave("./Resultados/datos_mascotas_distrito_combinado.pdf", plot=datos_mascotas_distrito_graph, scale=3
)

datos_mascotas_distrito_bruto_graph <- ggplot(arrange(subset(datos_mascotas_distrito.m, variable =="num_totales"),COD_DISTRITO),aes(x=DISTRITO, y=value)) +
  geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),
            stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas_distrito.m$DISTRITO)))) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_grid( tipo ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
datos_mascotas_distrito_bruto_graph

ggsave("./Resultados/datos_mascotas_distrito_bruto.pdf", plot=datos_mascotas_distrito_bruto_graph, scale=3
)

datos_mascotas_distrito_relative_graph <- ggplot(arrange(subset(datos_mascotas_distrito.m, variable =="per_num_perros_per_mil"), COD_DISTRITO),aes(x=DISTRITO, y=value)) +
  geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),
            stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas_distrito.m$DISTRITO)))) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=8, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_grid( tipo ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
datos_mascotas_distrito_relative_graph

ggsave("./Resultados/datos_mascotas_distrito_relative.pdf", plot=datos_mascotas_distrito_relative_graph, scale=3
)

#Medias

Medias_barrios_mascotas <- datos_mascotas_summary %>%
  group_by(tipo, dato) %>%
  summarise(Average_Value = mean(num_totales, na.rm = TRUE))

Medias_distrito_mascotas <- datos_mascotas_distrito.m %>%
  group_by(tipo, variable) %>%
  summarise(Average_Value = mean(value, na.rm = TRUE))

### Bancos ####

Bancos <- read.csv("./Datos_brutos/bancos202502.csv", header = TRUE, sep=";")

Bancos$DISTRITO <- gsub(" - ","-", Bancos$DISTRITO)

Bancos_summary <- Bancos %>%
  group_by(DISTRITO,COD_DISTRITO,BARRIO,COD_BARRIO) %>%
  summarise(num_totales = n())

Bancos_summary <- merge(Bancos_summary, Barrios, by.x="COD_BARRIO", by.y = "cod_barrio")
Bancos_summary <- Bancos_summary[,c(1:5,11,14:18)]
Bancos_summary$BARRIO <- gsub("VILLAVERDE ALTO - CASCO HISTORICO DE VILLAVERDE","VILLAVERDE ALTO", Bancos_summary$BARRIO)
Bancos_summary$BARRIO <- factor(Bancos_summary$BARRIO, levels=c("PALACIO","EMBAJADORES","CORTES","JUSTICIA","UNIVERSIDAD","SOL","IMPERIAL","ACACIAS","CHOPERA","LEGAZPI","DELICIAS","PALOS DE LA FRONTERA","ATOCHA","PACIFICO","ADELFAS","ESTRELLA","IBIZA","LOS JERONIMOS","NINO JESUS","RECOLETOS","GOYA","FUENTE DEL BERRO","GUINDALERA","LISTA","CASTELLANA","EL VISO","PROSPERIDAD","CIUDAD JARDIN","HISPANOAMERICA","NUEVA ESPANA","CASTILLA","BELLAS VISTAS","CUATRO CAMINOS","CASTILLEJOS","ALMENARA","VALDEACEDERAS","BERRUGUETE","GAZTAMBIDE","ARAPILES","TRAFALGAR","ALMAGRO","RIOS ROSAS","VALLEHERMOSO","EL PARDO","FUENTELARREINA","PENAGRANDE","PILAR","LA PAZ","VALVERDE","MIRASIERRA","EL GOLOSO","CASA DE CAMPO","ARGUELLES","CIUDAD UNIVERSITARIA","VALDEZARZA","VALDEMARIN","EL PLANTIO","ARAVACA","LOS CARMENES","PUERTA DEL ANGEL","LUCERO","ALUCHE","CAMPAMENTO","CUATRO VIENTOS","AGUILAS","COMILLAS","OPANEL","SAN ISIDRO","VISTA ALEGRE","PUERTA BONITA","BUENAVISTA","ABRANTES","ORCASITAS","ORCASUR","SAN FERMIN","ALMENDRALES","MOSCARDO","ZOFIO","PRADOLONGO","ENTREVIAS","SAN DIEGO","PALOMERAS BAJAS","PALOMERAS SURESTE","PORTAZGO","NUMANCIA","PAVONES","HORCAJO","MARROQUINA","MEDIA LEGUA","FONTARRON","VINATEROS","VENTAS","PUEBLO NUEVO","QUINTANA","LA CONCEPCION","SAN PASCUAL","SAN JUAN BAUTISTA","COLINA","ATALAYA","COSTILLARES","PALOMAS","PIOVERA","CANILLAS","PINAR DEL REY","APOSTOL SANTIAGO","VALDEFUENTES","VILLAVERDE ALTO","SAN CRISTOBAL","BUTARQUE","LOS ROSALES","ANGELES","CASCO HISTORICO DE VALLECAS","SANTA EUGENIA","ENSANCHE DE VALLECAS","CASCO HISTORICO DE VICALVARO","VALDEBERNARDO","VALDERRIVAS","EL CANAVERAL","SIMANCAS","HELLIN","AMPOSTA","ARCOS","ROSAS","REJAS","CANILLEJAS","EL SALVADOR","ALAMEDA DE OSUNA","AEROPUERTO","CASCO HISTORICO DE BARAJAS","TIMON","CORRALEJOS"))
Bancos_summary$bancos_km2 <- Bancos_summary$num_totales/Bancos_summary$superficie_km
Bancos_summary$bancos_habit_per_mil <- Bancos_summary$num_totales/Bancos_summary$num_personas*1000
Bancos_summary$bancos_personas_km2 <- Bancos_summary$bancos_habit_per_mil/Bancos_summary$superficie_km

Bancos_summary.m <- melt(Bancos_summary, measure.vars=c(5,12,13,14))

medias_faceta_banco <- Bancos_summary.m %>%
  group_by(variable) %>%
  summarise(media = median(value, na.rm = TRUE), .groups = "drop")

bancos_graph <- ggplot(arrange(Bancos_summary.m, COD_BARRIO),aes(x=BARRIO, y=value)) +
  geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary.m$DISTRITO)))) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=6, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 12, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = medias_faceta_banco, aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_grid( variable ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
bancos_graph

ggsave("./Resultados/datos_bancos_barrios_juntos_wo_Legend.pdf", plot=bancos_graph, scale=3)

#Ordenar según la renta

bancos_descend <- ggplot(Bancos_summary.m,aes(x= reorder(BARRIO, desc(Renta_disponible_media_por_persona_2020_barrio)), y=value)) +
  geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),stat='identity',position="dodge", color="black") +
  #geom_bar (aes(fill =Renta_disponible_media_por_persona_2020_barrio  ),stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary.m$DISTRITO)))) +
  #scale_fill_gradientn(colours=terrain.colors(6)) +
  #scale_colour_brewer(palette = "Paired") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=6, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = medias_faceta_banco, aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + #theme(legend.position="none") +
  facet_grid( variable ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
bancos_descend

ggsave("./Resultados/datos_bancos_barrios_juntos_desc_renta.pdf", plot=bancos_descend, scale=3)

Bancos_summary.m_trial <- Bancos_summary.m
Bancos_summary.m_trial <- Bancos_summary.m_trial %>%
  mutate(BARRIO = factor(BARRIO, levels = Bancos_summary.m_trial %>%
                       filter(variable == "num_totales") %>%
                       arrange(desc(value)) %>%
                       pull(BARRIO)))


bancos_descend_renta <- ggplot(Bancos_summary.m_trial,aes(x= BARRIO, y=value)) +
  #geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),stat='identity',position="dodge", color="black") +
  geom_bar (aes(fill =Renta_disponible_media_por_persona_2020_barrio  ),stat='identity',position="dodge", color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas$DISTRITO)))) +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=6, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = medias_faceta_banco, aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + #theme(legend.position="none") +
  facet_grid( variable ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")
bancos_descend_renta

ggsave("./Resultados/datos_bancos_barrios_juntos_desc_num_total.pdf", plot=bancos_descend_renta, scale=3)

medias_faceta_banco_barrio <- subset(Bancos_summary.m, variable =="num_totales") %>%
  group_by(variable, DISTRITO) %>%
  summarise(media = median(value, na.rm = TRUE), .groups = "drop")

bancos_distrito <- ggplot((subset(Bancos_summary.m, variable =="num_totales")),aes(x= as.character(COD_BARRIO), y=value)) +
  #geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),stat='identity',position="dodge", color="black") +
  geom_bar (aes(fill =Renta_disponible_media_por_persona_2020_barrio  ),stat='identity',position="dodge", color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas$DISTRITO)))) +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  theme_classic() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=6, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 11, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_banco, variable =="num_totales"), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + #theme(legend.position="none") +
  facet_wrap( DISTRITO ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")
bancos_distrito

ggsave("./Resultados/datos_bancos_barrios_distrito_num_total.pdf", plot=bancos_distrito, scale=3)

bancos_distrito_barrio_km2 <- ggplot((subset(Bancos_summary.m, variable =="bancos_km2")),aes(x= as.character(COD_BARRIO), y=value)) +
  #geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),stat='identity',position="dodge", color="black") +
  geom_bar (aes(fill =Renta_disponible_media_por_persona_2020_barrio  ),stat='identity',position="dodge", color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas$DISTRITO)))) +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  theme_classic() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=6, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 11, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_banco, variable =="bancos_km2"), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + #theme(legend.position="none") +
  facet_wrap( DISTRITO ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")
bancos_distrito_barrio_km2

ggsave("./Resultados/datos_bancos_barrios_distrito_km2.pdf", plot=bancos_distrito_barrio_km2, scale=3)

bancos_distrito_barrio_habitante <- ggplot((subset(Bancos_summary.m, variable =="bancos_habit_per_mil")),aes(x= as.character(COD_BARRIO), y=value)) +
  #geom_bar (aes(fill = (factor(DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS")))),stat='identity',position="dodge", color="black") +
  geom_bar (aes(fill =Renta_disponible_media_por_persona_2020_barrio  ),stat='identity',position="dodge", color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(datos_mascotas$DISTRITO)))) +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  theme_classic() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=6, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 11, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_banco, variable =="bancos_habit_per_mil"), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + #theme(legend.position="none") +
  facet_wrap( DISTRITO ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")
bancos_distrito_barrio_habitante

ggsave("./Resultados/datos_bancos_barrios_distrito_habitantes.pdf", plot=bancos_distrito_barrio_habitante, scale=3)


#Bancos_distrito

Bancos_summary_distrito <- Bancos %>%
  group_by(DISTRITO,COD_DISTRITO) %>%
  summarise(num_totales = n())

Bancos_summary_distrito <- merge(Bancos_summary_distrito, Distritos, by.x="COD_DISTRITO", by.y = "cod_distrito")
Bancos_summary_distrito <- Bancos_summary_distrito[,c(1:3,9,12:14)]
Bancos_summary_distrito$DISTRITO <- factor(Bancos_summary_distrito$DISTRITO, levels=c("CENTRO","ARGANZUELA","RETIRO","SALAMANCA","CHAMARTIN","TETUAN","CHAMBERI","FUENCARRAL-EL PARDO","MONCLOA-ARAVACA","LATINA","CARABANCHEL","USERA","PUENTE DE VALLECAS","MORATALAZ","CIUDAD LINEAL","HORTALEZA","VILLAVERDE","VILLA DE VALLECAS","VICALVARO","SAN BLAS-CANILLEJAS","BARAJAS"))
Bancos_summary_distrito$bancos_km2 <- Bancos_summary_distrito$num_totales/Bancos_summary_distrito$superficie_km
Bancos_summary_distrito$bancos_habit_per_mil <- Bancos_summary_distrito$num_totales/Bancos_summary_distrito$num_personas*1000
Bancos_summary_distrito$bancos_personas_km2 <- Bancos_summary_distrito$bancos_habit_per_mil/Bancos_summary_distrito$superficie_km

#write.csv(Bancos_summary_distrito, file="./Resultados/Bancos_summary.csv", quote=FALSE, dec=",", sep=";", col.names = TRUE, row.names = FALSE)

Bancos_summary_distrito.m <- melt(Bancos_summary_distrito, measure.vars=c(3,8,9,10))

medias_faceta_distrito <- Bancos_summary_distrito.m %>%
  group_by(variable) %>%
  summarise(media = median(value, na.rm = TRUE), .groups = "drop")

bancos_distrito_total <- ggplot(Bancos_summary_distrito.m, aes(x=DISTRITO, y=value)) +
  #geom_bar (aes(fill = DISTRITO),stat='identity', color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary_distrito.m$DISTRITO)))) +
  geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 10, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_distrito), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + #theme(legend.position="none") +
  facet_grid( variable ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")

bancos_distrito_total

ggsave("./Resultados/datos_bancos_distritos_juntos_w_legend.pdf", plot=bancos_distrito_total, scale=3)

#Ordenar según la renta

bancos_distrito_total_renta_descend <- ggplot(Bancos_summary_distrito.m, aes(x=reorder(DISTRITO, desc(Renta_bruta_media_por_persona_2022_INE_distrito)), y=value)) +
  #geom_bar (aes(fill = DISTRITO),stat='identity', color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary_distrito.m$DISTRITO)))) +
  geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 10, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_distrito), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_wrap( variable ~., scales="free", nrow=2) +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")
bancos_distrito_total_renta_descend

ggsave("./Resultados/datos_bancos_distritos_juntos_renta_descend.pdf", plot=bancos_distrito_total_renta_descend, scale=3)

bancos_distrito_total_descend <- ggplot(subset(Bancos_summary_distrito.m, variable=="bancos_km2"), aes(x=reorder(DISTRITO, desc(value)), y=value)) +
  #geom_bar (aes(fill = DISTRITO),stat='identity', color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary_distrito.m$DISTRITO)))) +
  geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 10, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_distrito, variable =="bancos_km2"), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_wrap( variable ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")
bancos_distrito_total_descend

ggsave("./Resultados/datos_bancos_distritos_km2_descend.pdf", plot=bancos_distrito_total_descend, scale=3)

bancos_distrito_total_descend_habit <- ggplot(subset(Bancos_summary_distrito.m, variable=="bancos_habit_per_mil"), aes(x=reorder(DISTRITO, desc(value)), y=value)) +
  #geom_bar (aes(fill = DISTRITO),stat='identity', color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary_distrito.m$DISTRITO)))) +
  geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 10, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_distrito, variable =="bancos_habit_per_mil"), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_wrap( variable ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")
bancos_distrito_total_descend_habit

ggsave("./Resultados/datos_bancos_distritos_habitantes_descend.pdf", plot=bancos_distrito_total_descend_habit, scale=3)

bancos_distrito_total_descend_num_totales <- ggplot(subset(Bancos_summary_distrito.m, variable=="num_totales"), aes(x=reorder(DISTRITO, desc(value)), y=value)) +
  #geom_bar (aes(fill = DISTRITO),stat='identity', color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary_distrito.m$DISTRITO)))) +
  geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 10, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_distrito, variable =="num_totales"), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_wrap( variable ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")
bancos_distrito_total_descend_num_totales

ggsave("./Resultados/datos_bancos_distritos_num_totales_descend.pdf", plot=bancos_distrito_total_descend_num_totales, scale=3)

bancos_distrito_point <- ggplot(Bancos_summary_distrito.m, aes(x=Renta_bruta_media_por_persona_2022_INE_distrito, y=value)) +
  #geom_point (aes(fill = DISTRITO), shape=21, size=8, colour="black") +
  geom_point (aes(fill = DISTRITO, size=num_personas), shape=21, colour="black") +
  scale_size(range = c(5,12)) +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary_distrito.m$DISTRITO)))) +
  #geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  #scale_fill_gradientn(colours=terrain.colors(6)) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(size=12, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  #scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  ylab("Número de elementos \n") +
  #xlab("\n") + theme(legend.position="none") +
  theme(axis.title.x = element_text(size=12, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  facet_wrap( variable ~., scales="free") +
  geom_hline(data = subset(medias_faceta_distrito), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
bancos_distrito_point

ggsave("./Resultados/datos_bancos_distritos_points_size_population.pdf", plot=bancos_distrito_point, scale=3)

bancos_distrito_km2_point <- ggplot(subset(Bancos_summary_distrito.m, variable=="bancos_km2"), aes(x=Renta_bruta_media_por_persona_2022_INE_distrito, y=value)) +
  geom_point (aes(fill = DISTRITO), shape=21, size=8, colour="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary_distrito.m$DISTRITO)))) +
  #geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  #scale_fill_gradientn(colours=terrain.colors(6)) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(size=12, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  #scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  ylab("Número de elementos \n") +
  #xlab("\n") + theme(legend.position="none") +
  theme(axis.title.x = element_text(size=12, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  facet_wrap( variable ~., scales="free") +
  geom_hline(data = subset(medias_faceta_distrito, variable =="bancos_km2"), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
bancos_distrito_km2_point

ggsave("./Resultados/datos_bancos_distritos_km2_points.pdf", plot=bancos_distrito_km2_point, scale=3)



### Áreas infantiles, de mayores y circuitos deportivos ####

#Areas <- read.csv("./Datos_brutos/areas_deportivas_infantiles_mayores.csv", header = TRUE, sep=";", fileEncoding = "Latin1")
#Areas$DISTRITO <- gsub(" - ","-", Areas$DISTRITO)

#Areas_summary <- Areas %>%
 # group_by(DISTRITO,COD_DISTRITO,BARRIO,COD_BARRIO,TIPO) %>%
 # summarise(num_totales = n())

Areas <- read.table("./Datos_brutos/areas_deportivas_infantiles_mayores.txt", header = TRUE, sep="\t", fileEncoding = "Latin1")

Areas_summary <- merge(Areas, Barrios, by.x="COD_BARRIO", by.y="cod_barrio")
Areas_summary <- Areas_summary[,c(1:4,7:10,13:17)]
Areas_summary$barrio <- gsub("VillaverdeAlto.CascoHistoricodeVillaverde","VillaverdeAlto", Areas_summary$barrio)
Areas_summary$barrio <- factor(Areas_summary$barrio, levels=c("Palacio","Embajadores","Cortes","Justicia","Universidad","Imperial","Acacias","Chopera","Legazpi","Delicias","PalosdeMoguer","Atocha","Pacifico","Adelfas","Estrella","LosJeronimos","NinoJesus","Goya","FuentedelBerro","Guindalera","Lista","Castellana","ElViso","Prosperidad","CiudadJardin","Hispanoamerica","NuevaEspana","Castilla","BellasVistas","CuatroCaminos","Castillejos","Almenara","Valdeacederas","Berruguete","Gaztambide","Arapiles","Trafalgar","Almagro","RiosRosas","Vallehermoso","ElPardo","Fuentelarreina","Penagrande","Pilar","LaPaz","Valverde","Mirasierra","ElGoloso","CasadeCampo","CiudadUniversitaria","Valdezarza","Valdemarin","ElPlantio","Aravaca","LosCarmenes","Puertadelangel","Lucero","Aluche","Campamento","CuatroVientos","aguilas","Comillas","Opanel","SanIsidro","VistaAlegre","PuertaBonita","Buenavista","Abrantes","Orcasitas","Orcasur","SanFermin","Almendrales","Moscardo","Zofio","Pradolongo","Entrevias","SanDiego","PalomerasBajas","PalomerasSureste","Portazgo","Numancia","Pavones","Horcajo","Marroquina","MediaLegua","Fontarron","Vinateros","Ventas","PuebloNuevo","Quintana","Concepcion","SanPascual","SanJuanBautista","Colina","Atalaya","Costillares","Palomas","Piovera","Canillas","PinardelRey","ApostolSantiago","Valdefuentes","VillaverdeAlto","SanCristobal","Butarque","LosRosales","angeles","CascoHistoricodeVallecas","SantaEugenia","EnsanchedeVallecas","CascoHistoricodeVicalvaro","Valdebernardo","Valderrivas","ElCanaveral","Simancas","Hellin","Amposta","Arcos","Rosas","Rejas","Canillejas","ElSalvador","AlamedadeOsuna","Aeropuerto","CascoHistoricodeBarajas","Timon","Corralejos"))
Areas_summary <- melt(Areas_summary, measure.vars=c(2,3,4#,13,14,15
))
Areas_summary$areas_km2 <- Areas_summary$value/Areas_summary$superficie_km
Areas_summary$areas_habit_per_mil <- Areas_summary$value/Areas_summary$num_personas*10000
Areas_summary$areas_personas_km2 <- Areas_summary$areas_habit_per_mil/Areas_summary$superficie_km

Areas_summary$distrito <- gsub("^ ", "", x = Areas_summary$distrito)

Areas_summary <- melt(Areas_summary, measure.vars=c(12,13,14,15#,13,14,15
), variable.name = "data_type")

medias_faceta_areas <- Areas_summary %>%
  group_by(variable, data_type) %>%
  summarise(media = median(value, na.rm = TRUE), .groups = "drop")

areas_graph <- ggplot(arrange(filter(Areas_summary, data_type == "value"), COD_BARRIO),aes(x=barrio, y=value)) +
  geom_bar (aes(fill = (factor(distrito, levels=c("Centro","Arganzuela","Retiro","Salamanca","Chamartin","Tetuan","Chamberi","Fuencarral-El Pardo","Moncloa-Aravaca","Latina","Carabanchel","Usera","Puente de Vallecas","Moratalaz","Ciudad Lineal","Hortaleza","Villaverde","Villa de Vallecas","Vicalvaro","San Blas-Canillejas","Barajas")))),stat='identity',position="dodge", color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Areas_summary$distrito)))) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=6, face="bold", color="black")) +
  #theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 12, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = filter(medias_faceta_areas, data_type == "value"), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") +
  facet_grid(variable ~., scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
areas_graph

ggsave("./Resultados/datos_areas_barrios_juntos_wo_Legend.pdf", plot=areas_graph, scale=2.25)


#Areas_distrito

Areas_summary_distrito <- filter(Areas_summary, data_type == "value") %>%
  group_by(distrito,cod_distrito,variable) %>%
  summarise(num_totales = sum(value))

Areas_summary_distrito <- merge(Areas_summary_distrito, Distritos, by = "cod_distrito")
Areas_summary_distrito <- Areas_summary_distrito[,c(1:4,10,13:15)]
Areas_summary_distrito$distrito.x <- factor(Areas_summary_distrito$distrito.x, levels=c("Centro","Arganzuela","Retiro","Salamanca","Chamartin","Tetuan","Chamberi","Fuencarral-El Pardo","Moncloa-Aravaca","Latina","Carabanchel","Usera","Puente de Vallecas","Moratalaz","Ciudad Lineal","Hortaleza","Villaverde","Villa de Vallecas","Vicalvaro","San Blas-Canillejas","Barajas"))
Areas_summary_distrito$areas_km2 <- Areas_summary_distrito$num_totales/Areas_summary_distrito$superficie_km
Areas_summary_distrito$areas_habit_per_mil <- Areas_summary_distrito$num_totales/Areas_summary_distrito$num_personas*1000
Areas_summary_distrito$areas_personas_km2 <- Areas_summary_distrito$areas_habit_per_mil/Areas_summary_distrito$superficie_km

Areas_summary_distrito <- melt(Areas_summary_distrito, measure.vars=c(4,9,10,11), variable.name = "TIPO")

medias_faceta_distrito_areas <- Areas_summary_distrito %>%
  group_by(variable,TIPO) %>%
  summarise(media = median(value, na.rm = TRUE), .groups = "drop")

areas_distrito_total <- ggplot(Areas_summary_distrito, aes(x=distrito.x, y=value)) +
  geom_bar (aes(fill = distrito.x),stat='identity', color="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Areas_summary_distrito$distrito.x)))) +
  #geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  #scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(strip.text = element_text(color = "black", size = 10, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_distrito_areas), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + #theme(legend.position="none") +
  facet_wrap( variable ~ TIPO, scales="free") +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) + labs(fill="Renta")

areas_distrito_total

ggsave("./Resultados/datos_areas_distritos_juntos_w_legend.pdf", plot=areas_distrito_total, scale=3)

#Ordenar según la renta

area_distrito_total_renta_descend <- ggplot(Areas_summary_distrito, aes(x=reorder(distrito.x, desc(Renta_bruta_media_por_persona_2022_INE_distrito)), y=value)) +
  #geom_bar (aes(fill = DISTRITO),stat='identity', color="black") +
  #scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Bancos_summary_distrito.m$DISTRITO)))) +
  geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  scale_fill_gradientn(colours=brewer.pal(11, "RdYlGn")) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold", color="black")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
    theme(strip.text = element_text(color = "black", size = 10, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  geom_hline(data = subset(medias_faceta_distrito_areas), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  ylab("Número de elementos \n") +
  xlab("\n") + theme(legend.position="none") + labs(fill="Renta") +
  facet_wrap( variable ~TIPO, scales="free", nrow=3) +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12)) 
area_distrito_total_renta_descend

ggsave("./Resultados/datos_areas_distritos_juntos_renta_descend_wo_legend.pdf", plot=area_distrito_total_renta_descend, scale=3)

areas_distrito_point <- ggplot(Areas_summary_distrito, aes(x=Renta_bruta_media_por_persona_2022_INE_distrito, y=value)) +
  geom_point (aes(fill = distrito.x, size=num_personas), shape=21, colour="black") +
  scale_fill_manual("Distrito", values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(Areas_summary_distrito$distrito.x)))) +
  scale_size(range = c(3,8)) +
  #geom_bar (aes(fill =Renta_bruta_media_por_persona_2022_INE_distrito  ),stat='identity',position="dodge", color="black") +
  #scale_fill_gradientn(colours=terrain.colors(6)) +
  #scale_colour_brewer(palette = "Paired") +
  #scale_fill_manual("Season", values=figure_col_general) +
  theme_classic() +
  theme(axis.text.x = element_text(size=12, face="bold", color="black")) +
  theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  #scale_x_discrete(expand = c(0, 0.6)) + scale_y_continuous(expand = c(0, 0))+
  ylab("Número de elementos \n") +
  #xlab("\n") + theme(legend.position="none") +
  theme(axis.title.x = element_text(size=12, face="bold", color="black")) +
  theme(axis.title.y = element_text(size=12, face="bold", color="black")) +
  facet_wrap( variable ~TIPO, scales="free") +
  geom_hline(data = subset(medias_faceta_distrito_areas), aes(yintercept = media), 
             color = "black", linetype = "dashed", linewidth = 0.5, inherit.aes = FALSE) +
  theme(legend.text = element_text(size=12), legend.title =element_text(size=12))
areas_distrito_point

ggsave("./Resultados/datos_areas_distritos_points_size_population.pdf", plot=areas_distrito_point, scale=3)

