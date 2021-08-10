library(dplyr)
library(stringr)

# tabla de frecuencias datos iniciales
data<-read.csv("data.csv", encoding="UTF-8")
data$PROPORCION_O_IMPORT
E<-factor(data$PROPORCION_O_IMPORTE)
tabla<-(as.matrix(table(data$PROPORCION_O_IMPORTE)))
tabla<-as.data.frame(tabla)






# tabla de tipos de variables para datos iniciales
orden<-as.data.frame(data$PROPORCION_O_IMPORT)
colnames(orden)<-c("variable")
orden$variable<-as.character(orden$variable)
orden<-orden %>% mutate(clasific=ifelse(str_detect(variable,"RATIO BRUTO") ,"ratio",
                                 ifelse(str_detect(variable,"IMPORTE ") ,"importe",
                                 ifelse(str_detect(variable,"FUSIÃ“N") ,"fusion",
                                 ifelse(str_detect(variable,"NO SE REC") ,"no_intereses",
                                 ifelse(str_detect(variable,"SE ") ,"acto",
                                 ifelse(str_detect(variable," X ") ,"intercambio",
                                 ifelse(str_detect(variable," POR ") ,"intercambio",
                                 ifelse(str_detect(variable,"DIVIDENDO ") ,"dividendo",
                                 ifelse(str_detect(variable,"REDISTRIBUCIÃ“N ") ,"acto",
                                 ifelse(str_detect(variable,"ACCION") ,"acciones",
                                 ifelse(str_detect(variable,"SERIE") ,"series",
                                 ifelse(str_detect(variable,"NO DIST") ,"no_distribution",
                                 ifelse(str_detect(variable,"SIN DIST") ,"no_distribution",
                                 ifelse(str_detect(variable,"PENDIENTE") ,"pendiente_informar",
                                 ifelse(str_detect(variable,"OPTION") ,"option",
                                 ifelse(str_detect(variable,"A ") ,"tasa de cambio","otro")))))))))))))))))%>%
                 mutate(first3= substr(variable,1,3)) %>% 
                 mutate (clasific=ifelse(clasific == "otro"&(str_detect(first3,"MXN")|str_detect(first3,"USD")),"valor",clasific))

# tabla de frecuencias de la claficacion para datos iniciales
tabla_final<-orden %>% 
  group_by(clasific) %>% 
  summarise(frec = n()) %>% mutate (prop=frec/21749)

# II. nuevos datos con la nueva columna de Julian
# filtramos data
new_data<-read.csv("new_data.csv", encoding="UTF-8")%>% select(PROPORCION_O_IMPORTE,TipoValorStr)
new_data$TipoValorStr<-as.factor(new_data$TipoValorStr)
levels(new_data$TipoValorStr)
subset<-c("0",
          "1 ",
          "1B",
          "1E",
          "3 ",
          "41",
          "CF",
          "FE",
          "FF",
          "FH")
dividendos<-new_data%>% filter(TipoValorStr %in% subset)

# creo clasificacion
colnames(dividendos)<-c("variable","tipo")
orden<-dividendos %>% mutate(clasific=ifelse(str_detect(variable,"RATIO BRUTO") ,"ratio",
                                        ifelse(str_detect(variable,"IMPORTE ") ,"importe",
                                               ifelse(str_detect(variable,"FUSIÓN") ,"fusion",
                                                      ifelse(str_detect(variable,"NO SE REC") ,"no_intereses",
                                                             ifelse(str_detect(variable,"SE ") ,"acto",
                                                                    ifelse(str_detect(variable,"ACCION") ,"acciones",
                                                                    ifelse(str_detect(variable,"SERIE") ,"series",
                                                                    ifelse(str_detect(variable," X ") ,"intercambio",
                                                                           ifelse(str_detect(variable," POR ") ,"intercambio",
                                                                                  ifelse(str_detect(variable,"DIVIDENDO ") ,"dividendo",
                                                                                         ifelse(str_detect(variable,"REDISTRIBUCIÓN ") ,"acto",
                                                                                                  ifelse(str_detect(variable,"NO DIST") ,"no_distribution",
                                                                                                                     ifelse(str_detect(variable,"SIN DIST") ,"no_distribution",
                                                                                                                            ifelse(str_detect(variable,"PENDIENTE") ,"pendiente_informar",
                                                                                                                                   ifelse(str_detect(variable,"OPTION") ,"option",
                                                                                                                                          ifelse(str_detect(variable,"A ") ,"tasa de cambio","otro")))))))))))))))))%>%
  mutate(first3= substr(variable,1,3)) %>% 
  mutate (clasific=ifelse(clasific == "otro"&(str_detect(first3,"MXN")|str_detect(first3,"USD")|str_detect(first3,"EUR")),"valor",clasific))

orden$first3<- NULL
orden<-orden%>% arrange(tipo)

# creo tabla de frecuencias de la claificacion con los nuevos datos

tabla_final<-orden %>% 
  group_by(clasific,tipo) %>% 
  summarise(frec = n()) %>% arrange(tipo)


# CREO JOIN CON EJEMPLOS
heads<-aggregate(variable ~ clasific+tipo, data=orden, head, 1)
ejemplos<-merge(tabla_final,heads,  by = c ("clasific", "tipo"))%>% arrange(tipo)



# escribo los datos

write.csv(tabla_final,"dividendos_frecuencias")
write.csv(orden,"dividendos")
write.csv(ejemplos,"clasif_der_ejemplos")
