library("pacman")
p_load("vroom",
       "tidyverse",
       "dplyr",
       "ggplot2",
       "ggsci",
       "scales",
       "Rmisc") #Para summaryES

#Para descargar datos del repositorio de github abrir el repositorio que debe ser público
#abrir el archivo y poner en "Raw"
#copiar información de RAW
#datos obtenidos del siguiente dato http://www.ugr.es/~bioestad/_private/Practica_4.pdf

glucosa.data <- vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/tratamientoglucosa/main/Glucosa")

head(glucosa.data)

densidad1 <- glucosa.data %>% 
  ggplot(glucosa.data, 
         mapping = aes(x=Valores,
                       fill= TratMed,
                       color=TratMed))+
  geom_density(alpha=0.2)+
  xlim(30,110)
densidad1

"gráfica de barra"
head(glucosa.data)
resumen <-  summarySE(glucosa.data, measurevar="Valores", groupvars=c("TratMed"))

resumen

graf1 <- resumen %>% 
  ggplot(mapping = aes(x=TratMed,
                       y=Valores, 
                       fill=TratMed))+
  geom_bar(stat = "identity",colour="black", size=.8)+
  theme_classic()+
  ylab("Valores de glucosa \n (ug/dL)")+
  xlab("Condición")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))
graf1

resumen

graf2 <- graf1+
  scale_y_continuous(breaks = seq(from=0, 
                                  to=150, 
                                  by=20),
                   labels=comma)+
  geom_errorbar(aes(x=, ymin=Valores-se, ymax=Valores+se),
                width=0.2, 
                colour="black", 
                alpha=1, 
                size=.8)
graf2

graf3 <- graf2+
  theme(plot.title= element_text(size=10))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_aaas()+
  scale_x_discrete(expand = c(0.2,0))


graf3

miny=0
maxy=120

marcasy <- seq(from=miny,
               to=maxy,
               by=20)
marcasy
etiquetasy <- comma(marcasy)
etiquetasy

graf4 <- graf3+
  scale_y_continuous(limits=c(miny,maxy), #colocamos los límites del eje y
                     breaks=marcasy, # que marcas queremos
                     labels=etiquetasy,#como será lo que aparece
                     expand=c(0,0.2))+
  theme(axis.line = element_line(size = 0.8))+
  theme(text = element_text(size=20))

graf4

#crear un nuevo data frame para sacar los datos por pares
estadistica <- as.data.frame(glucosa.data %>% 
  filter(Tratamiento=="Tibolona"))

estadistica

View(estadistica)

estadistica2 <- as.data.frame(glucosa.data %>% 
                               filter(Tratamiento=="Ciclo-secuencial"))

estadistica2

View(estadistica2)

#Varianza por separado

var.test(estadistica$Valores~estadistica$TratMed, data=estadistica) #Se hace la evaluación de lso grupos por separado, la unica columna nueva de los valores separados es TratMed

var.test(estadistica2$Valores~estadistica2$TratMed, data=estadistica2)

#Prueba de normalidad
shapiro.test(estadistica$Valores)
shapiro.test(estadistica2$Valores)

#Prueba t

t.test(Valores~TratMed, data=estadistica, var.equal=T)

t.test(Valores~TratMed, data=estadistica2, var.equal=T)

#Conclusión: si hay diferencia significativa con el tratamiento de Ciclo-secuencial

ggsave(file="barrasglucosa.png",
       plot=graf4,
       width=10,
       height=7,
       dpi=300)
