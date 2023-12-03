library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(ggpubr)
library(fastDummies)
library(FactoMineR)
library(clustMixType)
library(cluster)
library(factoextra)
library(ggpubr)
library(openxlsx)

Datos_ENPA <- read_excel("C:/Users/feder/Desktop/Datos ENPA.xlsx")


Datos_ENPA$Compra = case_when(Datos_ENPA$P38_5=="Si" ~ "Regularmente compro algunos insumos online",
                              Datos_ENPA$P38_4=="Si" ~ "Regularmente compro algunos insumos online",
                              Datos_ENPA$P38_3=="Si" ~ "He realizado 1 o 2 compras online de insumos",
                              Datos_ENPA$P38_2=="Si" ~ "Fui de compras pero no concreté ninguna",
                              Datos_ENPA$P38_1=="Si" ~ "No fui de compras online")

Datos_ENPA$Compra = factor(Datos_ENPA$Compra,levels = c("No fui de compras online",
                                                        "Fui de compras pero no concreté ninguna",
                                                        "He realizado 1 o 2 compras online de insumos",
                                                        "Regularmente compro algunos insumos online"),ordered = T)


table(Datos_ENPA$Compra,Datos_ENPA$P22_8)

Datos_ENPA %>%
  count(Compra) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Compra, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor")+
  #scale_x_discrete(labels = c("Invertiré asociado \n12 meses","Invertiré solo \n12 meses","Invertiré solo \n5 años","No planeo \ninvertir","Ya invertí \n12 meses","Ya invertí \n5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Datos_ENPA$Compra))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
                position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#E0F4EA", "#A9DFBF", "#27AE60", "#138D75")) +
  labs(title = "Comportamiento de los productores en relación a la compra online de insumos", fill = "Frecuencia de\ncompra") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0))

# Beneficios
freq=numeric(0)
tabla=table(Datos_ENPA$Compra,Datos_ENPA$P41_9)
freq=c(freq,tabla[,2])

freq=as.numeric(freq)

freq=matrix(freq, nrow = 9, ncol = 4, byrow = T)

freq= data.frame(freq)

write.xlsx(freq,"D:/Otros/Austral/tablas/P41.xlsx",overwrite = F)


# Desventajas
freq=numeric(0)
tabla=table(Datos_ENPA$Compra,Datos_ENPA$P42_10)
freq=c(freq,tabla[,2])

freq=as.numeric(freq)

freq=matrix(freq, nrow = 10, ncol = 4, byrow = T)

freq= data.frame(freq)

write.xlsx(freq,"D:/Otros/Austral/tablas/P42.xlsx",overwrite = F)


# Futuro
freq=numeric(0)
tabla=table(Datos_ENPA$Compra,Datos_ENPA$P43)

tabla = as.numeric(tabla[1:4,1:4])

tabla = matrix(tabla,nrow=4,ncol=4,byrow=T)

tabla= data.frame(tabla)
write.xlsx(tabla,"D:/Otros/Austral/tablas/P43.xlsx",overwrite = F)

# Descriptivo

table(Datos_ENPA$Estilo_cognitivo)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$Estilo_cognitivo)

Datos_ENPA %>%
  mutate(Estilo_cognitivo=as.factor(Estilo_cognitivo)) %>% 
  count(Compra, Estilo_cognitivo) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=Estilo_cognitivo) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#F5C710","#2297E6"),name="Estilo cognitivo\ndel productor")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$Edad)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$Edad)

Datos_ENPA %>%
  mutate(Edad=as.factor(Edad)) %>% 
  count(Compra, Edad) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=Edad) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B","#CD0BBC"),name="Edad del productor")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

Datos_ENPA %>% 
  group_by(Compra) %>% 
  summarise(`Edad promedio`=mean(P2años))


table(Datos_ENPA$Tamaño)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$Tamaño)

Datos_ENPA %>%
  mutate(Tamaño=as.factor(Tamaño)) %>% 
  count(Compra, Tamaño) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=Tamaño) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Tamaño del productor")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$Provincia)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$Provincia)

Datos_ENPA %>%
  mutate(Provincia=as.factor(Provincia)) %>% 
  count(Compra, Provincia) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=Provincia) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Provincia del productor")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según provincia del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

Datos_ENPA %>%
  mutate(ProvReg=as.factor(ProvReg)) %>% 
  count(Compra, ProvReg) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=ProvReg) +
  geom_bar(stat="identity") +
  #scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Provincia del productor")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según región del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

# Pregunta 9

table(Datos_ENPA$P9,Datos_ENPA$Compra)

Datos_ENPA$Tierra = case_when(Datos_ENPA$P9<=0.25 ~ " Menos del 25%",
                              Datos_ENPA$P9<=0.50 ~ "Entre 25% y 50%",
                              Datos_ENPA$P9<=0.75 ~ "Entre 50% y 75%",
                              T ~ "Más del 75%")
table(Datos_ENPA$Tierra)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$Tierra)

Datos_ENPA %>%
  mutate(Tierra=as.factor(Tierra)) %>% 
  count(Compra, Tierra) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=Tierra) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Propiedad de la tierra", labels=c("Menos del 25%","Entre 25% y 50%","Entre 50% y 75%","Más del 75%"))+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según propiedad de la tierra") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

Datos_ENPA %>% 
  group_by(Compra) %>% 
  summarise(`Tierra promedio`=mean(P9))

# Pregunta 31
table(Datos_ENPA$P31)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$P31)

Datos_ENPA %>%
  mutate(P31=as.factor(P31)) %>% 
  count(Compra, P31) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=P31) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Planificación\nfinanciera")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según planificación financiera") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


# Pregunta 32
table(Datos_ENPA$P32)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$P32)

Datos_ENPA %>%
  mutate(P32=as.factor(P32)) %>% 
  count(Compra, P32) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=P32) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Planificación\ninversiones")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según planificación de inversiones") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

# P35

Datos_ENPA$P35a1_1 = case_when(Datos_ENPA$P35a1_1=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a1_1=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a1_1))

Datos_ENPA$P35a1_2 = case_when(Datos_ENPA$P35a1_2=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a1_2=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a1_2))

Datos_ENPA$P35a1_3 = case_when(Datos_ENPA$P35a1_3=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a1_3=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a1_3))

Datos_ENPA$P35a2_1 = case_when(Datos_ENPA$P35a2_1=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a2_1=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a2_1))

Datos_ENPA$P35a2_2 = case_when(Datos_ENPA$P35a2_2=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a2_2=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a2_2))

Datos_ENPA$P35a2_3 = case_when(Datos_ENPA$P35a2_3=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a2_3=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a2_3))

Datos_ENPA$P35a3_1 = case_when(Datos_ENPA$P35a3_1=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a3_1=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a3_1))

Datos_ENPA$P35a3_2 = case_when(Datos_ENPA$P35a3_2=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a3_2=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a3_2))

Datos_ENPA$P35a3_3 = case_when(Datos_ENPA$P35a3_3=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a3_3=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a3_3))

Datos_ENPA$P35a3_4 = case_when(Datos_ENPA$P35a3_4=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a3_4=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a3_4))

Datos_ENPA$P35a3_5 = case_when(Datos_ENPA$P35a3_5=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a3_5=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a3_5))

Datos_ENPA$P35a3_6 = case_when(Datos_ENPA$P35a3_6=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a3_6=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a3_6))

Datos_ENPA$P35a3_7 = case_when(Datos_ENPA$P35a3_7=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a3_7=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a3_7))

Datos_ENPA$P35a4_1 = case_when(Datos_ENPA$P35a4_1=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a4_1=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a4_1))

Datos_ENPA$P35a4_2 = case_when(Datos_ENPA$P35a4_2=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a4_2=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a4_2))

Datos_ENPA$P35a4_3 = case_when(Datos_ENPA$P35a4_3=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a4_3=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a4_3))

Datos_ENPA$P35a4_4 = case_when(Datos_ENPA$P35a4_4=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a4_4=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a4_4))

Datos_ENPA$P35a4_5 = case_when(Datos_ENPA$P35a4_5=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a4_5=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a4_5))

Datos_ENPA$P35a4_6 = case_when(Datos_ENPA$P35a4_6=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a4_6=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a4_6))

Datos_ENPA$P35a4_7 = case_when(Datos_ENPA$P35a4_7=="1 No es importante" ~ 1,
                               Datos_ENPA$P35a4_7=="9 Es muy importante" ~ 9,
                               T ~ as.numeric(Datos_ENPA$P35a4_7))


tabla=Datos_ENPA %>% 
  group_by(Compra) %>% 
  summarise(Impresos_media=mean(c(P35a1_1,P35a1_2,P35a1_3)),
            Persona_media=mean(c(P35a2_1,P35a2_2,P35a2_3)),
            Digitales_media=mean(c(P35a3_1,P35a3_2,P35a3_3,P35a3_4,P35a3_5,P35a3_6,P35a3_7)),
            Redes_media=mean(c(P35a4_1,P35a4_2,P35a4_3,P35a4_4,P35a4_5,P35a4_6,P35a4_7)))

write.xlsx(tabla, "D:/Otros/Austral/tablas/P35 grupos compra.xlsx",overwrite = F)

tabla2=Datos_ENPA %>%
  group_by(Compra) %>% 
  summarise(`Revistas agrícolas`=mean(P35a1_1),
            `Publicaciones universitarias`=mean(P35a1_2),
            `Suplementos y diarios agrícolas`=mean(P35a1_3),
            `Reuniones`=mean(P35a2_1),
            `Ferias agrícolas`=mean(P35a2_2),
            `Jornadas a campo`=mean(P35a2_3),
            `Sitios web fabricantes`=mean(P35a3_1),
            `Sitios web distribuidores`=mean(P35a3_2),
            `Sitios web exclusivos de agro`=mean(P35a3_3),
            `Boletín informativo`=mean(P35a3_4),
            `Radio/TV`=mean(P35a3_5),
            `Podcast`=mean(P35a3_6),
            `Videollamadas`=mean(P35a3_7),
            `Facebook`=mean(P35a4_1),
            `Linkedin`=mean(P35a4_2),
            `Mensaje de texto`=mean(P35a4_3),
            `Twitter`=mean(P35a4_4),
            `Youtube`=mean(P35a4_5),
            `Instagram`=mean(P35a4_6),
            `Whatsapp`=mean(P35a4_7))

write.xlsx(tabla2, "D:/Otros/Austral/tablas/P35 compra.xlsx",overwrite = F)

# Fuente de información importante

Datos_ENPA$P35_1 = case_when(Datos_ENPA$P35a1_1>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_2 = case_when(Datos_ENPA$P35a1_2>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_3 = case_when(Datos_ENPA$P35a1_3>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_4 = case_when(Datos_ENPA$P35a2_1>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_5 = case_when(Datos_ENPA$P35a2_2>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_6 = case_when(Datos_ENPA$P35a2_3>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_7 = case_when(Datos_ENPA$P35a3_1>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_8 = case_when(Datos_ENPA$P35a3_2>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_9 = case_when(Datos_ENPA$P35a3_3>=7 ~ 1,
                             T ~ 0)

Datos_ENPA$P35_10 = case_when(Datos_ENPA$P35a3_4>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_11 = case_when(Datos_ENPA$P35a3_5>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_12 = case_when(Datos_ENPA$P35a3_6>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_13 = case_when(Datos_ENPA$P35a3_7>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_14 = case_when(Datos_ENPA$P35a4_1>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_15 = case_when(Datos_ENPA$P35a4_2>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_16 = case_when(Datos_ENPA$P35a4_3>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_17 = case_when(Datos_ENPA$P35a4_4>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_18 = case_when(Datos_ENPA$P35a4_5>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_19 = case_when(Datos_ENPA$P35a4_6>=7 ~ 1,
                              T ~ 0)

Datos_ENPA$P35_20 = case_when(Datos_ENPA$P35a4_7>=7 ~ 1,
                              T ~ 0)

tabla3=Datos_ENPA %>%
  group_by(Compra) %>% 
  summarise(`Revistas agrícolas`=round(mean(P35_1)*100,2),
            `Publicaciones universitarias`=round(mean(P35_2)*100,2),
            `Suplementos y diarios agrícolas`=round(mean(P35_3)*100,2),
            `Reuniones`=round(mean(P35_4)*100,2),
            `Ferias agrícolas`=round(mean(P35_5)*100,2),
            `Jornadas a campo`=round(mean(P35_6)*100,2),
            `Sitios web fabricantes`=round(mean(P35_7)*100,2),
            `Sitios web distribuidores`=round(mean(P35_8)*100,2),
            `Sitios web exclusivos de agro`=round(mean(P35_9)*100,2),
            `Boletín informativo`=round(mean(P35_10)*100,2),
            `Radio/TV`=round(mean(P35_11)*100,2),
            `Podcast`=round(mean(P35_12)*100,2),
            `Videollamadas`=round(mean(P35_13)*100,2),
            `Facebook`=round(mean(P35_14)*100,2),
            `Linkedin`=round(mean(P35_15)*100,2),
            `Mensaje de texto`=round(mean(P35_16)*100,2),
            `Twitter`=round(mean(P35_17)*100,2),
            `Youtube`=round(mean(P35_18)*100,2),
            `Instagram`=round(mean(P35_19)*100,2),
            `Whatsapp`=round(mean(P35_20)*100,2))

Datos_ENPA$P35_A = case_when((Datos_ENPA$P35a1_1>=7 | Datos_ENPA$P35a1_2>=7 | Datos_ENPA$P35a1_3>=7) ~ 1,
                             T ~ 0)

Datos_ENPA$P35_B = case_when((Datos_ENPA$P35a2_1>=7 | Datos_ENPA$P35a2_2>=7 | Datos_ENPA$P35a2_3>=7) ~ 1,
                             T ~ 0)

Datos_ENPA$P35_C = case_when((Datos_ENPA$P35a3_1>=7 | Datos_ENPA$P35a3_2>=7 | Datos_ENPA$P35a3_3>=7 | Datos_ENPA$P35a3_4>=7 | Datos_ENPA$P35a3_5>=7 | Datos_ENPA$P35a3_6>=7 | Datos_ENPA$P35a3_7>=7) ~ 1,
                             T ~ 0)

Datos_ENPA$P35_D = case_when((Datos_ENPA$P35a4_1>=7 | Datos_ENPA$P35a4_2>=7 | Datos_ENPA$P35a4_3>=7 | Datos_ENPA$P35a4_4>=7 | Datos_ENPA$P35a4_5>=7 | Datos_ENPA$P35a4_6>=7 | Datos_ENPA$P35a4_7>=7) ~ 1,
                             T ~ 0)

tabla4=Datos_ENPA %>%
  group_by(Compra) %>% 
  summarise(`Medios impresos`=round(mean(P35_A)*100,2),
            `En persona`=round(mean(P35_B)*100,2),
            `Medios digitales`=round(mean(P35_C)*100,2),
            `Redes sociales`=round(mean(P35_D)*100,2))

# Pregunta 23

Datos_ENPA$P23_A = case_when(Datos_ENPA$P23_1 == "1ro" ~ 1,
                             Datos_ENPA$P23_1 == "2do" ~ 2,
                             Datos_ENPA$P23_1 == "3ro" ~ 3,
                             Datos_ENPA$P23_1 == "4to" ~ 4,
                             T ~ 5)

Datos_ENPA$P23_B = case_when(Datos_ENPA$P23_2 == "1ro" ~ 1,
                             Datos_ENPA$P23_2 == "2do" ~ 2,
                             Datos_ENPA$P23_2 == "3ro" ~ 3,
                             Datos_ENPA$P23_2 == "4to" ~ 4,
                             T ~ 5)

Datos_ENPA$P23_C = case_when(Datos_ENPA$P23_3 == "1ro" ~ 1,
                             Datos_ENPA$P23_3 == "2do" ~ 2,
                             Datos_ENPA$P23_3 == "3ro" ~ 3,
                             Datos_ENPA$P23_3 == "4to" ~ 4,
                             T ~ 5)

Datos_ENPA$P23_D = case_when(Datos_ENPA$P23_4 == "1ro" ~ 1,
                             Datos_ENPA$P23_4 == "2do" ~ 2,
                             Datos_ENPA$P23_4 == "3ro" ~ 3,
                             Datos_ENPA$P23_4 == "4to" ~ 4,
                             T ~ 5)

Datos_ENPA$P23_E = case_when(Datos_ENPA$P23_5 == "1ro" ~ 1,
                             Datos_ENPA$P23_5 == "2do" ~ 2,
                             Datos_ENPA$P23_5 == "3ro" ~ 3,
                             Datos_ENPA$P23_5 == "4to" ~ 4,
                             T ~ 5)

tabla=Datos_ENPA %>% 
  group_by(Compra) %>% 
  summarise(`Precio`= mean(P23_A),
            `Desconfianza uso de datos` = mean(P23_B),
            `No soluciona un problema` = mean(P23_C),
            `Falta de infraestructura` = mean(P23_D),
            `Desconocimiento` = mean(P23_E))

write.xlsx(tabla,"D:/Otros/Austral/tablas/P23 compra.xlsx",overwrite = F)


# P34
table(Datos_ENPA$P34)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$P34)

Datos_ENPA %>%
  mutate(P34=as.factor(P34)) %>% 
  count(Compra, P34) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=P34) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Conectividad",labels=c("No experimento\nlimitaciones","Con alguna frecuencia","Con mucha frecuencia","Sí, experimento\nmuchas limitaciones"))+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según dificultades de conectividad") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

# P25
table(Datos_ENPA$P25b_1)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$P25b_1)

Datos_ENPA %>%
  mutate(P25b_1=as.factor(P25b_1)) %>% 
  count(Compra, P25b_1) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=P25b_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según análisis y fertilización de suelos") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$P25b_3)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$P25b_3)

Datos_ENPA %>%
  mutate(P25b_3=as.factor(P25b_3)) %>% 
  count(Compra, P25b_3) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=P25b_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según rotación con cultivos de servicio") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$P25b_5)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$P25b_5)

Datos_ENPA %>%
  mutate(P25b_5=as.factor(P25b_5)) %>% 
  count(Compra, P25b_5) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=P25b_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según conservación de suelos") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$P25b_6)
chisq.test(Datos_ENPA$Compra,Datos_ENPA$P25b_6)

Datos_ENPA %>%
  mutate(P25b_6=as.factor(P25b_6)) %>% 
  count(Compra, P25b_6) %>%       
  group_by(Compra) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Compra, pct, fill=P25b_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza")+
  ylab("Porcentaje") +
  xlab("Grupos compra online")+
  scale_x_discrete(labels = c("No fui de\ncompras online","Fui de compras\npero no concreté ninguna","He realizado 1 o 2\ncompras online de insumos","Regularmente compro\nalgunos insumos online")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según manejo integrado de plagas") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
