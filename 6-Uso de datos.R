Datos_cluster = Datos_ordinal[,c(23:25,32)]

set.seed(767)
b=best_k_proto(Datos_cluster,k=4,i=150)

table(b$cluster)

silhouette_fede(Datos_cluster,b$cluster,4)


Datos_ENPA$`Uso de datos` = b$cluster

Datos_ENPA$`Uso de datos` = case_when(Datos_ENPA$`Uso de datos`==1 ~ "Condiciones meteorológicas",
                                      Datos_ENPA$`Uso de datos`==2 ~ " Todo",
                                      Datos_ENPA$`Uso de datos`==3 ~ "Monitoreo satelital",
                                      T ~ "Nada")
table(Datos_ENPA$`Uso de datos`)

# Clusters
g2=Datos_ENPA %>%
  mutate(P22_5=as.factor(P22_5)) %>% 
  count(`Uso de datos`, P22_5) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P22_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Monitoreo satelital de cultivos") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_6=as.factor(P22_6)) %>% 
  count(`Uso de datos`, P22_6) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P22_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Imágenes fotográficas a través de robots y drones") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_7=as.factor(P22_7)) %>% 
  count(`Uso de datos`, P22_7) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P22_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Plataforma de gestión agrícola") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g5=Datos_ENPA %>%
  mutate(P22_10=as.factor(P22_10)) %>% 
  count(`Uso de datos`, P22_10) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P22_10) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Relevamiento online condiciones meteorológicas") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

ggarrange(g2,g3,g4,g5,nrow = 2, ncol = 2)

# Descriptivo

table(Datos_ENPA$Estilo_cognitivo)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$Estilo_cognitivo)

Datos_ENPA %>%
  mutate(Estilo_cognitivo=as.factor(Estilo_cognitivo)) %>% 
  count(`Uso de datos`, Estilo_cognitivo) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=Estilo_cognitivo) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#F5C710","#2297E6"),name="Estilo cognitivo\ndel productor")+
  ylab("Porcentaje") +
  xlab("Clusters uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$Edad)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$Edad)

Datos_ENPA %>%
  mutate(Edad=as.factor(Edad)) %>% 
  count(`Uso de datos`, Edad) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=Edad) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B","#CD0BBC"),name="Edad del productor")+
  ylab("Porcentaje") +
  xlab("Clusters uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

tabla=Datos_ENPA %>% 
  group_by(`Uso de datos`) %>% 
  summarise(`Edad promedio`=mean(P2años),
            `Edad mediana` = median(P2años))

write.xlsx(tabla, "D:/Otros/Austral/tablas/Edad datos.xlsx",overwrite = F)

table(Datos_ENPA$Tamaño)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$Tamaño)

Datos_ENPA %>%
  mutate(Tamaño=as.factor(Tamaño)) %>% 
  count(`Uso de datos`, Tamaño) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=Tamaño) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Tamaño del productor")+
  ylab("Porcentaje") +
  xlab("Clusters uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$Provincia)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$Provincia)

Datos_ENPA %>%
  mutate(Provincia=as.factor(Provincia)) %>% 
  count(`Uso de datos`, Provincia) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=Provincia) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Provincia del productor")+
  ylab("Porcentaje") +
  xlab("Clusters uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según provincia del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

Datos_ENPA %>%
  mutate(ProvReg=as.factor(ProvReg)) %>% 
  count(`Uso de datos`, ProvReg) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=ProvReg) +
  geom_bar(stat="identity") +
  #scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Provincia del productor")+
  ylab("Porcentaje") +
  xlab("Clusters uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según región del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

# Pregunta 9

table(Datos_ENPA$P9,Datos_ENPA$`Uso de datos`)

Datos_ENPA$Tierra = case_when(Datos_ENPA$P9<=0.25 ~ " Menos del 25%",
                              Datos_ENPA$P9<=0.50 ~ "Entre 25% y 50%",
                              Datos_ENPA$P9<=0.75 ~ "Entre 50% y 75%",
                              T ~ "Más del 75%")
table(Datos_ENPA$Tierra)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$Tierra)

Datos_ENPA %>%
  mutate(Tierra=as.factor(Tierra)) %>% 
  count(`Uso de datos`, Tierra) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=Tierra) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#61D04F","#2297E6","#F5C710","#DF536B"),name="Propiedad de la tierra", labels=c("Menos del 25%","Entre 25% y 50%","Entre 50% y 75%","Más del 75%"))+
  ylab("Porcentaje") +
  xlab("Clusters uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según propiedad de la tierra") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

Datos_ENPA %>% 
  group_by(`Uso de datos`) %>% 
  summarise(`Tierra promedio`=mean(P9))

# Pregunta 31
table(Datos_ENPA$P31)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$P31)

Datos_ENPA %>%
  mutate(P31=as.factor(P31)) %>% 
  count(`Uso de datos`, P31) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P31) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Planificación\nfinanciera")+
  ylab("Porcentaje") +
  xlab("Clusters uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según planificación financiera") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


# Pregunta 32
table(Datos_ENPA$P32)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$P32)

Datos_ENPA %>%
  mutate(P32=as.factor(P32)) %>% 
  count(`Uso de datos`, P32) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P32) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Planificación\ninversiones")+
  ylab("Porcentaje") +
  xlab("Clusters uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según planificación de inversiones") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


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
  group_by(`Uso de datos`) %>% 
  summarise(`Precio`= mean(P23_A),
            `Desconfianza uso de datos` = mean(P23_B),
            `No soluciona un problema` = mean(P23_C),
            `Falta de infraestructura` = mean(P23_D),
            `Desconocimiento` = mean(P23_E))

write.xlsx(tabla, "D:/Otros/Austral/tablas/P23 datos.xlsx",overwrite = F)


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
  group_by(`Uso de datos`) %>% 
  summarise(Impresos_media=mean(c(P35a1_1,P35a1_2,P35a1_3)),
            Persona_media=mean(c(P35a2_1,P35a2_2,P35a2_3)),
            Digitales_media=mean(c(P35a3_1,P35a3_2,P35a3_3,P35a3_4,P35a3_5,P35a3_6,P35a3_7)),
            Redes_media=mean(c(P35a4_1,P35a4_2,P35a4_3,P35a4_4,P35a4_5,P35a4_6,P35a4_7)))

write.xlsx(tabla, "D:/Otros/Austral/tablas/P35 grupos datos.xlsx",overwrite = F)

tabla2=Datos_ENPA %>%
  group_by(`Uso de datos`) %>% 
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

write.xlsx(tabla2, "D:/Otros/Austral/tablas/P35 datos.xlsx",overwrite = F)

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
  group_by(`Uso de datos`) %>% 
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
  group_by(`Uso de datos`) %>% 
  summarise(`Medios impresos`=round(mean(P35_A)*100,2),
            `En persona`=round(mean(P35_B)*100,2),
            `Medios digitales`=round(mean(P35_C)*100,2),
            `Redes sociales`=round(mean(P35_D)*100,2))

# P25
table(Datos_ENPA$P25b_1)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$P25b_1)

Datos_ENPA %>%
  mutate(P25b_1=as.factor(P25b_1)) %>% 
  count(`Uso de datos`, P25b_1) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P25b_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza")+
  ylab("Porcentaje") +
  xlab("Clusters Uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según análisis y fertilización de suelos") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$P25b_3)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$P25b_3)

Datos_ENPA %>%
  mutate(P25b_3=as.factor(P25b_3)) %>% 
  count(`Uso de datos`, P25b_3) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P25b_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza")+
  ylab("Porcentaje") +
  xlab("Clusters Uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según rotación con cultivos de servicio") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$P25b_5)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$P25b_5)

Datos_ENPA %>%
  mutate(P25b_5=as.factor(P25b_5)) %>% 
  count(`Uso de datos`, P25b_5) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P25b_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza")+
  ylab("Porcentaje") +
  xlab("Clusters Uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según conservación de suelos") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

table(Datos_ENPA$P25b_6)
chisq.test(Datos_ENPA$`Uso de datos`,Datos_ENPA$P25b_6)

Datos_ENPA %>%
  mutate(P25b_6=as.factor(P25b_6)) %>% 
  count(`Uso de datos`, P25b_6) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P25b_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza")+
  ylab("Porcentaje") +
  xlab("Clusters Uso de datos")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Composición de los clusters según manejo integrado de plagas") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
