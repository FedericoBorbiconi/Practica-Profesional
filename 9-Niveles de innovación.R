library(openxlsx)
library(egg)
Nada_total=Datos_ENPA[which(Datos_ENPA$Biomasa=="Nada" & 
                   Datos_ENPA$Compra=="No fui de compras online" &
                   Datos_ENPA$`Uso de datos`=="Nada" &
                   Datos_ENPA$`Tecnologías producción`=="Nada"),]

Todo_básico = Datos_ENPA[which(Datos_ENPA$Biomasa!="Nada" & 
                                 Datos_ENPA$Compra!="No fui de compras online" &
                                 Datos_ENPA$Compra!="Fui de compras pero no concreté ninguna" &
                                 Datos_ENPA$`Uso de datos`!="Nada" &
                                 Datos_ENPA$`Tecnologías producción`!="Nada"),]

Todo_full = Datos_ENPA[which(Datos_ENPA$Biomasa==" Todo" & 
                               Datos_ENPA$Compra=="Regularmente compro algunos insumos online" &
                               Datos_ENPA$`Uso de datos`==" Todo" &
                               Datos_ENPA$`Tecnologías producción`=="Todo"),]

Algo_full = Datos_ENPA[which(Datos_ENPA$Biomasa==" Todo" | 
                               Datos_ENPA$Compra=="Regularmente compro algunos insumos online" |
                               Datos_ENPA$`Uso de datos`==" Todo" |
                               Datos_ENPA$`Tecnologías producción`=="Todo"),]

# Todo_básico
# Clusters
frecuencia <- as.data.frame(table(Todo_básico$Biomasa))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710")) +
  labs(fill = "Cluster Biomasa") +
  ggtitle("Biomasa") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(as.character(Todo_básico$Compra)))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#F5C710","#61D04F"), labels = c("He realizado 1 o 2 compras\nonline de insumos","Regularmente compro\nalgunos insumos online")) +
  labs(fill = "Frecuencia de compra") +
  ggtitle("Compra online de insumos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Todo_básico$`Uso de datos`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710")) +
  labs(fill = "Cluster Uso de datos") +
  ggtitle("Uso de datos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))



frecuencia <- as.data.frame(table(Todo_básico$`Tecnologías producción`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g4=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#F5C710","#61D04F")) +
  labs(fill = "Cluster Tecnologías\nde producción") +
  ggtitle("Tecnologías de producción") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(g1,g2,g3,g4,nrow=2,ncol=2)

# Descriptivo
frecuencia <- as.data.frame(table(Todo_básico$Estilo_cognitivo))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#F5C710")) +
  labs(title = "Distribución de los productores según su estilo cognitivo", fill = "Estilo cognitivo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

Todo_básico %>% 
  ggplot(aes(P49))+
  geom_bar(fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(0,9,1)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  xlab("Estilo cognitivo") +
  ylab("Cantidad de productores") +
  ggtitle("Productores innovadores") +
  theme_bw()

median(Todo_básico$P49)
median(Datos_ENPA$P49)


Todo_básico %>% 
  ggplot(aes(P2años))+
  geom_histogram(breaks=seq(22,78,4), fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  xlab("Edad del productor") +
  ylab("Cantidad de productores") +
  ggtitle("Distribución de los productores según su edad") +
  theme_bw()

quantile(Todo_básico$P2años,seq(0,1,0.01))

frecuencia <- as.data.frame(table(Todo_básico$Tamaño))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Tamaño") +
  ggtitle("Distribución de los productores según su tamaño") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Todo_básico$Provincia))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#DF536B")) +
  labs(fill = "Provincia") +
  ggtitle("Distribución de los productores según su provincia") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Todo_básico$ProvReg))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_fill_manual(values = c("#61D04F","#2297E6","#DF536B")) +
  labs(fill = "Región") +
  ggtitle("Distribución de los productores según su región") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

round(table(Datos_ENPA$ProvReg)/880*100,2)

Todo_básico$Tierra = case_when(Todo_básico$P9<=0.25 ~ " Menos del 25%",
                               Todo_básico$P9<=0.50 ~ "Entre 25% y 50%",
                               Todo_básico$P9<=0.75 ~ "Entre 50% y 75%",
                              T ~ "Más del 75%")

frecuencia <- as.data.frame(table(Todo_básico$Tierra))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Propiedad de\nla tierra") +
  ggtitle("Distribución de los productores según la propiedad de la tierra") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

mean(Todo_básico$P9)
sd(Todo_básico$P9)

table(Algo_full$P9)/253*100

frecuencia <- as.data.frame(table(as.character(Todo_básico$P31)))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#DF536B")) +
  labs(fill = "Planifica") +
  ggtitle("Distribución de los productores según planificación financiera") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Todo_básico$P32))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Distribución de los productores según planificación de inversiones") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# P25
frecuencia <- as.data.frame(table(Todo_básico$P25b_1))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Distribución de los productores según análisis y fertilización de suelos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Todo_básico$P25b_3))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Distribución de los productores según rotación con cultivos de servicio") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Todo_básico$P25b_5))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Distribución de los productores según conservación de suelo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Todo_básico$P25b_6))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Distribución de los productores según manejo integrado de plagas") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Pregunta 23

Todo_básico$P23_A = case_when(Todo_básico$P23_1 == "1ro" ~ 1,
                             Todo_básico$P23_1 == "2do" ~ 2,
                             Todo_básico$P23_1 == "3ro" ~ 3,
                             Todo_básico$P23_1 == "4to" ~ 4,
                             T ~ 5)

Todo_básico$P23_B = case_when(Todo_básico$P23_2 == "1ro" ~ 1,
                             Todo_básico$P23_2 == "2do" ~ 2,
                             Todo_básico$P23_2 == "3ro" ~ 3,
                             Todo_básico$P23_2 == "4to" ~ 4,
                             T ~ 5)

Todo_básico$P23_C = case_when(Todo_básico$P23_3 == "1ro" ~ 1,
                             Todo_básico$P23_3 == "2do" ~ 2,
                             Todo_básico$P23_3 == "3ro" ~ 3,
                             Todo_básico$P23_3 == "4to" ~ 4,
                             T ~ 5)

Todo_básico$P23_D = case_when(Todo_básico$P23_4 == "1ro" ~ 1,
                             Todo_básico$P23_4 == "2do" ~ 2,
                             Todo_básico$P23_4 == "3ro" ~ 3,
                             Todo_básico$P23_4 == "4to" ~ 4,
                             T ~ 5)

Todo_básico$P23_E = case_when(Todo_básico$P23_5 == "1ro" ~ 1,
                             Todo_básico$P23_5 == "2do" ~ 2,
                             Todo_básico$P23_5 == "3ro" ~ 3,
                             Todo_básico$P23_5 == "4to" ~ 4,
                             T ~ 5)

tabla=Todo_básico %>% 
  summarise(`Precio`= mean(P23_A),
            `Desconfianza uso de datos` = mean(P23_B),
            `No soluciona un problema` = mean(P23_C),
            `Falta de infraestructura` = mean(P23_D),
            `Desconocimiento` = mean(P23_E))

write.xlsx(tabla, "D:/Otros/Austral/tablas/P23 innovadores.xlsx",overwrite = F)


tabla=Todo_básico %>% 
  summarise(Impresos_media=mean(c(P35a1_1,P35a1_2,P35a1_3)),
            Persona_media=mean(c(P35a2_1,P35a2_2,P35a2_3)),
            Digitales_media=mean(c(P35a3_1,P35a3_2,P35a3_3,P35a3_4,P35a3_5,P35a3_6,P35a3_7)),
            Redes_media=mean(c(P35a4_1,P35a4_2,P35a4_3,P35a4_4,P35a4_5,P35a4_6,P35a4_7)))


tabla2=Todo_básico %>%
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

write.xlsx(tabla2, "D:/Otros/Austral/tablas/P35 innovadores.xlsx",overwrite = F)


# Algo_full
# Clusters
frecuencia <- as.data.frame(table(Algo_full$Biomasa))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Cluster Biomasa") +
  ggtitle("Biomasa") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$Compra))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#F5C710","#2297E6","#61D04F")) +
  labs(fill = "Frecuencia de compra") +
  ggtitle("Compra online de insumos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Algo_full$`Uso de datos`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#F5C710","#2297E6","#DF536B")) +
  labs(fill = "Cluster Uso de datos") +
  ggtitle("Uso de datos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))



frecuencia <- as.data.frame(table(Algo_full$`Tecnologías producción`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g4=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#F5C710","#61D04F")) +
  labs(fill = "Cluster Tecnologías\nde producción") +
  ggtitle("Tecnologías de producción") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(g1,g2,g3,g4,nrow=2,ncol=2)

# Descriptivo
frecuencia <- as.data.frame(table(Algo_full$Estilo_cognitivo))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#F5C710","#2297E6")) +
  labs(title = "Distribución de los productores según su estilo cognitivo", fill = "Estilo cognitivo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

Algo_full %>% 
  ggplot(aes(P49))+
  geom_bar(fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(0,9,1)) +
  #scale_y_continuous(breaks = seq(0,80,10)) +
  xlab("Estilo cognitivo") +
  ylab("Cantidad de productores") +
  ggtitle("Productores innovadores") +
  theme_bw()

median(Algo_full$P49)
median(Datos_ENPA$P49)

g1=Algo_full %>% 
  ggplot(aes(P2años))+
  geom_histogram(breaks=seq(22,78,4), fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  scale_y_continuous(breaks = seq(0,40,5)) +
  xlab("Edad del productor") +
  ylab("Cantidad de productores") +
  ggtitle("Productores innovadores") +
  theme_bw()

g3=Datos_ENPA %>% 
  ggplot(aes(P2años))+
  geom_histogram(breaks=seq(22,78,4), fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  #scale_y_continuous(breaks = seq(0,40,5)) +
  xlab("Edad del productor") +
  ylab("Cantidad de productores") +
  ggtitle("Perfil general") +
  theme_bw()

g2=Nada_total %>% 
  ggplot(aes(P2años))+
  geom_histogram(breaks=seq(22,78,4), fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  scale_y_continuous(breaks = seq(0,35,5), limits = c(0,35)) +
  xlab("Edad del productor") +
  ylab("Cantidad de productores") +
  ggtitle("Productores no innovadores") +
  theme_bw()

ggarrange(g1,g2,g3, nrow = 2)

quantile(Algo_full$P2años,seq(0,1,0.05))

frecuencia <- as.data.frame(table(Algo_full$Tamaño))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Tamaño") +
  ggtitle("Distribución de los productores según su tamaño") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Algo_full$Provincia))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Provincia") +
  ggtitle("Distribución de los productores según su provincia") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$ProvReg))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_fill_manual(values = c("#61D04F","#2297E6","#DF536B")) +
  labs(fill = "Región") +
  ggtitle("Distribución de los productores según su región") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

round(table(Datos_ENPA$ProvReg)/880*100,2)

Algo_full$Tierra = case_when(Algo_full$P9<=0.25 ~ " Menos del 25%",
                               Algo_full$P9<=0.50 ~ "Entre 25% y 50%",
                               Algo_full$P9<=0.75 ~ "Entre 50% y 75%",
                               T ~ "Más del 75%")

frecuencia <- as.data.frame(table(Algo_full$P9))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Propiedad de\nla tierra") +
  ggtitle("Distribución de los productores según la propiedad de la tierra") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

mean(Algo_full$P9)
sd(Algo_full$P9)

frecuencia <- as.data.frame(table(as.character(Algo_full$P31)))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Distribución de los productores según planificación financiera") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Algo_full$P32))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Distribución de los productores según planificación de inversiones") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# P25
frecuencia <- as.data.frame(table(Algo_full$P25b_1))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Lo realiza") +
  ggtitle("Distribución de los productores según análisis y fertilización de suelos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$P25b_3))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Lo realiza") +
  ggtitle("Distribución de los productores según rotación con cultivos de servicio") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$P25b_5))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Lo realiza") +
  ggtitle("Distribución de los productores según conservación de suelo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$P25b_6))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Lo realiza") +
  ggtitle("Distribución de los productores según manejo integrado de plagas") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Pregunta 23

Algo_full$P23_A = case_when(Algo_full$P23_1 == "1ro" ~ 1,
                              Algo_full$P23_1 == "2do" ~ 2,
                              Algo_full$P23_1 == "3ro" ~ 3,
                              Algo_full$P23_1 == "4to" ~ 4,
                              T ~ 5)

Algo_full$P23_B = case_when(Algo_full$P23_2 == "1ro" ~ 1,
                              Algo_full$P23_2 == "2do" ~ 2,
                              Algo_full$P23_2 == "3ro" ~ 3,
                              Algo_full$P23_2 == "4to" ~ 4,
                              T ~ 5)

Algo_full$P23_C = case_when(Algo_full$P23_3 == "1ro" ~ 1,
                              Algo_full$P23_3 == "2do" ~ 2,
                              Algo_full$P23_3 == "3ro" ~ 3,
                              Algo_full$P23_3 == "4to" ~ 4,
                              T ~ 5)

Algo_full$P23_D = case_when(Algo_full$P23_4 == "1ro" ~ 1,
                              Algo_full$P23_4 == "2do" ~ 2,
                              Algo_full$P23_4 == "3ro" ~ 3,
                              Algo_full$P23_4 == "4to" ~ 4,
                              T ~ 5)

Algo_full$P23_E = case_when(Algo_full$P23_5 == "1ro" ~ 1,
                              Algo_full$P23_5 == "2do" ~ 2,
                              Algo_full$P23_5 == "3ro" ~ 3,
                              Algo_full$P23_5 == "4to" ~ 4,
                              T ~ 5)

tabla=Algo_full %>% 
  summarise(`Precio`= mean(P23_A),
            `Desconfianza uso de datos` = mean(P23_B),
            `No soluciona un problema` = mean(P23_C),
            `Falta de infraestructura` = mean(P23_D),
            `Desconocimiento` = mean(P23_E))

write.xlsx(tabla, "D:/Otros/Austral/tablas/P23 algo full.xlsx",overwrite = F)


tabla=Algo_full %>% 
  summarise(Impresos_media=mean(c(P35a1_1,P35a1_2,P35a1_3)),
            Persona_media=mean(c(P35a2_1,P35a2_2,P35a2_3)),
            Digitales_media=mean(c(P35a3_1,P35a3_2,P35a3_3,P35a3_4,P35a3_5,P35a3_6,P35a3_7)),
            Redes_media=mean(c(P35a4_1,P35a4_2,P35a4_3,P35a4_4,P35a4_5,P35a4_6,P35a4_7)))


tabla2=Algo_full %>%
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

write.xlsx(tabla2, "D:/Otros/Austral/tablas/P35 algo full.xlsx",overwrite = F)


Algo_full$P37_1 = case_when(Algo_full$P37_1 == "1 Fuerte desconfianza" ~ 1,
                            Algo_full$P37_1 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Algo_full$P37_1))

Algo_full$P37_2 = case_when(Algo_full$P37_2 == "1 Fuerte desconfianza" ~ 1,
                            Algo_full$P37_2 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Algo_full$P37_2))

Algo_full$P37_3 = case_when(Algo_full$P37_3 == "1 Fuerte desconfianza" ~ 1,
                            Algo_full$P37_3 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Algo_full$P37_3))

Algo_full$P37_4 = case_when(Algo_full$P37_4 == "1 Fuerte desconfianza" ~ 1,
                            Algo_full$P37_4 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Algo_full$P37_4))

Algo_full$P37_5 = case_when(Algo_full$P37_5 == "1 Fuerte desconfianza" ~ 1,
                            Algo_full$P37_5 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Algo_full$P37_5))

Algo_full$P37_6 = case_when(Algo_full$P37_6 == "1 Fuerte desconfianza" ~ 1,
                            Algo_full$P37_6 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Algo_full$P37_6))

Algo_full$P37_7 = case_when(Algo_full$P37_7 == "1 Fuerte desconfianza" ~ 1,
                            Algo_full$P37_7 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Algo_full$P37_7))

tabla=Algo_full %>% 
  summarise(`Consultores independientes`=mean(P37_1),
            `Financiadores`=mean(P37_2),
            `Representantes de proveedores locales`=mean(P37_3),
            `Representantes de fabricantes`=mean(P37_4),
            `Otros agricultores en su área`=mean(P37_5),
            `Otros agricultores fuera de su área`=mean(P37_6),
            `Universidades`=mean(P37_7))

write.xlsx(tabla, "D:/Otros/Austral/tablas/P37 algo full.xlsx",overwrite = F)

Nada_total$P37_1 = case_when(Nada_total$P37_1 == "1 Fuerte desconfianza" ~ 1,
                            Nada_total$P37_1 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Nada_total$P37_1))

Nada_total$P37_2 = case_when(Nada_total$P37_2 == "1 Fuerte desconfianza" ~ 1,
                            Nada_total$P37_2 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Nada_total$P37_2))

Nada_total$P37_3 = case_when(Nada_total$P37_3 == "1 Fuerte desconfianza" ~ 1,
                            Nada_total$P37_3 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Nada_total$P37_3))

Nada_total$P37_4 = case_when(Nada_total$P37_4 == "1 Fuerte desconfianza" ~ 1,
                            Nada_total$P37_4 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Nada_total$P37_4))

Nada_total$P37_5 = case_when(Nada_total$P37_5 == "1 Fuerte desconfianza" ~ 1,
                            Nada_total$P37_5 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Nada_total$P37_5))

Nada_total$P37_6 = case_when(Nada_total$P37_6 == "1 Fuerte desconfianza" ~ 1,
                            Nada_total$P37_6 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Nada_total$P37_6))

Nada_total$P37_7 = case_when(Nada_total$P37_7 == "1 Fuerte desconfianza" ~ 1,
                            Nada_total$P37_7 == "9 Fuerte confianza" ~ 9,
                            T ~ as.numeric(Nada_total$P37_7))

tabla2=Nada_total %>% 
  summarise(`Consultores independientes`=mean(P37_1),
            `Financiadores`=mean(P37_2),
            `Representantes de proveedores locales`=mean(P37_3),
            `Representantes de fabricantes`=mean(P37_4),
            `Otros agricultores en su área`=mean(P37_5),
            `Otros agricultores fuera de su área`=mean(P37_6),
            `Universidades`=mean(P37_7))

write.xlsx(tabla2, "D:/Otros/Austral/tablas/P37 nada total.xlsx",overwrite = F)


# Nada total
frecuencia <- as.data.frame(table(Nada_total$Estilo_cognitivo))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#F5C710","#DF536B")) +
  labs(title = "Estilo cognitivo de los productores", fill = "Estilo cognitivo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

g1=Nada_total %>% 
  ggplot(aes(P49))+
  geom_bar(fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(0,9,1)) +
  #scale_y_continuous(breaks = seq(0,80,10)) +
  xlab("Estilo cognitivo") +
  ylab("Cantidad de productores") +
  ggtitle("Productores no innovadores") +
  theme_bw()

g2=Datos_ENPA %>% 
  ggplot(aes(P49))+
  geom_bar(fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(0,9,1)) +
  #scale_y_continuous(breaks = seq(0,80,10)) +
  xlab("Estilo cognitivo") +
  ylab("Cantidad de productores") +
  ggtitle("Perfil general") +
  theme_bw()

ggarrange(g1,g2,nrow = 2)
median(Nada_total$P49)
median(Datos_ENPA$P49)

Nada_total %>% 
  ggplot(aes(P2años))+
  geom_histogram(breaks=seq(22,78,4), fill="#2297E6", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  scale_y_continuous(breaks = seq(0,26,2)) +
  xlab("Edad del productor") +
  ylab("Cantidad de productores") +
  ggtitle("Distribución de la edad de los productores") +
  theme_bw()

quantile(Nada_total$P2años,seq(0,1,0.01))


frecuencia <- as.data.frame(table(Nada_total$Tamaño))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Tamaño") +
  ggtitle("Tamaño de los productores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Nada_total$Provincia))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Provincia") +
  ggtitle("Provincia de los productores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Nada_total$ProvReg))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_fill_manual(values = c("#61D04F","#2297E6","#DF536B")) +
  labs(fill = "Región") +
  ggtitle("Distribución de los productores según su región") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

Nada_total$Tierra = case_when(Nada_total$P9<=0.25 ~ " Menos del 25%",
                               Nada_total$P9<=0.50 ~ "Entre 25% y 50%",
                               Nada_total$P9<=0.75 ~ "Entre 50% y 75%",
                               T ~ "Más del 75%")

frecuencia <- as.data.frame(table(Nada_total$Tierra))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(fill = "Propiedad de\nla tierra") +
  ggtitle("Propiedad de la tierra de los productores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Nada_total$P31))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Planificación financiera de los productores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Nada_total$P32))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Planifica") +
  ggtitle("Planificación de inversiones de los productores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


# P25
frecuencia <- as.data.frame(table(Nada_total$P25b_1))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Lo realiza") +
  ggtitle("Distribución de los productores según análisis y fertilización de suelos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Nada_total$P25b_3))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Lo realiza") +
  ggtitle("Distribución de los productores según rotación con cultivos de servicio") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Nada_total$P25b_5))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Lo realiza") +
  ggtitle("Distribución de los productores según conservación de suelo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Nada_total$P25b_6))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#DF536B","#61D04F")) +
  labs(fill = "Lo realiza") +
  ggtitle("Distribución de los productores según manejo integrado de plagas") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Pregunta 23

Nada_total$P23_A = case_when(Nada_total$P23_1 == "1ro" ~ 1,
                            Nada_total$P23_1 == "2do" ~ 2,
                            Nada_total$P23_1 == "3ro" ~ 3,
                            Nada_total$P23_1 == "4to" ~ 4,
                            T ~ 5)

Nada_total$P23_B = case_when(Nada_total$P23_2 == "1ro" ~ 1,
                            Nada_total$P23_2 == "2do" ~ 2,
                            Nada_total$P23_2 == "3ro" ~ 3,
                            Nada_total$P23_2 == "4to" ~ 4,
                            T ~ 5)

Nada_total$P23_C = case_when(Nada_total$P23_3 == "1ro" ~ 1,
                            Nada_total$P23_3 == "2do" ~ 2,
                            Nada_total$P23_3 == "3ro" ~ 3,
                            Nada_total$P23_3 == "4to" ~ 4,
                            T ~ 5)

Nada_total$P23_D = case_when(Nada_total$P23_4 == "1ro" ~ 1,
                            Nada_total$P23_4 == "2do" ~ 2,
                            Nada_total$P23_4 == "3ro" ~ 3,
                            Nada_total$P23_4 == "4to" ~ 4,
                            T ~ 5)

Nada_total$P23_E = case_when(Nada_total$P23_5 == "1ro" ~ 1,
                            Nada_total$P23_5 == "2do" ~ 2,
                            Nada_total$P23_5 == "3ro" ~ 3,
                            Nada_total$P23_5 == "4to" ~ 4,
                            T ~ 5)

tabla=Nada_total %>% 
  summarise(`Precio`= mean(P23_A),
            `Desconfianza uso de datos` = mean(P23_B),
            `No soluciona un problema` = mean(P23_C),
            `Falta de infraestructura` = mean(P23_D),
            `Desconocimiento` = mean(P23_E))

write.xlsx(tabla, "D:/Otros/Austral/tablas/P23 nada total.xlsx",overwrite = F)


tabla=Nada_total %>% 
  summarise(Impresos_media=mean(c(P35a1_1,P35a1_2,P35a1_3)),
            Persona_media=mean(c(P35a2_1,P35a2_2,P35a2_3)),
            Digitales_media=mean(c(P35a3_1,P35a3_2,P35a3_3,P35a3_4,P35a3_5,P35a3_6,P35a3_7)),
            Redes_media=mean(c(P35a4_1,P35a4_2,P35a4_3,P35a4_4,P35a4_5,P35a4_6,P35a4_7)))


tabla2=Nada_total %>%
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

write.xlsx(tabla2, "D:/Otros/Austral/tablas/P35 nada total.xlsx",overwrite = F)
