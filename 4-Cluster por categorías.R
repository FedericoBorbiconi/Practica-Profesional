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
library(patchwork)
Datos_ENPA <- read_excel("D:/Otros/Austral/Datos ENPA.xlsx")

# Pregunta 49 (Estilo cognitivo)
Datos_ENPA$Estilo_cognitivo = case_when(Datos_ENPA$P49<4 ~ "Intuitivo",
                                        Datos_ENPA$P49<7 ~ "Balanceado",
                                        T ~ "Analítico")

# Edad Categórica
Datos_ENPA$Edad = case_when(Datos_ENPA$P2años<35 ~ " Menor a 35 años",
                            Datos_ENPA$P2años<45 ~ "35 a 44 años",
                            Datos_ENPA$P2años<55 ~ "45 a 54 años",
                            Datos_ENPA$P2años<65 ~ "55 a 64 años",
                            T ~ "Mayor a 65 años")

# Tamaño del productor
Datos_ENPA$Tamaño = case_when(Datos_ENPA$P8a1<601 ~ "Mediano",
                              Datos_ENPA$P8a1<1841 ~ "Comercial",
                              Datos_ENPA$P8a1<10000 ~ "Grande",
                              T ~ "Mega")

Datos_ENPA$Tamaño = factor(Datos_ENPA$Tamaño,levels = c("Mediano",
                                                        "Comercial",
                                                        "Grande",
                                                        "Mega"),ordered = T)

Datos_ENPA$Tamaño = case_when(Datos_ENPA$P8a1<601 ~ "Mediano",
                              Datos_ENPA$P8a1<1841 ~ "Comercial",
                              T ~ "Grande")

Datos_ENPA$Tamaño = factor(Datos_ENPA$Tamaño,levels = c("Mediano",
                                                        "Comercial",
                                                        "Grande"),ordered = T)


# Base cluster ordinal
Datos_ordinal=Datos_ENPA %>% 
  dplyr::select(P16f,P16g,P16h,
                P18_3,P18_6,P18_7,P18_8,P18_9,
                P25a_1,P25b_1,P25d_1,
                P25a_3,P25b_3,P25d_3,
                P25a_5,P25b_5,P25d_5,
                P25a_6,P25b_6,P25d_6,
                P22_1,P22_2,P22_5,P22_6,P22_7,P22_8,P22_11,P22_12,
                P38_3,P38_4,P38_5,
                P22_10)

col_names <- names(Datos_ordinal)
Datos_ordinal <- Datos_ordinal %>%
  mutate(across(all_of(col_names), as.factor))
table(Datos_ordinal$P16f)

Datos_ordinal$P16f = case_when(Datos_ordinal$P16f=="No planeo invertir" ~ "No planeo invertir",
                               Datos_ordinal$P16f=="Invertiré asociado con otros en los próximos 5 años" ~ "Invertiré en los próximos 5 años",
                               Datos_ordinal$P16f=="Invertiré por mi cuenta en los próximos 5 años" ~ "Invertiré en los próximos 5 años",
                               Datos_ordinal$P16f=="Invertiré asociado con otros en los próximos 12 meses" ~ "Invertiré en los próximos 12 meses",
                               Datos_ordinal$P16f=="Invertiré por mi cuenta en los próximos 12 meses" ~ "Invertiré en los próximos 12 meses",
                               Datos_ordinal$P16f=="Ya he invertido en este rubro en los últimos 12 meses" ~ "Ya invertí en los últimos 12 meses",
                               Datos_ordinal$P16f=="Ya he invertido en este rubro en los últimos 5 años" ~ "Ya invertí en los últimos 5 años")


Datos_ordinal$P16f = factor(Datos_ordinal$P16f,levels = c("No planeo invertir","Invertiré en los próximos 5 años",
                                                          "Invertiré en los próximos 12 meses",
                                                          "Ya invertí en los últimos 12 meses",
                                                          "Ya invertí en los últimos 5 años"),ordered = T)


Datos_ordinal$P16g = case_when(Datos_ordinal$P16g=="No planeo invertir" ~ "No planeo invertir",
                               Datos_ordinal$P16g=="Invertiré asociado con otros en los próximos 5 años" ~ "Invertiré en los próximos 5 años",
                               Datos_ordinal$P16g=="Invertiré por mi cuenta en los próximos 5 años" ~ "Invertiré en los próximos 5 años",
                               Datos_ordinal$P16g=="Invertiré asociado con otros en los próximos 12 meses" ~ "Invertiré en los próximos 12 meses",
                               Datos_ordinal$P16g=="Invertiré por mi cuenta en los próximos 12 meses" ~ "Invertiré en los próximos 12 meses",
                               Datos_ordinal$P16g=="Ya he invertido en este rubro en los últimos 12 meses" ~ "Ya invertí en los últimos 12 meses",
                               Datos_ordinal$P16g=="Ya he invertido en este rubro en los últimos 5 años" ~ "Ya invertí en los últimos 5 años")


Datos_ordinal$P16g = factor(Datos_ordinal$P16g,levels = c("No planeo invertir","Invertiré en los próximos 5 años",
                                                          "Invertiré en los próximos 12 meses",
                                                          "Ya invertí en los últimos 12 meses",
                                                          "Ya invertí en los últimos 5 años"),ordered = T)

Datos_ordinal$P16h = case_when(Datos_ordinal$P16h=="No planeo invertir" ~ "No planeo invertir",
                               Datos_ordinal$P16h=="Invertiré asociado con otros en los próximos 5 años" ~ "Invertiré en los próximos 5 años",
                               Datos_ordinal$P16h=="Invertiré por mi cuenta en los próximos 5 años" ~ "Invertiré en los próximos 5 años",
                               Datos_ordinal$P16h=="Invertiré asociado con otros en los próximos 12 meses" ~ "Invertiré en los próximos 12 meses",
                               Datos_ordinal$P16h=="Invertiré por mi cuenta en los próximos 12 meses" ~ "Invertiré en los próximos 12 meses",
                               Datos_ordinal$P16h=="Ya he invertido en este rubro en los últimos 12 meses" ~ "Ya invertí en los últimos 12 meses",
                               Datos_ordinal$P16h=="Ya he invertido en este rubro en los últimos 5 años" ~ "Ya invertí en los últimos 5 años")


Datos_ordinal$P16h = factor(Datos_ordinal$P16h,levels = c("No planeo invertir","Invertiré en los próximos 5 años",
                                                          "Invertiré en los próximos 12 meses",
                                                          "Ya invertí en los últimos 12 meses",
                                                          "Ya invertí en los últimos 5 años"),ordered = T)



Datos_ordinal$P18_3 = factor(Datos_ordinal$P18_3, levels = c("No lo hago y no creo hacerlo en el futuro (5 años)",
                                                             "Actualmente no lo hago, pero posiblemente lo haga a futuro (5 años)",
                                                             "Lo hago actualmente de (de forma limitada)",
                                                             "Lo hago actualmente (de forma intensiva)"), ordered = T)

Datos_ordinal$P18_6 = factor(Datos_ordinal$P18_6, levels = c("No lo hago y no creo hacerlo en el futuro (5 años)",
                                                             "Actualmente no lo hago, pero posiblemente lo haga a futuro (5 años)",
                                                             "Lo hago actualmente de (de forma limitada)",
                                                             "Lo hago actualmente (de forma intensiva)"), ordered = T)

Datos_ordinal$P18_7 = factor(Datos_ordinal$P18_7, levels = c("No lo hago y no creo hacerlo en el futuro (5 años)",
                                                             "Actualmente no lo hago, pero posiblemente lo haga a futuro (5 años)",
                                                             "Lo hago actualmente de (de forma limitada)",
                                                             "Lo hago actualmente (de forma intensiva)"), ordered = T)

Datos_ordinal$P18_8 = factor(Datos_ordinal$P18_8, levels = c("No lo hago y no creo hacerlo en el futuro (5 años)",
                                                             "Actualmente no lo hago, pero posiblemente lo haga a futuro (5 años)",
                                                             "Lo hago actualmente de (de forma limitada)",
                                                             "Lo hago actualmente (de forma intensiva)"), ordered = T)

Datos_ordinal$P18_9 = factor(Datos_ordinal$P18_9, levels = c("No lo hago y no creo hacerlo en el futuro (5 años)",
                                                             "Actualmente no lo hago, pero posiblemente lo haga a futuro (5 años)",
                                                             "Lo hago actualmente de (de forma limitada)",
                                                             "Lo hago actualmente (de forma intensiva)"), ordered = T)

# Funciones
best_k_proto = function(data, k, i=20, cluster=T){
  
  clusters <- vector("list", length = i)
  medida=numeric(i)
  
  for(j in 1:i){
    resultado = kproto(data,k,type = "gower")
    clusters[[j]]=resultado
    medida[j]=resultado$tot.withinss
  }
  
  cat(which.min(medida))
  cat("\n")
  cat(medida)
  if(cluster==T){
    return(clusters[[which.min(medida)]])
  } else{
    return(mean(medida))
  }
  
  
}

silhouette_fede = function(data,cluster,k){
  n=nrow(data)
  a=numeric(n)
  b=numeric(n)
  s=numeric(n)
  
  
  for(i in 1:n){
    c=cluster[i]
    A = data[unique(c(i,which(cluster==c))),]
    dist=daisy(A,metric = "gower")
    a[i]=mean(dist[1:(nrow(A)-1)])
    
    for(j in 1:k){
      if(j!=c){
        B = data[c(i,which(cluster==j)),]
        dist=daisy(B,metric = "gower")
        aux=mean(dist[1:(nrow(B)-1)])
        if(b[i]==0 | aux<=b[i]){
          b[i]=aux
        }
      }
    }
    s[i]=(b[i]-a[i])/max(a[i],b[i])
  }
  
  return(mean(s))
}

# Biomasa

Datos_cluster = Datos_ordinal[,c(1:6)]

set.seed(167)
a=best_k_proto(Datos_cluster,k=4,i=150)

table(a$cluster)

silhouette_fede(Datos_cluster,a$cluster,4)

Datos_ENPA$Biomasa = a$cluster

Datos_ENPA$Biomasa = case_when(Datos_ENPA$Biomasa==1 ~ "Todo",
                               Datos_ENPA$Biomasa==2 ~ "Desperdicios",
                               Datos_ENPA$Biomasa==3 ~ "Extrusora\nde soja",
                               T ~ "Nada")

Datos_ENPA$Biomasa = factor(Datos_ENPA$Biomasa,levels = c("Todo",
                                                        "Desperdicios",
                                                        "Extrusora\nde soja",
                                                        "Nada"),ordered = T)
# Gráficos
Datos_ENPA$P16f = case_when(Datos_ENPA$P16f=="No planeo invertir" ~ "No planeo invertir",
                            Datos_ENPA$P16f=="Ya he invertido en este rubro en los últimos 12 meses" ~ "Ya invertí",
                            Datos_ENPA$P16f=="Ya he invertido en este rubro en los últimos 5 años" ~ "Ya invertí",
                            T ~ "Planeo invertir")

Datos_ENPA$P16g = case_when(Datos_ENPA$P16g=="No planeo invertir" ~ "No planeo invertir",
                            Datos_ENPA$P16g=="Ya he invertido en este rubro en los últimos 12 meses" ~ "Ya invertí",
                            Datos_ENPA$P16g=="Ya he invertido en este rubro en los últimos 5 años" ~ "Ya invertí",
                            T ~ "Planeo invertir")

Datos_ENPA$P16h = case_when(Datos_ENPA$P16h=="No planeo invertir" ~ "No planeo invertir",
                            Datos_ENPA$P16h=="Ya he invertido en este rubro en los últimos 12 meses" ~ "Ya invertí",
                            Datos_ENPA$P16h=="Ya he invertido en este rubro en los últimos 5 años" ~ "Ya invertí",
                            T ~ "Planeo invertir")


g1=Datos_ENPA %>%
  mutate(P16f=as.factor(P16f)) %>% 
  count(Biomasa, P16f) %>%       
  group_by(Biomasa) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(Biomasa, pct, fill=P16f) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022","#7D212B"),name="Comportamiento\ndel productor",labels = c("No planeo invertir","Planeo invertir","Ya invertí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Inversión en biocombustibles") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P16g=as.factor(P16g)) %>% 
  count(Biomasa, P16g) %>%       
  group_by(Biomasa) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(Biomasa, pct, fill=P16g) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022","#7D212B"),name="Comportamiento\ndel productor",labels = c("No planeo invertir","Planeo invertir","Ya invertí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Inversión en biodigestor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g3=Datos_ENPA %>%
  mutate(P16h=as.factor(P16h)) %>% 
  count(Biomasa, P16h) %>%       
  group_by(Biomasa) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(Biomasa, pct, fill=P16h) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022","#7D212B"),name="Comportamiento\ndel productor",labels = c("No planeo invertir","Planeo invertir","Ya invertí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Inversión en extrusora de soja") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g4=Datos_ENPA %>%
  mutate(P18_3=as.factor(P18_3)) %>% 
  count(Biomasa, P18_3) %>%       
  group_by(Biomasa) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(Biomasa, pct, fill=P18_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#7D212B","#C00022","#D35865","#767676"),name="Comportamiento\ndel productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Uso de desperdicio de animales para biogás") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g5=Datos_ENPA %>%
  mutate(P18_6=as.factor(P18_6)) %>% 
  count(Biomasa, P18_6) %>%       
  group_by(Biomasa) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(Biomasa, pct, fill=P18_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#7D212B","#C00022","#D35865","#767676"),name="Comportamiento\ndel productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Uso de residuos de cosecha para energía") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g6=Datos_ENPA %>%
  mutate(P18_7=as.factor(P18_7)) %>% 
  count(Biomasa, P18_7) %>%       
  group_by(Biomasa) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(Biomasa, pct, fill=P18_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#7D212B","#C00022","#D35865","#767676"),name="Comportamiento\ndel productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Uso de residuos de cosecha para biomateriales") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

ggarrange(g1,g2,g3,g4,g5,g6, nrow = 3, ncol = 2)


frecuencia <- as.data.frame(table(Datos_ENPA$Biomasa))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#D35865","#767676")) +
  labs(title = "Distribución de los productores según transformación de biomasa", fill = "Grupo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0))

# Compra online de insumos

Datos_ENPA$Compra = case_when(Datos_ENPA$P38_5=="Si" ~ "Regularmente compro\nalgunos insumos online",
                              Datos_ENPA$P38_4=="Si" ~ "Regularmente compro\nalgunos insumos online",
                              Datos_ENPA$P38_3=="Si" ~ "He realizado 1 o 2\ncompras online de insumos",
                              Datos_ENPA$P38_2=="Si" ~ "Fui de compras pero\nno concreté ninguna",
                              Datos_ENPA$P38_1=="Si" ~ "No fui de\ncompras online")

Datos_ENPA$Compra = factor(Datos_ENPA$Compra,levels = c("No fui de\ncompras online",
                                                        "Fui de compras pero\nno concreté ninguna",
                                                        "He realizado 1 o 2\ncompras online de insumos",
                                                        "Regularmente compro\nalgunos insumos online"),ordered = T)

# Gráficos
frecuencia <- as.data.frame(table(Datos_ENPA$Compra))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","gray75","#C00022","#7D212B")) +
  labs(title = "Comportamiento de los productores en relación a la compra online de insumos", fill = "Frecuencia de\ncompra") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0))

# Uso de datos
Datos_cluster = Datos_ordinal[,c(23:25,32)]

set.seed(767)
b=best_k_proto(Datos_cluster,k=4,i=150)

table(b$cluster)

silhouette_fede(Datos_cluster,b$cluster,4)


Datos_ENPA$`Uso de datos` = b$cluster

Datos_ENPA$`Uso de datos` = case_when(Datos_ENPA$`Uso de datos`==1 ~ "Condiciones\nmeteorológicas",
                                      Datos_ENPA$`Uso de datos`==2 ~ "Todo",
                                      Datos_ENPA$`Uso de datos`==3 ~ "Monitoreo\nsatelital",
                                      T ~ "Nada")

Datos_ENPA$`Uso de datos` = factor(Datos_ENPA$`Uso de datos`,levels = c("Todo",
                                                        "Condiciones\nmeteorológicas",
                                                        "Monitoreo\nsatelital",
                                                        "Nada"),ordered = T)

# Gráficos
g2=Datos_ENPA %>%
  mutate(P22_5=as.factor(P22_5)) %>% 
  count(`Uso de datos`, P22_5) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P22_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Monitoreo satelital de cultivos") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_6=as.factor(P22_6)) %>% 
  count(`Uso de datos`, P22_6) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P22_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Imágenes fotográficas a través de robots y drones") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_7=as.factor(P22_7)) %>% 
  count(`Uso de datos`, P22_7) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P22_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Plataforma de gestión agrícola") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g5=Datos_ENPA %>%
  mutate(P22_10=as.factor(P22_10)) %>% 
  count(`Uso de datos`, P22_10) %>%       
  group_by(`Uso de datos`) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(`Uso de datos`, pct, fill=P22_10) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Relevamiento online condiciones meteorológicas") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

ggarrange(g2,g3,g4,g5,nrow = 2, ncol = 2)

frecuencia <- as.data.frame(table(Datos_ENPA$`Uso de datos`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#D35865","#767676")) +
  labs(title = "Distribución de los productores según uso de datos", fill = "Grupo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0))

# Tecnologías de producción

Datos_cluster = Datos_ordinal[,c(21,27,28)]

set.seed(666)
c=best_k_proto(Datos_cluster,k=3,i=150)

table(c$cluster)

silhouette_fede(Datos_cluster,c$cluster,3)

Datos_ENPA$`Tecnologías producción` = c$cluster

Datos_ENPA$`Tecnologías producción` = case_when(Datos_ENPA$`Tecnologías producción`==1 ~ "Nada",
                                                Datos_ENPA$`Tecnologías producción`==2 ~ "Siembra y\nfertilización variable",
                                                T ~ "Todo")

table(Datos_ENPA$`Tecnologías producción`)

# Gráficos
g1=Datos_ENPA %>%
  mutate(P22_1=as.factor(P22_1)) %>% 
  count(`Tecnologías producción`, P22_1) %>%       
  group_by(`Tecnologías producción`) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(`Tecnologías producción`, pct, fill=P22_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Siembra y fertilización variable por ambiente") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g6=Datos_ENPA %>%
  mutate(P22_11=as.factor(P22_11)) %>% 
  count(`Tecnologías producción`, P22_11) %>%       
  group_by(`Tecnologías producción`) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(`Tecnologías producción`, pct, fill=P22_11) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Riego por ambientes") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g7=Datos_ENPA %>%
  mutate(P22_12=as.factor(P22_12)) %>% 
  count(`Tecnologías producción`, P22_12) %>%       
  group_by(`Tecnologías producción`) %>%
  mutate(pct= round(prop.table(n) * 100,0)) %>% 
  ggplot() + aes(`Tecnologías producción`, pct, fill=P22_12) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#767676","#C00022"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Cluster")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5),
            color="white") +
  ggtitle("Pulverización selectiva") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

ggarrange(g1,g6,g7,nrow = 2, ncol=2)

frecuencia <- as.data.frame(table(Datos_ENPA$`Tecnologías producción`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022","#7D212B")) +
  labs(title = "Distribución de los productores según agricultura de precisión", fill = "Grupo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0))


# Análisis entre clusters
Datos_grupos=Datos_ENPA %>% 
  select(Biomasa,Compra,`Uso de datos`,`Tecnologías producción`)

# Tablas
# Tablas biomasa
table(Datos_grupos$Biomasa,Datos_grupos$`Uso de datos`)

chisq.test(Datos_grupos$Biomasa,Datos_grupos$`Uso de datos`)

table(Datos_grupos$Biomasa,Datos_grupos$`Tecnologías producción`)

chisq.test(Datos_grupos$Biomasa,Datos_grupos$`Tecnologías producción`)

table(Datos_grupos$Biomasa,Datos_grupos$Compra)

chisq.test(Datos_grupos$Biomasa,Datos_grupos$Compra)

# Tablas Uso de datos

table(Datos_grupos$`Uso de datos`,Datos_grupos$`Tecnologías producción`)

chisq.test(Datos_grupos$`Uso de datos`,Datos_grupos$`Tecnologías producción`)

table(Datos_grupos$`Uso de datos`,Datos_grupos$Compra)

chisq.test(Datos_grupos$`Uso de datos`,Datos_grupos$Compra)

# Tabla tecno prod

table(Datos_grupos$`Tecnologías producción`,Datos_grupos$Compra)

chisq.test(Datos_grupos$`Tecnologías producción`,Datos_grupos$Compra)

# Diagramas de venn
library(ggVennDiagram)

# Más innovadores
x <- list(Biomasa = which(Datos_grupos$Biomasa==" Todo"),
          `Compra online` = which(Datos_grupos$Compra=="Regularmente compro\nalgunos insumos online"),
          `Uso de datos` = which(Datos_grupos$`Uso de datos`==" Todo"),
          `Tecnologías\nproducción` = unique(which(Datos_grupos$`Tecnologías producción`=="Todo")))

ggVennDiagram(x,color = "black", lwd = 0.8, lty = 1, label = "count") +
  scale_color_manual(values=c("black","black","black","black"))+
  scale_fill_gradient(low = "#F4FAFE", high = "#C00022")+
  theme(legend.position='none')


# Menos innovadores
y <- list(Biomasa = which(Datos_grupos$Biomasa=="Nada"),
          `Compra online` = which(Datos_grupos$Compra=="No fui de\ncompras online"),
          `Uso de datos` = which(Datos_grupos$`Uso de datos`=="Nada"),
          `Tecnologías\nproducción` = unique(which(Datos_grupos$`Tecnologías producción`=="Nada")))

ggVennDiagram(y,color = "black", lwd = 0.8, lty = 1, label = "count") +
  scale_color_manual(values=c("black","black","black","black"))+
  scale_fill_gradient(low = "#F4FAFE", high = "#C00022") +
  theme(legend.position='none')

# Al menos innovan en algo
z <- list(Biomasa = which(Datos_grupos$Biomasa!="Nada"),
          `Compra online` = which(Datos_grupos$Compra!="No fui de compras online"),
          `Uso de datos` = which(Datos_grupos$`Uso de datos`!="Nada"),
          `Tecnologías\nproducción` = unique(which(Datos_grupos$`Tecnologías producción`!="Nada")))

ggVennDiagram(z,color = "black", lwd = 0.8, lty = 1) +
  scale_color_manual(values=c("black","black","black","black"))+
  scale_fill_gradient(low = "#F4FAFE", high = "coral")

# Al menos innovan en algo sin explorar compra online
w <- list(Biomasa = which(Datos_grupos$Biomasa!="Nada"),
          `Compra online` = which(Datos_grupos$Compra!="No fui de\ncompras online" & Datos_grupos$Compra!="Fui de compras pero\nno concreté ninguna"),
          `Uso de datos` = which(Datos_grupos$`Uso de datos`!="Nada"),
          `Tecnologías\nproducción` = unique(which(Datos_grupos$`Tecnologías producción`!="Nada")))

ggVennDiagram(w,color = "black", lwd = 0.8, lty = 1, label = "count") +
  scale_color_manual(values=c("black","black","black","black"))+
  scale_fill_gradient(low = "#F4FAFE", high = "#C00022")+
  theme(legend.position='none')

# Grupos

Nada_total=Datos_ENPA[which(Datos_ENPA$Biomasa=="Nada" & 
                              Datos_ENPA$Compra=="No fui de\ncompras online" &
                              Datos_ENPA$`Uso de datos`=="Nada" &
                              Datos_ENPA$`Tecnologías producción`=="Nada"),]

Algo_full = Datos_ENPA[which(Datos_ENPA$Biomasa=="Todo" | 
                               Datos_ENPA$Compra=="Regularmente compro\nalgunos insumos online" |
                               Datos_ENPA$`Uso de datos`=="Todo" |
                               Datos_ENPA$`Tecnologías producción`=="Todo"),]

Todo_básico = Datos_ENPA[which(Datos_ENPA$Biomasa!="Nada" & 
                                 Datos_ENPA$Compra!="No fui de\ncompras online" &
                                 Datos_ENPA$Compra!="Fui de compras pero\nno concreté ninguna" &
                                 Datos_ENPA$`Uso de datos`!="Nada" &
                                 Datos_ENPA$`Tecnologías producción`!="Nada"),]

# Algo_full
frecuencia <- as.data.frame(table(Algo_full$Biomasa))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#D35865","#767676")) +
  labs(fill = "Cluster Biomasa") +
  ggtitle("Biomasa") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Algo_full$Compra))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","gray75","#C00022","#7D212B")) +
  labs(fill = "Frecuencia de compra") +
  ggtitle("Compra online de insumos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Algo_full$`Uso de datos`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#D35865","#767676")) +
  labs(fill = "Cluster Uso de datos") +
  ggtitle("Uso de datos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Algo_full$`Tecnologías producción`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g4=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022","#7D212B")) +
  labs(fill = "Cluster Agricultura\nde precisión") +
  ggtitle("Agricultura de precisión") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Composición del grupo de productores innovadores", face = "bold", size = 14))


# Todo_básico
frecuencia <- as.data.frame(table(as.character(Todo_básico$Biomasa)))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#767676")) +
  labs(fill = "Cluster Biomasa") +
  ggtitle("Biomasa") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(as.character(Todo_básico$Compra)))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022"), labels = c("He realizado 1 o 2 compras\nonline de insumos","Regularmente compro\nalgunos insumos online")) +
  labs(fill = "Frecuencia de compra") +
  ggtitle("Compra online de insumos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(as.character(Todo_básico$`Uso de datos`)))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#767676")) +
  labs(fill = "Cluster Uso de datos") +
  ggtitle("Uso de datos") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Todo_básico$`Tecnologías producción`))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g4=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022")) +
  labs(fill = "Cluster Agricultura\nde precisión") +
  ggtitle("Agricultura de precisión") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Composición del grupo de productores muy innovadores", face = "bold", size = 14))


# Edad
g1=Datos_ENPA %>% 
  ggplot(aes(P2años))+
  geom_histogram(aes(y=after_stat(density)),breaks=seq(22,78,4), fill="#D35865", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  #scale_y_continuous(limits = c(0,0.07)) +
  geom_vline(aes(xintercept = median(P2años),
                 color = "mediana"),
             linetype = "dashed",
             linewidth = 1) +
  scale_color_manual(values = c(mediana = "black"))+
  xlab("Edad del productor") +
  ylab("Densidad de productores") +
  ggtitle("Perfil general") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = 0)

quantile(Datos_ENPA$P2años,seq(0,1,0.01))
median(Datos_ENPA$P2años)

g2=Nada_total %>% 
  ggplot(aes(P2años))+
  geom_histogram(aes(y=after_stat(density)),breaks=seq(22,78,4), fill="#D35865", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  #scale_y_continuous(limits = c(0,0.07)) +
  geom_vline(aes(xintercept = median(P2años),
                 color = "mediana"),
             linetype = "dashed",
             linewidth = 1) +
  scale_color_manual(values = c(mediana = "black"))+
  xlab("Edad del productor") +
  ylab("Densidad de productores") +
  ggtitle("No innovadores") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = 0)

quantile(Nada_total$P2años,seq(0,1,0.01))
median(Nada_total$P2años)

g3=Algo_full %>% 
  ggplot(aes(P2años))+
  geom_histogram(aes(y=after_stat(density)),breaks=seq(22,78,4), fill="#D35865", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  #scale_y_continuous(limits = c(0,0.07)) +
  geom_vline(aes(xintercept = median(P2años),
                 color = "mediana"),
             linetype = "dashed",
             linewidth = 1) +
  scale_color_manual(values = c(mediana = "black"))+
  xlab("Edad del productor") +
  ylab("Densidad de productores") +
  ggtitle("Innovadores") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = 0)

quantile(Algo_full$P2años,seq(0,1,0.01))
median(Algo_full$P2años)

g4=Todo_básico %>% 
  ggplot(aes(P2años))+
  geom_histogram(aes(y=after_stat(density)),breaks=seq(22,78,4), fill="#D35865", color="black")+
  scale_x_continuous(breaks = seq(22,78,4)) +
  #scale_y_continuous(limits = c(0,0.07)) +
  geom_vline(aes(xintercept = median(P2años),
                 color = "mediana"),
             linetype = "dashed",
             linewidth = 1) +
  scale_color_manual(values = c(mediana = "black"))+
  xlab("Edad del productor") +
  ylab("Densidad de productores") +
  ggtitle("Muy Innovadores") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = 0)

quantile(Todo_básico$P2años,seq(0,1,0.01))
median(Todo_básico$P2años)

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Distribución de los productores según su edad", face = "bold", size = 14))

# Tamaño
frecuencia <- as.data.frame(table(Datos_ENPA$Tamaño))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022","#7D212B")) +
  labs(fill = "Tamaño") +
  ggtitle("Perfil general") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Nada_total$Tamaño))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022","#7D212B")) +
  labs(fill = "Tamaño") +
  ggtitle("No innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$Tamaño))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022","#7D212B")) +
  labs(fill = "Tamaño") +
  ggtitle("Innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Todo_básico$Tamaño))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g4=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022","#7D212B")) +
  labs(fill = "Tamaño") +
  ggtitle("Muy Innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Distribución de los productores según su tamaño", face = "bold", size = 14))


# Provincia
frecuencia <- as.data.frame(table(Datos_ENPA$Provincia))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#D35865","#767676")) +
  labs(fill = "Provincia") +
  ggtitle("Perfil general") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Nada_total$Provincia))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#D35865","#767676")) +
  labs(fill = "Provincia") +
  ggtitle("No innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$Provincia))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#D35865","#767676")) +
  labs(fill = "Provincia") +
  ggtitle("Innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Todo_básico$Provincia))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g4=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#7D212B","#C00022","#767676")) +
  labs(fill = "Provincia") +
  ggtitle("Muy Innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Distribución de los productores según su provincia", face = "bold", size = 14))


# Planificación financiera
frecuencia <- as.data.frame(table(Datos_ENPA$P31))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022")) +
  labs(fill = "Planifica") +
  ggtitle("Perfil general") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Nada_total$P31))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022")) +
  labs(fill = "Planifica") +
  ggtitle("220 No innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$P31))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022")) +
  labs(fill = "Planifica") +
  ggtitle("253 Innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Todo_básico$P31))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g4=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#C00022")) +
  labs(fill = "Planifica") +
  ggtitle("40 Innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Distribución de los productores según planificación financiera", face = "bold", size = 14))

# Planificación inversiones
frecuencia <- as.data.frame(table(Datos_ENPA$P32))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022")) +
  labs(fill = "Planifica") +
  ggtitle("Perfil general") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Nada_total$P32))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022")) +
  labs(fill = "Planifica") +
  ggtitle("No innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Algo_full$P32))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022")) +
  labs(fill = "Planifica") +
  ggtitle("Innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Todo_básico$P32))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 0)

g4=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#767676","#C00022")) +
  labs(fill = "Planifica") +
  ggtitle("Muy Innovadores") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Distribución de los productores según planificación de inversiones", face = "bold", size = 14))
