
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(ggpubr)
library(broom)
library(egg)
Datos_ENPA <- read_excel("C:/Users/feder/Desktop/Datos ENPA.xlsx")

# Pregunta 49 (Estilo cognitivo)
Datos_ENPA$Estilo_cognitivo = case_when(Datos_ENPA$P49<4 ~ "Intuitivo",
                                        Datos_ENPA$P49<7 ~ "Balanceado",
                                        T ~ "Analítico")

# Edad Categórica
Datos_ENPA$Edad = case_when(Datos_ENPA$P2años<35 ~ "Menor a 35 años",
                            Datos_ENPA$P2años<45 ~ "35 a 44 años",
                            Datos_ENPA$P2años<55 ~ "45 a 54 años",
                            Datos_ENPA$P2años<65 ~ "55 a 64 años",
                            T ~ "Mayor a 65 años")

Datos_ENPA$Edad = factor(Datos_ENPA$Edad,levels = c("Menor a 35 años",
                                                    "35 a 44 años",
                                                    "45 a 54 años",
                                                    "55 a 64 años",
                                                    "Mayor a 65 años"),ordered = T)

# Tamaño del productor
Datos_ENPA$Tamaño = case_when(Datos_ENPA$P8a1<601 ~ " Mediano",
                              Datos_ENPA$P8a1<1841 ~ "Comercial",
                              Datos_ENPA$P8a1<10000 ~ "Grande",
                              T ~ "Mega")

table(Datos_ENPA$Tamaño)

# Características sociodemográficas
frecuencia <- as.data.frame(table(Datos_ENPA$Estilo_cognitivo))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g1=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#F5C710","#DF536B")) +
  labs(title = "Estilo cognitivo del productor", fill = "Estilo cognitivo") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

frecuencia <- as.data.frame(table(Datos_ENPA$Edad))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g2=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC")) +
  labs(title = "Edad del productor", fill = "Edad") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


frecuencia <- as.data.frame(table(Datos_ENPA$Tamaño))
frecuencia$Porcentaje <- round((frecuencia$Freq / sum(frecuencia$Freq)) * 100, 2)

g3=ggplot(frecuencia, aes(x = "", y = Porcentaje, fill = Var1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color="black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#61D04F","#2297E6","#F5C710","#DF536B")) +
  labs(title = "Tamaño del productor", fill = "Tamaño") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(g1,g2,g3)

# Pregunta 16

table(Datos_ENPA$P16f)
table(Datos_ENPA$P16f,Datos_ENPA$Estilo_cognitivo)
table(Datos_ENPA$P16f,Datos_ENPA$Edad)
g1=Datos_ENPA %>%
  mutate(P16f=as.factor(P16f)) %>% 
  count(P16f) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P16f, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor")+
  scale_x_discrete(labels = c("Invertiré asociado \n12 meses","Invertiré solo \n12 meses","Invertiré solo \n5 años","No planeo \ninvertir","Ya invertí \n12 meses","Ya invertí \n5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P16f=as.factor(P16f)) %>% 
  count(Estilo_cognitivo, P16f) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P16f) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P16f=as.factor(P16f)) %>% 
  count(Edad, P16f) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P16f) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+
  scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P16f=as.factor(P16f)) %>% 
  count(Tamaño, P16f) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P16f) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°1: Inversión en biocombustibles (bioetanol y biodiesel)", face = "bold", size = 14))


table(Datos_ENPA$P16g)
table(Datos_ENPA$P16g,Datos_ENPA$Estilo_cognitivo)
table(Datos_ENPA$P16g,Datos_ENPA$Edad)
g1=Datos_ENPA %>%
  mutate(P16g=as.factor(P16g)) %>% 
  count(P16g) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P16g, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor")+
  scale_x_discrete(labels = c("Invertiré asociado \n12 meses","Invertiré solo \n12 meses","Invertiré solo \n5 años","No planeo \ninvertir","Ya invertí \n12 meses","Ya invertí \n5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P16g=as.factor(P16g)) %>% 
  count(Estilo_cognitivo, P16g) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P16g) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P16g=as.factor(P16g)) %>% 
  count(Edad, P16g) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P16g) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P16g=as.factor(P16g)) %>% 
  count(Tamaño, P16g) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P16g) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°2: Inversión en biodigestor para la producción de biogás", face = "bold", size = 14))


table(Datos_ENPA$P16h)
table(Datos_ENPA$P16h,Datos_ENPA$Estilo_cognitivo)
table(Datos_ENPA$P16h,Datos_ENPA$Edad)
g1=Datos_ENPA %>%
  mutate(P16h=as.factor(P16h)) %>% 
  count(P16h) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P16h, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor")+
  scale_x_discrete(labels = c("Invertiré asociado \n12 meses","Invertiré solo \n12 meses","Invertiré solo \n5 años","No planeo \ninvertir","Ya invertí \n12 meses","Ya invertí \n5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P16h=as.factor(P16h)) %>% 
  count(Estilo_cognitivo, P16h) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P16h) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P16h=as.factor(P16h)) %>% 
  count(Edad, P16h) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P16h) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P16h=as.factor(P16h)) %>% 
  count(Tamaño, P16h) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P16h) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B","#CD0BBC","#28E2E5"),name="Comportamiento del productor",labels = c("Invertiré asociado 12 meses","Invertiré solo 12 meses","Invertiré solo 5 años","No planeo invertir","Ya invertí 12 meses","Ya invertí 5 años"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°3: Inversión en extrusora de soja", face = "bold", size = 14))


# Pregunta 18

table(Datos_ENPA$P18_3)

g1=Datos_ENPA %>%
  mutate(P18_3=as.factor(P18_3)) %>% 
  count(P18_3) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P18_3, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor")+
  scale_x_discrete(labels = c("No ahora, si en 5 años","Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P18_3=as.factor(P18_3)) %>% 
  count(Estilo_cognitivo, P18_3) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P18_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P18_3=as.factor(P18_3)) %>% 
  count(Edad, P18_3) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P18_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P18_3=as.factor(P18_3)) %>% 
  count(Tamaño, P18_3) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P18_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°4: Uso de desperdicio de animales para biogás", face = "bold", size = 14))


table(Datos_ENPA$P18_6)

g1=Datos_ENPA %>%
  mutate(P18_6=as.factor(P18_6)) %>% 
  count(P18_6) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P18_6, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor")+
  scale_x_discrete(labels = c("No ahora, si en 5 años","Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P18_6=as.factor(P18_6)) %>% 
  count(Estilo_cognitivo, P18_6) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P18_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P18_6=as.factor(P18_6)) %>% 
  count(Edad, P18_6) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P18_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P18_6=as.factor(P18_6)) %>% 
  count(Tamaño, P18_6) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P18_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°5: Uso de residuos de cosecha para energía", face = "bold", size = 14))


table(Datos_ENPA$P18_7)

g1=Datos_ENPA %>%
  mutate(P18_7=as.factor(P18_7)) %>% 
  count(P18_7) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P18_7, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor")+
  scale_x_discrete(labels = c("No ahora, si en 5 años","Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P18_7=as.factor(P18_7)) %>% 
  count(Estilo_cognitivo, P18_7) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P18_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P18_7=as.factor(P18_7)) %>% 
  count(Edad, P18_7) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P18_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P18_7=as.factor(P18_7)) %>% 
  count(Tamaño, P18_7) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P18_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°6: Uso de residuos de cosecha para biomateriales", face = "bold", size = 14))


table(Datos_ENPA$P18_8)

g1=Datos_ENPA %>%
  mutate(P18_8=as.factor(P18_8)) %>% 
  count(P18_8) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P18_8, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor")+
  scale_x_discrete(labels = c("No ahora, si en 5 años","Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P18_8=as.factor(P18_8)) %>% 
  count(Estilo_cognitivo, P18_8) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P18_8) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P18_8=as.factor(P18_8)) %>% 
  count(Edad, P18_8) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P18_8) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P18_8=as.factor(P18_8)) %>% 
  count(Tamaño, P18_8) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P18_8) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°7: Uso de biofertilizantes", face = "bold", size = 14))


table(Datos_ENPA$P18_9)

g1=Datos_ENPA %>%
  mutate(P18_9=as.factor(P18_9)) %>% 
  count(P18_9) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P18_9, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Comportamiento del productor") +
  scale_x_discrete(labels = c("No ahora, si en 5 años","Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P18_9=as.factor(P18_9)) %>% 
  count(Estilo_cognitivo, P18_9) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P18_9) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P18_9=as.factor(P18_9)) %>% 
  count(Edad, P18_9) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P18_9) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P18_9=as.factor(P18_9)) %>% 
  count(Tamaño, P18_9) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P18_9) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#2297E6","#61D04F","#F5C710","#DF536B"),name="Comportamiento del productor",labels = c("No ahora, si en 5 años", "Hoy en forma intensiva","Hoy en forma limitada","Ni ahora ni en 5 años"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°8: Uso de energías renovables", face = "bold", size = 14))

# Pregunta 25
# Análisis, diagnóstico de fertilidad y fertilización de suelos

# Últimos 12 meses
table(Datos_ENPA$P25a_1)

g1=Datos_ENPA %>%
  mutate(P25a_1=as.factor(P25a_1)) %>% 
  count(P25a_1) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25a_1, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25a_1=as.factor(P25a_1)) %>% 
  count(Estilo_cognitivo, P25a_1) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25a_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25a_1=as.factor(P25a_1)) %>% 
  count(Edad, P25a_1) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25a_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25a_1=as.factor(P25a_1)) %>% 
  count(Tamaño, P25a_1) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25a_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°9: Análisis, diagnóstico de fertilidad y fertilización de suelos (últimos 12 meses)", face = "bold", size = 14))

# Últimos 5 años
table(Datos_ENPA$P25b_1)

g1=Datos_ENPA %>%
  mutate(P25b_1=as.factor(P25b_1)) %>% 
  count(P25b_1) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25b_1, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25b_1=as.factor(P25b_1)) %>% 
  count(Estilo_cognitivo, P25b_1) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25b_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25b_1=as.factor(P25b_1)) %>% 
  count(Edad, P25b_1) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25b_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25b_1=as.factor(P25b_1)) %>% 
  count(Tamaño, P25b_1) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25b_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°10: Análisis, diagnóstico de fertilidad y fertilización de suelos (últimos 5 años)", face = "bold", size = 14))

# Porcentaje destinado
table(Datos_ENPA$P25c_1)

# Próximos 5 años
table(Datos_ENPA$P25d_1)

g1=Datos_ENPA %>%
  mutate(P25d_1=as.factor(P25d_1)) %>% 
  count(P25d_1) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25d_1, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25d_1=as.factor(P25d_1)) %>% 
  count(Estilo_cognitivo, P25d_1) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25d_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25d_1=as.factor(P25d_1)) %>% 
  count(Edad, P25d_1) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25d_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25d_1=as.factor(P25d_1)) %>% 
  count(Tamaño, P25d_1) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25d_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°11: Análisis, diagnóstico de fertilidad y fertilización de suelos (próximos 5 años)", face = "bold", size = 14))

# Rotación de cultivos
# Últimos 12 meses
table(Datos_ENPA$P25a_2)

g1=Datos_ENPA %>%
  mutate(P25a_2=as.factor(P25a_2)) %>% 
  count(P25a_2) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25a_2, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25a_2=as.factor(P25a_2)) %>% 
  count(Estilo_cognitivo, P25a_2) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25a_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25a_2=as.factor(P25a_2)) %>% 
  count(Edad, P25a_2) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25a_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25a_2=as.factor(P25a_2)) %>% 
  count(Tamaño, P25a_2) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25a_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°12: Rotación de cultivos (últimos 12 meses)", face = "bold", size = 14))

# Últimos 5 años
table(Datos_ENPA$P25b_2)

g1=Datos_ENPA %>%
  mutate(P25b_2=as.factor(P25b_2)) %>% 
  count(P25b_2) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25b_2, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25b_2=as.factor(P25b_2)) %>% 
  count(Estilo_cognitivo, P25b_2) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25b_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25b_2=as.factor(P25b_2)) %>% 
  count(Edad, P25b_2) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25b_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25b_2=as.factor(P25b_2)) %>% 
  count(Tamaño, P25b_2) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25b_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°13: Rotación de cultivos (últimos 5 años)", face = "bold", size = 14))

# Porcentaje destinado
table(Datos_ENPA$P25c_2)


# Próximos 5 años
table(Datos_ENPA$P25d_2)

g1=Datos_ENPA %>%
  mutate(P25d_2=as.factor(P25d_2)) %>% 
  count(P25d_2) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25d_2, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25d_2=as.factor(P25d_2)) %>% 
  count(Estilo_cognitivo, P25d_2) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25d_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25d_2=as.factor(P25d_2)) %>% 
  count(Edad, P25d_2) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25d_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25d_2=as.factor(P25d_2)) %>% 
  count(Tamaño, P25d_2) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25d_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°14: Rotación de cultivos (próximos 5 años)", face = "bold", size = 14))

# Rotación con cultivos de servicio

# últimos 12 meses

table(Datos_ENPA$P25a_3)

g1=Datos_ENPA %>%
  mutate(P25a_3=as.factor(P25a_3)) %>% 
  count(P25a_3) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25a_3, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25a_3=as.factor(P25a_3)) %>% 
  count(Estilo_cognitivo, P25a_3) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25a_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25a_3=as.factor(P25a_3)) %>% 
  count(Edad, P25a_3) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25a_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25a_3=as.factor(P25a_3)) %>% 
  count(Tamaño, P25a_3) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25a_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°15: Rotación con cultivos de servicio (últimos 12 meses)", face = "bold", size = 14))

# Últimos 5 años
table(Datos_ENPA$P25b_3)

g1=Datos_ENPA %>%
  mutate(P25b_3=as.factor(P25b_3)) %>% 
  count(P25b_3) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25b_3, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25b_3=as.factor(P25b_3)) %>% 
  count(Estilo_cognitivo, P25b_3) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25b_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25b_3=as.factor(P25b_3)) %>% 
  count(Edad, P25b_3) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25b_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25b_3=as.factor(P25b_3)) %>% 
  count(Tamaño, P25b_3) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25b_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°16: Rotación con cultivos de servicio (últimos 5 años)", face = "bold", size = 14))

# Porcentaje destinado
table(Datos_ENPA$P25c_3)

# Próximos 5 años
table(Datos_ENPA$P25d_3)

g1=Datos_ENPA %>%
  mutate(P25d_3=as.factor(P25d_3)) %>% 
  count(P25d_3) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25d_3, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25d_3=as.factor(P25d_3)) %>% 
  count(Estilo_cognitivo, P25d_3) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25d_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25d_3=as.factor(P25d_3)) %>% 
  count(Edad, P25d_3) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25d_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25d_3=as.factor(P25d_3)) %>% 
  count(Tamaño, P25d_3) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25d_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°17: Rotación con cultivos de servicio (próximos 5 años)", face = "bold", size = 14))

# Siembra directa

# Últimos 12 meses
table(Datos_ENPA$P25a_4)

g1=Datos_ENPA %>%
  mutate(P25a_4=as.factor(P25a_4)) %>% 
  count(P25a_4) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25a_4, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25a_4=as.factor(P25a_4)) %>% 
  count(Estilo_cognitivo, P25a_4) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25a_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25a_4=as.factor(P25a_4)) %>% 
  count(Edad, P25a_4) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25a_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25a_4=as.factor(P25a_4)) %>% 
  count(Tamaño, P25a_4) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25a_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°18: Siembra directa (últimos 12 meses)", face = "bold", size = 14))

# Últimos 5 años
table(Datos_ENPA$P25b_4)

g1=Datos_ENPA %>%
  mutate(P25b_4=as.factor(P25b_4)) %>% 
  count(P25b_4) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25b_4, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25b_4=as.factor(P25b_4)) %>% 
  count(Estilo_cognitivo, P25b_4) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25b_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25b_4=as.factor(P25b_4)) %>% 
  count(Edad, P25b_4) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25b_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25b_4=as.factor(P25b_4)) %>% 
  count(Tamaño, P25b_4) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25b_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°19: Siembra directa (últimos 5 años)", face = "bold", size = 14))

# Porcentaje destinado
table(Datos_ENPA$P25c_4)

# Próximos 5 años
table(Datos_ENPA$P25d_4)

g1=Datos_ENPA %>%
  mutate(P25d_4=as.factor(P25d_4)) %>% 
  count(P25d_4) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25d_4, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25d_4=as.factor(P25d_4)) %>% 
  count(Estilo_cognitivo, P25d_4) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25d_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25d_4=as.factor(P25d_4)) %>% 
  count(Edad, P25d_4) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25d_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25d_4=as.factor(P25d_4)) %>% 
  count(Tamaño, P25d_4) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25d_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°20: Siembra directa (próximos 5 años)", face = "bold", size = 14))

# Prácticas de conservación de suelos

# Últimos 12 meses
table(Datos_ENPA$P25a_5)

g1=Datos_ENPA %>%
  mutate(P25a_5=as.factor(P25a_5)) %>% 
  count(P25a_5) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25a_5, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25a_5=as.factor(P25a_5)) %>% 
  count(Estilo_cognitivo, P25a_5) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25a_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25a_5=as.factor(P25a_5)) %>% 
  count(Edad, P25a_5) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25a_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25a_5=as.factor(P25a_5)) %>% 
  count(Tamaño, P25a_5) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25a_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°21: Prácticas de conservación de suelos (últimos 12 meses)", face = "bold", size = 14))

# Últimos 5 años
table(Datos_ENPA$P25b_5)

g1=Datos_ENPA %>%
  mutate(P25b_5=as.factor(P25b_5)) %>% 
  count(P25b_5) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25b_5, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25b_5=as.factor(P25b_5)) %>% 
  count(Estilo_cognitivo, P25b_5) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25b_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25b_5=as.factor(P25b_5)) %>% 
  count(Edad, P25b_5) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25b_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25b_5=as.factor(P25b_5)) %>% 
  count(Tamaño, P25b_5) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25b_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°22: Prácticas de conservación de suelos (últimos 5 años)", face = "bold", size = 14))

# Porcentaje destinado
table(Datos_ENPA$P25c_5)

# Próximos 5 años
table(Datos_ENPA$P25d_5)

g1=Datos_ENPA %>%
  mutate(P25d_5=as.factor(P25d_5)) %>% 
  count(P25d_5) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25d_5, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25d_5=as.factor(P25d_5)) %>% 
  count(Estilo_cognitivo, P25d_5) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25d_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25d_5=as.factor(P25d_5)) %>% 
  count(Edad, P25d_5) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25d_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25d_5=as.factor(P25d_5)) %>% 
  count(Tamaño, P25d_5) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25d_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°23: Prácticas de conservación de suelos (próximos 5 años)", face = "bold", size = 14))

# Manejo integrado de plagas y enfermedades

# Últimos 12 meses
table(Datos_ENPA$P25a_6)

g1=Datos_ENPA %>%
  mutate(P25a_6=as.factor(P25a_6)) %>% 
  count(P25a_6) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25a_6, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25a_6=as.factor(P25a_6)) %>% 
  count(Estilo_cognitivo, P25a_6) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25a_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25a_6=as.factor(P25a_6)) %>% 
  count(Edad, P25a_6) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25a_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25a_6=as.factor(P25a_6)) %>% 
  count(Tamaño, P25a_6) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25a_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°24: Manejo integrado de plagas y enfermedades (últimos 12 meses)", face = "bold", size = 14))

# ültimos 5 años
table(Datos_ENPA$P25b_6)

g1=Datos_ENPA %>%
  mutate(P25b_6=as.factor(P25b_6)) %>% 
  count(P25b_6) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25b_6, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25b_6=as.factor(P25b_6)) %>% 
  count(Estilo_cognitivo, P25b_6) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25b_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25b_6=as.factor(P25b_6)) %>% 
  count(Edad, P25b_6) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25b_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25b_6=as.factor(P25b_6)) %>% 
  count(Tamaño, P25b_6) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25b_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°25: Manejo integrado de plagas y enfermedades (últimos 5 años)", face = "bold", size = 14))

# Porcentaje destinado
table(Datos_ENPA$P25c_6)
summary(Datos_ENPA$P25c_6[Datos_ENPA$P25c_6!=0])

# Próximos 5 años
table(Datos_ENPA$P25d_6)

g1=Datos_ENPA %>%
  mutate(P25d_6=as.factor(P25d_6)) %>% 
  count(P25d_6) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P25d_6, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P25d_6=as.factor(P25d_6)) %>% 
  count(Estilo_cognitivo, P25d_6) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P25d_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P25d_6=as.factor(P25d_6)) %>% 
  count(Edad, P25d_6) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P25d_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P25d_6=as.factor(P25d_6)) %>% 
  count(Tamaño, P25d_6) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P25d_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°26: Manejo integrado de plagas y enfermedades (próximos 5 años)", face = "bold", size = 14))



# Pregunta 22

table(Datos_ENPA$P22_1)
g1=Datos_ENPA %>%
  mutate(P22_1=as.factor(P22_1)) %>% 
  count(P22_1) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P22_1, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Usa la tecnología")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P22_1=as.factor(P22_1)) %>% 
  count(Estilo_cognitivo, P22_1) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P22_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_1=as.factor(P22_1)) %>% 
  count(Edad, P22_1) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P22_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_1=as.factor(P22_1)) %>% 
  count(Tamaño, P22_1) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P22_1) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°27: Uso de siembra y fertilización variable por ambiente", face = "bold", size = 14))


table(Datos_ENPA$P22_2)
g1=Datos_ENPA %>%
  mutate(P22_2=as.factor(P22_2)) %>% 
  count(P22_2) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P22_2, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Usa la tecnología")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P22_2=as.factor(P22_2)) %>% 
  count(Estilo_cognitivo, P22_2) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P22_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_2=as.factor(P22_2)) %>% 
  count(Edad, P22_2) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P22_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_2=as.factor(P22_2)) %>% 
  count(Tamaño, P22_2) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P22_2) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°28: Uso del piloto automático", face = "bold", size = 14))


table(Datos_ENPA$P22_5)
g1=Datos_ENPA %>%
  mutate(P22_5=as.factor(P22_5)) %>% 
  count(P22_5) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P22_5, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Usa la tecnología")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P22_5=as.factor(P22_5)) %>% 
  count(Estilo_cognitivo, P22_5) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P22_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_5=as.factor(P22_5)) %>% 
  count(Edad, P22_5) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P22_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_5=as.factor(P22_5)) %>% 
  count(Tamaño, P22_5) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P22_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°29: Uso del monitoreo satelital de los cultivos", face = "bold", size = 14))


table(Datos_ENPA$P22_6)
g1=Datos_ENPA %>%
  mutate(P22_6=as.factor(P22_6)) %>% 
  count(P22_6) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P22_6, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Usa la tecnología")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P22_6=as.factor(P22_6)) %>% 
  count(Estilo_cognitivo, P22_6) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P22_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_6=as.factor(P22_6)) %>% 
  count(Edad, P22_6) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P22_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_6=as.factor(P22_6)) %>% 
  count(Tamaño, P22_6) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P22_6) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°30: Uso de imágenes fotográficas a traves de robots y drones", face = "bold", size = 14))



table(Datos_ENPA$P22_7)
g1=Datos_ENPA %>%
  mutate(P22_7=as.factor(P22_7)) %>% 
  count(P22_7) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P22_7, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Usa la tecnología")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P22_7=as.factor(P22_7)) %>% 
  count(Estilo_cognitivo, P22_7) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P22_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_7=as.factor(P22_7)) %>% 
  count(Edad, P22_7) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P22_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_7=as.factor(P22_7)) %>% 
  count(Tamaño, P22_7) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P22_7) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°31: Uso de plataforma de gestión agrícola", face = "bold", size = 14))


table(Datos_ENPA$P22_8)
g1=Datos_ENPA %>%
  mutate(P22_8=as.factor(P22_8)) %>% 
  count(P22_8) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P22_8, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Usa la tecnología")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P22_8=as.factor(P22_8)) %>% 
  count(Estilo_cognitivo, P22_8) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P22_8) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_8=as.factor(P22_8)) %>% 
  count(Edad, P22_8) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P22_8) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_8=as.factor(P22_8)) %>% 
  count(Tamaño, P22_8) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P22_8) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°32: Uso de compra online de insumos", face = "bold", size = 14))



table(Datos_ENPA$P22_11)
g1=Datos_ENPA %>%
  mutate(P22_11=as.factor(P22_11)) %>% 
  count(P22_11) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P22_11, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Usa la tecnología")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P22_11=as.factor(P22_11)) %>% 
  count(Estilo_cognitivo, P22_11) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P22_11) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_11=as.factor(P22_11)) %>% 
  count(Edad, P22_11) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P22_11) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_11=as.factor(P22_11)) %>% 
  count(Tamaño, P22_11) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P22_11) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°33: Uso del riego por ambientes", face = "bold", size = 14))


table(Datos_ENPA$P22_12)
g1=Datos_ENPA %>%
  mutate(P22_12=as.factor(P22_12)) %>% 
  count(P22_12) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P22_12, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Usa la tecnología")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P22_12=as.factor(P22_12)) %>% 
  count(Estilo_cognitivo, P22_12) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P22_12) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P22_12=as.factor(P22_12)) %>% 
  count(Edad, P22_12) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P22_12) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P22_12=as.factor(P22_12)) %>% 
  count(Tamaño, P22_12) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P22_12) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Usa la \ntecnología",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°34: Uso de la pulverización selectiva", face = "bold", size = 14))

# Pregunta 38

table(Datos_ENPA$P38_3)

g1=Datos_ENPA %>%
  mutate(P38_3=as.factor(P38_3)) %>% 
  count(P38_3) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P38_3, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P38_3=as.factor(P38_3)) %>% 
  count(Estilo_cognitivo, P38_3) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P38_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P38_3=as.factor(P38_3)) %>% 
  count(Edad, P38_3) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P38_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P38_3=as.factor(P38_3)) %>% 
  count(Tamaño, P38_3) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P38_3) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°35: Realizó 1 o 2 compras virtuales de insumos agrícolas", face = "bold", size = 14))


table(Datos_ENPA$P38_4)

g1=Datos_ENPA %>%
  mutate(P38_4=as.factor(P38_4)) %>% 
  count(P38_4) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P38_4, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g2=Datos_ENPA %>%
  mutate(P38_4=as.factor(P38_4)) %>% 
  count(Estilo_cognitivo, P38_4) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P38_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P38_4=as.factor(P38_4)) %>% 
  count(Edad, P38_4) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P38_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P38_4=as.factor(P38_4)) %>% 
  count(Tamaño, P38_4) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P38_4) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°36: Realiza regularmente algunas compras online de insumos agrícolas", face = "bold", size = 14))


table(Datos_ENPA$P38_5)

g1=Datos_ENPA %>%
  mutate(P38_5=as.factor(P38_5)) %>% 
  count(P38_5) %>%       
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(P38_5, pct) +
  geom_bar(stat="identity", fill="#2297E6") +
  ylab("Porcentaje") +
  xlab("Lo realiza")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Comportamiento general") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))


g2=Datos_ENPA %>%
  mutate(P38_5=as.factor(P38_5)) %>% 
  count(Estilo_cognitivo, P38_5) %>%       
  group_by(Estilo_cognitivo) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Estilo_cognitivo, pct, fill=P38_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Estilo cognitivo del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según estilo cognitivo del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g3=Datos_ENPA %>%
  mutate(P38_5=as.factor(P38_5)) %>% 
  count(Edad, P38_5) %>%       
  group_by(Edad) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Edad, pct, fill=P38_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Edad del productor")+   scale_x_discrete(labels = c("Menor a\n35 años","35 a 44\naños","45 a 54\naños","55 a 64\naños","Mayor a\n65 años")) +
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según edad del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g4=Datos_ENPA %>%
  mutate(P38_5=as.factor(P38_5)) %>% 
  count(Tamaño, P38_5) %>%       
  group_by(Tamaño) %>%
  mutate(pct= prop.table(n) * 100) %>% 
  ggplot() + aes(Tamaño, pct, fill=P38_5) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#DF536B","#61D04F"),name="Lo realiza",labels = c("No", "Sí"))+
  ylab("Porcentaje") +
  xlab("Tamaño del productor")+
  geom_text(aes(label=paste(round(pct, 1),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Según tamaño del productor") +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

g=ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)

annotate_figure(g, top = text_grob("Gráfico N°37: Regularmente compra online tantos insumos agrícolas como pueda", face = "bold", size = 14))

# Analisis extra
# Nuevos negocios
a=table(Datos_ENPA$P16f,Datos_ENPA$P16g,Datos_ENPA$P16h)

a=as.data.frame(a)

# Nuevas prácticas
b=table(Datos_ENPA$P18_3,Datos_ENPA$P18_6,Datos_ENPA$P18_7,Datos_ENPA$P18_8,Datos_ENPA$P18_9)

b=as.data.frame(b)


c=table(Datos_ENPA$P25b_1,Datos_ENPA$P25b_3,Datos_ENPA$P25b_5,Datos_ENPA$P25b_6)

c=as.data.frame(c)


d=table(Datos_ENPA$P25d_1,Datos_ENPA$P25d_3,Datos_ENPA$P25d_5,Datos_ENPA$P25d_6)

d=as.data.frame(d)

# Nuevas tecnologías
e=table(Datos_ENPA$P22_1,Datos_ENPA$P22_2,Datos_ENPA$P22_5,Datos_ENPA$P22_6,
        Datos_ENPA$P22_7,Datos_ENPA$P22_8,Datos_ENPA$P22_11,Datos_ENPA$P22_12)

e=as.data.frame(e)
