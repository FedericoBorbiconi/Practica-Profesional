library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(ggpubr)
library(fastDummies)
library(cluster)
library(factoextra)
library(clustMixType)

Datos_ENPA <- read_excel("C:/Users/feder/Desktop/Datos ENPA.xlsx")

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
Datos_ENPA$Tamaño = case_when(Datos_ENPA$P8a1<601 ~ " Mediano",
                              Datos_ENPA$P8a1<1841 ~ "Comercial",
                              Datos_ENPA$P8a1<10000 ~ "Grande",
                              T ~ "Mega")

# Datos Dummy
Datos_Dummy=Datos_ENPA %>% 
  dplyr::select(P16f,P16g,P16h,
                P18_3,P18_6,P18_7,P18_8,P18_9,
                P25a_1,P25b_1,P25c_1,P25d_1,
                P25a_3,P25b_3,P25c_3,P25d_3,
                P25a_5,P25b_5,P25c_5,P25d_5,
                P25a_6,P25b_6,P25c_6,P25d_6,
                P22_1,P22_2,P22_5,P22_6,P22_7,P22_8,P22_11,P22_12,
                P38_3,P38_4,P38_5)


Datos_Dummy = dummy_cols(Datos_Dummy)

Datos_Dummy = Datos_Dummy[,36:119]

# Distancia asimetrica

distancias = dist(Datos_Dummy, method = "binary")

fviz_dist(distancias)

# Cluster complete
cluster_complete = hclust(distancias, method = "complete")

a=as.factor(cutree(cluster_complete,3))

table(a)

dist_clust = cophenetic(cluster_complete)

cor(distancias,dist_clust)

# Cluster Ward.D
cluster_ward.D = hclust(distancias, method = "ward.D")

b=as.factor(cutree(cluster_ward.D,3))

table(b)

dist_clust = cophenetic(cluster_ward.D)

cor(distancias,dist_clust)

# Cluster Ward.D2
cluster_ward.D2 = hclust(distancias, method = "ward.D2")

c=as.factor(cutree(cluster_ward.D2,3))

table(c)

dist_clust = cophenetic(cluster_ward.D2)

cor(distancias,dist_clust)


# BASE ORDINAL
Datos_ordinal=Datos_ENPA %>% 
  dplyr::select(P16f,P16g,P16h,
                P18_3,P18_6,P18_7,P18_8,P18_9,
                P25a_1,P25b_1,P25d_1,
                P25a_3,P25b_3,P25d_3,
                P25a_5,P25b_5,P25d_5,
                P25a_6,P25b_6,P25d_6,
                P22_1,P22_2,P22_5,P22_6,P22_7,P22_8,P22_11,P22_12,
                P38_3,P38_4,P38_5)

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

# distancia de gower

distancias_gower = daisy(Datos_ordinal,metric = "gower", type = list(ordratio=1:8,asymm = 9:31))

fviz_dist(distancias_gower)

# Cluster complete
cluster_complete = hclust(distancias_gower, method = "complete")

d=as.factor(cutree(cluster_complete,3))

table(d)

dist_clust = cophenetic(cluster_complete)

cor(distancias,dist_clust)

# Cluster Ward.D
cluster_ward.D = hclust(distancias, method = "ward.D")

e=as.factor(cutree(cluster_ward.D,3))

table(e)

dist_clust = cophenetic(cluster_ward.D)

cor(distancias,dist_clust)

# Cluster Ward.D2
cluster_ward.D2 = hclust(distancias, method = "ward.D2")

f=as.factor(cutree(cluster_ward.D2,3))

table(f)

dist_clust = cophenetic(cluster_ward.D2)

cor(distancias,dist_clust)

# K-Prototype
cluster_kproto = kproto(Datos_ordinal,3,type = "gower")
g=cluster_kproto$cluster
table(g)

# validation_kproto() Esta función tira varias métricas para evaluar los clusters pero solo funciona con type=standard
# El paper tiene algunas recomendaciones de cual usar que se podrían implementar en la función
best_k_proto = function(data, k, i=20){
  
  clusters <- vector("list", length = i)
  medida=numeric(i)
  
  for(j in 1:i){
    resultado = kproto(data,k,type = "gower")
    clusters[[j]]=resultado$cluster
    medida[j]=resultado$tot.withinss
  }
  
  cat(which.min(medida))
  cat("\n")
  cat(medida)
  return(clusters[[which.min(medida)]])
  
}

y=best_k_proto(Datos_ordinal,3,i=20)
