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
Datos_ENPA$Tamaño = case_when(Datos_ENPA$P8a1<601 ~ " Mediano",
                              Datos_ENPA$P8a1<1841 ~ "Comercial",
                              Datos_ENPA$P8a1<10000 ~ "Grande",
                              T ~ "Mega")

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

# Análisis de tendencia de clusters
distancias_todo = daisy(Datos_ordinal[1:31],metric = "gower")

fviz_dist(distancias_todo,show_labels = F)

distancias_negocios = daisy(Datos_ordinal[1:3],metric = "gower")

a=fviz_dist(distancias_negocios,show_labels = F)+
  labs(title = "Nuevos negocios")

distancias_practicas = daisy(Datos_ordinal[4:20],metric = "gower")

b=fviz_dist(distancias_practicas,show_labels = F)+
  labs(title = "Nuevas prácticas")

distancias_tecnologias = daisy(Datos_ordinal[c(21:31)],metric = "gower")

c=fviz_dist(distancias_tecnologias,show_labels = F)+
  labs(title = "Nuevas tecnologías")

ggarrange(a,c)

# ANÁLISIS BASADO EN CORRESPONDENCIAS MÚLTIPLE

# Datos binarios
Datos_bin=Datos_ENPA %>% 
  dplyr::select(P25a_1,P25b_1,P25d_1,
                P25a_3,P25b_3,P25d_3,
                P25a_5,P25b_5,P25d_5,
                P25a_6,P25b_6,P25d_6,
                P22_1,P22_2,P22_5,P22_6,P22_7,P22_8,P22_11,P22_12,
                P38_3,P38_4,P38_5)


# Análisis correspondencia múltiple

MCA = MCA(Datos_bin, ncp=11, graph = FALSE)

MCA$eig

Datos_MCA=as.data.frame(MCA$ind$coord)


Datos_MCA = cbind.data.frame(Datos_MCA,Datos_ordinal[,1:8]) 

# Funciones kprototype y validación (Están armadas con las ponderaciones para el caso de correspondencia,
# si se quiere usar para los datos directamente hay que borrar los argumentos weights y lambda)
best_k_proto = function(data, k, i=20, cluster=T){
  
  clusters <- vector("list", length = i)
  medida=numeric(i)
  
  for(j in 1:i){
    resultado = kproto(data,k,type = "gower",lambda = c(rep(1,11),rep(1/4,8))) 
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

validation_kproto_fede = function(data, k, i=50){
  
  x=numeric(max(k))
  
  for(j in k){
    x[j]=best_k_proto(data,j,i,F)
  }
  cat("\n\n")
  return(x)
}

silhouette_fede = function(data,cluster,k){
  n=nrow(data)
  a=numeric(n)
  b=numeric(n)
  s=numeric(n)
  
  
  for(i in 1:n){
    c=cluster[i]
    A = data[unique(c(i,which(cluster==c))),]
    dist=daisy(A,metric = "gower",weights = c(rep(1,11),rep(1/4,8)))
    a[i]=mean(dist[1:(nrow(A)-1)])
    
    for(j in 1:k){
      if(j!=c){
        B = data[c(i,which(cluster==j)),]
        dist=daisy(B,metric = "gower",weights = c(rep(1,11),rep(1/4,8)))
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


# Gráfico para definir el número de clusters
m=validation_kproto_fede(Datos_MCA,1:7,i=50)

plot(m)

# 3 Clusters
w=best_k_proto(Datos_MCA,3,i=100)

silhouette_fede(Datos_MCA,w$cluster,3)

# 4 Clusters
x=best_k_proto(Datos_MCA,4,i=100)

silhouette_fede(Datos_MCA,x$cluster,4)

# 5 Clusters
y=best_k_proto(Datos_MCA,5,i=100)

silhouette_fede(Datos_MCA,y$cluster,5)


# ANÁLISIS CON LOS DATOS SIN TRANSFORMAR (Suele demorar más en correr asi que como mucho evaluaría
# 100 modelos de cada tipo (Argumento i en las funciones))

# Funciones ajustadas sin las ponderaciones
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

# Gráfico para definir el número de clusters
m=validation_kproto_fede(Datos_ordinal,1:7,i=50)

plot(m)

# 3 Clusters
a=best_k_proto(Datos_ordinal,3,i=100)

silhouette_fede(Datos_ordinal,a$cluster,3)

# 4 Clusters
b=best_k_proto(Datos_ordinal,4,i=100)

silhouette_fede(Datos_ordinal,b$cluster,4)

# 5 Clusters
c=best_k_proto(Datos_ordinal,5,i=100)

silhouette_fede(Datos_ordinal,c$cluster,5)
