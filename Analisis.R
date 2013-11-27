##### Carga de datos ############

# Sub prueba 1
s1f1<-read.csv("./DATA/S1-F1.csv", as.is=TRUE)
s1f2<-read.csv("./DATA/S1-F2.csv", as.is=TRUE)
s1f3<-read.csv("./DATA/S1-F3.csv", as.is=TRUE)
s1f4<-read.csv("./DATA/S1-F4.csv", as.is=TRUE)
s1f5<-read.csv("./DATA/S1-F5.csv", as.is=TRUE)

# Sub prueba 2
s2f1<-read.csv("./DATA/S2-F1.csv", as.is=TRUE)
s2f2<-read.csv("./DATA/S2-F2.csv", as.is=TRUE)
s2f3<-read.csv("./DATA/S2-F3.csv", as.is=TRUE)
s2f4<-read.csv("./DATA/S2-F4.csv", as.is=TRUE)
s2f5<-read.csv("./DATA/S2-F5.csv", as.is=TRUE)

# Sub prueba 3
s3f1<-read.csv("./DATA/S3-F1.csv", as.is=TRUE)
s3f2<-read.csv("./DATA/S3-F2.csv", as.is=TRUE)
s3f3<-read.csv("./DATA/S3-F3.csv", as.is=TRUE)
s3f4<-read.csv("./DATA/S3-F4.csv", as.is=TRUE)
s3f5<-read.csv("./DATA/S3-F5.csv", as.is=TRUE)

# Sub prueba 4
s4f1<-read.csv("./DATA/S4-F1.csv", as.is=TRUE)
s4f2<-read.csv("./DATA/S4-F2.csv", as.is=TRUE)
s4f3<-read.csv("./DATA/S4-F3.csv", as.is=TRUE)
s4f4<-read.csv("./DATA/S4-F4.csv", as.is=TRUE)
s4f5<-read.csv("./DATA/S4-F5.csv", as.is=TRUE)

# Claves

s1clave <- read.csv("./DATA/s1clave.csv", as.is=TRUE)
s2clave <- read.csv("./DATA/s2clave.csv", as.is=TRUE)
s3clave <- read.csv("./DATA/s3clave.csv", as.is=TRUE)
s4clave <- read.csv("./DATA/s4clave.csv", as.is=TRUE)



##### Paquetes a utilizar ######

library("eRm", lib.loc="/home/albert1t0/R/x86_64-pc-linux-gnu-library/3.0") # Para análisis de Rasch
library("CTT", lib.loc="/home/albert1t0/R/x86_64-pc-linux-gnu-library/3.0") # Para teoría clásica

############ Funciones adicionales para el Análisis #################

## Variante para poder guardar en excel los datos

distractor.analysis2 <- function (items, key = NA, scores = NA, p.table = FALSE, write.csv = NA) 
{
  items <- as.data.frame(items)
  if (length(key) == 1) 
    key <- c(rep(key, ncol(items)))
  if (missing(scores)) 
    scores <- as.data.frame(score(items, key)$score)
  if (missing(key)) 
    warning("No se encontro clave")
  else if (!length(key) == ncol(items)) {
    warning("No se encontró clave o algunas claves han sido omitidas.")
  }
  score.level <- quantile(scores[, 1], c(0, 1/3, 2/3, 1))
  score.level <- cut(scores[, 1], score.level, include.lowest = TRUE, 
                     labels = c("bajo", "medio", "alto"))
  itemtab <- function(response) {
    xtabs(~response + score.level)
  }
  itemtabp <- function(response) {
    round(prop.table(xtabs(~response + score.level), 2), 
          3)
  }
  all.levels <- sort(unique(unlist(items)))
  for (i in 1:ncol(items)) {
    items[, i] <- factor(items[, i], levels = all.levels, 
                         labels = ifelse(all.levels == key[i], paste("*", 
                                                                     all.levels, sep = ""), paste(" ", all.levels, 
                                                                                                  sep = "")))
  }
  out <- list()
  if (p.table == FALSE) 
    for (i in 1:ncol(items)) {
      out[[i]] <- itemtab(items[, i])
    }
  else for (i in 1:ncol(items)) {
    out[[i]] <- itemtabp(items[, i])
  }
  names(out) <- colnames(items)
  if (!is.na(write.csv == "NA")) {
    response <- ifelse(all.levels == key[i], paste("*", all.levels, 
                                                   sep = ""), paste(" ", all.levels, sep = ""))
    item <- c(rep(NA, length(all.levels)))
    for (i in 1:ncol(items)) {
      x <- as.data.frame(cbind(item, response, as.vector(out[[i]][, 
                                                                  1]), as.vector(out[[i]][, 2]), as.vector(out[[i]][, 
                                                                                                                    3])))
      names(x) <- c(paste("item:", i), "respuestas", "bajo", 
                    "medio", "alto")
      suppressWarnings(write.table(x, write.csv, row.names = FALSE, 
                                   na = " ", append = TRUE, sep=","))
    }
  }
  names(out) <- paste("item:", c(seq(1:ncol(items))))
  out
}

## Variante de Item análisis Teoría Clásica

item.anal<-function(items, key = NA, name = NA)
{
  if(!is.na(name == "NA" )) {
    name1<-paste(name,"_cnt.csv",sep="")
    name2<-paste(name,"_prop.csv",sep="")
    name3<-paste(name,"_itm.csv",sep="")
  }
  else { ## No se escribe archivos de distractores
    name1 <- NA
    name2 <- NA
    name3 <- NA
  }   
  distractores1<-distractor.analysis2(items,as.character(key),write.csv = name1)
  distractores2<-distractor.analysis2(items,as.character(key), p.table= TRUE, write.csv = name2)
  puntajes<-score(items,as.character(key),output.scored=TRUE)
  rel<-reliability(puntajes$scored)
  item.table <- as.data.frame(round(cbind(rel$item.mean,rel$pbis,rel$alpha.if.deleted),digits = 4))
  colnames(item.table)<-c("Dificultad","Corrrelación","AlphaExcluido")
  rownames(item.table)<-paste("Item",1:rel$N_item,sep="_")
  out <- list(Puntaje = rel$scale.mean, Puntaje.sd = rel$scale.sd, Personas = rel$N_person, Alpha = rel$alpha, Item = item.table, Distractores = distractores1, Distractores_Prop = distractores2, Calificacion = puntajes$score)
  write.table(item.table,name3,sep=",")
  class(out) <- "Análisis de items"
  out
}

##### Funciones adaptadas para Análisis de Rasch ##############


####### Análisis Clásico de Comprensión de textos ######

cla.s1f1 <- item.anal(s1f1[,4:43],s1clave[1,2:41],name="s1f1")
cla.s1f2 <- item.anal(s1f2[,4:43],s1clave[2,2:41],name="s1f2")
cla.s1f3 <- item.anal(s1f3[,4:43],s1clave[3,2:41],name="s1f3")
cla.s1f4 <- item.anal(s1f4[,4:43],s1clave[4,2:41],name="s1f4")
cla.s1f5 <- item.anal(s1f5[,4:43],s1clave[5,2:41],name="s1f5")


####### Análisis Clásico de Alfabetización Matemática ###### 

cla.s2f1 <- item.anal(s2f1[,4:43],s2clave[1,2:41],name="s2f1")
cla.s2f2 <- item.anal(s2f2[,4:43],s2clave[2,2:41],name="s2f2")
cla.s2f3 <- item.anal(s2f3[,4:43],s2clave[3,2:41],name="s2f3")
cla.s2f4 <- item.anal(s2f4[,4:43],s2clave[4,2:41],name="s2f4")
cla.s2f5 <- item.anal(s2f5[,4:43],s2clave[5,2:41],name="s2f5")


####### Análisis Clásico Desarrollo del estudiante ########

cla.s3f1 <- item.anal(s3f1[,4:43],s3clave[1,2:41],name="s3f1")
cla.s3f2 <- item.anal(s3f2[,4:43],s3clave[2,2:41],name="s3f2")
cla.s3f3 <- item.anal(s3f3[,4:43],s3clave[3,2:41],name="s3f3")
cla.s3f4 <- item.anal(s3f4[,4:43],s3clave[4,2:41],name="s3f4")
cla.s3f5 <- item.anal(s3f5[,4:43],s3clave[5,2:41],name="s3f5")

###### Análisis Clásico Enfoques pedagógicos #############

cla.s4f1 <- item.anal(s4f1[,4:42],s4clave[1,2:40],name="s4f1")
cla.s4f2 <- item.anal(s4f2[,4:41],s4clave[2,2:39],name="s4f2")
cla.s4f3 <- item.anal(s4f3[,4:42],s4clave[3,2:40],name="s4f3")
cla.s4f4 <- item.anal(s4f4[,4:42],s4clave[4,2:40],name="s4f4")
cla.s4f5 <- item.anal(s4f5[,4:42],s4clave[5,2:40],name="s4f5")

##### Preparación Análisis de Rasch ##############

########### Combinar formas para análsis completo por áreas ##########

# Comprensión de textos
s1 <- merge(s1f1,s1f2, all.x=TRUE, all.y=TRUE)
s1 <- merge(s1,s1f3, all.x=TRUE, all.y=TRUE)
s1 <- merge(s1,s1f4, all.x=TRUE, all.y=TRUE)
s1 <- merge(s1,s1f5, all.x=TRUE, all.y=TRUE)

# Alfabetización Matemática
s2 <- merge(s2f1,s2f2, all.x=TRUE, all.y=TRUE)
s2 <- merge(s2,s2f3, all.x=TRUE, all.y=TRUE)
s2 <- merge(s2,s2f4, all.x=TRUE, all.y=TRUE)
s2 <- merge(s2,s2f5, all.x=TRUE, all.y=TRUE)

# Desarrollo del estudiante
s3 <- merge(s3f1,s3f2, all.x=TRUE, all.y=TRUE)
s3 <- merge(s3,s3f3, all.x=TRUE, all.y=TRUE)
s3 <- merge(s3,s3f4, all.x=TRUE, all.y=TRUE)
s3 <- merge(s3,s3f5, all.x=TRUE, all.y=TRUE)

# Enfoques pedagógicos
s4 <- merge(s4f1,s4f2, all.x=TRUE, all.y=TRUE)
s4 <- merge(s4,s4f3, all.x=TRUE, all.y=TRUE)
s4 <- merge(s4,s4f4, all.x=TRUE, all.y=TRUE)
s4 <- merge(s4,s4f5, all.x=TRUE, all.y=TRUE)

### Etiquetas para elborar las claves ####

write.table(names(s1),"s1-labels.csv", sep=",")
write.table(names(s2),"s2-labels.csv", sep=",")
write.table(names(s3),"s3-labels.csv", sep=",")
write.table(names(s4),"s4-labels.csv", sep=",")

###### Claves por sección #######

clave.s1 <- c("C","A","B","A","C","D","A","D","C","B","B","C","B","C","D","D","D","D","A","A","A","D","C","C","D","C","A","D","B","C","C","C","A","C","B","C","D","A","D","B","C","D","C","A","D","C","A","D","B","B","A","B","A","B","D","D","A","C","B","C","B","D","C","C","A","C","D","A","A","B","C","A","D","A","B","C","D","B","A","B","C","B","B","B","D","C","A","D","B","A","D","D","A","C","B","D","A","C","D","B")
clave.s2 <- c("C","B","A","D","C","C","C","A","D","B","A","A","A","C","C","C","D","A","A","D","D","B","D","C","C","D","A","A","B","B","A","B","C","D","B","B","B","A","A","C","A","B","C","C","A","C","C","A","C","B","B","A","C","A","A","B","B","E","C","C","B","D","B","A","B","D","D","D","A","C","D","A","D","B","D","E","C","D","D","D","B","A","A","A","A","B","C","B","D","D","A","B","B","C","B","B","B","B","B","D")
clave.s3 <- c("C","C","A","D","D","B","D","A","C","B","A","C","C","D","B","A","B","B","D","A","C","D","C","B","A","A","D","C","B","D","D","A","C","A","C","B","B","D","D","A","D","A","A","D","C","C","A","D","B","B","A","B","D","A","C","D","A","B","B","C","C","A","C","B","D","B","A","D","C","D","A","B","C","A","A","D","B","B","D","C","B","D","C","A","D","B","C","A","C","D","C","B","B","A","D","C","A","A","D","B")
clave.s4 <- c("B","C","C","A","C","D","C","D","B","C","B","C","C","B","A","A","C","D","C","B","B","B","C","C","D","A","A","C","B","B","A","D","C","D","A","B","B","B","C","B","D","D","B","C","D","B","A","A","C","B","A","B","D","C","B","D","D","C","B","A","C","D","D","B","B","A","B","A","B","B","B","A","B","A","A","C","B","C","D","D","C ","C","C","B","A","D","D","B","D","B","D","C","A","A","A","A","A")

######### Calificar matrices con buenas y malas ############

califica.s1 <- as.data.frame(score(s1[,4:103],clave.s1, output.scored = TRUE))
califica.s2 <- as.data.frame(score(s2[,4:103],clave.s2, output.scored = TRUE))
califica.s3 <- as.data.frame(score(s3[,4:103],clave.s3, output.scored = TRUE))
califica.s4 <- as.data.frame(score(s4[,4:100],clave.s4, output.scored = TRUE))

names(califica.s1) <- substr(names(califica.s1),8,14)
names(califica.s2) <- substr(names(califica.s2),8,14)
names(califica.s3) <- substr(names(califica.s3),8,14)
names(califica.s4) <- substr(names(califica.s4),8,14)

### Ajuste de Rasch de las Matrices de respuestas #####

rasch.s1 <- RM(califica.s1[,2:101], se= TRUE, sum0 = TRUE)
rasch.s2 <- RM(califica.s2[,2:101], se= TRUE, sum0 = TRUE)
rasch.s3 <- RM(califica.s3[,2:101], se= TRUE, sum0 = TRUE)
rasch.s4 <- RM(califica.s4[,2:98], se= TRUE, sum0 = TRUE)

person.s1 <- person.parameter(rasch.s1)
person.s2 <- person.parameter(rasch.s2)
person.s3 <- person.parameter(rasch.s3)
person.s4 <- person.parameter(rasch.s4)

### Prueba de ajuste ####

lrres.s1 <- LRtest(rasch.s1, splitcr = "mean", se = TRUE)
lrres.s2 <- LRtest(rasch.s2, splitcr = "mean", se = TRUE)
lrres.s3 <- LRtest(rasch.s3, splitcr = "mean", se = TRUE)
lrres.s4 <- LRtest(rasch.s4, splitcr = "mean", se = TRUE)

##### Gráficas de ajuste #########

plotGOF(lrres.s1, ctrline = list(gamma = 0.95, col = "red", lty = "dashed"), main = "Ajuste - Comprensión de textos", cex = .5, xlab = "Grupo 1 - Bajo el promedio", ylab = "Grupo 2 - Sobre el promedio")
plotGOF(lrres.s2, ctrline = list(gamma = 0.95, col = "red", lty = "dashed"), main = "Ajuste - Alfabetización Matemática", cex = .5, xlab = "Grupo 1 - Bajo el promedio", ylab = "Grupo 2 - Sobre el promedio")
plotGOF(lrres.s3, ctrline = list(gamma = 0.95, col = "red", lty = "dashed"), main = "Ajuste - Desarrollo del estudiante", cex = .5, xlab = "Grupo 1 - Bajo el promedio", ylab = "Grupo 2 - Sobre el promedio")
plotGOF(lrres.s4, ctrline = list(gamma = 0.95, col = "red", lty = "dashed"), main = "Ajuste - Enfoques pedagógicos", cex = .5, xlab = "Grupo 1 - Bajo el promedio", ylab = "Grupo 2 - Sobre el promedio")

# Para el primer grupo por colores
library(colorspace)
colors <- rainbow_hcl(20)

plotGOF(lrres.s1, beta.subset = 1:20, conf = list(which = 1:20, col = colors), main = "Gráfico de Ajuste S1", cex = .7, xlab = "Grupo 1 - Bajo el promedio", ylab = "Grupo 2 - Sobre el promedio")
plotGOF(lrres.s2, beta.subset = 1:20, conf = list(which = 1:20, col = colors), main = "Gráfico de Ajuste S2", cex = .7, xlab = "Grupo 1 - Bajo el promedio", ylab = "Grupo 2 - Sobre el promedio")
plotGOF(lrres.s3, beta.subset = 1:20, conf = list(which = 1:20, col = colors), main = "Gráfico de Ajuste S3", cex = .7, xlab = "Grupo 1 - Bajo el promedio", ylab = "Grupo 2 - Sobre el promedio")
plotGOF(lrres.s4, beta.subset = 1:20, conf = list(which = 1:20, col = colors), main = "Gráfico de Ajuste S4", cex = .7, xlab = "Grupo 1 - Bajo el promedio", ylab = "Grupo 2 - Sobre el promedio")

# Mapa de items

plotPImap(rasch.s1, sorted = TRUE, cex.gen = .4, main = "Mapa de Ítemes S1", latdim = "Comprensión de textos" )
plotPImap(rasch.s2, sorted = TRUE, cex.gen = .4, main = "Mapa de Ítemes S2", latdim = "Alfabetización Matemática" )
plotPImap(rasch.s3, sorted = TRUE, cex.gen = .4, main = "Mapa de Ítemes S3", latdim = "Desarrollo del estudiante" )
plotPImap(rasch.s4, sorted = TRUE, cex.gen = .4, main = "Mapa de Ítemes S4", latdim = "Enfoques pedagógicos" )

# Curva de los ítemes

plotICC(rasch.s1, ask = FALSE, empICC = list("raw"), empCI = list(lty = "solid"))


########## Grabando tablas de ítemes análisis clásico ############

write.table(cla.s1f1$Item,"Items-s1f1.csv",sep=",")
write.table(cla.s1f2$Item,"Items-s1f2.csv",sep=",")
write.table(cla.s1f3$Item,"Items-s1f3.csv",sep=",")
write.table(cla.s1f4$Item,"Items-s1f4.csv",sep=",")
write.table(cla.s1f5$Item,"Items-s1f5.csv",sep=",")

write.table(cla.s2f1$Item,"Items-s2f1.csv",sep=",")
write.table(cla.s2f2$Item,"Items-s2f2.csv",sep=",")
write.table(cla.s2f3$Item,"Items-s2f3.csv",sep=",")
write.table(cla.s2f4$Item,"Items-s2f4.csv",sep=",")
write.table(cla.s2f5$Item,"Items-s2f5.csv",sep=",")

write.table(cla.s3f1$Item,"Items-s3f1.csv",sep=",")
write.table(cla.s3f2$Item,"Items-s3f2.csv",sep=",")
write.table(cla.s3f3$Item,"Items-s3f3.csv",sep=",")
write.table(cla.s3f4$Item,"Items-s3f4.csv",sep=",")
write.table(cla.s3f5$Item,"Items-s3f5.csv",sep=",")

write.table(cla.s4f1$Item,"Items-s4f1.csv",sep=",")
write.table(cla.s4f2$Item,"Items-s4f2.csv",sep=",")
write.table(cla.s4f3$Item,"Items-s4f3.csv",sep=",")
write.table(cla.s4f4$Item,"Items-s4f4.csv",sep=",")
write.table(cla.s1f5$Item,"Items-s1f5.csv",sep=",")
