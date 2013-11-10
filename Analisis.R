##### Carga de datos ############

# Forma 1
s1f1<-read.csv("./DATA/S1-F1.csv", as.is=TRUE)
s1f2<-read.csv("./DATA/S1-F2.csv", as.is=TRUE)
s1f3<-read.csv("./DATA/S1-F3.csv", as.is=TRUE)
s1f4<-read.csv("./DATA/S1-F4.csv", as.is=TRUE)
s1f5<-read.csv("./DATA/S1-F5.csv", as.is=TRUE)

# Forma 2
s2f1<-read.csv("./DATA/S2-F1.csv", as.is=TRUE)
s2f2<-read.csv("./DATA/S2-F2.csv", as.is=TRUE)
s2f3<-read.csv("./DATA/S2-F3.csv", as.is=TRUE)
s2f4<-read.csv("./DATA/S2-F4.csv", as.is=TRUE)
s2f5<-read.csv("./DATA/S2-F5.csv", as.is=TRUE)

# Forma 3
s3f1<-read.csv("./DATA/S3-F1.csv", as.is=TRUE)
s3f2<-read.csv("./DATA/S3-F2.csv", as.is=TRUE)
s3f3<-read.csv("./DATA/S3-F3.csv", as.is=TRUE)
s3f4<-read.csv("./DATA/S3-F4.csv", as.is=TRUE)
s3f5<-read.csv("./DATA/S3-F5.csv", as.is=TRUE)

# Forma 4
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

####### Análisis Clásico de Comprensión de textos ######

cla.s1f1 <- item.anal(s1f1[,4:43],s1clave[1,2:41],name="s1f1")
cla.s1f2 <- item.anal(s1f2[,4:43],s1clave[2,2:41],name="s1f2")
cla.s1f3 <- item.anal(s1f3[,4:43],s1clave[3,2:41],name="s1f3")
cla.s1f4 <- item.anal(s1f4[,4:43],s1clave[4,2:41],name="s1f4")
cla.s1f5 <- item.anal(s1f5[,4:43],s1clave[5,2:41],name="s1f5")


####### Análisis Clásico de Alfabetización Matemática ######  CLAVES INCOMPLETAS !!!!

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

