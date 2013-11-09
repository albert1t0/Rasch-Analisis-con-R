##### Paquetes a utilizar ######
library("eRm", lib.loc="/home/albert1t0/R/x86_64-pc-linux-gnu-library/3.0") # Para análisis de Rasch
library("CTT", lib.loc="/home/albert1t0/R/x86_64-pc-linux-gnu-library/3.0") # Para teoría clásica

##### Carga de datos ############

# Forma 1
s1f1<-read.csv("./DATA/S1-F1.csv", header=TRUE, sep=",", as.is=TRUE)
s1f2<-read.csv("./DATA/S1-F2.csv", header=TRUE, sep=",", as.is=TRUE)
s1f3<-read.csv("./DATA/S1-F3.csv", header=TRUE, sep=",", as.is=TRUE)
s1f4<-read.csv("./DATA/S1-F4.csv", header=TRUE, sep=",", as.is=TRUE)
s1f5<-read.csv("./DATA/S1-F5.csv", header=TRUE, sep=",", as.is=TRUE)

# Forma 2
s2f1<-read.csv("./DATA/S2-F1.csv", header=TRUE, sep=",", as.is=TRUE)
s2f2<-read.csv("./DATA/S2-F2.csv", header=TRUE, sep=",", as.is=TRUE)
s2f3<-read.csv("./DATA/S2-F3.csv", header=TRUE, sep=",", as.is=TRUE)
s2f4<-read.csv("./DATA/S2-F4.csv", header=TRUE, sep=",", as.is=TRUE)
s2f5<-read.csv("./DATA/S2-F5.csv", header=TRUE, sep=",", as.is=TRUE)

# Forma 3
s3f1<-read.csv("./DATA/S3-F1.csv", header=TRUE, sep=",", as.is=TRUE)
s3f2<-read.csv("./DATA/S3-F2.csv", header=TRUE, sep=",", as.is=TRUE)
s3f3<-read.csv("./DATA/S3-F3.csv", header=TRUE, sep=",", as.is=TRUE)
s3f4<-read.csv("./DATA/S3-F4.csv", header=TRUE, sep=",", as.is=TRUE)
s3f5<-read.csv("./DATA/S3-F5.csv", header=TRUE, sep=",", as.is=TRUE)

# Forma 4
s4f1<-read.csv("./DATA/S4-F1.csv", header=TRUE, sep=",", as.is=TRUE)
s4f2<-read.csv("./DATA/S4-F2.csv", header=TRUE, sep=",", as.is=TRUE)
s4f3<-read.csv("./DATA/S4-F3.csv", header=TRUE, sep=",", as.is=TRUE)
s4f4<-read.csv("./DATA/S4-F4.csv", header=TRUE, sep=",", as.is=TRUE)
s4f5<-read.csv("./DATA/S4-F5.csv", header=TRUE, sep=",", as.is=TRUE)

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

